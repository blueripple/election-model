{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE StrictData #-}

module Main
  (main)
where

import qualified BlueRipple.Configuration as BR
import qualified BlueRipple.Data.Types.Demographic as DT
import qualified BlueRipple.Data.Types.Geographic as GT
import qualified BlueRipple.Data.Types.Modeling as MT
import qualified BlueRipple.Data.ACS_PUMS as ACS
import qualified BlueRipple.Data.ACS_Tables as BRC
import qualified BlueRipple.Data.Small.DataFrames as BRDF
import qualified BlueRipple.Data.Small.Loaders as BRL
import qualified BlueRipple.Utilities.KnitUtils as BRK
import qualified BlueRipple.Data.CachingCore as BRCC
import qualified BlueRipple.Data.LoadersCore as BRLC
import qualified BlueRipple.Model.Demographic.DataPrep as DDP
import qualified BlueRipple.Model.Election2.DataPrep as DP
import qualified BlueRipple.Model.Election2.ModelCommon as MC
import qualified BlueRipple.Model.Election2.ModelCommon2 as MC2
import qualified BlueRipple.Model.Election2.ModelRunner as MR
import qualified BlueRipple.Utilities.HvegaJsonData as BRHJ
import qualified BlueRipple.Data.Redistricting as BLR
import qualified BlueRipple.Tools.StateLeg.Analysis as BSL
import qualified BlueRipple.Tools.StateLeg.Visualization as BSLV

import qualified Knit.Report as K
import qualified Knit.Effect.AtomicCache as KC
import qualified Text.Pandoc.Error as Pandoc
import qualified System.Console.CmdArgs as CmdArgs

import qualified Control.Foldl as FL
import Control.Lens (view, (^.))

import qualified Data.Vinyl.TypeLevel as V
import qualified Frames as F
import qualified Frames.Melt as F
import qualified Frames.Streamly.InCore as FSI

import Path (Dir, Rel)
import qualified Path
import qualified System.Environment as Env

import qualified Data.Map.Strict as M

import qualified Graphics.Vega.VegaLite.Configuration as FV


templateVars ∷ Map String String
templateVars =
  M.fromList
    [ ("lang", "English")
    , ("site-title", "Blue Ripple Politics")
    , ("home-url", "https://www.blueripplepolitics.org")
    --  , ("author"   , T.unpack yamlAuthor)
    ]

pandocTemplate ∷ K.TemplatePath
pandocTemplate = K.FullySpecifiedTemplatePath "../../research/pandoc-templates/blueripple_basic.html"


main :: IO ()
main = do
  cmdLine ← CmdArgs.cmdArgsRun BR.commandLine
  pandocWriterConfig ←
    K.mkPandocWriterConfig
    pandocTemplate
    templateVars
    (BRK.brWriterOptionsF . K.mindocOptionsF)
  cacheDir <- toText . fromMaybe ".kh-cache" <$> Env.lookupEnv("BR_CACHE_DIR")
  let knitConfig ∷ K.KnitConfig BRCC.SerializerC BRCC.CacheData Text =
        (K.defaultKnitConfig $ Just cacheDir)
          { K.outerLogPrefix = Just "2023-ElectionModel"
          , K.logIf = BR.knitLogSeverity $ BR.logLevel cmdLine -- K.logDiagnostic
          , K.pandocWriterConfig = pandocWriterConfig
          , K.serializeDict = BRCC.flatSerializeDict
          , K.persistCache = KC.persistStrictByteString (\t → toString (cacheDir <> "/" <> t))
          }
  resE ← K.knitHtmls knitConfig $ do
    K.logLE K.Info $ "Command Line: " <> show cmdLine
--    modelComparison cmdLine
    jointComparison cmdLine "PA"
  case resE of
    Right namedDocs →
      K.writeAllPandocResultsWithInfoAsHtml "" namedDocs
    Left err → putTextLn $ "Pandoc Error: " <> Pandoc.renderError err

jointComparison :: (K.KnitMany r, BRCC.CacheEffects r) => BR.CommandLine -> Text -> K.Sem r ()
jointComparison cmdLine sa = do
  let postInfo = BR.PostInfo (BR.postStage cmdLine) (BR.PubTimes BR.Unpublished Nothing)
      dmr = MC.tDesignMatrixRow_d
      agg = MC.WeightedAggregation MC.ContinuousBinomial DP.DesignEffectWeights
      cesAll = MC.CESSurvey (DP.AllSurveyed DP.Both)
      alphaModel = MC.St_A_S_E_R_StA_StS_StE_StR_AS_AE_AR_SE_SR_ER_StER
      actionConfig = MC.ActionConfig cesAll (MC.ModelConfig agg alphaModel (contramap F.rcast dmr))
      prefConfig = MC.PrefConfig (DP.Validated DP.Both) (MC.ModelConfig agg alphaModel (contramap F.rcast dmr))
  jointCompPostPaths <- postPaths "JointComp" cmdLine
  productPS_C <- BSL.modelC BSL.Prod actionConfig Nothing prefConfig Nothing sa
  K.ignoreCacheTime productPS_C >>= putStrLn . show . MC.unPSMap
  modelPS_C <- BSL.modelC BSL.Modeled actionConfig Nothing prefConfig Nothing sa
  K.ignoreCacheTime modelPS_C >>= putStrLn . show . MC.unPSMap
  draSLD_C <- BLR.allPassedSLD 2024 BRC.TY2021
  upperOnlyMap <- BRL.stateUpperOnlyMap
  singleCDMap <- BRL.stateSingleCDMap
  let withDRA m_C d_C = flip K.wctBind ((,) <$> m_C <*> d_C)
        $ \(model, dra) -> BSL.modelAndDRA model dra upperOnlyMap singleCDMap sa
      productDRA_C = withDRA productPS_C draSLD_C
      modelDRA_C = withDRA modelPS_C draSLD_C
  products <- K.ignoreCacheTime productDRA_C
  modeled <- K.ignoreCacheTime modelDRA_C
  BRK.brNewPost jointCompPostPaths postInfo "JointComp" $ do
    demoCompChart <- BSLV.modelDRAComparisonChart  jointCompPostPaths postInfo
      "demoComp" "Joint Table Comparison" (FV.fixedSizeVC 500 500 5)
      "Joint Table Type"
      [BSLV.LabeledFrame "Product" products
      ,BSLV.LabeledFrame "Modeled" modeled
      ]
    _ <- K.addHvega Nothing Nothing demoCompChart
    pure ()

modelComparison :: (K.KnitMany r, BRCC.CacheEffects r) => BR.CommandLine -> K.Sem r ()
modelComparison cmdLine = do
  let postInfo = BR.PostInfo (BR.postStage cmdLine) (BR.PubTimes BR.Unpublished Nothing)
      dmr = MC.tDesignMatrixRow_d
      cesVV = MC.CESSurvey (DP.Validated DP.Both)
      cesAll = MC.CESSurvey (DP.AllSurveyed DP.Both)
      agg = MC.WeightedAggregation MC.ContinuousBinomial DP.DesignEffectWeights
--        aggregations = [agg,  MC.WeightedAggregation MC.ContinuousBinomial DP.FullWeights, MC.WeightedAggregation MC.ContinuousBinomial DP.DesignEffectWeights]
      aggregations = [agg]
      alphaModelsT = [MC.A_S_E_R, MC.A_S_E_R_AS_AE_AR_SE_SR_ER, MC.St_A_S_E_R_StA_StS_StE_StR_AS_AE_AR_SE_SR_ER_StER]
      alphaModelsP = [MC.A_S_E_R, MC.A_S_E_R_AS_AE_AR_SE_SR_ER, MC.St_A_S_E_R_StA_StS_StE_StR_AS_AE_AR_SE_SR_ER_StER]
  let modelDir = "model/election2/stan/"
      cacheDir = "model/election2/"

  modelPostPaths <- postPaths "Models" cmdLine
  BRK.brNewPost modelPostPaths postInfo "Models" $ do
    let (srcWindow, cachedSrc) = ACS.acs1Yr2012_22
    acsA5ByState_C <- DDP.cachedACSa5ByState srcWindow cachedSrc 2022

    acsByPUMA_C <- cachedACSByPUMA

    acsByState_C <- BRCC.retrieveOrMakeD "model/election2/acsByStatePS.bin" acsA5ByState_C $ \acsFull ->
      pure $ DP.PSData @'[GT.StateAbbreviation] . fmap F.rcast . F.filterFrame ((== DT.Citizen) . view DT.citizenC) $ acsFull
    cesByCD_C <- DP.cesCountedDemPresVotesByCD False (DP.AllSurveyed DP.Both)
                 >>= DP.cachedPreppedCES (Right "analysis/election2/cesByCD.bin")
    (F.filterFrame ((== 0) . view DT.pWPopPerSqMile)  <$> K.ignoreCacheTime cesByCD_C) >>= BRLC.logFrame
      --K.knitError "STOP"
--      cesUW_C <- BRCC.retrieveOrMakeD "analysis/election2/cesUW.bin" cesByCD_C
--                 $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac . view DP.surveyed) (view DT.pWPopPerSqMile)

--      cesW_C <- BRCC.retrieveOrMakeD "analysis/election2/cesW.bin" cesByCD_C
--                  $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000*) .  view DP.surveyWeight) (view DT.pWPopPerSqMile)


    let  withoutDC :: (F.ElemOf (DP.PSDataR ks) GT.StateAbbreviation, FSI.RecVec (DP.PSDataR ks)) => DP.PSData ks -> DP.PSData ks
         withoutDC = DP.PSData . F.filterFrame ((/= "DC") . view GT.stateAbbreviation) . DP.unPSData

    presidentialElections_C <- BRL.presidentialElectionsWithIncumbency
    let dVSPres2020 = MR.VoteDTargets $ DP.ElexTargetConfig "Pres" (pure mempty) 2020 presidentialElections_C
--      houseElections_C <- BRL.houseElectionsWithIncumbency
--      let dVSHouse2022 = DP.ElexTargetConfig "House" (pure mempty) 2022 houseElections_C
    let actionConfig agg am = MC.ActionConfig cesAll (MC.ModelConfig agg am (contramap F.rcast dmr))
        cacheStructureF rerun gqName = case rerun of
          False -> MR.CacheStructure (Right modelDir) (Right cacheDir) gqName "AllCells" gqName
          True ->  MR.CacheStructure (Left modelDir) (Left cacheDir) gqName "AllCells" gqName
        runTurnoutModel psData gqName agg am =
          fst <<$>> MR.runBaseModel 2020
          (MR.modelCacheStructure $ cacheStructureF False gqName)
          (MC2.ActionOnly MC.Vote (actionConfig agg am)) psData
        runTurnoutModelAH psData gqName agg am =
          MR.runActionModelAH 2020 (cacheStructureF False gqName)
          MC.Vote (actionConfig agg am) Nothing psData
        runRegModel psData gqName agg am =
          fst <<$>> MR.runBaseModel 2020
          (MR.modelCacheStructure $ cacheStructureF False gqName)
          (MC2.ActionOnly MC.Reg (actionConfig agg am)) psData
        runRegModelAH psData gqName agg am =
          MR.runActionModelAH 2020 (cacheStructureF False gqName)
          MC.Reg (actionConfig agg am) Nothing psData

--      cesByCD <- K.ignoreCacheTime cesByCD_C
    let psByAggT :: MC.SurveyAggregation b -> F.FrameRec (DP.CESByR DP.CDKeyR) -> DP.PSData '[GT.StateAbbreviation]
        psByAggT a cd =
          let wgtd ws = case ws of
                DP.FullWeights -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000 *) .  view DP.surveyWeight) (view DT.pWPopPerSqMile) cd
                DP.CellWeights -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac .  view DP.surveyed) (view DT.pWPopPerSqMile) cd
                DP.DesignEffectWeights -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000 *) .  view DP.surveyedESS) (view DT.pWPopPerSqMile) cd
          in case a of
               MC.UnweightedAggregation -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac .  view DP.surveyed) (view DT.pWPopPerSqMile) cd
               MC.RoundedWeightedAggregation ws -> wgtd ws
               MC.WeightedAggregation _ ws -> wgtd ws
--          psByAggT_cached :: MC.SurveyAggregation b -> K.Sem r (K.ActionWithCacheTime r (DP.PSData '[GT.StateAbbreviation]))
        psByAggT_cached a = BRCC.retrieveOrMakeD ("analysis/election2/psByAggT" <> MC.addAggregationText a <> ".bin") cesByCD_C $ pure . psByAggT a

        g f (a, b) = f b >>= pure . (a, )
        h f = traverse (g f)
    turnoutTargets <- K.ignoreCacheTimeM $ MR.stateActionTargets 2020 MC.Vote
    acsByPUMA <- K.ignoreCacheTime acsByPUMA_C
    stateComparisonsACST <- MR.allModelsCompBy @'[GT.StateAbbreviation] (const $ pure acsByPUMA_C) runTurnoutModel "T_ACS_PUMA" aggregations alphaModelsT
                            >>= h (MR.addActionTargets turnoutTargets)
    stateComparisonsCESWT <- MR.allModelsCompBy @'[GT.StateAbbreviation] psByAggT_cached runTurnoutModel  "T_CES_CD" aggregations alphaModelsT
                             >>= h (MR.addActionTargets turnoutTargets)
    stateComparisonsAHT <- MR.allModelsCompBy @'[GT.StateAbbreviation] (const $ pure acsByPUMA_C) runTurnoutModelAH "AHT_ACS_PUMA" aggregations alphaModelsT
                           >>= h (MR.addActionTargets turnoutTargets)


    let jsonLocations = let (d, ue) = BRK.jsonLocations modelPostPaths postInfo in BRHJ.JsonLocations d ue

    turnoutStateChart <- MR.stateChart -- @[GT.StateAbbreviation, MR.ModelPr, BRDF.VAP, BRDF.BallotsCounted]
                         jsonLocations "TComp" "Turnout Model Comparison by State" "Turnout" (FV.fixedSizeVC 500 500 10)
                         (view DP.targetPop) (Just $ view DP.actionTarget)
                         ((fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("ACS " <> )) stateComparisonsACST)
                          <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("W CES " <> )) stateComparisonsCESWT)
                          <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH ACS " <>))  stateComparisonsAHT))

    _ <- K.addHvega Nothing Nothing turnoutStateChart
    let srText r = show (r ^. DT.education4C) <> "-" <> show (r ^. DT.race5C)
        asText r = show (r ^. DT.age5C) <> "-" <> show (r ^. DT.sexC)
        aserText r = show (r ^. DT.age5C) <> "-" <> show (r ^. DT.sexC) <> "-" <> show (r ^. DT.education4C) <> "-" <> show (r ^. DT.race5C)


    MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.turnoutDataBy @'[DT.Age5C]) psByAggT_cached runTurnoutModel
      "UW_Age" "Turnout: UW CES" (show . view DT.age5C) aggregations alphaModelsT
    MR.allModelsCompChart @'[DT.SexC] jsonLocations  (MR.turnoutDataBy @'[DT.SexC]) psByAggT_cached runTurnoutModel
      "UW_Sex" "Turnout: UW CES" (show . view DT.sexC) aggregations alphaModelsT
    MR.allModelsCompChart @'[DT.Age5C, DT.SexC] jsonLocations  (MR.turnoutDataBy @'[DT.Age5C, DT.SexC]) psByAggT_cached runTurnoutModel
      "UW_AgeSex" "Turnout: UW CES" asText aggregations alphaModelsT
    MR.allModelsCompChart @'[DT.Education4C] jsonLocations  (MR.turnoutDataBy @'[DT.Education4C]) psByAggT_cached runTurnoutModel
      "UW_Education" "Turnout: UW CES" (show . view DT.education4C) aggregations alphaModelsT
    MR.allModelsCompChart @'[DT.Race5C] jsonLocations  (MR.turnoutDataBy @'[DT.Race5C]) psByAggT_cached runTurnoutModel
      "UW_Race" "Turnout: UW CES" (show . view DT.race5C) aggregations alphaModelsT

      -- voter validated subset
    cesVVByCD_C <- DP.cesCountedDemPresVotesByCD False (DP.Validated DP.Both)
                   >>= DP.cachedPreppedCES (Right "analysis/election2/cesVVByCD.bin") . fmap (F.filterFrame ((> 0) . view DP.votesInRaceW))

    K.logLE K.Info "Chosen cesVVByCD rows:"
    K.ignoreCacheTime cesVVByCD_C >>= BRLC.logFrame . F.takeRows 100 . F.filterFrame ((== 0) . view DP.votesInRaceW)
--      K.knitError "STOP"
    let psByAggP :: MC.SurveyAggregation b -> F.FrameRec (DP.CESByR DP.CDKeyR) -> DP.PSData '[GT.StateAbbreviation]
        psByAggP a cd =
          let wgtd ws = case ws of
                DP.FullWeights -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000 *) .  view DP.votesInRaceW) (view DT.pWPopPerSqMile) cd
                DP.CellWeights -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac .  view DP.votesInRace) (view DT.pWPopPerSqMile) cd
                DP.DesignEffectWeights -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000 *) .  view DP.votesInRaceESS) (view DT.pWPopPerSqMile) cd
          in case a of
               MC.UnweightedAggregation -> MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac . view DP.votesInRace) (view DT.pWPopPerSqMile) cd
               MC.RoundedWeightedAggregation ws -> wgtd ws
               MC.WeightedAggregation _ ws -> wgtd ws
--          psByAggP_cached :: MC.SurveyAggregation b -> K.Sem r (K.ActionWithCacheTime r (DP.PSData '[GT.StateAbbreviation]))
        psByAggP_cached a = BRCC.retrieveOrMakeD ("analysis/election2/psByAggP" <> MC.addAggregationText a <> ".bin") cesVVByCD_C $ pure . psByAggP a


--      vvPrefByAge <- K.ignoreCacheTime cesVVByCD_C >>= pure . FL.fold (MR.ratioFld @'[DT.Age5C] (realToFrac . view DP.dVotes) (realToFrac . view DP.votesInRace))
--      K.logLE K.Info $ "vvbyCD by Age:" <> show (FL.fold FL.list vvPrefByAge)
--      let oneCatTest a s e r x = (x ^. DT.age5C == a) && (x ^. DT.sexC == s) && (x ^. DT.education4C == e) && (x ^. DT.race5C == r)
--      vvAlphaMRatio <-  K.ignoreCacheTime cesVVByCD_C
--        >>= pure . FL.fold (MR.oneCatRatioFld (realToFrac . view DP.dVotes) (realToFrac . view DP.votesInRace) (oneCatTest DT.A5_45To64 DT.Male DT.E4_HSGrad DT.R5_WhiteNonHispanic) )
--      vvAlphaFRatio <-  K.ignoreCacheTime cesVVByCD_C
--        >>= pure . FL.fold (MR.oneCatRatioFld (realToFrac . view DP.dVotes) (realToFrac . view DP.votesInRace) (oneCatTest DT.A5_45To64 DT.Female DT.E4_HSGrad DT.R5_WhiteNonHispanic) )
--      K.logLE K.Info $ "P(alpha0 + aSex)=" <> show vvAlphaMRatio <> "; P(alpha0 - aSex)=" <> show vvAlphaFRatio

      {-
      -- ps for turnout
      cesVVUWV_C <- BRCC.retrieveOrMakeD "analysis/election2/cesVVUWV.bin" cesVVByCD_C $ \cesVVByCD -> do
        K.logLE K.Info "Rebuilding cesVVUWV"
        pure $ MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac . view DP.votesInRace) (view DT.pWPopPerSqMile) cesVVByCD
      cesVVWV_C <- BRCC.retrieveOrMakeD "analysis/election2/cesVVWV.bin" cesVVByCD_C $ \cesVVByCD -> do
        K.logLE K.Info "Rebuilding cesVVWV"
        pure $ MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000 *) . view DP.votesInRaceW) (view DT.pWPopPerSqMile) cesVVByCD


      K.ignoreCacheTime cesVVByCD_C >>= K.logLE K.Info . ("cesVVByCD rows=" <>) . show . FL.fold FL.length
      K.ignoreCacheTime cesVVByCD_C >>= K.logLE K.Info . ("cesVVByCD N=" <>) . show .
        FL.fold FL.list . FL.fold (MR.sumFld @'[DT.Age5C, DT.SexC] (realToFrac . view DP.votesInRace))
      K.ignoreCacheTime cesVVUWV_C >>= K.logLE K.Info . ("cesVVUWV rows=" <>) . show . FL.fold FL.length . DP.unPSData
      K.ignoreCacheTime cesVVUWV_C >>= K.logLE K.Info . ("cesVVUWV N=" <>) . show .
        FL.fold FL.list . FL.fold (MR.sumFld @'[DT.Age5C, DT.SexC] (realToFrac . view DT.popCount)) . DP.unPSData
      vvuwv <- K.ignoreCacheTime cesVVUWV_C >>= pure . fmap (view DT.popCount) . FL.fold FL.list . DP.unPSData

      let cesWgtd2pVotes_C = MR.surveyPSData @(GT.StateAbbreviation ': DP.DCatsR)
                             (view DP.surveyWeight) (realToFrac . view DP.votesInRace) (view DT.pWPopPerSqMile)
                             <$> cesVVByCD_C
-}

    modeledTurnoutPSMap_C <- runTurnoutModel acsByState_C "ACSByState" agg MC.St_A_S_E_R_StA_StS_StE_StR_AS_AE_AR_SE_SR_ER_StER
    let acs2pDeps = (,) <$> modeledTurnoutPSMap_C <*> acsByState_C
    acs2pVotes_C <- BRCC.retrieveOrMakeD "analysis/election2/acs2pVotes.bin" acs2pDeps $
                    (\(mtm, acsPS) -> MR.psMapProduct (\ci pop -> round $ MT.ciMid ci * realToFrac pop) acsPS $ MC.unPSMap mtm)
--        ces

    cesVVByState_C <-  F.filterFrame ((> 0) . view DP.votesInRaceW ) <<$>> DP.cesCountedDemPresVotesByState False (DP.Validated DP.Both)
    prefTargets <- K.ignoreCacheTimeM $ MR.statePrefDTargets dVSPres2020 (cacheStructureF False "AllCells")
--      BRLC.logFrame prefTargets
    cesImpliedPrefTargets <- K.ignoreCacheTimeM $ MR.statePrefDTargets (MR.CESImpliedDVotes cesVVByState_C)  (cacheStructureF False "AllCells")
--      BRLC.logFrame cesImpliedPrefTargets

    let prefConfig agg am = MC.PrefConfig (DP.Validated DP.Both) (MC.ModelConfig agg am (contramap F.rcast dmr))
        runPrefModel psData gqName agg am = fst <<$>> MR.runBaseModel 2020
                                            (MR.modelCacheStructure $ cacheStructureF False gqName) (MC2.PrefOnly MC.Vote $ prefConfig agg am) psData
        runPrefModelAH psData dst gqName agg am =
          MR.runPrefModelAH 2020 (cacheStructureF False gqName) (actionConfig agg am) Nothing (prefConfig agg am) Nothing dst psData
{-          runFullModelAH psData dst gqName agg am =
            MR.runFullModelAH 2020 (cacheStructureF False gqName) (actionConfig agg am) Nothing (prefConfig agg am) Nothing dst psData
-}
    stateComparisonsACSP <- MR.allModelsCompBy @'[GT.StateAbbreviation] (const $ pure acs2pVotes_C) runPrefModel "P_ACS_Votes" aggregations alphaModelsP
                            >>= h (MR.addPrefTargets cesImpliedPrefTargets)
    stateComparisonsCESP <- MR.allModelsCompBy @'[GT.StateAbbreviation] psByAggP_cached runPrefModel "P_CES_Votes" aggregations alphaModelsP
                            >>= h (MR.addPrefTargets cesImpliedPrefTargets)
    stateComparisonsCESWVP <- MR.allModelsCompBy @'[GT.StateAbbreviation] psByAggP_cached runPrefModel "P_CES_WVotes" aggregations alphaModelsP
                              >>= h (MR.addPrefTargets cesImpliedPrefTargets)
    stateComparisonsAHP_P2020 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (const $ pure acsByPUMA_C) (\psd -> runPrefModelAH psd dVSPres2020) "AHP_ACS_PUMA" aggregations alphaModelsP
                                 >>= h (MR.addPrefTargets cesImpliedPrefTargets)
--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (MR.turnoutDataBy @'[DT.Education4C, DT.Race5C]) (runTurnoutModelAH acsByPUMA_C)
--        "ACS_Education_Race" "TurnoutAH: Design CES" srText aggregations alphaModels
    prefStateChart <- MR.stateChart jsonLocations "PComp" "Pref Comparison by State" "Pref" (FV.fixedSizeVC 500 500 10)
                      (view DP.targetPop) (Just $ view DP.actionTarget)
                      ((fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("ACS (Modeled T) " <> )) stateComparisonsACSP)
                    --                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("UWV (CESVV) " <> )) stateComparisonsCESP)
                       <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("CESWV_" <> )) stateComparisonsCESWVP)
                       <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_P2020" <>)) $ stateComparisonsAHP_P2020)
--                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_H2022" <>)) $ stateComparisonsAHP_H2022)
                      )
    _ <- K.addHvega Nothing Nothing prefStateChart
    MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.prefDataBy @'[DT.Age5C])  psByAggP_cached runPrefModel
      "Age" "Pref" (show . view DT.age5C) aggregations alphaModelsP
    MR.allModelsCompChart @'[DT.SexC] jsonLocations (MR.prefDataBy @'[DT.SexC]) psByAggP_cached runPrefModel
      "Sex" "Pref" (show . view DT.sexC) aggregations alphaModelsP
    MR.allModelsCompChart @'[DT.Age5C, DT.SexC] jsonLocations  (MR.prefDataBy @'[DT.Age5C, DT.SexC]) psByAggP_cached runPrefModel
      "Age_Sex" "Pref" asText aggregations alphaModelsP
    MR.allModelsCompChart @'[DT.Education4C] jsonLocations (MR.prefDataBy @'[DT.Education4C]) psByAggP_cached runPrefModel
      "Education" "Pref" (show . view DT.education4C) aggregations alphaModelsP
    MR.allModelsCompChart @'[DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Race5C]) psByAggP_cached runPrefModel
      "Race" "Pref" (show . view DT.race5C) aggregations alphaModelsP


      --let runBothModel psData gqName agg am = fst <<$>> MR.

    pure ()
  pure ()

cachedACSByPUMA :: forall r . (K.KnitEffects r, BRCC.CacheEffects r) => K.Sem r (K.ActionWithCacheTime r (DP.PSData [BRDF.StateAbbreviation, GT.PUMA]))
cachedACSByPUMA = do
  let (srcWindow, cachedSrc) = ACS.acs1Yr2012_22 @r
  fmap (DP.PSData . fmap F.rcast . F.filterFrame ((== DT.Citizen) . view DT.citizenC)) <$> DDP.cachedACSa5ByPUMA srcWindow cachedSrc 2022

acsByPUMA :: forall r . (K.KnitEffects r, BRCC.CacheEffects r) => K.Sem r  (F.FrameRec DDP.ACSa5ByPUMAR)
acsByPUMA = do
  let (srcWindow, cachedSrc) = ACS.acs1Yr2012_22 @r
  stateXWalk <- K.ignoreCacheTimeM $ BRL.stateAbbrCrosswalkLoader
  acs <- K.ignoreCacheTimeM cachedSrc
  DDP.aCSa5ByPUMA acs stateXWalk 2022

acsA5ByCD :: forall r . (K.KnitEffects r, BRCC.CacheEffects r) => K.Sem r  (F.FrameRec DDP.ACSa5ByCDR)
acsA5ByCD = do
  let (srcWindow, cachedSrc) = ACS.acs1Yr2012_22 @r
  stateXWalk <- K.ignoreCacheTimeM $ BRL.stateAbbrCrosswalkLoader
  cdFromPUMA <- K.ignoreCacheTimeM $ BRL.allCDFromPUMA2012Loader
  acs <- K.ignoreCacheTimeM cachedSrc
  DDP.aCSa5ByCD cdFromPUMA stateXWalk acs 2022 Nothing
--weightedAggregation ::

postDir ∷ Path.Path Rel Dir
postDir = [Path.reldir|br-2023-electionModel/posts|]

postLocalDraft
  ∷ Path.Path Rel Dir
  → Maybe (Path.Path Rel Dir)
  → Path.Path Rel Dir
postLocalDraft p mRSD = case mRSD of
  Nothing → postDir BR.</> p BR.</> [Path.reldir|draft|]
  Just rsd → postDir BR.</> p BR.</> rsd

postInputs ∷ Path.Path Rel Dir → Path.Path Rel Dir
postInputs p = postDir BR.</> p BR.</> [Path.reldir|inputs|]

sharedInputs ∷ Path.Path Rel Dir
sharedInputs = postDir BR.</> [Path.reldir|Shared|] BR.</> [Path.reldir|inputs|]

postOnline ∷ Path.Path Rel t → Path.Path Rel t
postOnline p = [Path.reldir|research/Election|] BR.</> p

postPaths
  ∷ (K.KnitEffects r)
  ⇒ Text
  → BR.CommandLine
  → K.Sem r (BR.PostPaths BR.Abs)
postPaths t cmdLine = do
  let mRelSubDir = case cmdLine of
        BR.CLLocalDraft _ _ mS _ → maybe Nothing BR.parseRelDir $ fmap toString mS
        _ → Nothing
  postSpecificP ← K.knitEither $ first show $ Path.parseRelDir $ toString t
  BR.postPaths
    BR.defaultLocalRoot
    sharedInputs
    (postInputs postSpecificP)
    (postLocalDraft postSpecificP mRelSubDir)
    (postOnline postSpecificP)
