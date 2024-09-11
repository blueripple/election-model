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
    let postInfo = BR.PostInfo (BR.postStage cmdLine) (BR.PubTimes BR.Unpublished Nothing)
        dmr = MC.tDesignMatrixRow_0
        cesVV = MC.CESSurvey (DP.Validated DP.Both)
        cesAll = MC.CESSurvey (DP.AllSurveyed DP.Both)
        aggregations = [MC.UnweightedAggregation]
--        aggregations = [MC.WeightedAggregation MC.ContinuousBinomial]
        alphaModelsT = [MC.A_S_E_R, MC.A_S_E_R_AS_AE_AR_SE_SR_ER, MC.St_A_S_E_R_StA_StS_StE_StR_AS_AE_AR_SE_SR_ER_StER] -- [MC.A, MC.S, MC.A_S, MC.AS] --, MC.St_A_S_E_R] --, MC.St_A_S_E_R_ER_StR, MC.St_A_S_E_R_AE_AR_ER_StR]
        alphaModelsP = [MC.A_S_E_R, MC.A_S_E_R_AS_AE_AR_SE_SR_ER, MC.St_A_S_E_R_StA_StS_StE_StR_AS_AE_AR_SE_SR_ER_StER] --, MC.St_A_S_E_R] --, MC.St_A_S_E_R_ER_StR, MC.St_A_S_E_R_AE_AR_ER_StR]
--    rawCES_C <- DP.cesCountedDemPresVotesByCD False
--    cpCES_C <-  DP.cachedPreppedCES (Right "model/election2/test/CESTurnoutModelDataRaw.bin") rawCES_C
--    rawCPS_C <- DP.cpsCountedTurnoutByState
--    cpCPS_C <- DP.cachedPreppedCPS (Right "model/election2/test/CPSTurnoutModelDataRaw.bin") rawCPS_C
--    cps <- K.ignoreCacheTime cpCPS_C
--    ces <- K.ignoreCacheTime cpCES_C
    let modelDir = "model/election2/stan/"
        cacheDir = "model/election2/"

    modelPostPaths <- postPaths "Models" cmdLine

    BRK.brNewPost modelPostPaths postInfo "Models" $ do
{-      cesSurvey <- K.ignoreCacheTimeM $ DP.cesCountedDemPresVotesByCD False
      BRLC.logFrame $ F.takeRows 1000 cesSurvey
      K.knitError "STOP"
-}
      let (srcWindow, cachedSrc) = ACS.acs1Yr2012_22
      acsA5ByState_C <- DDP.cachedACSa5ByState srcWindow cachedSrc 2022

      acsByPUMA_C <- acsByPUMA

      acsByState_C <- BRCC.retrieveOrMakeD "model/election2/acsByStatePS.bin" acsA5ByState_C $ \acsFull ->
        pure $ DP.PSData @'[GT.StateAbbreviation] . fmap F.rcast . F.filterFrame ((== DT.Citizen) . view DT.citizenC) $ acsFull
      cesByCD_C <- DP.cesCountedDemPresVotesByCD False (DP.AllSurveyed DP.Both) DP.DesignEffectWeights
                   >>= DP.cachedPreppedCES (Right "analysis/election2/cesByCD.bin")
      cesUW_C <- BRCC.retrieveOrMakeD "analysis/election2/cesUW.bin" cesByCD_C
                 $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac . view DP.surveyed) (view DT.pWPopPerSqMile)

      cesW_C <- BRCC.retrieveOrMakeD "analysis/election2/cesW.bin" cesByCD_C
                  $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000*) .  view DP.surveyWeight) (view DT.pWPopPerSqMile)


--      cesW_C <- BRCC.retrieveOrMakeD "analysis/election2/cesWV.bin" cesByCD_C
--                $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (view DP.surveyWeight) (view DT.pWPopPerSqMile)

{-    cesDesignWeights_C <- BRCC.retrieveOrMakeD "analysis/election2/cesDesignWeights.bin" cesByCD_C
        $ pure . MR.surveyPSData @'[GT.StateAbbreviation] ((1000*) . view DP.surveyedW) (view DT.pWPopPerSqMile)
      cesRoundedDesignWeights_C <- BRCC.retrieveOrMakeD "analysis/election2/cesRoundedDesignWeights.bin" cesByCD_C
        $ pure . MR.surveyPSData @'[GT.StateAbbreviation] ((1000*) . realToFrac . round @_ @Int . view DP.surveyedW) (view DT.pWPopPerSqMile)
-}
{-      let g x y = if x > 0 then x / y else 0
          test r = abs (g (realToFrac (r ^. DP.dVotes))  (realToFrac (r ^. DP.votesInRace)) - g (r ^. DP.dVotesW) (r ^. DP.votesInRaceW)) > 1e-10
          f r = "uw = " <>  show (g (realToFrac (r ^. DP.dVotes))  (realToFrac (r ^. DP.votesInRace)))
            <> "; dw = " <> show (g (r ^. DP.dVotesW) (r ^. DP.votesInRaceW))
      K.ignoreCacheTime cesByCD_C >>= BRLC.logFrame . F.filterFrame test
      K.knitError "STOP"
-}
{-      ahTResMap_C <- MR.runTurnoutModelAH @'[GT.StateAbbreviation] 2020 modelDirE cacheDirE "State" cmdLine survey
                   (MC.WeightedAggregation MC.ContinuousBinomial MC.NoAchenHur) (contramap F.rcast dmr) MC.NoPSTargets MC.St_A_S_E_R acsByState_C
      K.ignoreCacheTime ahTResMap_C >>= K.logLE K.Info . show . MC.unPSMap
-}
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

{-          runDVSModel psData gqName agg am = fst
            <<$>> MR.runFullModel 2020 (MR.modelCacheStructure $ cacheStructureF gqName) (turnoutConfig agg am) (prefConfig agg am) psData
          runDVSModelAH psData dst gqName agg am =
            MR.runFullModelAH 2020
            (cacheStructureF gqName) (turnoutConfig agg am) Nothing (prefConfig agg am) Nothing dst psData
-}
          g f (a, b) = f b >>= pure . (a, )
          h f = traverse (g f)
      turnoutTargets <- K.ignoreCacheTimeM $ MR.stateActionTargets 2020 MC.Vote
      stateComparisonsACST <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runTurnoutModel acsByPUMA_C) "T_ACS_PUMA" aggregations alphaModelsT
                              >>= h (MR.addActionTargets turnoutTargets)
      stateComparisonsCESWT <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runTurnoutModel cesW_C) "T_CES_CD" aggregations alphaModelsT
                               >>= h (MR.addActionTargets turnoutTargets)
      stateComparisonsAHT <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runTurnoutModelAH acsByPUMA_C) "AHT_ACS_PUMA" aggregations alphaModelsT
                             >>= h (MR.addActionTargets turnoutTargets)

--      stateComparisonsAHP_H2022 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runPrefModelAH dVSHouse2022) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
--      stateComparisonsDVS <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runDVSModel acsByPUMA_C) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
--      stateComparisonsAHDVS_P2020 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runDVSModelAH acsByPUMA_C dVSPres2020) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
--      stateComparisonsAHDVS_H2022 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runDVSModelAH dVSHouse2022) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP

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

--      MR.allModelsCompChart  @[DT.Age5C, DT.SexC, DT.Education4C, DT.Race5C] jsonLocations (MR.turnoutDataBy @[DT.Age5C, DT.SexC, DT.Education4C, DT.Race5C]) (runTurnoutModel cesUW_C)
--        "UW_ASER" "Turnout: UW CES" aserText aggregations alphaModels
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.turnoutDataBy @'[DT.Age5C]) (runTurnoutModel cesUW_C)
        "UW_Age" "Turnout: UW CES" (show . view DT.age5C) aggregations alphaModelsT
      MR.allModelsCompChart @'[DT.SexC] jsonLocations  (MR.turnoutDataBy @'[DT.SexC]) (runTurnoutModel cesUW_C)
        "UW_Sex" "Turnout: UW CES" (show . view DT.sexC) aggregations alphaModelsT
      MR.allModelsCompChart @'[DT.Age5C, DT.SexC] jsonLocations  (MR.turnoutDataBy @'[DT.Age5C, DT.SexC]) (runTurnoutModel cesUW_C)
        "UW_AgeSex" "Turnout: UW CES" asText aggregations alphaModelsT
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations  (MR.turnoutDataBy @'[DT.Education4C]) (runTurnoutModel cesUW_C)
        "UW_Education" "Turnout: UW CES" (show . view DT.education4C) aggregations alphaModelsT
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations  (MR.turnoutDataBy @'[DT.Race5C]) (runTurnoutModel $ cesUW_C)
        "UW_Race" "Turnout: UW CES" (show . view DT.race5C) aggregations alphaModelsT

--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations  (MR.turnoutDataBy  @'[DT.Education4C, DT.Race5C]) (runTurnoutModel $ cesUW_C)
--        "UW_EduRace" "Turnout: Design CES" srText aggregations alphaModelsT
{-
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.turnoutDataBy @'[DT.Age5C]) (runTurnoutModel acsByPUMA_C)
        "ACS_Age" "Turnout: ACS" (show . view DT.age5C) aggregations alphaModelsT
      MR.allModelsCompChart @'[DT.SexC] jsonLocations  (MR.turnoutDataBy @'[DT.SexC]) (runTurnoutModel acsByPUMA_C)
        "ACS_Sex" "Turnout: ACS" (show . view DT.sexC) aggregations alphaModelsT
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations  (MR.turnoutDataBy @'[DT.Education4C]) (runTurnoutModel acsByPUMA_C)
        "ACS_Education" "Turnout: ACS" (show . view DT.education4C) aggregations alphaModelsT
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations  (MR.turnoutDataBy @'[DT.Race5C]) (runTurnoutModel $ acsByPUMA_C)
        "ACS_Race" "Turnout: ACS" (show . view DT.race5C) aggregations alphaModelsT
--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations  (MR.turnoutDataBy  @'[DT.Education4C, DT.Race5C]) (runTurnoutModel $ acsByPUMA_C)
--        "ACS_EduRace" "Turnout: ACS" srText aggregations alphaModelsT
-}
      -- voter validated subset
      cesVVByCD_C <- DP.cesCountedDemPresVotesByCD False (DP.Validated DP.Both) DP.DesignEffectWeights
                   >>= DP.cachedPreppedCES (Right "analysis/election2/cesVVByCD.bin") . fmap (F.filterFrame ((> 0) . view DP.votesInRace))

--      dvByCD :: [Int] <- K.ignoreCacheTime cesVVByCD_C >>= pure . fmap (view DP.dVotes) . FL.fold FL.list
--      K.logLE K.Info $ "cesByCD (votesInRace > 0) rows=" <> show (length dvByCD)
--      K.logLE K.Info $ show $ take 1000 dvByCD
      vvPrefByAge <- K.ignoreCacheTime cesVVByCD_C >>= pure . FL.fold (MR.ratioFld @'[DT.Age5C] (realToFrac . view DP.dVotes) (realToFrac . view DP.votesInRace))
      K.logLE K.Info $ "vvbyCD by Age:" <> show (FL.fold FL.list vvPrefByAge)
      let oneCatTest a s e r x = (x ^. DT.age5C == a) && (x ^. DT.sexC == s) && (x ^. DT.education4C == e) && (x ^. DT.race5C == r)
      vvAlphaMRatio <-  K.ignoreCacheTime cesVVByCD_C
        >>= pure . FL.fold (MR.oneCatRatioFld (realToFrac . view DP.dVotes) (realToFrac . view DP.votesInRace) (oneCatTest DT.A5_45To64 DT.Male DT.E4_HSGrad DT.R5_WhiteNonHispanic) )
      vvAlphaFRatio <-  K.ignoreCacheTime cesVVByCD_C
        >>= pure . FL.fold (MR.oneCatRatioFld (realToFrac . view DP.dVotes) (realToFrac . view DP.votesInRace) (oneCatTest DT.A5_45To64 DT.Female DT.E4_HSGrad DT.R5_WhiteNonHispanic) )
      K.logLE K.Info $ "P(alpha0 + aSex)=" <> show vvAlphaMRatio <> "; P(alpha0 - aSex)=" <> show vvAlphaFRatio
--      cesVVUW_C <- BRCC.retrieveOrMakeD "analysis/election2/cesVVUW.bin" cesVVByCD_C
--                  $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac . view DP.surveyed) (view DT.pWPopPerSqMile)
--      cesVVW_C <- BRCC.retrieveOrMakeD "analysis/election2/cesVVW.bin" cesVVByCD_C
--                $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (const 1) ((1000 *) . view DP.surveyWeight) (view DT.pWPopPerSqMile)

      -- ps for turnout
      cesVVUWV_C <- BRCC.retrieveOrMakeD "analysis/election2/cesVVUWV.bin" cesVVByCD_C $ \cesVVByCD -> do
        K.logLE K.Info "Rebuilding cesVVUWV"
        pure $ MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (realToFrac . view DP.votesInRace) (view DT.pWPopPerSqMile) cesVVByCD

      K.ignoreCacheTime cesVVByCD_C >>= K.logLE K.Info . ("cesVVByCD rows=" <>) . show . FL.fold FL.length
      K.ignoreCacheTime cesVVByCD_C >>= K.logLE K.Info . ("cesVVByCD N=" <>) . show .
        FL.fold FL.list . FL.fold (MR.sumFld @'[DT.Age5C, DT.SexC] (realToFrac . view DP.votesInRace))
      K.ignoreCacheTime cesVVUWV_C >>= K.logLE K.Info . ("cesVVUWV rows=" <>) . show . FL.fold FL.length . DP.unPSData
      K.ignoreCacheTime cesVVUWV_C >>= K.logLE K.Info . ("cesVVUWV N=" <>) . show .
        FL.fold FL.list . FL.fold (MR.sumFld @'[DT.Age5C, DT.SexC] (realToFrac . view DT.popCount)) . DP.unPSData
      vvuwv <- K.ignoreCacheTime cesVVUWV_C >>= pure . fmap (view DT.popCount) . FL.fold FL.list . DP.unPSData
--      K.logLE K.Info $ show $ zip (reverse vvByCD) (reverse vvuwv)
      cesVVWV_C <- BRCC.retrieveOrMakeD "analysis/election2/cesVVWV.bin" cesVVByCD_C
                   $ pure . MR.surveyPSData @'[GT.StateAbbreviation] (const 1) (\r -> 1000 * realToFrac (r ^. DP.votesInRace) * r ^. DP.surveyWeight) (view DT.pWPopPerSqMile)

      let cesWgtd2pVotes_C = MR.surveyPSData @(GT.StateAbbreviation ': DP.DCatsR)
                             (view DP.surveyWeight) (realToFrac . view DP.votesInRace) (view DT.pWPopPerSqMile)
                             <$> cesVVByCD_C

      modeledTurnoutPSMap_C <- runTurnoutModel acsByState_C "ACSByState" MC.UnweightedAggregation MC.St_A_S_E_R_StA_StS_StE_StR_AS_AE_AR_SE_SR_ER_StER
      let acs2pDeps = (,) <$> modeledTurnoutPSMap_C <*> acsByState_C
      acs2pVotes_C <- BRCC.retrieveOrMakeD "analysis/election2/acs2pVotes.bin" acs2pDeps $
        (\(mtm, acsPS) -> MR.psMapProduct (\ci pop -> round $ MT.ciMid ci * realToFrac pop) acsPS $ MC.unPSMap mtm)
--        ces

      cesVVByState_C <-  DP.cesCountedDemPresVotesByState False (DP.Validated DP.Both) DP.DesignEffectWeights
--      K.ignoreCacheTime acs2pVotes_C >>= BRLC.logFrame . F.takeRows 100 . DP.unPSData
      prefTargets <- K.ignoreCacheTimeM $ MR.statePrefDTargets dVSPres2020 (cacheStructureF False "AllCells")
      BRLC.logFrame prefTargets
      cesImpliedPrefTargets <- K.ignoreCacheTimeM $ MR.statePrefDTargets (MR.CESImpliedDVotes cesVVByState_C)  (cacheStructureF False "AllCells")
      BRLC.logFrame cesImpliedPrefTargets

      let prefConfig agg am = MC.PrefConfig (DP.Validated DP.Both) (MC.ModelConfig agg am (contramap F.rcast dmr))
          runPrefModel psData gqName agg am = fst <<$>> MR.runBaseModel 2020
            (MR.modelCacheStructure $ cacheStructureF False gqName) (MC2.PrefOnly MC.Vote $ prefConfig agg am) psData
          runPrefModelAH psData dst gqName agg am =
            MR.runPrefModelAH 2020 (cacheStructureF False gqName) (actionConfig agg am) Nothing (prefConfig agg am) Nothing dst psData

      stateComparisonsACSP <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runPrefModel acs2pVotes_C) "P_ACS_Votes" aggregations alphaModelsP
                              >>= h (MR.addPrefTargets cesImpliedPrefTargets)
      stateComparisonsCESP <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runPrefModel cesVVUWV_C) "P_CES_Votes" aggregations alphaModelsP
                              >>= h (MR.addPrefTargets cesImpliedPrefTargets)
      stateComparisonsCESWVP <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runPrefModel cesVVWV_C) "P_CES_WVotes" aggregations alphaModelsP
                              >>= h (MR.addPrefTargets cesImpliedPrefTargets)
--      stateComparisonsAHP_P2020 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runPrefModelAH acsByPUMA_C dVSPres2020) "AHP_ACS_PUMA" aggregations alphaModels
--                                   >>= h (MR.addPrefTargets prefTargets)
--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (MR.turnoutDataBy @'[DT.Education4C, DT.Race5C]) (runTurnoutModelAH acsByPUMA_C)
--        "ACS_Education_Race" "TurnoutAH: Design CES" srText aggregations alphaModels
      prefStateChart <- MR.stateChart jsonLocations "PComp" "Pref Comparison by State" "Pref" (FV.fixedSizeVC 500 500 10)
                        (view DP.targetPop) (Just $ view DP.actionTarget)
                        ((fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("ACS (Modeled T) " <> )) stateComparisonsACSP)
--                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("UWV (CESVV) " <> )) stateComparisonsCESP)
                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("CESWV_" <> )) stateComparisonsCESWVP)
--                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_P2020" <>)) $ stateComparisonsAHP_P2020)
--                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_H2022" <>)) $ stateComparisonsAHP_H2022)
                        )
      _ <- K.addHvega Nothing Nothing prefStateChart
--      MR.allModelsCompChart @[DT.Age5C, DT.SexC, DT.Education4C, DT.Race5C] jsonLocations (MR.prefDataBy @[DT.Age5C, DT.SexC, DT.Education4C, DT.Race5C])  (runPrefModel cesVVUWV_C)
--        "UWV_ASER" "Pref: CES VV UWV" aserText aggregations alphaModels
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.prefDataBy @'[DT.Age5C])  (runPrefModel cesVVUWV_C)
        "UWV_Age" "Pref: CES VV UWV" (show . view DT.age5C) aggregations alphaModelsP
      MR.allModelsCompChart @'[DT.SexC] jsonLocations (MR.prefDataBy @'[DT.SexC]) (runPrefModel cesVVUWV_C)
        "UWV_Sex" "Pref: CES VV UWV" (show . view DT.sexC) aggregations alphaModelsP
      MR.allModelsCompChart @'[DT.Age5C, DT.SexC] jsonLocations  (MR.prefDataBy @'[DT.Age5C, DT.SexC]) (runPrefModel cesVVUWV_C)
        "UW_AgeSex" "Turnout: UW CES" asText aggregations alphaModelsP
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations (MR.prefDataBy @'[DT.Education4C]) (runPrefModel cesVVUWV_C)
        "UWV_Education" "Pref: CES VV UWV" (show . view DT.education4C) aggregations alphaModelsP
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Race5C]) (runPrefModel cesVVUWV_C)
        "UWV_Race" "Pref: CES VV UWV" (show . view DT.race5C) aggregations alphaModelsP

--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Education4C, DT.Race5C]) (runPrefModel cesUWV_C)
--        "UWV_Education_Race" "Pref: CES VV UWV" srText aggregations alphaModels
{-
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.prefDataBy @'[DT.Age5C])  (runPrefModel cesVVW_C)
        "W_Age" "Pref: W" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations (MR.prefDataBy @'[DT.SexC]) (runPrefModel cesVVW_C)
        "W_Sex" "Pref: W" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations (MR.prefDataBy @'[DT.Education4C]) (runPrefModel cesVVW_C)
        "W_Education" "Pref: W" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Race5C]) (runPrefModel cesVVW_C)
        "W_Race" "Pref: W" (show . view DT.race5C) aggregations alphaModels
--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Education4C, DT.Race5C]) (runPrefModel cesW_C)
--        "W_Education_Race" "Pref: W" srText aggregations alphaModels
-}
{-
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.prefDataBy @'[DT.Age5C]) (runPrefModel acsByPUMA_C)
        "ACS_Age" "Pref: ACS" (show . view DT.age5C) aggregations alphaModelsP
      MR.allModelsCompChart @'[DT.SexC] jsonLocations (MR.prefDataBy @'[DT.SexC]) (runPrefModel acsByPUMA_C)
        "ACS_Sex" "Pref: ACS" (show . view DT.sexC) aggregations alphaModelsP
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations (MR.prefDataBy @'[DT.Education4C]) (runPrefModel acsByPUMA_C)
        "ACS_Education" "Pref: ACS" (show . view DT.education4C) aggregations alphaModelsP
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Race5C]) (runPrefModel acsByPUMA_C)
        "ACS_Race" "Pref: ACS" (show . view DT.race5C) aggregations alphaModelsP
--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Education4C, DT.Race5C]) (runPrefModel acsByPUMA_C)
--        "ACS_Education_Race" "Pref: ACS" srText aggregations alphaModelsP
-}
{-
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.prefDataBy @'[DT.Age5C]) (runPrefModelAH cesVVUWV_C dVSPres2020)
        "UWV_Age" "PrefAH: UWV" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations (MR.prefDataBy @'[DT.SexC]) (runPrefModelAH cesVVUWV_C dVSPres2020)
        "uWV_Sex" "PrefAH: UWV" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations (MR.prefDataBy @'[DT.Education4C]) (runPrefModelAH cesVVUWV_C dVSPres2020)
        "UWV_Education" "PrefAH: UWV" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Race5C]) (runPrefModelAH cesVVUWV_C dVSPres2020)
        "UWV_Race" "PrefAH: UWV" (show . view DT.race5C) aggregations alphaModels
--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (MR.prefDataBy @'[DT.Education4C, DT.Race5C]) (runPrefModelAH acsByPUMA_C dVSPres2020)
--        "ACS_Education_Race" "PrefAH: ACS" srText aggregations alphaModels
-}
{-
      regTargets <- K.ignoreCacheTimeM $ MR.stateActionTargets 2020 MC.Reg
      stateComparisonsACSR <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runRegModel acsByPUMA_C) "R_ACS_PUMA" aggregations alphaModelsR
                              >>= h (MR.addActionTargets regTargets)
      stateComparisonsCESR <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runRegModel cesW_C) "R_CES_PUMA" aggregations alphaModelsR
                              >>= h (MR.addActionTargets regTargets)
      stateComparisonsAHR <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runRegModelAH acsByPUMA_C) "AHR_ACS_PUMA" aggregations alphaModelsR
                             >>= h (MR.addActionTargets regTargets)
      regStateChart <- MR.stateChart -- @[GT.StateAbbreviation, MR.ModelPr, BRDF.VAP, BRDF.BallotsCounted]
                       jsonLocations "RComp" "Registration Model Comparison by State" "Registration" (FV.fixedSizeVC 500 500 10)
                       (view DP.targetPop) (Just $ view DP.actionTarget)
                       ((fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("ACS_" <>)) stateComparisonsACSR)
                       <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("CES_" <>)) stateComparisonsCESR)
                        <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_" <>)) $ stateComparisonsAHR))
      _ <- K.addHvega Nothing Nothing regStateChart
      let srText r = show (r ^. DT.education4C) <> "-" <> show (r ^. DT.race5C)

      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (MR.regDataBy @'[DT.Age5C]) (runRegModel $ fmap withoutDC cesUW_C)
        "UW_Age" "Reg: UW CES" (show . view DT.age5C) aggregations alphaModelsR
      MR.allModelsCompChart @'[DT.SexC] jsonLocations  (MR.regDataBy @'[DT.SexC]) (runRegModel  $ fmap withoutDC cesUW_C)
        "UW_Sex" "Reg: UW CES" (show . view DT.sexC) aggregations alphaModelsR
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations  (MR.regDataBy @'[DT.Education4C]) (runRegModel  $ fmap withoutDC cesUW_C)
        "UW_Education" "Reg: UW CES" (show . view DT.education4C) aggregations alphaModelsR
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations  (MR.regDataBy @'[DT.Race5C]) (runRegModel $ fmap withoutDC cesUW_C)
        "UW_Race" "Reg: UW CES" (show . view DT.race5C) aggregations alphaModelsR
--      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations  (MR.regDataBy  @'[DT.Education4C, DT.Race5C]) (runRegModel $ fmap withoutDC  cesUW_C)
--        "UW_EduRace" "Turnout: Design CES" srText aggregations alphaModelsR
-}

{-
      dvsStateChart <- MR.stateChart jsonLocations "DVSComp" "DVS Comparison by State" "Pref" (FV.fixedSizeVC 500 500 10) (view BRDF.vAP) Nothing
                        (fmap (second $ (fmap (MR.modelCIToModelPr))) stateComparisonsDVS
                        <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_P2020" <>)) $ stateComparisonsAHDVS_P2020)
                        <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_H2022" <>)) $ stateComparisonsAHDVS_H2022)
                        )

      _ <- K.addHvega Nothing Nothing dvsStateChart

      MR.allModelsCompChart @'[DT.Age5C] jsonLocations runDVSModel "Age" "DVS" (show . view DT.age5C) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.SexC] jsonLocations runDVSModel "Sex" "DVS" (show . view DT.sexC) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations runDVSModel "Education" "DVS" (show . view DT.education4C) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations runDVSModel "Race" "DVS" (show . view DT.race5C) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations runDVSModel "Education_Race" "DVS" srText aggregations alphaModelsD

      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (runDVSModelAH dVSPres2020) "Age" "DVSAH" (show . view DT.age5C) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.SexC] jsonLocations (runDVSModelAH dVSPres2020) "Sex" "DVSAH" (show . view DT.sexC) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations (runDVSModelAH dVSPres2020) "Education" "DVSAH" (show . view DT.education4C) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations (runDVSModelAH dVSPres2020) "Race" "DVSAH" (show . view DT.race5C) aggregations alphaModelsD
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (runDVSModelAH dVSPres2020) "Education_Race" "DVSAH" srText aggregations alphaModelsD
-}

      pure ()
    pure ()
  pure ()
  case resE of
    Right namedDocs →
      K.writeAllPandocResultsWithInfoAsHtml "" namedDocs
    Left err → putTextLn $ "Pandoc Error: " <> Pandoc.renderError err

acsByPUMA :: forall r . (K.KnitEffects r, BRCC.CacheEffects r) => K.Sem r (K.ActionWithCacheTime r (DP.PSData [BRDF.StateAbbreviation, GT.PUMA]))
acsByPUMA = do
  let (srcWindow, cachedSrc) = ACS.acs1Yr2012_22 @r
  fmap (DP.PSData . fmap F.rcast . F.filterFrame ((== DT.Citizen) . view DT.citizenC)) <$> DDP.cachedACSa5ByPUMA srcWindow cachedSrc 2022

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
