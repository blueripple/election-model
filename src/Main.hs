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
import qualified BlueRipple.Data.ACS_PUMS as ACS
import qualified BlueRipple.Data.Small.DataFrames as BRDF
import qualified BlueRipple.Data.Small.Loaders as BRL
import qualified BlueRipple.Utilities.KnitUtils as BRK
import qualified BlueRipple.Data.CachingCore as BRCC
import qualified BlueRipple.Model.Demographic.DataPrep as DDP
import qualified BlueRipple.Model.Election2.DataPrep as DP
import qualified BlueRipple.Model.Election2.ModelCommon as MC
import qualified BlueRipple.Model.Election2.ModelRunner as MR
import qualified BlueRipple.Utilities.HvegaJsonData as BRHJ

import qualified Knit.Report as K
import qualified Knit.Effect.AtomicCache as KC
import qualified Text.Pandoc.Error as Pandoc
import qualified System.Console.CmdArgs as CmdArgs

--import qualified Control.Foldl as FL
import Control.Lens (view, (^.))

import qualified Frames as F

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
        dmr = MC.tDesignMatrixRow_d
        survey = MC.CESSurvey
        aggregations = [MC.WeightedAggregation MC.ContinuousBinomial]
        alphaModels = [MC.St_A_S_E_R, MC.St_A_S_E_R_ER_StR, MC.St_A_S_E_R_ER_StR_StER]
--    rawCES_C <- DP.cesCountedDemPresVotesByCD False
--    cpCES_C <-  DP.cachedPreppedCES (Right "model/election2/test/CESTurnoutModelDataRaw.bin") rawCES_C
--    rawCPS_C <- DP.cpsCountedTurnoutByState
--    cpCPS_C <- DP.cachedPreppedCPS (Right "model/election2/test/CPSTurnoutModelDataRaw.bin") rawCPS_C
--    cps <- K.ignoreCacheTime cpCPS_C
--    ces <- K.ignoreCacheTime cpCES_C
    let modelDirE = Right "model/election2/stan/"
        cacheDirE = Right "model/election2/"

    modelPostPaths <- postPaths "Models" cmdLine

    BRK.brNewPost modelPostPaths postInfo "Models" $ do

      acsA5ByState_C <- DDP.cachedACSa5ByState ACS.acs1Yr2012_21 2021
--      acsByState <- K.ignoreCacheTime acsA5ByState_C
      let --allStates = FL.fold (FL.premap (view GT.stateAbbreviation) FL.set) acsByState
--          avgACSPWDensity = FL.fold (FL.premap (view DT.pWPopPerSqMile) FL.mean) acsByState
      acsByState_C <- BRCC.retrieveOrMakeD "model/election2/acsByStatePS.bin" acsA5ByState_C $ \acsFull ->
        pure $ DP.PSData @'[GT.StateAbbreviation] . fmap F.rcast . F.filterFrame ((== DT.Citizen) . view DT.citizenC) $ acsFull
{-      ahTResMap_C <- MR.runTurnoutModelAH @'[GT.StateAbbreviation] 2020 modelDirE cacheDirE "State" cmdLine survey
                   (MC.WeightedAggregation MC.ContinuousBinomial MC.NoAchenHur) (contramap F.rcast dmr) MC.NoPSTargets MC.St_A_S_E_R acsByState_C
      K.ignoreCacheTime ahTResMap_C >>= K.logLE K.Info . show . MC.unPSMap
-}
      presidentialElections_C <- BRL.presidentialElectionsWithIncumbency
      let dVSPres2020 = DP.ElexTargetConfig "Pres" (pure mempty) 2020 presidentialElections_C
      houseElections_C <- BRL.houseElectionsWithIncumbency
      let dVSHouse2022 = DP.ElexTargetConfig "House" (pure mempty) 2022 houseElections_C
      let turnoutConfig agg am = MC.TurnoutConfig survey (MC.ModelConfig agg am (contramap F.rcast dmr))
          prefConfig agg am = MC.PrefConfig (MC.ModelConfig agg am (contramap F.rcast dmr))
          cacheStructureF gqName = MR.CacheStructure modelDirE cacheDirE gqName "AllCells" gqName
          runTurnoutModel gqName agg am = fst <<$>> MR.runTurnoutModel 2020
            (MR.modelCacheStructure $ cacheStructureF gqName) (turnoutConfig agg am) acsByState_C
          runTurnoutModelAH gqName agg am = MR.runTurnoutModelAH 2020 (cacheStructureF gqName) (turnoutConfig agg am) Nothing acsByState_C
          runPrefModel gqName agg am = fst <<$>> MR.runPrefModel 2020
            (MR.modelCacheStructure $ cacheStructureF gqName) (prefConfig agg am) acsByState_C
          runPrefModelAH dst gqName agg am =
            MR.runPrefModelAH 2020 (cacheStructureF gqName) (turnoutConfig agg am) Nothing (prefConfig agg am) Nothing dst acsByState_C
          runDVSModel gqName agg am = fst
            <<$>> MR.runFullModel 2020 (MR.modelCacheStructure $ cacheStructureF gqName) (turnoutConfig agg am) (prefConfig agg am) acsByState_C
          runDVSModelAH dst gqName agg am =
            MR.runFullModelAH 2020
            (cacheStructureF gqName) (turnoutConfig agg am) Nothing (prefConfig agg am) Nothing dst acsByState_C
          g f (a, b) = f b >>= pure . (a, )
          h f = traverse (g f)
      stateComparisonsT <- MR.allModelsCompBy @'[GT.StateAbbreviation] runTurnoutModel "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      stateComparisonsAHT <- MR.allModelsCompBy @'[GT.StateAbbreviation] runTurnoutModelAH "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      stateComparisonsP <- MR.allModelsCompBy @'[GT.StateAbbreviation] runPrefModel "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      stateComparisonsAHP_P2020 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runPrefModelAH dVSPres2020) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      stateComparisonsAHP_H2022 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runPrefModelAH dVSHouse2022) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      stateComparisonsDVS <- MR.allModelsCompBy @'[GT.StateAbbreviation] runDVSModel "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      stateComparisonsAHDVS_P2020 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runDVSModelAH dVSPres2020) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      stateComparisonsAHDVS_H2022 <- MR.allModelsCompBy @'[GT.StateAbbreviation] (runDVSModelAH dVSHouse2022) "State" aggregations alphaModels >>= h MR.addBallotsCountedVEP
      let jsonLocations = let (d, ue) = BRK.jsonLocations modelPostPaths postInfo in BRHJ.JsonLocations d ue
      turnoutStateChart <- MR.stateChart -- @[GT.StateAbbreviation, MR.ModelPr, BRDF.VAP, BRDF.BallotsCounted]
                           jsonLocations "TComp" "Turnout Model Comparison by State" "Turnout" (FV.fixedSizeVC 500 500 10)
                           (view BRDF.vAP) (Just $ view BRDF.ballotsCountedVEP)
                           (fmap (second $ (fmap (MR.modelCIToModelPr))) stateComparisonsT
                           <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_" <>)) $ stateComparisonsAHT))
      _ <- K.addHvega Nothing Nothing turnoutStateChart
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations runTurnoutModel "Age" "Turnout" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations runTurnoutModel "Sex" "Turnout" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations runTurnoutModel "Education" "Turnout" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations runTurnoutModel "Race" "Turnout" (show . view DT.race5C) aggregations alphaModels
      let srText r = show (r ^. DT.education4C) <> "-" <> show (r ^. DT.race5C)
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations runTurnoutModel "Education_Race" "Turnout" srText aggregations alphaModels

      MR.allModelsCompChart @'[DT.Age5C] jsonLocations runTurnoutModelAH "Age" "TurnoutAH" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations runTurnoutModelAH "Sex" "TurnoutAH" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations runTurnoutModelAH "Education" "TurnoutAH" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations runTurnoutModelAH "Race" "TurnoutAH" (show . view DT.race5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations runTurnoutModelAH "Education_Race" "TurnoutAH" srText aggregations alphaModels

      prefStateChart <- MR.stateChart jsonLocations "PComp" "Pref Comparison by State" "Pref" (FV.fixedSizeVC 500 500 10) (view BRDF.vAP) Nothing
                        (fmap (second $ (fmap (MR.modelCIToModelPr))) stateComparisonsP
                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_P2020" <>)) $ stateComparisonsAHP_P2020)
                         <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_H2022" <>)) $ stateComparisonsAHP_H2022)
                        )
      _ <- K.addHvega Nothing Nothing prefStateChart
      MR.allModelsCompChart @'[DT.Age5C] jsonLocations runPrefModel "Age" "Pref" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations runPrefModel "Sex" "Pref" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations runPrefModel "Education" "Pref" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations runPrefModel "Race" "Pref" (show . view DT.race5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations runPrefModel "Education_Race" "Pref" srText aggregations alphaModels

      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (runPrefModelAH dVSPres2020) "Age" "PrefAH" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations (runPrefModelAH dVSPres2020) "Sex" "PrefAH" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations (runPrefModelAH dVSPres2020) "Education" "PrefAH" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations (runPrefModelAH dVSPres2020) "Race" "PrefAH" (show . view DT.race5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (runPrefModelAH dVSPres2020) "Education_Race" "PrefAH" srText aggregations alphaModels

      dvsStateChart <- MR.stateChart jsonLocations "DVSComp" "DVS Comparison by State" "Pref" (FV.fixedSizeVC 500 500 10) (view BRDF.vAP) Nothing
                        (fmap (second $ (fmap (MR.modelCIToModelPr))) stateComparisonsDVS
                        <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_P2020" <>)) $ stateComparisonsAHDVS_P2020)
                        <> (fmap (second $ (fmap (MR.modelCIToModelPr))) $ fmap (first ("AH_H2022" <>)) $ stateComparisonsAHDVS_H2022)
                        )

      _ <- K.addHvega Nothing Nothing dvsStateChart

      MR.allModelsCompChart @'[DT.Age5C] jsonLocations runDVSModel "Age" "DVS" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations runDVSModel "Sex" "DVS" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations runDVSModel "Education" "DVS" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations runDVSModel "Race" "DVS" (show . view DT.race5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations runDVSModel "Education_Race" "DVS" srText aggregations alphaModels

      MR.allModelsCompChart @'[DT.Age5C] jsonLocations (runDVSModelAH dVSPres2020) "Age" "DVSAH" (show . view DT.age5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.SexC] jsonLocations (runDVSModelAH dVSPres2020) "Sex" "DVSAH" (show . view DT.sexC) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C] jsonLocations (runDVSModelAH dVSPres2020) "Education" "DVSAH" (show . view DT.education4C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Race5C] jsonLocations (runDVSModelAH dVSPres2020) "Race" "DVSAH" (show . view DT.race5C) aggregations alphaModels
      MR.allModelsCompChart @'[DT.Education4C, DT.Race5C] jsonLocations (runDVSModelAH dVSPres2020) "Education_Race" "DVSAH" srText aggregations alphaModels
      pure ()
    pure ()
  pure ()
  case resE of
    Right namedDocs →
      K.writeAllPandocResultsWithInfoAsHtml "" namedDocs
    Left err → putTextLn $ "Pandoc Error: " <> Pandoc.renderError err

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
