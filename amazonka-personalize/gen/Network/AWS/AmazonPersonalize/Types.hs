{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types
    (
    -- * Service Configuration
      amazonPersonalize

    -- * Errors
    , _ResourceAlreadyExistsException
    , _InvalidNextTokenException
    , _InvalidInputException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * RecipeProvider
    , RecipeProvider (..)

    -- * TrainingMode
    , TrainingMode (..)

    -- * Algorithm
    , Algorithm
    , algorithm
    , aDefaultHyperParameters
    , aAlgorithmARN
    , aTrainingInputMode
    , aDefaultHyperParameterRanges
    , aAlgorithmImage
    , aLastUpdatedDateTime
    , aName
    , aCreationDateTime
    , aDefaultResourceConfig
    , aRoleARN

    -- * AlgorithmImage
    , AlgorithmImage
    , algorithmImage
    , aiName
    , aiDockerURI

    -- * AutoMLConfig
    , AutoMLConfig
    , autoMLConfig
    , amlcRecipeList
    , amlcMetricName

    -- * AutoMLResult
    , AutoMLResult
    , autoMLResult
    , amlrBestRecipeARN

    -- * BatchInferenceJob
    , BatchInferenceJob
    , batchInferenceJob
    , bijFailureReason
    , bijStatus
    , bijJobOutput
    , bijJobName
    , bijLastUpdatedDateTime
    , bijNumResults
    , bijBatchInferenceJobConfig
    , bijBatchInferenceJobARN
    , bijFilterARN
    , bijCreationDateTime
    , bijSolutionVersionARN
    , bijRoleARN
    , bijJobInput

    -- * BatchInferenceJobConfig
    , BatchInferenceJobConfig
    , batchInferenceJobConfig
    , bijcItemExplorationConfig

    -- * BatchInferenceJobInput
    , BatchInferenceJobInput
    , batchInferenceJobInput
    , bijiS3DataSource

    -- * BatchInferenceJobOutput
    , BatchInferenceJobOutput
    , batchInferenceJobOutput
    , bijoS3DataDestination

    -- * BatchInferenceJobSummary
    , BatchInferenceJobSummary
    , batchInferenceJobSummary
    , bijsFailureReason
    , bijsStatus
    , bijsJobName
    , bijsLastUpdatedDateTime
    , bijsBatchInferenceJobARN
    , bijsCreationDateTime
    , bijsSolutionVersionARN

    -- * Campaign
    , Campaign
    , campaign
    , camFailureReason
    , camStatus
    , camLastUpdatedDateTime
    , camCampaignConfig
    , camLatestCampaignUpdate
    , camName
    , camMinProvisionedTPS
    , camCreationDateTime
    , camCampaignARN
    , camSolutionVersionARN

    -- * CampaignConfig
    , CampaignConfig
    , campaignConfig
    , ccItemExplorationConfig

    -- * CampaignSummary
    , CampaignSummary
    , campaignSummary
    , csFailureReason
    , csStatus
    , csLastUpdatedDateTime
    , csName
    , csCreationDateTime
    , csCampaignARN

    -- * CampaignUpdateSummary
    , CampaignUpdateSummary
    , campaignUpdateSummary
    , cusFailureReason
    , cusStatus
    , cusLastUpdatedDateTime
    , cusCampaignConfig
    , cusMinProvisionedTPS
    , cusCreationDateTime
    , cusSolutionVersionARN

    -- * CategoricalHyperParameterRange
    , CategoricalHyperParameterRange
    , categoricalHyperParameterRange
    , chprValues
    , chprName

    -- * ContinuousHyperParameterRange
    , ContinuousHyperParameterRange
    , continuousHyperParameterRange
    , cMaxValue
    , cName
    , cMinValue

    -- * DataSource
    , DataSource
    , dataSource
    , dsDataLocation

    -- * Dataset
    , Dataset
    , dataset
    , dStatus
    , dDatasetARN
    , dLastUpdatedDateTime
    , dSchemaARN
    , dName
    , dDatasetType
    , dCreationDateTime
    , dDatasetGroupARN

    -- * DatasetGroup
    , DatasetGroup
    , datasetGroup
    , dgFailureReason
    , dgStatus
    , dgKmsKeyARN
    , dgLastUpdatedDateTime
    , dgName
    , dgCreationDateTime
    , dgDatasetGroupARN
    , dgRoleARN

    -- * DatasetGroupSummary
    , DatasetGroupSummary
    , datasetGroupSummary
    , dgsFailureReason
    , dgsStatus
    , dgsLastUpdatedDateTime
    , dgsName
    , dgsCreationDateTime
    , dgsDatasetGroupARN

    -- * DatasetImportJob
    , DatasetImportJob
    , datasetImportJob
    , dijFailureReason
    , dijStatus
    , dijDatasetARN
    , dijJobName
    , dijLastUpdatedDateTime
    , dijDatasetImportJobARN
    , dijDataSource
    , dijCreationDateTime
    , dijRoleARN

    -- * DatasetImportJobSummary
    , DatasetImportJobSummary
    , datasetImportJobSummary
    , dijsFailureReason
    , dijsStatus
    , dijsJobName
    , dijsLastUpdatedDateTime
    , dijsDatasetImportJobARN
    , dijsCreationDateTime

    -- * DatasetSchema
    , DatasetSchema
    , datasetSchema
    , dsLastUpdatedDateTime
    , dsSchema
    , dsSchemaARN
    , dsName
    , dsCreationDateTime

    -- * DatasetSchemaSummary
    , DatasetSchemaSummary
    , datasetSchemaSummary
    , dssLastUpdatedDateTime
    , dssSchemaARN
    , dssName
    , dssCreationDateTime

    -- * DatasetSummary
    , DatasetSummary
    , datasetSummary
    , datStatus
    , datDatasetARN
    , datLastUpdatedDateTime
    , datName
    , datDatasetType
    , datCreationDateTime

    -- * DefaultCategoricalHyperParameterRange
    , DefaultCategoricalHyperParameterRange
    , defaultCategoricalHyperParameterRange
    , defIsTunable
    , defValues
    , defName

    -- * DefaultContinuousHyperParameterRange
    , DefaultContinuousHyperParameterRange
    , defaultContinuousHyperParameterRange
    , dchprMaxValue
    , dchprIsTunable
    , dchprName
    , dchprMinValue

    -- * DefaultHyperParameterRanges
    , DefaultHyperParameterRanges
    , defaultHyperParameterRanges
    , dhprIntegerHyperParameterRanges
    , dhprCategoricalHyperParameterRanges
    , dhprContinuousHyperParameterRanges

    -- * DefaultIntegerHyperParameterRange
    , DefaultIntegerHyperParameterRange
    , defaultIntegerHyperParameterRange
    , dihprMaxValue
    , dihprIsTunable
    , dihprName
    , dihprMinValue

    -- * EventTracker
    , EventTracker
    , eventTracker
    , etStatus
    , etTrackingId
    , etLastUpdatedDateTime
    , etAccountId
    , etName
    , etCreationDateTime
    , etDatasetGroupARN
    , etEventTrackerARN

    -- * EventTrackerSummary
    , EventTrackerSummary
    , eventTrackerSummary
    , etsStatus
    , etsLastUpdatedDateTime
    , etsName
    , etsCreationDateTime
    , etsEventTrackerARN

    -- * FeatureTransformation
    , FeatureTransformation
    , featureTransformation
    , ftStatus
    , ftFeatureTransformationARN
    , ftLastUpdatedDateTime
    , ftName
    , ftCreationDateTime
    , ftDefaultParameters

    -- * Filter
    , Filter
    , filter'
    , fFailureReason
    , fStatus
    , fFilterExpression
    , fLastUpdatedDateTime
    , fName
    , fFilterARN
    , fCreationDateTime
    , fDatasetGroupARN

    -- * FilterSummary
    , FilterSummary
    , filterSummary
    , fsFailureReason
    , fsStatus
    , fsLastUpdatedDateTime
    , fsName
    , fsFilterARN
    , fsCreationDateTime
    , fsDatasetGroupARN

    -- * HPOConfig
    , HPOConfig
    , hPOConfig
    , hpocAlgorithmHyperParameterRanges
    , hpocHpoResourceConfig
    , hpocHpoObjective

    -- * HPOObjective
    , HPOObjective
    , hPOObjective
    , hpooMetricName
    , hpooType
    , hpooMetricRegex

    -- * HPOResourceConfig
    , HPOResourceConfig
    , hPOResourceConfig
    , hporcMaxNumberOfTrainingJobs
    , hporcMaxParallelTrainingJobs

    -- * HyperParameterRanges
    , HyperParameterRanges
    , hyperParameterRanges
    , hprIntegerHyperParameterRanges
    , hprCategoricalHyperParameterRanges
    , hprContinuousHyperParameterRanges

    -- * IntegerHyperParameterRange
    , IntegerHyperParameterRange
    , integerHyperParameterRange
    , ihprMaxValue
    , ihprName
    , ihprMinValue

    -- * Recipe
    , Recipe
    , recipe
    , rStatus
    , rAlgorithmARN
    , rRecipeARN
    , rFeatureTransformationARN
    , rLastUpdatedDateTime
    , rName
    , rCreationDateTime
    , rRecipeType
    , rDescription

    -- * RecipeSummary
    , RecipeSummary
    , recipeSummary
    , rsStatus
    , rsRecipeARN
    , rsLastUpdatedDateTime
    , rsName
    , rsCreationDateTime

    -- * S3DataConfig
    , S3DataConfig
    , s3DataConfig
    , sdcKmsKeyARN
    , sdcPath

    -- * Solution
    , Solution
    , solution
    , sSolutionARN
    , sStatus
    , sPerformAutoML
    , sRecipeARN
    , sLastUpdatedDateTime
    , sEventType
    , sName
    , sAutoMLResult
    , sCreationDateTime
    , sDatasetGroupARN
    , sLatestSolutionVersion
    , sSolutionConfig
    , sPerformHPO

    -- * SolutionConfig
    , SolutionConfig
    , solutionConfig
    , scFeatureTransformationParameters
    , scHpoConfig
    , scEventValueThreshold
    , scAutoMLConfig
    , scAlgorithmHyperParameters

    -- * SolutionSummary
    , SolutionSummary
    , solutionSummary
    , ssSolutionARN
    , ssStatus
    , ssLastUpdatedDateTime
    , ssName
    , ssCreationDateTime

    -- * SolutionVersion
    , SolutionVersion
    , solutionVersion
    , svFailureReason
    , svSolutionARN
    , svStatus
    , svPerformAutoML
    , svTunedHPOParams
    , svRecipeARN
    , svLastUpdatedDateTime
    , svEventType
    , svCreationDateTime
    , svDatasetGroupARN
    , svTrainingMode
    , svTrainingHours
    , svSolutionConfig
    , svPerformHPO
    , svSolutionVersionARN

    -- * SolutionVersionSummary
    , SolutionVersionSummary
    , solutionVersionSummary
    , svsFailureReason
    , svsStatus
    , svsLastUpdatedDateTime
    , svsCreationDateTime
    , svsSolutionVersionARN

    -- * TunedHPOParams
    , TunedHPOParams
    , tunedHPOParams
    , thpopAlgorithmHyperParameters
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.AmazonPersonalize.Types.RecipeProvider
import Network.AWS.AmazonPersonalize.Types.TrainingMode
import Network.AWS.AmazonPersonalize.Types.Algorithm
import Network.AWS.AmazonPersonalize.Types.AlgorithmImage
import Network.AWS.AmazonPersonalize.Types.AutoMLConfig
import Network.AWS.AmazonPersonalize.Types.AutoMLResult
import Network.AWS.AmazonPersonalize.Types.BatchInferenceJob
import Network.AWS.AmazonPersonalize.Types.BatchInferenceJobConfig
import Network.AWS.AmazonPersonalize.Types.BatchInferenceJobInput
import Network.AWS.AmazonPersonalize.Types.BatchInferenceJobOutput
import Network.AWS.AmazonPersonalize.Types.BatchInferenceJobSummary
import Network.AWS.AmazonPersonalize.Types.Campaign
import Network.AWS.AmazonPersonalize.Types.CampaignConfig
import Network.AWS.AmazonPersonalize.Types.CampaignSummary
import Network.AWS.AmazonPersonalize.Types.CampaignUpdateSummary
import Network.AWS.AmazonPersonalize.Types.CategoricalHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.ContinuousHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.DataSource
import Network.AWS.AmazonPersonalize.Types.Dataset
import Network.AWS.AmazonPersonalize.Types.DatasetGroup
import Network.AWS.AmazonPersonalize.Types.DatasetGroupSummary
import Network.AWS.AmazonPersonalize.Types.DatasetImportJob
import Network.AWS.AmazonPersonalize.Types.DatasetImportJobSummary
import Network.AWS.AmazonPersonalize.Types.DatasetSchema
import Network.AWS.AmazonPersonalize.Types.DatasetSchemaSummary
import Network.AWS.AmazonPersonalize.Types.DatasetSummary
import Network.AWS.AmazonPersonalize.Types.DefaultCategoricalHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.DefaultContinuousHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.DefaultHyperParameterRanges
import Network.AWS.AmazonPersonalize.Types.DefaultIntegerHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.EventTracker
import Network.AWS.AmazonPersonalize.Types.EventTrackerSummary
import Network.AWS.AmazonPersonalize.Types.FeatureTransformation
import Network.AWS.AmazonPersonalize.Types.Filter
import Network.AWS.AmazonPersonalize.Types.FilterSummary
import Network.AWS.AmazonPersonalize.Types.HPOConfig
import Network.AWS.AmazonPersonalize.Types.HPOObjective
import Network.AWS.AmazonPersonalize.Types.HPOResourceConfig
import Network.AWS.AmazonPersonalize.Types.HyperParameterRanges
import Network.AWS.AmazonPersonalize.Types.IntegerHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.Recipe
import Network.AWS.AmazonPersonalize.Types.RecipeSummary
import Network.AWS.AmazonPersonalize.Types.S3DataConfig
import Network.AWS.AmazonPersonalize.Types.Solution
import Network.AWS.AmazonPersonalize.Types.SolutionConfig
import Network.AWS.AmazonPersonalize.Types.SolutionSummary
import Network.AWS.AmazonPersonalize.Types.SolutionVersion
import Network.AWS.AmazonPersonalize.Types.SolutionVersionSummary
import Network.AWS.AmazonPersonalize.Types.TunedHPOParams

-- | API version @2018-05-22@ of the Amazon Personalize SDK configuration.
amazonPersonalize :: Service
amazonPersonalize
  = Service{_svcAbbrev = "AmazonPersonalize",
            _svcSigner = v4, _svcPrefix = "personalize",
            _svcVersion = "2018-05-22",
            _svcEndpoint = defaultEndpoint amazonPersonalize,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "AmazonPersonalize",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has
              (hasCode "ProvisionedThroughputExceededException" .
                 hasStatus 400)
              e
            = Just "throughput_exceeded"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The specified resource already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException
  = _MatchServiceError amazonPersonalize
      "ResourceAlreadyExistsException"

-- | The token is not valid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException
  = _MatchServiceError amazonPersonalize
      "InvalidNextTokenException"

-- | Provide a valid value for the field or parameter.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException
  = _MatchServiceError amazonPersonalize
      "InvalidInputException"

-- | Could not find the specified resource.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError amazonPersonalize
      "ResourceNotFoundException"

-- | The limit on the number of requests per second has been exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError amazonPersonalize
      "LimitExceededException"

-- | The specified resource is in use.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError amazonPersonalize
      "ResourceInUseException"
