{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Personalize is a machine learning service that makes it easy to add individualized recommendations to customers.
--
--
module Network.AWS.AmazonPersonalize
    (
    -- * Service Configuration
      amazonPersonalize

    -- * Errors
    -- $errors

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListDatasetGroups (Paginated)
    , module Network.AWS.AmazonPersonalize.ListDatasetGroups

    -- ** CreateBatchInferenceJob 
    , module Network.AWS.AmazonPersonalize.CreateBatchInferenceJob

    -- ** CreateFilter 
    , module Network.AWS.AmazonPersonalize.CreateFilter

    -- ** CreateDatasetImportJob 
    , module Network.AWS.AmazonPersonalize.CreateDatasetImportJob

    -- ** DescribeSolution 
    , module Network.AWS.AmazonPersonalize.DescribeSolution

    -- ** DeleteCampaign 
    , module Network.AWS.AmazonPersonalize.DeleteCampaign

    -- ** UpdateCampaign 
    , module Network.AWS.AmazonPersonalize.UpdateCampaign

    -- ** ListCampaigns (Paginated)
    , module Network.AWS.AmazonPersonalize.ListCampaigns

    -- ** DescribeDataset 
    , module Network.AWS.AmazonPersonalize.DescribeDataset

    -- ** CreateSolutionVersion 
    , module Network.AWS.AmazonPersonalize.CreateSolutionVersion

    -- ** CreateCampaign 
    , module Network.AWS.AmazonPersonalize.CreateCampaign

    -- ** DescribeFilter 
    , module Network.AWS.AmazonPersonalize.DescribeFilter

    -- ** ListEventTrackers (Paginated)
    , module Network.AWS.AmazonPersonalize.ListEventTrackers

    -- ** CreateSolution 
    , module Network.AWS.AmazonPersonalize.CreateSolution

    -- ** DeleteEventTracker 
    , module Network.AWS.AmazonPersonalize.DeleteEventTracker

    -- ** DescribeDatasetImportJob 
    , module Network.AWS.AmazonPersonalize.DescribeDatasetImportJob

    -- ** ListSchemas (Paginated)
    , module Network.AWS.AmazonPersonalize.ListSchemas

    -- ** CreateEventTracker 
    , module Network.AWS.AmazonPersonalize.CreateEventTracker

    -- ** DeleteSolution 
    , module Network.AWS.AmazonPersonalize.DeleteSolution

    -- ** DescribeCampaign 
    , module Network.AWS.AmazonPersonalize.DescribeCampaign

    -- ** DeleteDataset 
    , module Network.AWS.AmazonPersonalize.DeleteDataset

    -- ** CreateDataset 
    , module Network.AWS.AmazonPersonalize.CreateDataset

    -- ** DescribeSolutionVersion 
    , module Network.AWS.AmazonPersonalize.DescribeSolutionVersion

    -- ** DescribeEventTracker 
    , module Network.AWS.AmazonPersonalize.DescribeEventTracker

    -- ** ListDatasetImportJobs (Paginated)
    , module Network.AWS.AmazonPersonalize.ListDatasetImportJobs

    -- ** DeleteFilter 
    , module Network.AWS.AmazonPersonalize.DeleteFilter

    -- ** ListBatchInferenceJobs (Paginated)
    , module Network.AWS.AmazonPersonalize.ListBatchInferenceJobs

    -- ** ListFilters 
    , module Network.AWS.AmazonPersonalize.ListFilters

    -- ** DeleteDatasetGroup 
    , module Network.AWS.AmazonPersonalize.DeleteDatasetGroup

    -- ** DescribeSchema 
    , module Network.AWS.AmazonPersonalize.DescribeSchema

    -- ** DescribeAlgorithm 
    , module Network.AWS.AmazonPersonalize.DescribeAlgorithm

    -- ** ListSolutionVersions (Paginated)
    , module Network.AWS.AmazonPersonalize.ListSolutionVersions

    -- ** DescribeBatchInferenceJob 
    , module Network.AWS.AmazonPersonalize.DescribeBatchInferenceJob

    -- ** CreateSchema 
    , module Network.AWS.AmazonPersonalize.CreateSchema

    -- ** DescribeRecipe 
    , module Network.AWS.AmazonPersonalize.DescribeRecipe

    -- ** ListSolutions (Paginated)
    , module Network.AWS.AmazonPersonalize.ListSolutions

    -- ** DescribeDatasetGroup 
    , module Network.AWS.AmazonPersonalize.DescribeDatasetGroup

    -- ** DescribeFeatureTransformation 
    , module Network.AWS.AmazonPersonalize.DescribeFeatureTransformation

    -- ** GetSolutionMetrics 
    , module Network.AWS.AmazonPersonalize.GetSolutionMetrics

    -- ** DeleteSchema 
    , module Network.AWS.AmazonPersonalize.DeleteSchema

    -- ** ListDatasets (Paginated)
    , module Network.AWS.AmazonPersonalize.ListDatasets

    -- ** CreateDatasetGroup 
    , module Network.AWS.AmazonPersonalize.CreateDatasetGroup

    -- ** ListRecipes (Paginated)
    , module Network.AWS.AmazonPersonalize.ListRecipes

    -- * Types

    -- ** RecipeProvider
    , RecipeProvider (..)

    -- ** TrainingMode
    , TrainingMode (..)

    -- ** Algorithm
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

    -- ** AlgorithmImage
    , AlgorithmImage
    , algorithmImage
    , aiName
    , aiDockerURI

    -- ** AutoMLConfig
    , AutoMLConfig
    , autoMLConfig
    , amlcRecipeList
    , amlcMetricName

    -- ** AutoMLResult
    , AutoMLResult
    , autoMLResult
    , amlrBestRecipeARN

    -- ** BatchInferenceJob
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

    -- ** BatchInferenceJobConfig
    , BatchInferenceJobConfig
    , batchInferenceJobConfig
    , bijcItemExplorationConfig

    -- ** BatchInferenceJobInput
    , BatchInferenceJobInput
    , batchInferenceJobInput
    , bijiS3DataSource

    -- ** BatchInferenceJobOutput
    , BatchInferenceJobOutput
    , batchInferenceJobOutput
    , bijoS3DataDestination

    -- ** BatchInferenceJobSummary
    , BatchInferenceJobSummary
    , batchInferenceJobSummary
    , bijsFailureReason
    , bijsStatus
    , bijsJobName
    , bijsLastUpdatedDateTime
    , bijsBatchInferenceJobARN
    , bijsCreationDateTime
    , bijsSolutionVersionARN

    -- ** Campaign
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

    -- ** CampaignConfig
    , CampaignConfig
    , campaignConfig
    , ccItemExplorationConfig

    -- ** CampaignSummary
    , CampaignSummary
    , campaignSummary
    , csFailureReason
    , csStatus
    , csLastUpdatedDateTime
    , csName
    , csCreationDateTime
    , csCampaignARN

    -- ** CampaignUpdateSummary
    , CampaignUpdateSummary
    , campaignUpdateSummary
    , cusFailureReason
    , cusStatus
    , cusLastUpdatedDateTime
    , cusCampaignConfig
    , cusMinProvisionedTPS
    , cusCreationDateTime
    , cusSolutionVersionARN

    -- ** CategoricalHyperParameterRange
    , CategoricalHyperParameterRange
    , categoricalHyperParameterRange
    , chprValues
    , chprName

    -- ** ContinuousHyperParameterRange
    , ContinuousHyperParameterRange
    , continuousHyperParameterRange
    , cMaxValue
    , cName
    , cMinValue

    -- ** DataSource
    , DataSource
    , dataSource
    , dsDataLocation

    -- ** Dataset
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

    -- ** DatasetGroup
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

    -- ** DatasetGroupSummary
    , DatasetGroupSummary
    , datasetGroupSummary
    , dgsFailureReason
    , dgsStatus
    , dgsLastUpdatedDateTime
    , dgsName
    , dgsCreationDateTime
    , dgsDatasetGroupARN

    -- ** DatasetImportJob
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

    -- ** DatasetImportJobSummary
    , DatasetImportJobSummary
    , datasetImportJobSummary
    , dijsFailureReason
    , dijsStatus
    , dijsJobName
    , dijsLastUpdatedDateTime
    , dijsDatasetImportJobARN
    , dijsCreationDateTime

    -- ** DatasetSchema
    , DatasetSchema
    , datasetSchema
    , dsLastUpdatedDateTime
    , dsSchema
    , dsSchemaARN
    , dsName
    , dsCreationDateTime

    -- ** DatasetSchemaSummary
    , DatasetSchemaSummary
    , datasetSchemaSummary
    , dssLastUpdatedDateTime
    , dssSchemaARN
    , dssName
    , dssCreationDateTime

    -- ** DatasetSummary
    , DatasetSummary
    , datasetSummary
    , datStatus
    , datDatasetARN
    , datLastUpdatedDateTime
    , datName
    , datDatasetType
    , datCreationDateTime

    -- ** DefaultCategoricalHyperParameterRange
    , DefaultCategoricalHyperParameterRange
    , defaultCategoricalHyperParameterRange
    , defIsTunable
    , defValues
    , defName

    -- ** DefaultContinuousHyperParameterRange
    , DefaultContinuousHyperParameterRange
    , defaultContinuousHyperParameterRange
    , dchprMaxValue
    , dchprIsTunable
    , dchprName
    , dchprMinValue

    -- ** DefaultHyperParameterRanges
    , DefaultHyperParameterRanges
    , defaultHyperParameterRanges
    , dhprIntegerHyperParameterRanges
    , dhprCategoricalHyperParameterRanges
    , dhprContinuousHyperParameterRanges

    -- ** DefaultIntegerHyperParameterRange
    , DefaultIntegerHyperParameterRange
    , defaultIntegerHyperParameterRange
    , dihprMaxValue
    , dihprIsTunable
    , dihprName
    , dihprMinValue

    -- ** EventTracker
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

    -- ** EventTrackerSummary
    , EventTrackerSummary
    , eventTrackerSummary
    , etsStatus
    , etsLastUpdatedDateTime
    , etsName
    , etsCreationDateTime
    , etsEventTrackerARN

    -- ** FeatureTransformation
    , FeatureTransformation
    , featureTransformation
    , ftStatus
    , ftFeatureTransformationARN
    , ftLastUpdatedDateTime
    , ftName
    , ftCreationDateTime
    , ftDefaultParameters

    -- ** Filter
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

    -- ** FilterSummary
    , FilterSummary
    , filterSummary
    , fsFailureReason
    , fsStatus
    , fsLastUpdatedDateTime
    , fsName
    , fsFilterARN
    , fsCreationDateTime
    , fsDatasetGroupARN

    -- ** HPOConfig
    , HPOConfig
    , hPOConfig
    , hpocAlgorithmHyperParameterRanges
    , hpocHpoResourceConfig
    , hpocHpoObjective

    -- ** HPOObjective
    , HPOObjective
    , hPOObjective
    , hpooMetricName
    , hpooType
    , hpooMetricRegex

    -- ** HPOResourceConfig
    , HPOResourceConfig
    , hPOResourceConfig
    , hporcMaxNumberOfTrainingJobs
    , hporcMaxParallelTrainingJobs

    -- ** HyperParameterRanges
    , HyperParameterRanges
    , hyperParameterRanges
    , hprIntegerHyperParameterRanges
    , hprCategoricalHyperParameterRanges
    , hprContinuousHyperParameterRanges

    -- ** IntegerHyperParameterRange
    , IntegerHyperParameterRange
    , integerHyperParameterRange
    , ihprMaxValue
    , ihprName
    , ihprMinValue

    -- ** Recipe
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

    -- ** RecipeSummary
    , RecipeSummary
    , recipeSummary
    , rsStatus
    , rsRecipeARN
    , rsLastUpdatedDateTime
    , rsName
    , rsCreationDateTime

    -- ** S3DataConfig
    , S3DataConfig
    , s3DataConfig
    , sdcKmsKeyARN
    , sdcPath

    -- ** Solution
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

    -- ** SolutionConfig
    , SolutionConfig
    , solutionConfig
    , scFeatureTransformationParameters
    , scHpoConfig
    , scEventValueThreshold
    , scAutoMLConfig
    , scAlgorithmHyperParameters

    -- ** SolutionSummary
    , SolutionSummary
    , solutionSummary
    , ssSolutionARN
    , ssStatus
    , ssLastUpdatedDateTime
    , ssName
    , ssCreationDateTime

    -- ** SolutionVersion
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

    -- ** SolutionVersionSummary
    , SolutionVersionSummary
    , solutionVersionSummary
    , svsFailureReason
    , svsStatus
    , svsLastUpdatedDateTime
    , svsCreationDateTime
    , svsSolutionVersionARN

    -- ** TunedHPOParams
    , TunedHPOParams
    , tunedHPOParams
    , thpopAlgorithmHyperParameters
    ) where

import Network.AWS.AmazonPersonalize.CreateBatchInferenceJob
import Network.AWS.AmazonPersonalize.CreateCampaign
import Network.AWS.AmazonPersonalize.CreateDataset
import Network.AWS.AmazonPersonalize.CreateDatasetGroup
import Network.AWS.AmazonPersonalize.CreateDatasetImportJob
import Network.AWS.AmazonPersonalize.CreateEventTracker
import Network.AWS.AmazonPersonalize.CreateFilter
import Network.AWS.AmazonPersonalize.CreateSchema
import Network.AWS.AmazonPersonalize.CreateSolution
import Network.AWS.AmazonPersonalize.CreateSolutionVersion
import Network.AWS.AmazonPersonalize.DeleteCampaign
import Network.AWS.AmazonPersonalize.DeleteDataset
import Network.AWS.AmazonPersonalize.DeleteDatasetGroup
import Network.AWS.AmazonPersonalize.DeleteEventTracker
import Network.AWS.AmazonPersonalize.DeleteFilter
import Network.AWS.AmazonPersonalize.DeleteSchema
import Network.AWS.AmazonPersonalize.DeleteSolution
import Network.AWS.AmazonPersonalize.DescribeAlgorithm
import Network.AWS.AmazonPersonalize.DescribeBatchInferenceJob
import Network.AWS.AmazonPersonalize.DescribeCampaign
import Network.AWS.AmazonPersonalize.DescribeDataset
import Network.AWS.AmazonPersonalize.DescribeDatasetGroup
import Network.AWS.AmazonPersonalize.DescribeDatasetImportJob
import Network.AWS.AmazonPersonalize.DescribeEventTracker
import Network.AWS.AmazonPersonalize.DescribeFeatureTransformation
import Network.AWS.AmazonPersonalize.DescribeFilter
import Network.AWS.AmazonPersonalize.DescribeRecipe
import Network.AWS.AmazonPersonalize.DescribeSchema
import Network.AWS.AmazonPersonalize.DescribeSolution
import Network.AWS.AmazonPersonalize.DescribeSolutionVersion
import Network.AWS.AmazonPersonalize.GetSolutionMetrics
import Network.AWS.AmazonPersonalize.ListBatchInferenceJobs
import Network.AWS.AmazonPersonalize.ListCampaigns
import Network.AWS.AmazonPersonalize.ListDatasetGroups
import Network.AWS.AmazonPersonalize.ListDatasetImportJobs
import Network.AWS.AmazonPersonalize.ListDatasets
import Network.AWS.AmazonPersonalize.ListEventTrackers
import Network.AWS.AmazonPersonalize.ListFilters
import Network.AWS.AmazonPersonalize.ListRecipes
import Network.AWS.AmazonPersonalize.ListSchemas
import Network.AWS.AmazonPersonalize.ListSolutionVersions
import Network.AWS.AmazonPersonalize.ListSolutions
import Network.AWS.AmazonPersonalize.Types
import Network.AWS.AmazonPersonalize.UpdateCampaign
import Network.AWS.AmazonPersonalize.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AmazonPersonalize'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
