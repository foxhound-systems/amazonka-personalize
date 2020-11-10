{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AmazonPersonalize
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AmazonPersonalize where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AmazonPersonalize
import Test.AWS.AmazonPersonalize.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListDatasetGroups $
--             listDatasetGroups
--
--         , requestCreateBatchInferenceJob $
--             createBatchInferenceJob
--
--         , requestCreateFilter $
--             createFilter
--
--         , requestCreateDatasetImportJob $
--             createDatasetImportJob
--
--         , requestDescribeSolution $
--             describeSolution
--
--         , requestDeleteCampaign $
--             deleteCampaign
--
--         , requestUpdateCampaign $
--             updateCampaign
--
--         , requestListCampaigns $
--             listCampaigns
--
--         , requestDescribeDataset $
--             describeDataset
--
--         , requestCreateSolutionVersion $
--             createSolutionVersion
--
--         , requestCreateCampaign $
--             createCampaign
--
--         , requestDescribeFilter $
--             describeFilter
--
--         , requestListEventTrackers $
--             listEventTrackers
--
--         , requestCreateSolution $
--             createSolution
--
--         , requestDeleteEventTracker $
--             deleteEventTracker
--
--         , requestDescribeDatasetImportJob $
--             describeDatasetImportJob
--
--         , requestListSchemas $
--             listSchemas
--
--         , requestCreateEventTracker $
--             createEventTracker
--
--         , requestDeleteSolution $
--             deleteSolution
--
--         , requestDescribeCampaign $
--             describeCampaign
--
--         , requestDeleteDataset $
--             deleteDataset
--
--         , requestCreateDataset $
--             createDataset
--
--         , requestDescribeSolutionVersion $
--             describeSolutionVersion
--
--         , requestDescribeEventTracker $
--             describeEventTracker
--
--         , requestListDatasetImportJobs $
--             listDatasetImportJobs
--
--         , requestDeleteFilter $
--             deleteFilter
--
--         , requestListBatchInferenceJobs $
--             listBatchInferenceJobs
--
--         , requestListFilters $
--             listFilters
--
--         , requestDeleteDatasetGroup $
--             deleteDatasetGroup
--
--         , requestDescribeSchema $
--             describeSchema
--
--         , requestDescribeAlgorithm $
--             describeAlgorithm
--
--         , requestListSolutionVersions $
--             listSolutionVersions
--
--         , requestDescribeBatchInferenceJob $
--             describeBatchInferenceJob
--
--         , requestCreateSchema $
--             createSchema
--
--         , requestDescribeRecipe $
--             describeRecipe
--
--         , requestListSolutions $
--             listSolutions
--
--         , requestDescribeDatasetGroup $
--             describeDatasetGroup
--
--         , requestDescribeFeatureTransformation $
--             describeFeatureTransformation
--
--         , requestGetSolutionMetrics $
--             getSolutionMetrics
--
--         , requestDeleteSchema $
--             deleteSchema
--
--         , requestListDatasets $
--             listDatasets
--
--         , requestCreateDatasetGroup $
--             createDatasetGroup
--
--         , requestListRecipes $
--             listRecipes
--
--           ]

--     , testGroup "response"
--         [ responseListDatasetGroups $
--             listDatasetGroupsResponse
--
--         , responseCreateBatchInferenceJob $
--             createBatchInferenceJobResponse
--
--         , responseCreateFilter $
--             createFilterResponse
--
--         , responseCreateDatasetImportJob $
--             createDatasetImportJobResponse
--
--         , responseDescribeSolution $
--             describeSolutionResponse
--
--         , responseDeleteCampaign $
--             deleteCampaignResponse
--
--         , responseUpdateCampaign $
--             updateCampaignResponse
--
--         , responseListCampaigns $
--             listCampaignsResponse
--
--         , responseDescribeDataset $
--             describeDatasetResponse
--
--         , responseCreateSolutionVersion $
--             createSolutionVersionResponse
--
--         , responseCreateCampaign $
--             createCampaignResponse
--
--         , responseDescribeFilter $
--             describeFilterResponse
--
--         , responseListEventTrackers $
--             listEventTrackersResponse
--
--         , responseCreateSolution $
--             createSolutionResponse
--
--         , responseDeleteEventTracker $
--             deleteEventTrackerResponse
--
--         , responseDescribeDatasetImportJob $
--             describeDatasetImportJobResponse
--
--         , responseListSchemas $
--             listSchemasResponse
--
--         , responseCreateEventTracker $
--             createEventTrackerResponse
--
--         , responseDeleteSolution $
--             deleteSolutionResponse
--
--         , responseDescribeCampaign $
--             describeCampaignResponse
--
--         , responseDeleteDataset $
--             deleteDatasetResponse
--
--         , responseCreateDataset $
--             createDatasetResponse
--
--         , responseDescribeSolutionVersion $
--             describeSolutionVersionResponse
--
--         , responseDescribeEventTracker $
--             describeEventTrackerResponse
--
--         , responseListDatasetImportJobs $
--             listDatasetImportJobsResponse
--
--         , responseDeleteFilter $
--             deleteFilterResponse
--
--         , responseListBatchInferenceJobs $
--             listBatchInferenceJobsResponse
--
--         , responseListFilters $
--             listFiltersResponse
--
--         , responseDeleteDatasetGroup $
--             deleteDatasetGroupResponse
--
--         , responseDescribeSchema $
--             describeSchemaResponse
--
--         , responseDescribeAlgorithm $
--             describeAlgorithmResponse
--
--         , responseListSolutionVersions $
--             listSolutionVersionsResponse
--
--         , responseDescribeBatchInferenceJob $
--             describeBatchInferenceJobResponse
--
--         , responseCreateSchema $
--             createSchemaResponse
--
--         , responseDescribeRecipe $
--             describeRecipeResponse
--
--         , responseListSolutions $
--             listSolutionsResponse
--
--         , responseDescribeDatasetGroup $
--             describeDatasetGroupResponse
--
--         , responseDescribeFeatureTransformation $
--             describeFeatureTransformationResponse
--
--         , responseGetSolutionMetrics $
--             getSolutionMetricsResponse
--
--         , responseDeleteSchema $
--             deleteSchemaResponse
--
--         , responseListDatasets $
--             listDatasetsResponse
--
--         , responseCreateDatasetGroup $
--             createDatasetGroupResponse
--
--         , responseListRecipes $
--             listRecipesResponse
--
--           ]
--     ]

-- Requests

requestListDatasetGroups :: ListDatasetGroups -> TestTree
requestListDatasetGroups = req
    "ListDatasetGroups"
    "fixture/ListDatasetGroups.yaml"

requestCreateBatchInferenceJob :: CreateBatchInferenceJob -> TestTree
requestCreateBatchInferenceJob = req
    "CreateBatchInferenceJob"
    "fixture/CreateBatchInferenceJob.yaml"

requestCreateFilter :: CreateFilter -> TestTree
requestCreateFilter = req
    "CreateFilter"
    "fixture/CreateFilter.yaml"

requestCreateDatasetImportJob :: CreateDatasetImportJob -> TestTree
requestCreateDatasetImportJob = req
    "CreateDatasetImportJob"
    "fixture/CreateDatasetImportJob.yaml"

requestDescribeSolution :: DescribeSolution -> TestTree
requestDescribeSolution = req
    "DescribeSolution"
    "fixture/DescribeSolution.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign = req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign = req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestListCampaigns :: ListCampaigns -> TestTree
requestListCampaigns = req
    "ListCampaigns"
    "fixture/ListCampaigns.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset = req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestCreateSolutionVersion :: CreateSolutionVersion -> TestTree
requestCreateSolutionVersion = req
    "CreateSolutionVersion"
    "fixture/CreateSolutionVersion.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign = req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestDescribeFilter :: DescribeFilter -> TestTree
requestDescribeFilter = req
    "DescribeFilter"
    "fixture/DescribeFilter.yaml"

requestListEventTrackers :: ListEventTrackers -> TestTree
requestListEventTrackers = req
    "ListEventTrackers"
    "fixture/ListEventTrackers.yaml"

requestCreateSolution :: CreateSolution -> TestTree
requestCreateSolution = req
    "CreateSolution"
    "fixture/CreateSolution.yaml"

requestDeleteEventTracker :: DeleteEventTracker -> TestTree
requestDeleteEventTracker = req
    "DeleteEventTracker"
    "fixture/DeleteEventTracker.yaml"

requestDescribeDatasetImportJob :: DescribeDatasetImportJob -> TestTree
requestDescribeDatasetImportJob = req
    "DescribeDatasetImportJob"
    "fixture/DescribeDatasetImportJob.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas = req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestCreateEventTracker :: CreateEventTracker -> TestTree
requestCreateEventTracker = req
    "CreateEventTracker"
    "fixture/CreateEventTracker.yaml"

requestDeleteSolution :: DeleteSolution -> TestTree
requestDeleteSolution = req
    "DeleteSolution"
    "fixture/DeleteSolution.yaml"

requestDescribeCampaign :: DescribeCampaign -> TestTree
requestDescribeCampaign = req
    "DescribeCampaign"
    "fixture/DescribeCampaign.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset = req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset = req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestDescribeSolutionVersion :: DescribeSolutionVersion -> TestTree
requestDescribeSolutionVersion = req
    "DescribeSolutionVersion"
    "fixture/DescribeSolutionVersion.yaml"

requestDescribeEventTracker :: DescribeEventTracker -> TestTree
requestDescribeEventTracker = req
    "DescribeEventTracker"
    "fixture/DescribeEventTracker.yaml"

requestListDatasetImportJobs :: ListDatasetImportJobs -> TestTree
requestListDatasetImportJobs = req
    "ListDatasetImportJobs"
    "fixture/ListDatasetImportJobs.yaml"

requestDeleteFilter :: DeleteFilter -> TestTree
requestDeleteFilter = req
    "DeleteFilter"
    "fixture/DeleteFilter.yaml"

requestListBatchInferenceJobs :: ListBatchInferenceJobs -> TestTree
requestListBatchInferenceJobs = req
    "ListBatchInferenceJobs"
    "fixture/ListBatchInferenceJobs.yaml"

requestListFilters :: ListFilters -> TestTree
requestListFilters = req
    "ListFilters"
    "fixture/ListFilters.yaml"

requestDeleteDatasetGroup :: DeleteDatasetGroup -> TestTree
requestDeleteDatasetGroup = req
    "DeleteDatasetGroup"
    "fixture/DeleteDatasetGroup.yaml"

requestDescribeSchema :: DescribeSchema -> TestTree
requestDescribeSchema = req
    "DescribeSchema"
    "fixture/DescribeSchema.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm = req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

requestListSolutionVersions :: ListSolutionVersions -> TestTree
requestListSolutionVersions = req
    "ListSolutionVersions"
    "fixture/ListSolutionVersions.yaml"

requestDescribeBatchInferenceJob :: DescribeBatchInferenceJob -> TestTree
requestDescribeBatchInferenceJob = req
    "DescribeBatchInferenceJob"
    "fixture/DescribeBatchInferenceJob.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema = req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestDescribeRecipe :: DescribeRecipe -> TestTree
requestDescribeRecipe = req
    "DescribeRecipe"
    "fixture/DescribeRecipe.yaml"

requestListSolutions :: ListSolutions -> TestTree
requestListSolutions = req
    "ListSolutions"
    "fixture/ListSolutions.yaml"

requestDescribeDatasetGroup :: DescribeDatasetGroup -> TestTree
requestDescribeDatasetGroup = req
    "DescribeDatasetGroup"
    "fixture/DescribeDatasetGroup.yaml"

requestDescribeFeatureTransformation :: DescribeFeatureTransformation -> TestTree
requestDescribeFeatureTransformation = req
    "DescribeFeatureTransformation"
    "fixture/DescribeFeatureTransformation.yaml"

requestGetSolutionMetrics :: GetSolutionMetrics -> TestTree
requestGetSolutionMetrics = req
    "GetSolutionMetrics"
    "fixture/GetSolutionMetrics.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema = req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets = req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestCreateDatasetGroup :: CreateDatasetGroup -> TestTree
requestCreateDatasetGroup = req
    "CreateDatasetGroup"
    "fixture/CreateDatasetGroup.yaml"

requestListRecipes :: ListRecipes -> TestTree
requestListRecipes = req
    "ListRecipes"
    "fixture/ListRecipes.yaml"

-- Responses

responseListDatasetGroups :: ListDatasetGroupsResponse -> TestTree
responseListDatasetGroups = res
    "ListDatasetGroupsResponse"
    "fixture/ListDatasetGroupsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListDatasetGroups)

responseCreateBatchInferenceJob :: CreateBatchInferenceJobResponse -> TestTree
responseCreateBatchInferenceJob = res
    "CreateBatchInferenceJobResponse"
    "fixture/CreateBatchInferenceJobResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateBatchInferenceJob)

responseCreateFilter :: CreateFilterResponse -> TestTree
responseCreateFilter = res
    "CreateFilterResponse"
    "fixture/CreateFilterResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateFilter)

responseCreateDatasetImportJob :: CreateDatasetImportJobResponse -> TestTree
responseCreateDatasetImportJob = res
    "CreateDatasetImportJobResponse"
    "fixture/CreateDatasetImportJobResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateDatasetImportJob)

responseDescribeSolution :: DescribeSolutionResponse -> TestTree
responseDescribeSolution = res
    "DescribeSolutionResponse"
    "fixture/DescribeSolutionResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeSolution)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign = res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DeleteCampaign)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign = res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy UpdateCampaign)

responseListCampaigns :: ListCampaignsResponse -> TestTree
responseListCampaigns = res
    "ListCampaignsResponse"
    "fixture/ListCampaignsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListCampaigns)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset = res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeDataset)

responseCreateSolutionVersion :: CreateSolutionVersionResponse -> TestTree
responseCreateSolutionVersion = res
    "CreateSolutionVersionResponse"
    "fixture/CreateSolutionVersionResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateSolutionVersion)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign = res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateCampaign)

responseDescribeFilter :: DescribeFilterResponse -> TestTree
responseDescribeFilter = res
    "DescribeFilterResponse"
    "fixture/DescribeFilterResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeFilter)

responseListEventTrackers :: ListEventTrackersResponse -> TestTree
responseListEventTrackers = res
    "ListEventTrackersResponse"
    "fixture/ListEventTrackersResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListEventTrackers)

responseCreateSolution :: CreateSolutionResponse -> TestTree
responseCreateSolution = res
    "CreateSolutionResponse"
    "fixture/CreateSolutionResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateSolution)

responseDeleteEventTracker :: DeleteEventTrackerResponse -> TestTree
responseDeleteEventTracker = res
    "DeleteEventTrackerResponse"
    "fixture/DeleteEventTrackerResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DeleteEventTracker)

responseDescribeDatasetImportJob :: DescribeDatasetImportJobResponse -> TestTree
responseDescribeDatasetImportJob = res
    "DescribeDatasetImportJobResponse"
    "fixture/DescribeDatasetImportJobResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeDatasetImportJob)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas = res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListSchemas)

responseCreateEventTracker :: CreateEventTrackerResponse -> TestTree
responseCreateEventTracker = res
    "CreateEventTrackerResponse"
    "fixture/CreateEventTrackerResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateEventTracker)

responseDeleteSolution :: DeleteSolutionResponse -> TestTree
responseDeleteSolution = res
    "DeleteSolutionResponse"
    "fixture/DeleteSolutionResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DeleteSolution)

responseDescribeCampaign :: DescribeCampaignResponse -> TestTree
responseDescribeCampaign = res
    "DescribeCampaignResponse"
    "fixture/DescribeCampaignResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeCampaign)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset = res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DeleteDataset)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset = res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateDataset)

responseDescribeSolutionVersion :: DescribeSolutionVersionResponse -> TestTree
responseDescribeSolutionVersion = res
    "DescribeSolutionVersionResponse"
    "fixture/DescribeSolutionVersionResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeSolutionVersion)

responseDescribeEventTracker :: DescribeEventTrackerResponse -> TestTree
responseDescribeEventTracker = res
    "DescribeEventTrackerResponse"
    "fixture/DescribeEventTrackerResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeEventTracker)

responseListDatasetImportJobs :: ListDatasetImportJobsResponse -> TestTree
responseListDatasetImportJobs = res
    "ListDatasetImportJobsResponse"
    "fixture/ListDatasetImportJobsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListDatasetImportJobs)

responseDeleteFilter :: DeleteFilterResponse -> TestTree
responseDeleteFilter = res
    "DeleteFilterResponse"
    "fixture/DeleteFilterResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DeleteFilter)

responseListBatchInferenceJobs :: ListBatchInferenceJobsResponse -> TestTree
responseListBatchInferenceJobs = res
    "ListBatchInferenceJobsResponse"
    "fixture/ListBatchInferenceJobsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListBatchInferenceJobs)

responseListFilters :: ListFiltersResponse -> TestTree
responseListFilters = res
    "ListFiltersResponse"
    "fixture/ListFiltersResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListFilters)

responseDeleteDatasetGroup :: DeleteDatasetGroupResponse -> TestTree
responseDeleteDatasetGroup = res
    "DeleteDatasetGroupResponse"
    "fixture/DeleteDatasetGroupResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DeleteDatasetGroup)

responseDescribeSchema :: DescribeSchemaResponse -> TestTree
responseDescribeSchema = res
    "DescribeSchemaResponse"
    "fixture/DescribeSchemaResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeSchema)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm = res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeAlgorithm)

responseListSolutionVersions :: ListSolutionVersionsResponse -> TestTree
responseListSolutionVersions = res
    "ListSolutionVersionsResponse"
    "fixture/ListSolutionVersionsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListSolutionVersions)

responseDescribeBatchInferenceJob :: DescribeBatchInferenceJobResponse -> TestTree
responseDescribeBatchInferenceJob = res
    "DescribeBatchInferenceJobResponse"
    "fixture/DescribeBatchInferenceJobResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeBatchInferenceJob)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema = res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateSchema)

responseDescribeRecipe :: DescribeRecipeResponse -> TestTree
responseDescribeRecipe = res
    "DescribeRecipeResponse"
    "fixture/DescribeRecipeResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeRecipe)

responseListSolutions :: ListSolutionsResponse -> TestTree
responseListSolutions = res
    "ListSolutionsResponse"
    "fixture/ListSolutionsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListSolutions)

responseDescribeDatasetGroup :: DescribeDatasetGroupResponse -> TestTree
responseDescribeDatasetGroup = res
    "DescribeDatasetGroupResponse"
    "fixture/DescribeDatasetGroupResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeDatasetGroup)

responseDescribeFeatureTransformation :: DescribeFeatureTransformationResponse -> TestTree
responseDescribeFeatureTransformation = res
    "DescribeFeatureTransformationResponse"
    "fixture/DescribeFeatureTransformationResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DescribeFeatureTransformation)

responseGetSolutionMetrics :: GetSolutionMetricsResponse -> TestTree
responseGetSolutionMetrics = res
    "GetSolutionMetricsResponse"
    "fixture/GetSolutionMetricsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy GetSolutionMetrics)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema = res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy DeleteSchema)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets = res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListDatasets)

responseCreateDatasetGroup :: CreateDatasetGroupResponse -> TestTree
responseCreateDatasetGroup = res
    "CreateDatasetGroupResponse"
    "fixture/CreateDatasetGroupResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy CreateDatasetGroup)

responseListRecipes :: ListRecipesResponse -> TestTree
responseListRecipes = res
    "ListRecipesResponse"
    "fixture/ListRecipesResponse.proto"
    amazonPersonalize
    (Proxy :: Proxy ListRecipes)
