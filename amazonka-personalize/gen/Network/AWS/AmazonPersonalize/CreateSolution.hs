{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.CreateSolution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the configuration for training a model. A trained model is known as a solution. After the configuration is created, you train the model (create a solution) by calling the 'CreateSolutionVersion' operation. Every time you call @CreateSolutionVersion@ , a new version of the solution is created.
--
--
-- After creating a solution version, you check its accuracy by calling 'GetSolutionMetrics' . When you are satisfied with the version, you deploy it using 'CreateCampaign' . The campaign provides recommendations to a client through the <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations> API.
--
-- To train a model, Amazon Personalize requires training data and a recipe. The training data comes from the dataset group that you provide in the request. A recipe specifies the training algorithm and a feature transformation. You can specify one of the predefined recipes provided by Amazon Personalize. Alternatively, you can specify @performAutoML@ and Amazon Personalize will analyze your data and select the optimum USER_PERSONALIZATION recipe for you.
--
-- __Status__ 
--
-- A solution can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--     * DELETE PENDING > DELETE IN_PROGRESS
--
--
--
-- To get the status of the solution, call 'DescribeSolution' . Wait until the status shows as ACTIVE before calling @CreateSolutionVersion@ .
--
-- __Related APIs__ 
--
--     * 'ListSolutions' 
--
--     * 'CreateSolutionVersion' 
--
--     * 'DescribeSolution' 
--
--     * 'DeleteSolution' 
--
--
--
--     * 'ListSolutionVersions' 
--
--     * 'DescribeSolutionVersion' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateSolution
    (
    -- * Creating a Request
      createSolution
    , CreateSolution
    -- * Request Lenses
    , crePerformAutoML
    , creRecipeARN
    , creEventType
    , creSolutionConfig
    , crePerformHPO
    , creName
    , creDatasetGroupARN

    -- * Destructuring the Response
    , createSolutionResponse
    , CreateSolutionResponse
    -- * Response Lenses
    , crsSolutionARN
    , crsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSolution' smart constructor.
data CreateSolution = CreateSolution'{_crePerformAutoML
                                      :: !(Maybe Bool),
                                      _creRecipeARN :: !(Maybe Text),
                                      _creEventType :: !(Maybe Text),
                                      _creSolutionConfig ::
                                      !(Maybe SolutionConfig),
                                      _crePerformHPO :: !(Maybe Bool),
                                      _creName :: !Text,
                                      _creDatasetGroupARN :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSolution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crePerformAutoML' - Whether to perform automated machine learning (AutoML). The default is @false@ . For this case, you must specify @recipeArn@ . When set to @true@ , Amazon Personalize analyzes your training data and selects the optimal USER_PERSONALIZATION recipe and hyperparameters. In this case, you must omit @recipeArn@ . Amazon Personalize determines the optimal recipe by running tests with different values for the hyperparameters. AutoML lengthens the training process as compared to selecting a specific recipe.
--
-- * 'creRecipeARN' - The ARN of the recipe to use for model training. Only specified when @performAutoML@ is false.
--
-- * 'creEventType' - When your have multiple event types (using an @EVENT_TYPE@ schema field), this parameter specifies which event type (for example, 'click' or 'like') is used for training the model.
--
-- * 'creSolutionConfig' - The configuration to use with the solution. When @performAutoML@ is set to true, Amazon Personalize only evaluates the @autoMLConfig@ section of the solution configuration.
--
-- * 'crePerformHPO' - Whether to perform hyperparameter optimization (HPO) on the specified or selected recipe. The default is @false@ . When performing AutoML, this parameter is always @true@ and you should not set it to @false@ .
--
-- * 'creName' - The name for the solution.
--
-- * 'creDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group that provides the training data.
createSolution
    :: Text -- ^ 'creName'
    -> Text -- ^ 'creDatasetGroupARN'
    -> CreateSolution
createSolution pName_ pDatasetGroupARN_
  = CreateSolution'{_crePerformAutoML = Nothing,
                    _creRecipeARN = Nothing, _creEventType = Nothing,
                    _creSolutionConfig = Nothing,
                    _crePerformHPO = Nothing, _creName = pName_,
                    _creDatasetGroupARN = pDatasetGroupARN_}

-- | Whether to perform automated machine learning (AutoML). The default is @false@ . For this case, you must specify @recipeArn@ . When set to @true@ , Amazon Personalize analyzes your training data and selects the optimal USER_PERSONALIZATION recipe and hyperparameters. In this case, you must omit @recipeArn@ . Amazon Personalize determines the optimal recipe by running tests with different values for the hyperparameters. AutoML lengthens the training process as compared to selecting a specific recipe.
crePerformAutoML :: Lens' CreateSolution (Maybe Bool)
crePerformAutoML = lens _crePerformAutoML (\ s a -> s{_crePerformAutoML = a})

-- | The ARN of the recipe to use for model training. Only specified when @performAutoML@ is false.
creRecipeARN :: Lens' CreateSolution (Maybe Text)
creRecipeARN = lens _creRecipeARN (\ s a -> s{_creRecipeARN = a})

-- | When your have multiple event types (using an @EVENT_TYPE@ schema field), this parameter specifies which event type (for example, 'click' or 'like') is used for training the model.
creEventType :: Lens' CreateSolution (Maybe Text)
creEventType = lens _creEventType (\ s a -> s{_creEventType = a})

-- | The configuration to use with the solution. When @performAutoML@ is set to true, Amazon Personalize only evaluates the @autoMLConfig@ section of the solution configuration.
creSolutionConfig :: Lens' CreateSolution (Maybe SolutionConfig)
creSolutionConfig = lens _creSolutionConfig (\ s a -> s{_creSolutionConfig = a})

-- | Whether to perform hyperparameter optimization (HPO) on the specified or selected recipe. The default is @false@ . When performing AutoML, this parameter is always @true@ and you should not set it to @false@ .
crePerformHPO :: Lens' CreateSolution (Maybe Bool)
crePerformHPO = lens _crePerformHPO (\ s a -> s{_crePerformHPO = a})

-- | The name for the solution.
creName :: Lens' CreateSolution Text
creName = lens _creName (\ s a -> s{_creName = a})

-- | The Amazon Resource Name (ARN) of the dataset group that provides the training data.
creDatasetGroupARN :: Lens' CreateSolution Text
creDatasetGroupARN = lens _creDatasetGroupARN (\ s a -> s{_creDatasetGroupARN = a})

instance AWSRequest CreateSolution where
        type Rs CreateSolution = CreateSolutionResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateSolutionResponse' <$>
                   (x .?> "solutionArn") <*> (pure (fromEnum s)))

instance Hashable CreateSolution where

instance NFData CreateSolution where

instance ToHeaders CreateSolution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateSolution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSolution where
        toJSON CreateSolution'{..}
          = object
              (catMaybes
                 [("performAutoML" .=) <$> _crePerformAutoML,
                  ("recipeArn" .=) <$> _creRecipeARN,
                  ("eventType" .=) <$> _creEventType,
                  ("solutionConfig" .=) <$> _creSolutionConfig,
                  ("performHPO" .=) <$> _crePerformHPO,
                  Just ("name" .= _creName),
                  Just ("datasetGroupArn" .= _creDatasetGroupARN)])

instance ToPath CreateSolution where
        toPath = const "/"

instance ToQuery CreateSolution where
        toQuery = const mempty

-- | /See:/ 'createSolutionResponse' smart constructor.
data CreateSolutionResponse = CreateSolutionResponse'{_crsSolutionARN
                                                      :: !(Maybe Text),
                                                      _crsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'CreateSolutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsSolutionARN' - The ARN of the solution.
--
-- * 'crsResponseStatus' - -- | The response status code.
createSolutionResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateSolutionResponse
createSolutionResponse pResponseStatus_
  = CreateSolutionResponse'{_crsSolutionARN = Nothing,
                            _crsResponseStatus = pResponseStatus_}

-- | The ARN of the solution.
crsSolutionARN :: Lens' CreateSolutionResponse (Maybe Text)
crsSolutionARN = lens _crsSolutionARN (\ s a -> s{_crsSolutionARN = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateSolutionResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateSolutionResponse where
