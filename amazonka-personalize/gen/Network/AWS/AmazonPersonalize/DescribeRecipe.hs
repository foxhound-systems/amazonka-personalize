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
-- Module      : Network.AWS.AmazonPersonalize.DescribeRecipe
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a recipe.
--
--
-- A recipe contains three items:
--
--     * An algorithm that trains a model.
--
--     * Hyperparameters that govern the training.
--
--     * Feature transformation information for modifying the input data before training.
--
--
--
-- Amazon Personalize provides a set of predefined recipes. You specify a recipe when you create a solution with the 'CreateSolution' API. @CreateSolution@ trains a model by using the algorithm in the specified recipe and a training dataset. The solution, when deployed as a campaign, can provide recommendations using the <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations> API.
--
module Network.AWS.AmazonPersonalize.DescribeRecipe
    (
    -- * Creating a Request
      describeRecipe
    , DescribeRecipe
    -- * Request Lenses
    , drRecipeARN

    -- * Destructuring the Response
    , describeRecipeResponse
    , DescribeRecipeResponse
    -- * Response Lenses
    , drrsRecipe
    , drrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRecipe' smart constructor.
newtype DescribeRecipe = DescribeRecipe'{_drRecipeARN
                                         :: Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeRecipe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRecipeARN' - The Amazon Resource Name (ARN) of the recipe to describe.
describeRecipe
    :: Text -- ^ 'drRecipeARN'
    -> DescribeRecipe
describeRecipe pRecipeARN_
  = DescribeRecipe'{_drRecipeARN = pRecipeARN_}

-- | The Amazon Resource Name (ARN) of the recipe to describe.
drRecipeARN :: Lens' DescribeRecipe Text
drRecipeARN = lens _drRecipeARN (\ s a -> s{_drRecipeARN = a})

instance AWSRequest DescribeRecipe where
        type Rs DescribeRecipe = DescribeRecipeResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRecipeResponse' <$>
                   (x .?> "recipe") <*> (pure (fromEnum s)))

instance Hashable DescribeRecipe where

instance NFData DescribeRecipe where

instance ToHeaders DescribeRecipe where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeRecipe" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRecipe where
        toJSON DescribeRecipe'{..}
          = object
              (catMaybes [Just ("recipeArn" .= _drRecipeARN)])

instance ToPath DescribeRecipe where
        toPath = const "/"

instance ToQuery DescribeRecipe where
        toQuery = const mempty

-- | /See:/ 'describeRecipeResponse' smart constructor.
data DescribeRecipeResponse = DescribeRecipeResponse'{_drrsRecipe
                                                      :: !(Maybe Recipe),
                                                      _drrsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DescribeRecipeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRecipe' - An object that describes the recipe.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeRecipeResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeRecipeResponse
describeRecipeResponse pResponseStatus_
  = DescribeRecipeResponse'{_drrsRecipe = Nothing,
                            _drrsResponseStatus = pResponseStatus_}

-- | An object that describes the recipe.
drrsRecipe :: Lens' DescribeRecipeResponse (Maybe Recipe)
drrsRecipe = lens _drrsRecipe (\ s a -> s{_drrsRecipe = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeRecipeResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeRecipeResponse where
