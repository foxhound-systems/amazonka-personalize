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
-- Module      : Network.AWS.AmazonPersonalize.ListRecipes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available recipes. The response provides the properties for each recipe, including the recipe's Amazon Resource Name (ARN).
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListRecipes
    (
    -- * Creating a Request
      listRecipes
    , ListRecipes
    -- * Request Lenses
    , lrNextToken
    , lrMaxResults
    , lrRecipeProvider

    -- * Destructuring the Response
    , listRecipesResponse
    , ListRecipesResponse
    -- * Response Lenses
    , lrrsNextToken
    , lrrsRecipes
    , lrrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRecipes' smart constructor.
data ListRecipes = ListRecipes'{_lrNextToken ::
                                !(Maybe Text),
                                _lrMaxResults :: !(Maybe Nat),
                                _lrRecipeProvider :: !(Maybe RecipeProvider)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRecipes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - A token returned from the previous call to @ListRecipes@ for getting the next set of recipes (if they exist).
--
-- * 'lrMaxResults' - The maximum number of recipes to return.
--
-- * 'lrRecipeProvider' - The default is @SERVICE@ .
listRecipes
    :: ListRecipes
listRecipes
  = ListRecipes'{_lrNextToken = Nothing,
                 _lrMaxResults = Nothing, _lrRecipeProvider = Nothing}

-- | A token returned from the previous call to @ListRecipes@ for getting the next set of recipes (if they exist).
lrNextToken :: Lens' ListRecipes (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | The maximum number of recipes to return.
lrMaxResults :: Lens' ListRecipes (Maybe Natural)
lrMaxResults = lens _lrMaxResults (\ s a -> s{_lrMaxResults = a}) . mapping _Nat

-- | The default is @SERVICE@ .
lrRecipeProvider :: Lens' ListRecipes (Maybe RecipeProvider)
lrRecipeProvider = lens _lrRecipeProvider (\ s a -> s{_lrRecipeProvider = a})

instance AWSPager ListRecipes where
        page rq rs
          | stop (rs ^. lrrsNextToken) = Nothing
          | stop (rs ^. lrrsRecipes) = Nothing
          | otherwise =
            Just $ rq & lrNextToken .~ rs ^. lrrsNextToken

instance AWSRequest ListRecipes where
        type Rs ListRecipes = ListRecipesResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListRecipesResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "recipes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListRecipes where

instance NFData ListRecipes where

instance ToHeaders ListRecipes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListRecipes" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRecipes where
        toJSON ListRecipes'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lrNextToken,
                  ("maxResults" .=) <$> _lrMaxResults,
                  ("recipeProvider" .=) <$> _lrRecipeProvider])

instance ToPath ListRecipes where
        toPath = const "/"

instance ToQuery ListRecipes where
        toQuery = const mempty

-- | /See:/ 'listRecipesResponse' smart constructor.
data ListRecipesResponse = ListRecipesResponse'{_lrrsNextToken
                                                :: !(Maybe Text),
                                                _lrrsRecipes ::
                                                !(Maybe [RecipeSummary]),
                                                _lrrsResponseStatus :: !Int}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRecipesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsNextToken' - A token for getting the next set of recipes.
--
-- * 'lrrsRecipes' - The list of available recipes.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listRecipesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRecipesResponse
listRecipesResponse pResponseStatus_
  = ListRecipesResponse'{_lrrsNextToken = Nothing,
                         _lrrsRecipes = Nothing,
                         _lrrsResponseStatus = pResponseStatus_}

-- | A token for getting the next set of recipes.
lrrsNextToken :: Lens' ListRecipesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | The list of available recipes.
lrrsRecipes :: Lens' ListRecipesResponse [RecipeSummary]
lrrsRecipes = lens _lrrsRecipes (\ s a -> s{_lrrsRecipes = a}) . _Default . _Coerce

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRecipesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListRecipesResponse where
