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
-- Module      : Network.AWS.AmazonPersonalize.ListSchemas
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of schemas associated with the account. The response provides the properties for each schema, including the Amazon Resource Name (ARN). For more information on schemas, see 'CreateSchema' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.AmazonPersonalize.ListSchemas
    (
    -- * Creating a Request
      listSchemas
    , ListSchemas
    -- * Request Lenses
    , lsNextToken
    , lsMaxResults

    -- * Destructuring the Response
    , listSchemasResponse
    , ListSchemasResponse
    -- * Response Lenses
    , lsrsSchemas
    , lsrsNextToken
    , lsrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSchemas' smart constructor.
data ListSchemas = ListSchemas'{_lsNextToken ::
                                !(Maybe Text),
                                _lsMaxResults :: !(Maybe Nat)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSchemas' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken' - A token returned from the previous call to @ListSchemas@ for getting the next set of schemas (if they exist).
--
-- * 'lsMaxResults' - The maximum number of schemas to return.
listSchemas
    :: ListSchemas
listSchemas
  = ListSchemas'{_lsNextToken = Nothing,
                 _lsMaxResults = Nothing}

-- | A token returned from the previous call to @ListSchemas@ for getting the next set of schemas (if they exist).
lsNextToken :: Lens' ListSchemas (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | The maximum number of schemas to return.
lsMaxResults :: Lens' ListSchemas (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a}) . mapping _Nat

instance AWSPager ListSchemas where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsSchemas) = Nothing
          | otherwise =
            Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListSchemas where
        type Rs ListSchemas = ListSchemasResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 ListSchemasResponse' <$>
                   (x .?> "schemas" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListSchemas where

instance NFData ListSchemas where

instance ToHeaders ListSchemas where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.ListSchemas" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSchemas where
        toJSON ListSchemas'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lsNextToken,
                  ("maxResults" .=) <$> _lsMaxResults])

instance ToPath ListSchemas where
        toPath = const "/"

instance ToQuery ListSchemas where
        toQuery = const mempty

-- | /See:/ 'listSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'{_lsrsSchemas
                                                ::
                                                !(Maybe [DatasetSchemaSummary]),
                                                _lsrsNextToken :: !(Maybe Text),
                                                _lsrsResponseStatus :: !Int}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSchemasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsSchemas' - A list of schemas.
--
-- * 'lsrsNextToken' - A token used to get the next set of schemas (if they exist).
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listSchemasResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListSchemasResponse
listSchemasResponse pResponseStatus_
  = ListSchemasResponse'{_lsrsSchemas = Nothing,
                         _lsrsNextToken = Nothing,
                         _lsrsResponseStatus = pResponseStatus_}

-- | A list of schemas.
lsrsSchemas :: Lens' ListSchemasResponse [DatasetSchemaSummary]
lsrsSchemas = lens _lsrsSchemas (\ s a -> s{_lsrsSchemas = a}) . _Default . _Coerce

-- | A token used to get the next set of schemas (if they exist).
lsrsNextToken :: Lens' ListSchemasResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListSchemasResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListSchemasResponse where
