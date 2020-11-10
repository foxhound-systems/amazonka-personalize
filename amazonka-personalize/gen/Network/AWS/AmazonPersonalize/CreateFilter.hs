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
-- Module      : Network.AWS.AmazonPersonalize.CreateFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recommendation filter. For more information, see <https://docs.aws.amazon.com/personalize/latest/dg/filters.html Using Filters with Amazon Personalize> .
--
--
module Network.AWS.AmazonPersonalize.CreateFilter
    (
    -- * Creating a Request
      createFilter
    , CreateFilter
    -- * Request Lenses
    , cfName
    , cfDatasetGroupARN
    , cfFilterExpression

    -- * Destructuring the Response
    , createFilterResponse
    , CreateFilterResponse
    -- * Response Lenses
    , cfrsFilterARN
    , cfrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFilter' smart constructor.
data CreateFilter = CreateFilter'{_cfName :: !Text,
                                  _cfDatasetGroupARN :: !Text,
                                  _cfFilterExpression :: !(Sensitive Text)}
                      deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfName' - The name of the filter to create.
--
-- * 'cfDatasetGroupARN' - The ARN of the dataset group that the filter will belong to.
--
-- * 'cfFilterExpression' - The filter expression that designates the interaction types that the filter will filter out. A filter expression must follow the following format: @EXCLUDE itemId WHERE INTERACTIONS.event_type in ("EVENT_TYPE")@  Where "EVENT_TYPE" is the type of event to filter out. To filter out all items with any interactions history, set @"*"@ as the EVENT_TYPE. For more information, see <https://docs.aws.amazon.com/personalize/latest/dg/filters.html Using Filters with Amazon Personalize> .
createFilter
    :: Text -- ^ 'cfName'
    -> Text -- ^ 'cfDatasetGroupARN'
    -> Text -- ^ 'cfFilterExpression'
    -> CreateFilter
createFilter pName_ pDatasetGroupARN_
  pFilterExpression_
  = CreateFilter'{_cfName = pName_,
                  _cfDatasetGroupARN = pDatasetGroupARN_,
                  _cfFilterExpression =
                    _Sensitive # pFilterExpression_}

-- | The name of the filter to create.
cfName :: Lens' CreateFilter Text
cfName = lens _cfName (\ s a -> s{_cfName = a})

-- | The ARN of the dataset group that the filter will belong to.
cfDatasetGroupARN :: Lens' CreateFilter Text
cfDatasetGroupARN = lens _cfDatasetGroupARN (\ s a -> s{_cfDatasetGroupARN = a})

-- | The filter expression that designates the interaction types that the filter will filter out. A filter expression must follow the following format: @EXCLUDE itemId WHERE INTERACTIONS.event_type in ("EVENT_TYPE")@  Where "EVENT_TYPE" is the type of event to filter out. To filter out all items with any interactions history, set @"*"@ as the EVENT_TYPE. For more information, see <https://docs.aws.amazon.com/personalize/latest/dg/filters.html Using Filters with Amazon Personalize> .
cfFilterExpression :: Lens' CreateFilter Text
cfFilterExpression = lens _cfFilterExpression (\ s a -> s{_cfFilterExpression = a}) . _Sensitive

instance AWSRequest CreateFilter where
        type Rs CreateFilter = CreateFilterResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateFilterResponse' <$>
                   (x .?> "filterArn") <*> (pure (fromEnum s)))

instance Hashable CreateFilter where

instance NFData CreateFilter where

instance ToHeaders CreateFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateFilter where
        toJSON CreateFilter'{..}
          = object
              (catMaybes
                 [Just ("name" .= _cfName),
                  Just ("datasetGroupArn" .= _cfDatasetGroupARN),
                  Just ("filterExpression" .= _cfFilterExpression)])

instance ToPath CreateFilter where
        toPath = const "/"

instance ToQuery CreateFilter where
        toQuery = const mempty

-- | /See:/ 'createFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'{_cfrsFilterARN
                                                  :: !(Maybe Text),
                                                  _cfrsResponseStatus :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFilterARN' - The ARN of the new filter.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFilterResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFilterResponse
createFilterResponse pResponseStatus_
  = CreateFilterResponse'{_cfrsFilterARN = Nothing,
                          _cfrsResponseStatus = pResponseStatus_}

-- | The ARN of the new filter.
cfrsFilterARN :: Lens' CreateFilterResponse (Maybe Text)
cfrsFilterARN = lens _cfrsFilterARN (\ s a -> s{_cfrsFilterARN = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFilterResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFilterResponse where
