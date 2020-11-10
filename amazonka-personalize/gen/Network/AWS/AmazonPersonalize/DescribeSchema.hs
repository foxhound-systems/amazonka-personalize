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
-- Module      : Network.AWS.AmazonPersonalize.DescribeSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a schema. For more information on schemas, see 'CreateSchema' .
--
--
module Network.AWS.AmazonPersonalize.DescribeSchema
    (
    -- * Creating a Request
      describeSchema
    , DescribeSchema
    -- * Request Lenses
    , desSchemaARN

    -- * Destructuring the Response
    , describeSchemaResponse
    , DescribeSchemaResponse
    -- * Response Lenses
    , drsSchema
    , drsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSchema' smart constructor.
newtype DescribeSchema = DescribeSchema'{_desSchemaARN
                                         :: Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desSchemaARN' - The Amazon Resource Name (ARN) of the schema to retrieve.
describeSchema
    :: Text -- ^ 'desSchemaARN'
    -> DescribeSchema
describeSchema pSchemaARN_
  = DescribeSchema'{_desSchemaARN = pSchemaARN_}

-- | The Amazon Resource Name (ARN) of the schema to retrieve.
desSchemaARN :: Lens' DescribeSchema Text
desSchemaARN = lens _desSchemaARN (\ s a -> s{_desSchemaARN = a})

instance AWSRequest DescribeSchema where
        type Rs DescribeSchema = DescribeSchemaResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSchemaResponse' <$>
                   (x .?> "schema") <*> (pure (fromEnum s)))

instance Hashable DescribeSchema where

instance NFData DescribeSchema where

instance ToHeaders DescribeSchema where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DescribeSchema" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSchema where
        toJSON DescribeSchema'{..}
          = object
              (catMaybes [Just ("schemaArn" .= _desSchemaARN)])

instance ToPath DescribeSchema where
        toPath = const "/"

instance ToQuery DescribeSchema where
        toQuery = const mempty

-- | /See:/ 'describeSchemaResponse' smart constructor.
data DescribeSchemaResponse = DescribeSchemaResponse'{_drsSchema
                                                      :: !(Maybe DatasetSchema),
                                                      _drsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DescribeSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsSchema' - The requested schema.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeSchemaResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeSchemaResponse
describeSchemaResponse pResponseStatus_
  = DescribeSchemaResponse'{_drsSchema = Nothing,
                            _drsResponseStatus = pResponseStatus_}

-- | The requested schema.
drsSchema :: Lens' DescribeSchemaResponse (Maybe DatasetSchema)
drsSchema = lens _drsSchema (\ s a -> s{_drsSchema = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeSchemaResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeSchemaResponse where
