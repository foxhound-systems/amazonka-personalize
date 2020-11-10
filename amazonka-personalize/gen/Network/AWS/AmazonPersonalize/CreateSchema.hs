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
-- Module      : Network.AWS.AmazonPersonalize.CreateSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Personalize schema from the specified schema string. The schema you create must be in Avro JSON format.
--
--
-- Amazon Personalize recognizes three schema variants. Each schema is associated with a dataset type and has a set of required field and keywords. You specify a schema when you call 'CreateDataset' .
--
-- __Related APIs__ 
--
--     * 'ListSchemas' 
--
--     * 'DescribeSchema' 
--
--     * 'DeleteSchema' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateSchema
    (
    -- * Creating a Request
      createSchema
    , CreateSchema
    -- * Request Lenses
    , cssName
    , cssSchema

    -- * Destructuring the Response
    , createSchemaResponse
    , CreateSchemaResponse
    -- * Response Lenses
    , csrsSchemaARN
    , csrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSchema' smart constructor.
data CreateSchema = CreateSchema'{_cssName :: !Text,
                                  _cssSchema :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssName' - The name for the schema.
--
-- * 'cssSchema' - A schema in Avro JSON format.
createSchema
    :: Text -- ^ 'cssName'
    -> Text -- ^ 'cssSchema'
    -> CreateSchema
createSchema pName_ pSchema_
  = CreateSchema'{_cssName = pName_,
                  _cssSchema = pSchema_}

-- | The name for the schema.
cssName :: Lens' CreateSchema Text
cssName = lens _cssName (\ s a -> s{_cssName = a})

-- | A schema in Avro JSON format.
cssSchema :: Lens' CreateSchema Text
cssSchema = lens _cssSchema (\ s a -> s{_cssSchema = a})

instance AWSRequest CreateSchema where
        type Rs CreateSchema = CreateSchemaResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateSchemaResponse' <$>
                   (x .?> "schemaArn") <*> (pure (fromEnum s)))

instance Hashable CreateSchema where

instance NFData CreateSchema where

instance ToHeaders CreateSchema where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateSchema" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSchema where
        toJSON CreateSchema'{..}
          = object
              (catMaybes
                 [Just ("name" .= _cssName),
                  Just ("schema" .= _cssSchema)])

instance ToPath CreateSchema where
        toPath = const "/"

instance ToQuery CreateSchema where
        toQuery = const mempty

-- | /See:/ 'createSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'{_csrsSchemaARN
                                                  :: !(Maybe Text),
                                                  _csrsResponseStatus :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsSchemaARN' - The Amazon Resource Name (ARN) of the created schema.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createSchemaResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateSchemaResponse
createSchemaResponse pResponseStatus_
  = CreateSchemaResponse'{_csrsSchemaARN = Nothing,
                          _csrsResponseStatus = pResponseStatus_}

-- | The Amazon Resource Name (ARN) of the created schema.
csrsSchemaARN :: Lens' CreateSchemaResponse (Maybe Text)
csrsSchemaARN = lens _csrsSchemaARN (\ s a -> s{_csrsSchemaARN = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateSchemaResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateSchemaResponse where
