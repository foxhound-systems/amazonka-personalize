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
-- Module      : Network.AWS.AmazonPersonalize.DeleteSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a schema. Before deleting a schema, you must delete all datasets referencing the schema. For more information on schemas, see 'CreateSchema' .
--
--
module Network.AWS.AmazonPersonalize.DeleteSchema
    (
    -- * Creating a Request
      deleteSchema
    , DeleteSchema
    -- * Request Lenses
    , delSchemaARN

    -- * Destructuring the Response
    , deleteSchemaResponse
    , DeleteSchemaResponse
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSchema' smart constructor.
newtype DeleteSchema = DeleteSchema'{_delSchemaARN ::
                                     Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delSchemaARN' - The Amazon Resource Name (ARN) of the schema to delete.
deleteSchema
    :: Text -- ^ 'delSchemaARN'
    -> DeleteSchema
deleteSchema pSchemaARN_
  = DeleteSchema'{_delSchemaARN = pSchemaARN_}

-- | The Amazon Resource Name (ARN) of the schema to delete.
delSchemaARN :: Lens' DeleteSchema Text
delSchemaARN = lens _delSchemaARN (\ s a -> s{_delSchemaARN = a})

instance AWSRequest DeleteSchema where
        type Rs DeleteSchema = DeleteSchemaResponse
        request = postJSON amazonPersonalize
        response = receiveNull DeleteSchemaResponse'

instance Hashable DeleteSchema where

instance NFData DeleteSchema where

instance ToHeaders DeleteSchema where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DeleteSchema" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSchema where
        toJSON DeleteSchema'{..}
          = object
              (catMaybes [Just ("schemaArn" .= _delSchemaARN)])

instance ToPath DeleteSchema where
        toPath = const "/"

instance ToQuery DeleteSchema where
        toQuery = const mempty

-- | /See:/ 'deleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSchemaResponse' with the minimum fields required to make a request.
--
deleteSchemaResponse
    :: DeleteSchemaResponse
deleteSchemaResponse = DeleteSchemaResponse'

instance NFData DeleteSchemaResponse where
