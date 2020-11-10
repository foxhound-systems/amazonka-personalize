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
-- Module      : Network.AWS.AmazonPersonalize.DeleteFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a filter.
--
--
module Network.AWS.AmazonPersonalize.DeleteFilter
    (
    -- * Creating a Request
      deleteFilter
    , DeleteFilter
    -- * Request Lenses
    , dFilterARN

    -- * Destructuring the Response
    , deleteFilterResponse
    , DeleteFilterResponse
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFilter' smart constructor.
newtype DeleteFilter = DeleteFilter'{_dFilterARN ::
                                     Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFilterARN' - The ARN of the filter to delete.
deleteFilter
    :: Text -- ^ 'dFilterARN'
    -> DeleteFilter
deleteFilter pFilterARN_
  = DeleteFilter'{_dFilterARN = pFilterARN_}

-- | The ARN of the filter to delete.
dFilterARN :: Lens' DeleteFilter Text
dFilterARN = lens _dFilterARN (\ s a -> s{_dFilterARN = a})

instance AWSRequest DeleteFilter where
        type Rs DeleteFilter = DeleteFilterResponse
        request = postJSON amazonPersonalize
        response = receiveNull DeleteFilterResponse'

instance Hashable DeleteFilter where

instance NFData DeleteFilter where

instance ToHeaders DeleteFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DeleteFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteFilter where
        toJSON DeleteFilter'{..}
          = object
              (catMaybes [Just ("filterArn" .= _dFilterARN)])

instance ToPath DeleteFilter where
        toPath = const "/"

instance ToQuery DeleteFilter where
        toQuery = const mempty

-- | /See:/ 'deleteFilterResponse' smart constructor.
data DeleteFilterResponse = DeleteFilterResponse'
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFilterResponse' with the minimum fields required to make a request.
--
deleteFilterResponse
    :: DeleteFilterResponse
deleteFilterResponse = DeleteFilterResponse'

instance NFData DeleteFilterResponse where
