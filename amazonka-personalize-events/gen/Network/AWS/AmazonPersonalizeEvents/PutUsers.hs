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
-- Module      : Network.AWS.AmazonPersonalizeEvents.PutUsers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more users to a Users dataset. For more information see 'importing-users' .
--
--
module Network.AWS.AmazonPersonalizeEvents.PutUsers
    (
    -- * Creating a Request
      putUsers
    , PutUsers
    -- * Request Lenses
    , puDatasetARN
    , puUsers

    -- * Destructuring the Response
    , putUsersResponse
    , PutUsersResponse
    ) where

import Network.AWS.AmazonPersonalizeEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putUsers' smart constructor.
data PutUsers = PutUsers'{_puDatasetARN :: !Text,
                          _puUsers :: !(List1 User)}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutUsers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puDatasetARN' - The Amazon Resource Number (ARN) of the Users dataset you are adding the user or users to.
--
-- * 'puUsers' - A list of user data.
putUsers
    :: Text -- ^ 'puDatasetARN'
    -> NonEmpty User -- ^ 'puUsers'
    -> PutUsers
putUsers pDatasetARN_ pUsers_
  = PutUsers'{_puDatasetARN = pDatasetARN_,
              _puUsers = _List1 # pUsers_}

-- | The Amazon Resource Number (ARN) of the Users dataset you are adding the user or users to.
puDatasetARN :: Lens' PutUsers Text
puDatasetARN = lens _puDatasetARN (\ s a -> s{_puDatasetARN = a})

-- | A list of user data.
puUsers :: Lens' PutUsers (NonEmpty User)
puUsers = lens _puUsers (\ s a -> s{_puUsers = a}) . _List1

instance AWSRequest PutUsers where
        type Rs PutUsers = PutUsersResponse
        request = postJSON amazonPersonalizeEvents
        response = receiveNull PutUsersResponse'

instance Hashable PutUsers where

instance NFData PutUsers where

instance ToHeaders PutUsers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutUsers where
        toJSON PutUsers'{..}
          = object
              (catMaybes
                 [Just ("datasetArn" .= _puDatasetARN),
                  Just ("users" .= _puUsers)])

instance ToPath PutUsers where
        toPath = const "/users"

instance ToQuery PutUsers where
        toQuery = const mempty

-- | /See:/ 'putUsersResponse' smart constructor.
data PutUsersResponse = PutUsersResponse'
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutUsersResponse' with the minimum fields required to make a request.
--
putUsersResponse
    :: PutUsersResponse
putUsersResponse = PutUsersResponse'

instance NFData PutUsersResponse where
