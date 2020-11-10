{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalizeEvents.Types.User
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalizeEvents.Types.User where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents user metadata added to a Users dataset using the @PutUsers@ API.
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'{_uProperties :: !(Maybe Text),
                  _uUserId :: !Text}
              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uProperties' - A string map of user-specific metadata. Each element in the map consists of a key-value pair. For example,  @{"numberOfVideosWatched": "45"}@  The keys use camel case names that match the fields in the Users schema. In the above example, the @numberOfVideosWatched@ would match the 'NUMBER_OF_VIDEOS_WATCHED' field defined in the Users schema.
--
-- * 'uUserId' - The ID associated with the user.
user
    :: Text -- ^ 'uUserId'
    -> User
user pUserId_
  = User'{_uProperties = Nothing, _uUserId = pUserId_}

-- | A string map of user-specific metadata. Each element in the map consists of a key-value pair. For example,  @{"numberOfVideosWatched": "45"}@  The keys use camel case names that match the fields in the Users schema. In the above example, the @numberOfVideosWatched@ would match the 'NUMBER_OF_VIDEOS_WATCHED' field defined in the Users schema.
uProperties :: Lens' User (Maybe Text)
uProperties = lens _uProperties (\ s a -> s{_uProperties = a})

-- | The ID associated with the user.
uUserId :: Lens' User Text
uUserId = lens _uUserId (\ s a -> s{_uUserId = a})

instance Hashable User where

instance NFData User where

instance ToJSON User where
        toJSON User'{..}
          = object
              (catMaybes
                 [("properties" .=) <$> _uProperties,
                  Just ("userId" .= _uUserId)])
