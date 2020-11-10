{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AmazonPersonalizeEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AmazonPersonalizeEvents where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AmazonPersonalizeEvents
import Test.AWS.AmazonPersonalizeEvents.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutUsers $
--             putUsers
--
--         , requestPutItems $
--             putItems
--
--         , requestPutEvents $
--             putEvents
--
--           ]

--     , testGroup "response"
--         [ responsePutUsers $
--             putUsersResponse
--
--         , responsePutItems $
--             putItemsResponse
--
--         , responsePutEvents $
--             putEventsResponse
--
--           ]
--     ]

-- Requests

requestPutUsers :: PutUsers -> TestTree
requestPutUsers = req
    "PutUsers"
    "fixture/PutUsers.yaml"

requestPutItems :: PutItems -> TestTree
requestPutItems = req
    "PutItems"
    "fixture/PutItems.yaml"

requestPutEvents :: PutEvents -> TestTree
requestPutEvents = req
    "PutEvents"
    "fixture/PutEvents.yaml"

-- Responses

responsePutUsers :: PutUsersResponse -> TestTree
responsePutUsers = res
    "PutUsersResponse"
    "fixture/PutUsersResponse.proto"
    amazonPersonalizeEvents
    (Proxy :: Proxy PutUsers)

responsePutItems :: PutItemsResponse -> TestTree
responsePutItems = res
    "PutItemsResponse"
    "fixture/PutItemsResponse.proto"
    amazonPersonalizeEvents
    (Proxy :: Proxy PutItems)

responsePutEvents :: PutEventsResponse -> TestTree
responsePutEvents = res
    "PutEventsResponse"
    "fixture/PutEventsResponse.proto"
    amazonPersonalizeEvents
    (Proxy :: Proxy PutEvents)
