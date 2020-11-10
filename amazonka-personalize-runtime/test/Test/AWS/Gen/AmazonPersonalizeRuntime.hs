{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AmazonPersonalizeRuntime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AmazonPersonalizeRuntime where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AmazonPersonalizeRuntime
import Test.AWS.AmazonPersonalizeRuntime.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetRecommendations $
--             getRecommendations
--
--         , requestGetPersonalizedRanking $
--             getPersonalizedRanking
--
--           ]

--     , testGroup "response"
--         [ responseGetRecommendations $
--             getRecommendationsResponse
--
--         , responseGetPersonalizedRanking $
--             getPersonalizedRankingResponse
--
--           ]
--     ]

-- Requests

requestGetRecommendations :: GetRecommendations -> TestTree
requestGetRecommendations = req
    "GetRecommendations"
    "fixture/GetRecommendations.yaml"

requestGetPersonalizedRanking :: GetPersonalizedRanking -> TestTree
requestGetPersonalizedRanking = req
    "GetPersonalizedRanking"
    "fixture/GetPersonalizedRanking.yaml"

-- Responses

responseGetRecommendations :: GetRecommendationsResponse -> TestTree
responseGetRecommendations = res
    "GetRecommendationsResponse"
    "fixture/GetRecommendationsResponse.proto"
    amazonPersonalizeRuntime
    (Proxy :: Proxy GetRecommendations)

responseGetPersonalizedRanking :: GetPersonalizedRankingResponse -> TestTree
responseGetPersonalizedRanking = res
    "GetPersonalizedRankingResponse"
    "fixture/GetPersonalizedRankingResponse.proto"
    amazonPersonalizeRuntime
    (Proxy :: Proxy GetPersonalizedRanking)
