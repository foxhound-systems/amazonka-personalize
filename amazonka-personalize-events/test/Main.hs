{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Main (main) where

import Test.Tasty
import Test.AWS.AmazonPersonalizeEvents
import Test.AWS.AmazonPersonalizeEvents.Internal

main :: IO ()
main = defaultMain $ testGroup "AmazonPersonalizeEvents"
    [ testGroup "tests"    tests
    , testGroup "fixtures" fixtures
    ]
