{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalizeRuntime.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalizeRuntime.Types
    (
    -- * Service Configuration
      amazonPersonalizeRuntime

    -- * Errors
    , _InvalidInputException
    , _ResourceNotFoundException

    -- * PredictedItem
    , PredictedItem
    , predictedItem
    , piScore
    , piItemId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.AmazonPersonalizeRuntime.Types.PredictedItem

-- | API version @2018-05-22@ of the Amazon Personalize Runtime SDK configuration.
amazonPersonalizeRuntime :: Service
amazonPersonalizeRuntime
  = Service{_svcAbbrev = "AmazonPersonalizeRuntime",
            _svcSigner = v4, _svcPrefix = "personalize-runtime",
            _svcVersion = "2018-05-22",
            _svcEndpoint =
              defaultEndpoint amazonPersonalizeRuntime,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError =
              parseJSONError "AmazonPersonalizeRuntime",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has
              (hasCode "ProvisionedThroughputExceededException" .
                 hasStatus 400)
              e
            = Just "throughput_exceeded"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | Provide a valid value for the field or parameter.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException
  = _MatchServiceError amazonPersonalizeRuntime
      "InvalidInputException"
      . hasStatus 400

-- | The specified resource does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError amazonPersonalizeRuntime
      "ResourceNotFoundException"
      . hasStatus 404
