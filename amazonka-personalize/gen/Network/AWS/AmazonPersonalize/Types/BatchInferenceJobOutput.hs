{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.BatchInferenceJobOutput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.BatchInferenceJobOutput where

import Network.AWS.AmazonPersonalize.Types.S3DataConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The output configuration parameters of a batch inference job.
--
--
--
-- /See:/ 'batchInferenceJobOutput' smart constructor.
newtype BatchInferenceJobOutput = BatchInferenceJobOutput'{_bijoS3DataDestination
                                                           :: S3DataConfig}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'BatchInferenceJobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bijoS3DataDestination' - Information on the Amazon S3 bucket in which the batch inference job's output is stored.
batchInferenceJobOutput
    :: S3DataConfig -- ^ 'bijoS3DataDestination'
    -> BatchInferenceJobOutput
batchInferenceJobOutput pS3DataDestination_
  = BatchInferenceJobOutput'{_bijoS3DataDestination =
                               pS3DataDestination_}

-- | Information on the Amazon S3 bucket in which the batch inference job's output is stored.
bijoS3DataDestination :: Lens' BatchInferenceJobOutput S3DataConfig
bijoS3DataDestination = lens _bijoS3DataDestination (\ s a -> s{_bijoS3DataDestination = a})

instance FromJSON BatchInferenceJobOutput where
        parseJSON
          = withObject "BatchInferenceJobOutput"
              (\ x ->
                 BatchInferenceJobOutput' <$>
                   (x .: "s3DataDestination"))

instance Hashable BatchInferenceJobOutput where

instance NFData BatchInferenceJobOutput where

instance ToJSON BatchInferenceJobOutput where
        toJSON BatchInferenceJobOutput'{..}
          = object
              (catMaybes
                 [Just
                    ("s3DataDestination" .= _bijoS3DataDestination)])
