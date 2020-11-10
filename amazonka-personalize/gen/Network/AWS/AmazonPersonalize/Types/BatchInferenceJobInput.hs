{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.BatchInferenceJobInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.BatchInferenceJobInput where

import Network.AWS.AmazonPersonalize.Types.S3DataConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The input configuration of a batch inference job.
--
--
--
-- /See:/ 'batchInferenceJobInput' smart constructor.
newtype BatchInferenceJobInput = BatchInferenceJobInput'{_bijiS3DataSource
                                                         :: S3DataConfig}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'BatchInferenceJobInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bijiS3DataSource' - The URI of the Amazon S3 location that contains your input data. The Amazon S3 bucket must be in the same region as the API endpoint you are calling.
batchInferenceJobInput
    :: S3DataConfig -- ^ 'bijiS3DataSource'
    -> BatchInferenceJobInput
batchInferenceJobInput pS3DataSource_
  = BatchInferenceJobInput'{_bijiS3DataSource =
                              pS3DataSource_}

-- | The URI of the Amazon S3 location that contains your input data. The Amazon S3 bucket must be in the same region as the API endpoint you are calling.
bijiS3DataSource :: Lens' BatchInferenceJobInput S3DataConfig
bijiS3DataSource = lens _bijiS3DataSource (\ s a -> s{_bijiS3DataSource = a})

instance FromJSON BatchInferenceJobInput where
        parseJSON
          = withObject "BatchInferenceJobInput"
              (\ x ->
                 BatchInferenceJobInput' <$> (x .: "s3DataSource"))

instance Hashable BatchInferenceJobInput where

instance NFData BatchInferenceJobInput where

instance ToJSON BatchInferenceJobInput where
        toJSON BatchInferenceJobInput'{..}
          = object
              (catMaybes
                 [Just ("s3DataSource" .= _bijiS3DataSource)])
