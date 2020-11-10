{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.S3DataConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.S3DataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration details of an Amazon S3 input or output bucket.
--
--
--
-- /See:/ 's3DataConfig' smart constructor.
data S3DataConfig = S3DataConfig'{_sdcKmsKeyARN ::
                                  !(Maybe Text),
                                  _sdcPath :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3DataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcKmsKeyARN' - The Amazon Resource Name (ARN) of the Amazon Key Management Service (KMS) key that Amazon Personalize uses to encrypt or decrypt the input and output files of a batch inference job.
--
-- * 'sdcPath' - The file path of the Amazon S3 bucket.
s3DataConfig
    :: Text -- ^ 'sdcPath'
    -> S3DataConfig
s3DataConfig pPath_
  = S3DataConfig'{_sdcKmsKeyARN = Nothing,
                  _sdcPath = pPath_}

-- | The Amazon Resource Name (ARN) of the Amazon Key Management Service (KMS) key that Amazon Personalize uses to encrypt or decrypt the input and output files of a batch inference job.
sdcKmsKeyARN :: Lens' S3DataConfig (Maybe Text)
sdcKmsKeyARN = lens _sdcKmsKeyARN (\ s a -> s{_sdcKmsKeyARN = a})

-- | The file path of the Amazon S3 bucket.
sdcPath :: Lens' S3DataConfig Text
sdcPath = lens _sdcPath (\ s a -> s{_sdcPath = a})

instance FromJSON S3DataConfig where
        parseJSON
          = withObject "S3DataConfig"
              (\ x ->
                 S3DataConfig' <$>
                   (x .:? "kmsKeyArn") <*> (x .: "path"))

instance Hashable S3DataConfig where

instance NFData S3DataConfig where

instance ToJSON S3DataConfig where
        toJSON S3DataConfig'{..}
          = object
              (catMaybes
                 [("kmsKeyArn" .=) <$> _sdcKmsKeyARN,
                  Just ("path" .= _sdcPath)])
