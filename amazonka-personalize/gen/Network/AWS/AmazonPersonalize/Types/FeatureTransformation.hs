{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.FeatureTransformation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.FeatureTransformation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides feature transformation information. Feature transformation is the process of modifying raw input data into a form more suitable for model training.
--
--
--
-- /See:/ 'featureTransformation' smart constructor.
data FeatureTransformation = FeatureTransformation'{_ftStatus
                                                    :: !(Maybe Text),
                                                    _ftFeatureTransformationARN
                                                    :: !(Maybe Text),
                                                    _ftLastUpdatedDateTime ::
                                                    !(Maybe POSIX),
                                                    _ftName :: !(Maybe Text),
                                                    _ftCreationDateTime ::
                                                    !(Maybe POSIX),
                                                    _ftDefaultParameters ::
                                                    !(Maybe (Map Text Text))}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'FeatureTransformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ftStatus' - The status of the feature transformation. A feature transformation can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- * 'ftFeatureTransformationARN' - The Amazon Resource Name (ARN) of the FeatureTransformation object.
--
-- * 'ftLastUpdatedDateTime' - The last update date and time (in Unix time) of the feature transformation.
--
-- * 'ftName' - The name of the feature transformation.
--
-- * 'ftCreationDateTime' - The creation date and time (in Unix time) of the feature transformation.
--
-- * 'ftDefaultParameters' - Provides the default parameters for feature transformation.
featureTransformation
    :: FeatureTransformation
featureTransformation
  = FeatureTransformation'{_ftStatus = Nothing,
                           _ftFeatureTransformationARN = Nothing,
                           _ftLastUpdatedDateTime = Nothing, _ftName = Nothing,
                           _ftCreationDateTime = Nothing,
                           _ftDefaultParameters = Nothing}

-- | The status of the feature transformation. A feature transformation can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
ftStatus :: Lens' FeatureTransformation (Maybe Text)
ftStatus = lens _ftStatus (\ s a -> s{_ftStatus = a})

-- | The Amazon Resource Name (ARN) of the FeatureTransformation object.
ftFeatureTransformationARN :: Lens' FeatureTransformation (Maybe Text)
ftFeatureTransformationARN = lens _ftFeatureTransformationARN (\ s a -> s{_ftFeatureTransformationARN = a})

-- | The last update date and time (in Unix time) of the feature transformation.
ftLastUpdatedDateTime :: Lens' FeatureTransformation (Maybe UTCTime)
ftLastUpdatedDateTime = lens _ftLastUpdatedDateTime (\ s a -> s{_ftLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the feature transformation.
ftName :: Lens' FeatureTransformation (Maybe Text)
ftName = lens _ftName (\ s a -> s{_ftName = a})

-- | The creation date and time (in Unix time) of the feature transformation.
ftCreationDateTime :: Lens' FeatureTransformation (Maybe UTCTime)
ftCreationDateTime = lens _ftCreationDateTime (\ s a -> s{_ftCreationDateTime = a}) . mapping _Time

-- | Provides the default parameters for feature transformation.
ftDefaultParameters :: Lens' FeatureTransformation (HashMap Text Text)
ftDefaultParameters = lens _ftDefaultParameters (\ s a -> s{_ftDefaultParameters = a}) . _Default . _Map

instance FromJSON FeatureTransformation where
        parseJSON
          = withObject "FeatureTransformation"
              (\ x ->
                 FeatureTransformation' <$>
                   (x .:? "status") <*>
                     (x .:? "featureTransformationArn")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "defaultParameters" .!= mempty))

instance Hashable FeatureTransformation where

instance NFData FeatureTransformation where
