{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.SolutionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.SolutionConfig where

import Network.AWS.AmazonPersonalize.Types.AutoMLConfig
import Network.AWS.AmazonPersonalize.Types.HPOConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration properties for the solution.
--
--
--
-- /See:/ 'solutionConfig' smart constructor.
data SolutionConfig = SolutionConfig'{_scFeatureTransformationParameters
                                      :: !(Maybe (Map Text Text)),
                                      _scHpoConfig :: !(Maybe HPOConfig),
                                      _scEventValueThreshold :: !(Maybe Text),
                                      _scAutoMLConfig :: !(Maybe AutoMLConfig),
                                      _scAlgorithmHyperParameters ::
                                      !(Maybe (Map Text Text))}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SolutionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scFeatureTransformationParameters' - Lists the feature transformation parameters.
--
-- * 'scHpoConfig' - Describes the properties for hyperparameter optimization (HPO).
--
-- * 'scEventValueThreshold' - Only events with a value greater than or equal to this threshold are used for training a model.
--
-- * 'scAutoMLConfig' - The 'AutoMLConfig' object containing a list of recipes to search when AutoML is performed.
--
-- * 'scAlgorithmHyperParameters' - Lists the hyperparameter names and ranges.
solutionConfig
    :: SolutionConfig
solutionConfig
  = SolutionConfig'{_scFeatureTransformationParameters
                      = Nothing,
                    _scHpoConfig = Nothing,
                    _scEventValueThreshold = Nothing,
                    _scAutoMLConfig = Nothing,
                    _scAlgorithmHyperParameters = Nothing}

-- | Lists the feature transformation parameters.
scFeatureTransformationParameters :: Lens' SolutionConfig (HashMap Text Text)
scFeatureTransformationParameters = lens _scFeatureTransformationParameters (\ s a -> s{_scFeatureTransformationParameters = a}) . _Default . _Map

-- | Describes the properties for hyperparameter optimization (HPO).
scHpoConfig :: Lens' SolutionConfig (Maybe HPOConfig)
scHpoConfig = lens _scHpoConfig (\ s a -> s{_scHpoConfig = a})

-- | Only events with a value greater than or equal to this threshold are used for training a model.
scEventValueThreshold :: Lens' SolutionConfig (Maybe Text)
scEventValueThreshold = lens _scEventValueThreshold (\ s a -> s{_scEventValueThreshold = a})

-- | The 'AutoMLConfig' object containing a list of recipes to search when AutoML is performed.
scAutoMLConfig :: Lens' SolutionConfig (Maybe AutoMLConfig)
scAutoMLConfig = lens _scAutoMLConfig (\ s a -> s{_scAutoMLConfig = a})

-- | Lists the hyperparameter names and ranges.
scAlgorithmHyperParameters :: Lens' SolutionConfig (HashMap Text Text)
scAlgorithmHyperParameters = lens _scAlgorithmHyperParameters (\ s a -> s{_scAlgorithmHyperParameters = a}) . _Default . _Map

instance FromJSON SolutionConfig where
        parseJSON
          = withObject "SolutionConfig"
              (\ x ->
                 SolutionConfig' <$>
                   (x .:? "featureTransformationParameters" .!= mempty)
                     <*> (x .:? "hpoConfig")
                     <*> (x .:? "eventValueThreshold")
                     <*> (x .:? "autoMLConfig")
                     <*> (x .:? "algorithmHyperParameters" .!= mempty))

instance Hashable SolutionConfig where

instance NFData SolutionConfig where

instance ToJSON SolutionConfig where
        toJSON SolutionConfig'{..}
          = object
              (catMaybes
                 [("featureTransformationParameters" .=) <$>
                    _scFeatureTransformationParameters,
                  ("hpoConfig" .=) <$> _scHpoConfig,
                  ("eventValueThreshold" .=) <$>
                    _scEventValueThreshold,
                  ("autoMLConfig" .=) <$> _scAutoMLConfig,
                  ("algorithmHyperParameters" .=) <$>
                    _scAlgorithmHyperParameters])
