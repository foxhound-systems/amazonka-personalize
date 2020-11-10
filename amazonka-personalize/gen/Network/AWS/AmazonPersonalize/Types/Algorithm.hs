{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.Algorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.Algorithm where

import Network.AWS.AmazonPersonalize.Types.AlgorithmImage
import Network.AWS.AmazonPersonalize.Types.DefaultHyperParameterRanges
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a custom algorithm.
--
--
--
-- /See:/ 'algorithm' smart constructor.
data Algorithm = Algorithm'{_aDefaultHyperParameters
                            :: !(Maybe (Map Text Text)),
                            _aAlgorithmARN :: !(Maybe Text),
                            _aTrainingInputMode :: !(Maybe Text),
                            _aDefaultHyperParameterRanges ::
                            !(Maybe DefaultHyperParameterRanges),
                            _aAlgorithmImage :: !(Maybe AlgorithmImage),
                            _aLastUpdatedDateTime :: !(Maybe POSIX),
                            _aName :: !(Maybe Text),
                            _aCreationDateTime :: !(Maybe POSIX),
                            _aDefaultResourceConfig :: !(Maybe (Map Text Text)),
                            _aRoleARN :: !(Maybe Text)}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Algorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aDefaultHyperParameters' - Specifies the default hyperparameters.
--
-- * 'aAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
--
-- * 'aTrainingInputMode' - The training input mode.
--
-- * 'aDefaultHyperParameterRanges' - Specifies the default hyperparameters, their ranges, and whether they are tunable. A tunable hyperparameter can have its value determined during hyperparameter optimization (HPO).
--
-- * 'aAlgorithmImage' - The URI of the Docker container for the algorithm image.
--
-- * 'aLastUpdatedDateTime' - The date and time (in Unix time) that the algorithm was last updated.
--
-- * 'aName' - The name of the algorithm.
--
-- * 'aCreationDateTime' - The date and time (in Unix time) that the algorithm was created.
--
-- * 'aDefaultResourceConfig' - Specifies the default maximum number of training jobs and parallel training jobs.
--
-- * 'aRoleARN' - The Amazon Resource Name (ARN) of the role.
algorithm
    :: Algorithm
algorithm
  = Algorithm'{_aDefaultHyperParameters = Nothing,
               _aAlgorithmARN = Nothing,
               _aTrainingInputMode = Nothing,
               _aDefaultHyperParameterRanges = Nothing,
               _aAlgorithmImage = Nothing,
               _aLastUpdatedDateTime = Nothing, _aName = Nothing,
               _aCreationDateTime = Nothing,
               _aDefaultResourceConfig = Nothing,
               _aRoleARN = Nothing}

-- | Specifies the default hyperparameters.
aDefaultHyperParameters :: Lens' Algorithm (HashMap Text Text)
aDefaultHyperParameters = lens _aDefaultHyperParameters (\ s a -> s{_aDefaultHyperParameters = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the algorithm.
aAlgorithmARN :: Lens' Algorithm (Maybe Text)
aAlgorithmARN = lens _aAlgorithmARN (\ s a -> s{_aAlgorithmARN = a})

-- | The training input mode.
aTrainingInputMode :: Lens' Algorithm (Maybe Text)
aTrainingInputMode = lens _aTrainingInputMode (\ s a -> s{_aTrainingInputMode = a})

-- | Specifies the default hyperparameters, their ranges, and whether they are tunable. A tunable hyperparameter can have its value determined during hyperparameter optimization (HPO).
aDefaultHyperParameterRanges :: Lens' Algorithm (Maybe DefaultHyperParameterRanges)
aDefaultHyperParameterRanges = lens _aDefaultHyperParameterRanges (\ s a -> s{_aDefaultHyperParameterRanges = a})

-- | The URI of the Docker container for the algorithm image.
aAlgorithmImage :: Lens' Algorithm (Maybe AlgorithmImage)
aAlgorithmImage = lens _aAlgorithmImage (\ s a -> s{_aAlgorithmImage = a})

-- | The date and time (in Unix time) that the algorithm was last updated.
aLastUpdatedDateTime :: Lens' Algorithm (Maybe UTCTime)
aLastUpdatedDateTime = lens _aLastUpdatedDateTime (\ s a -> s{_aLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the algorithm.
aName :: Lens' Algorithm (Maybe Text)
aName = lens _aName (\ s a -> s{_aName = a})

-- | The date and time (in Unix time) that the algorithm was created.
aCreationDateTime :: Lens' Algorithm (Maybe UTCTime)
aCreationDateTime = lens _aCreationDateTime (\ s a -> s{_aCreationDateTime = a}) . mapping _Time

-- | Specifies the default maximum number of training jobs and parallel training jobs.
aDefaultResourceConfig :: Lens' Algorithm (HashMap Text Text)
aDefaultResourceConfig = lens _aDefaultResourceConfig (\ s a -> s{_aDefaultResourceConfig = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the role.
aRoleARN :: Lens' Algorithm (Maybe Text)
aRoleARN = lens _aRoleARN (\ s a -> s{_aRoleARN = a})

instance FromJSON Algorithm where
        parseJSON
          = withObject "Algorithm"
              (\ x ->
                 Algorithm' <$>
                   (x .:? "defaultHyperParameters" .!= mempty) <*>
                     (x .:? "algorithmArn")
                     <*> (x .:? "trainingInputMode")
                     <*> (x .:? "defaultHyperParameterRanges")
                     <*> (x .:? "algorithmImage")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "defaultResourceConfig" .!= mempty)
                     <*> (x .:? "roleArn"))

instance Hashable Algorithm where

instance NFData Algorithm where
