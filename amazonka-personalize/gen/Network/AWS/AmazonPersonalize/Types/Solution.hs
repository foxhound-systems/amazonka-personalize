{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.Solution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.Solution where

import Network.AWS.AmazonPersonalize.Types.AutoMLResult
import Network.AWS.AmazonPersonalize.Types.SolutionConfig
import Network.AWS.AmazonPersonalize.Types.SolutionVersionSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that provides information about a solution. A solution is a trained model that can be deployed as a campaign.
--
--
--
-- /See:/ 'solution' smart constructor.
data Solution = Solution'{_sSolutionARN ::
                          !(Maybe Text),
                          _sStatus :: !(Maybe Text),
                          _sPerformAutoML :: !(Maybe Bool),
                          _sRecipeARN :: !(Maybe Text),
                          _sLastUpdatedDateTime :: !(Maybe POSIX),
                          _sEventType :: !(Maybe Text),
                          _sName :: !(Maybe Text),
                          _sAutoMLResult :: !(Maybe AutoMLResult),
                          _sCreationDateTime :: !(Maybe POSIX),
                          _sDatasetGroupARN :: !(Maybe Text),
                          _sLatestSolutionVersion ::
                          !(Maybe SolutionVersionSummary),
                          _sSolutionConfig :: !(Maybe SolutionConfig),
                          _sPerformHPO :: !(Maybe Bool)}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Solution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSolutionARN' - The ARN of the solution.
--
-- * 'sStatus' - The status of the solution. A solution can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'sPerformAutoML' - When true, Amazon Personalize performs a search for the best USER_PERSONALIZATION recipe from the list specified in the solution configuration (@recipeArn@ must not be specified). When false (the default), Amazon Personalize uses @recipeArn@ for training.
--
-- * 'sRecipeARN' - The ARN of the recipe used to create the solution.
--
-- * 'sLastUpdatedDateTime' - The date and time (in Unix time) that the solution was last updated.
--
-- * 'sEventType' - The event type (for example, 'click' or 'like') that is used for training the model.
--
-- * 'sName' - The name of the solution.
--
-- * 'sAutoMLResult' - When @performAutoML@ is true, specifies the best recipe found.
--
-- * 'sCreationDateTime' - The creation date and time (in Unix time) of the solution.
--
-- * 'sDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group that provides the training data.
--
-- * 'sLatestSolutionVersion' - Describes the latest version of the solution, including the status and the ARN.
--
-- * 'sSolutionConfig' - Describes the configuration properties for the solution.
--
-- * 'sPerformHPO' - Whether to perform hyperparameter optimization (HPO) on the chosen recipe. The default is @false@ .
solution
    :: Solution
solution
  = Solution'{_sSolutionARN = Nothing,
              _sStatus = Nothing, _sPerformAutoML = Nothing,
              _sRecipeARN = Nothing,
              _sLastUpdatedDateTime = Nothing,
              _sEventType = Nothing, _sName = Nothing,
              _sAutoMLResult = Nothing,
              _sCreationDateTime = Nothing,
              _sDatasetGroupARN = Nothing,
              _sLatestSolutionVersion = Nothing,
              _sSolutionConfig = Nothing, _sPerformHPO = Nothing}

-- | The ARN of the solution.
sSolutionARN :: Lens' Solution (Maybe Text)
sSolutionARN = lens _sSolutionARN (\ s a -> s{_sSolutionARN = a})

-- | The status of the solution. A solution can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
sStatus :: Lens' Solution (Maybe Text)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a})

-- | When true, Amazon Personalize performs a search for the best USER_PERSONALIZATION recipe from the list specified in the solution configuration (@recipeArn@ must not be specified). When false (the default), Amazon Personalize uses @recipeArn@ for training.
sPerformAutoML :: Lens' Solution (Maybe Bool)
sPerformAutoML = lens _sPerformAutoML (\ s a -> s{_sPerformAutoML = a})

-- | The ARN of the recipe used to create the solution.
sRecipeARN :: Lens' Solution (Maybe Text)
sRecipeARN = lens _sRecipeARN (\ s a -> s{_sRecipeARN = a})

-- | The date and time (in Unix time) that the solution was last updated.
sLastUpdatedDateTime :: Lens' Solution (Maybe UTCTime)
sLastUpdatedDateTime = lens _sLastUpdatedDateTime (\ s a -> s{_sLastUpdatedDateTime = a}) . mapping _Time

-- | The event type (for example, 'click' or 'like') that is used for training the model.
sEventType :: Lens' Solution (Maybe Text)
sEventType = lens _sEventType (\ s a -> s{_sEventType = a})

-- | The name of the solution.
sName :: Lens' Solution (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | When @performAutoML@ is true, specifies the best recipe found.
sAutoMLResult :: Lens' Solution (Maybe AutoMLResult)
sAutoMLResult = lens _sAutoMLResult (\ s a -> s{_sAutoMLResult = a})

-- | The creation date and time (in Unix time) of the solution.
sCreationDateTime :: Lens' Solution (Maybe UTCTime)
sCreationDateTime = lens _sCreationDateTime (\ s a -> s{_sCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dataset group that provides the training data.
sDatasetGroupARN :: Lens' Solution (Maybe Text)
sDatasetGroupARN = lens _sDatasetGroupARN (\ s a -> s{_sDatasetGroupARN = a})

-- | Describes the latest version of the solution, including the status and the ARN.
sLatestSolutionVersion :: Lens' Solution (Maybe SolutionVersionSummary)
sLatestSolutionVersion = lens _sLatestSolutionVersion (\ s a -> s{_sLatestSolutionVersion = a})

-- | Describes the configuration properties for the solution.
sSolutionConfig :: Lens' Solution (Maybe SolutionConfig)
sSolutionConfig = lens _sSolutionConfig (\ s a -> s{_sSolutionConfig = a})

-- | Whether to perform hyperparameter optimization (HPO) on the chosen recipe. The default is @false@ .
sPerformHPO :: Lens' Solution (Maybe Bool)
sPerformHPO = lens _sPerformHPO (\ s a -> s{_sPerformHPO = a})

instance FromJSON Solution where
        parseJSON
          = withObject "Solution"
              (\ x ->
                 Solution' <$>
                   (x .:? "solutionArn") <*> (x .:? "status") <*>
                     (x .:? "performAutoML")
                     <*> (x .:? "recipeArn")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "eventType")
                     <*> (x .:? "name")
                     <*> (x .:? "autoMLResult")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn")
                     <*> (x .:? "latestSolutionVersion")
                     <*> (x .:? "solutionConfig")
                     <*> (x .:? "performHPO"))

instance Hashable Solution where

instance NFData Solution where
