{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.SolutionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.SolutionVersion where

import Network.AWS.AmazonPersonalize.Types.SolutionConfig
import Network.AWS.AmazonPersonalize.Types.TrainingMode
import Network.AWS.AmazonPersonalize.Types.TunedHPOParams
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that provides information about a specific version of a 'Solution' .
--
--
--
-- /See:/ 'solutionVersion' smart constructor.
data SolutionVersion = SolutionVersion'{_svFailureReason
                                        :: !(Maybe Text),
                                        _svSolutionARN :: !(Maybe Text),
                                        _svStatus :: !(Maybe Text),
                                        _svPerformAutoML :: !(Maybe Bool),
                                        _svTunedHPOParams ::
                                        !(Maybe TunedHPOParams),
                                        _svRecipeARN :: !(Maybe Text),
                                        _svLastUpdatedDateTime ::
                                        !(Maybe POSIX),
                                        _svEventType :: !(Maybe Text),
                                        _svCreationDateTime :: !(Maybe POSIX),
                                        _svDatasetGroupARN :: !(Maybe Text),
                                        _svTrainingMode ::
                                        !(Maybe TrainingMode),
                                        _svTrainingHours :: !(Maybe Double),
                                        _svSolutionConfig ::
                                        !(Maybe SolutionConfig),
                                        _svPerformHPO :: !(Maybe Bool),
                                        _svSolutionVersionARN :: !(Maybe Text)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SolutionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svFailureReason' - If training a solution version fails, the reason for the failure.
--
-- * 'svSolutionARN' - The ARN of the solution.
--
-- * 'svStatus' - The status of the solution version. A solution version can be in one of the following states:     * CREATE PENDING     * CREATE IN_PROGRESS     * ACTIVE     * CREATE FAILED
--
-- * 'svPerformAutoML' - When true, Amazon Personalize searches for the most optimal recipe according to the solution configuration. When false (the default), Amazon Personalize uses @recipeArn@ .
--
-- * 'svTunedHPOParams' - If hyperparameter optimization was performed, contains the hyperparameter values of the best performing model.
--
-- * 'svRecipeARN' - The ARN of the recipe used in the solution.
--
-- * 'svLastUpdatedDateTime' - The date and time (in Unix time) that the solution was last updated.
--
-- * 'svEventType' - The event type (for example, 'click' or 'like') that is used for training the model.
--
-- * 'svCreationDateTime' - The date and time (in Unix time) that this version of the solution was created.
--
-- * 'svDatasetGroupARN' - The Amazon Resource Name (ARN) of the dataset group providing the training data.
--
-- * 'svTrainingMode' - The scope of training used to create the solution version. The @FULL@ option trains the solution version based on the entirety of the input solution's training data, while the @UPDATE@ option processes only the training data that has changed since the creation of the last solution version. Choose @UPDATE@ when you want to start recommending items added to the dataset without retraining the model. /Important:/ The @UPDATE@ option can only be used after you've created a solution version with the @FULL@ option and the training solution uses the 'native-recipe-hrnn-coldstart' .
--
-- * 'svTrainingHours' - The time used to train the model. You are billed for the time it takes to train a model. This field is visible only after Amazon Personalize successfully trains a model.
--
-- * 'svSolutionConfig' - Describes the configuration properties for the solution.
--
-- * 'svPerformHPO' - Whether to perform hyperparameter optimization (HPO) on the chosen recipe. The default is @false@ .
--
-- * 'svSolutionVersionARN' - The ARN of the solution version.
solutionVersion
    :: SolutionVersion
solutionVersion
  = SolutionVersion'{_svFailureReason = Nothing,
                     _svSolutionARN = Nothing, _svStatus = Nothing,
                     _svPerformAutoML = Nothing,
                     _svTunedHPOParams = Nothing, _svRecipeARN = Nothing,
                     _svLastUpdatedDateTime = Nothing,
                     _svEventType = Nothing,
                     _svCreationDateTime = Nothing,
                     _svDatasetGroupARN = Nothing,
                     _svTrainingMode = Nothing,
                     _svTrainingHours = Nothing,
                     _svSolutionConfig = Nothing, _svPerformHPO = Nothing,
                     _svSolutionVersionARN = Nothing}

-- | If training a solution version fails, the reason for the failure.
svFailureReason :: Lens' SolutionVersion (Maybe Text)
svFailureReason = lens _svFailureReason (\ s a -> s{_svFailureReason = a})

-- | The ARN of the solution.
svSolutionARN :: Lens' SolutionVersion (Maybe Text)
svSolutionARN = lens _svSolutionARN (\ s a -> s{_svSolutionARN = a})

-- | The status of the solution version. A solution version can be in one of the following states:     * CREATE PENDING     * CREATE IN_PROGRESS     * ACTIVE     * CREATE FAILED
svStatus :: Lens' SolutionVersion (Maybe Text)
svStatus = lens _svStatus (\ s a -> s{_svStatus = a})

-- | When true, Amazon Personalize searches for the most optimal recipe according to the solution configuration. When false (the default), Amazon Personalize uses @recipeArn@ .
svPerformAutoML :: Lens' SolutionVersion (Maybe Bool)
svPerformAutoML = lens _svPerformAutoML (\ s a -> s{_svPerformAutoML = a})

-- | If hyperparameter optimization was performed, contains the hyperparameter values of the best performing model.
svTunedHPOParams :: Lens' SolutionVersion (Maybe TunedHPOParams)
svTunedHPOParams = lens _svTunedHPOParams (\ s a -> s{_svTunedHPOParams = a})

-- | The ARN of the recipe used in the solution.
svRecipeARN :: Lens' SolutionVersion (Maybe Text)
svRecipeARN = lens _svRecipeARN (\ s a -> s{_svRecipeARN = a})

-- | The date and time (in Unix time) that the solution was last updated.
svLastUpdatedDateTime :: Lens' SolutionVersion (Maybe UTCTime)
svLastUpdatedDateTime = lens _svLastUpdatedDateTime (\ s a -> s{_svLastUpdatedDateTime = a}) . mapping _Time

-- | The event type (for example, 'click' or 'like') that is used for training the model.
svEventType :: Lens' SolutionVersion (Maybe Text)
svEventType = lens _svEventType (\ s a -> s{_svEventType = a})

-- | The date and time (in Unix time) that this version of the solution was created.
svCreationDateTime :: Lens' SolutionVersion (Maybe UTCTime)
svCreationDateTime = lens _svCreationDateTime (\ s a -> s{_svCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the dataset group providing the training data.
svDatasetGroupARN :: Lens' SolutionVersion (Maybe Text)
svDatasetGroupARN = lens _svDatasetGroupARN (\ s a -> s{_svDatasetGroupARN = a})

-- | The scope of training used to create the solution version. The @FULL@ option trains the solution version based on the entirety of the input solution's training data, while the @UPDATE@ option processes only the training data that has changed since the creation of the last solution version. Choose @UPDATE@ when you want to start recommending items added to the dataset without retraining the model. /Important:/ The @UPDATE@ option can only be used after you've created a solution version with the @FULL@ option and the training solution uses the 'native-recipe-hrnn-coldstart' .
svTrainingMode :: Lens' SolutionVersion (Maybe TrainingMode)
svTrainingMode = lens _svTrainingMode (\ s a -> s{_svTrainingMode = a})

-- | The time used to train the model. You are billed for the time it takes to train a model. This field is visible only after Amazon Personalize successfully trains a model.
svTrainingHours :: Lens' SolutionVersion (Maybe Double)
svTrainingHours = lens _svTrainingHours (\ s a -> s{_svTrainingHours = a})

-- | Describes the configuration properties for the solution.
svSolutionConfig :: Lens' SolutionVersion (Maybe SolutionConfig)
svSolutionConfig = lens _svSolutionConfig (\ s a -> s{_svSolutionConfig = a})

-- | Whether to perform hyperparameter optimization (HPO) on the chosen recipe. The default is @false@ .
svPerformHPO :: Lens' SolutionVersion (Maybe Bool)
svPerformHPO = lens _svPerformHPO (\ s a -> s{_svPerformHPO = a})

-- | The ARN of the solution version.
svSolutionVersionARN :: Lens' SolutionVersion (Maybe Text)
svSolutionVersionARN = lens _svSolutionVersionARN (\ s a -> s{_svSolutionVersionARN = a})

instance FromJSON SolutionVersion where
        parseJSON
          = withObject "SolutionVersion"
              (\ x ->
                 SolutionVersion' <$>
                   (x .:? "failureReason") <*> (x .:? "solutionArn") <*>
                     (x .:? "status")
                     <*> (x .:? "performAutoML")
                     <*> (x .:? "tunedHPOParams")
                     <*> (x .:? "recipeArn")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "eventType")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn")
                     <*> (x .:? "trainingMode")
                     <*> (x .:? "trainingHours")
                     <*> (x .:? "solutionConfig")
                     <*> (x .:? "performHPO")
                     <*> (x .:? "solutionVersionArn"))

instance Hashable SolutionVersion where

instance NFData SolutionVersion where
