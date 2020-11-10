{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.AutoMLConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.AutoMLConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When the solution performs AutoML (@performAutoML@ is true in 'CreateSolution' ), Amazon Personalize determines which recipe, from the specified list, optimizes the given metric. Amazon Personalize then uses that recipe for the solution.
--
--
--
-- /See:/ 'autoMLConfig' smart constructor.
data AutoMLConfig = AutoMLConfig'{_amlcRecipeList ::
                                  !(Maybe [Text]),
                                  _amlcMetricName :: !(Maybe Text)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlcRecipeList' - The list of candidate recipes.
--
-- * 'amlcMetricName' - The metric to optimize.
autoMLConfig
    :: AutoMLConfig
autoMLConfig
  = AutoMLConfig'{_amlcRecipeList = Nothing,
                  _amlcMetricName = Nothing}

-- | The list of candidate recipes.
amlcRecipeList :: Lens' AutoMLConfig [Text]
amlcRecipeList = lens _amlcRecipeList (\ s a -> s{_amlcRecipeList = a}) . _Default . _Coerce

-- | The metric to optimize.
amlcMetricName :: Lens' AutoMLConfig (Maybe Text)
amlcMetricName = lens _amlcMetricName (\ s a -> s{_amlcMetricName = a})

instance FromJSON AutoMLConfig where
        parseJSON
          = withObject "AutoMLConfig"
              (\ x ->
                 AutoMLConfig' <$>
                   (x .:? "recipeList" .!= mempty) <*>
                     (x .:? "metricName"))

instance Hashable AutoMLConfig where

instance NFData AutoMLConfig where

instance ToJSON AutoMLConfig where
        toJSON AutoMLConfig'{..}
          = object
              (catMaybes
                 [("recipeList" .=) <$> _amlcRecipeList,
                  ("metricName" .=) <$> _amlcMetricName])
