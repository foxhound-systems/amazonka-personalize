{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.BatchInferenceJobConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.BatchInferenceJobConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration details of a batch inference job.
--
--
--
-- /See:/ 'batchInferenceJobConfig' smart constructor.
newtype BatchInferenceJobConfig = BatchInferenceJobConfig'{_bijcItemExplorationConfig
                                                           ::
                                                           Maybe
                                                             (Map Text Text)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'BatchInferenceJobConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bijcItemExplorationConfig' - A string to string map specifying the inference hyperparameters you wish to use for hyperparameter optimization. See 'customizing-solution-config-hpo' .
batchInferenceJobConfig
    :: BatchInferenceJobConfig
batchInferenceJobConfig
  = BatchInferenceJobConfig'{_bijcItemExplorationConfig
                               = Nothing}

-- | A string to string map specifying the inference hyperparameters you wish to use for hyperparameter optimization. See 'customizing-solution-config-hpo' .
bijcItemExplorationConfig :: Lens' BatchInferenceJobConfig (HashMap Text Text)
bijcItemExplorationConfig = lens _bijcItemExplorationConfig (\ s a -> s{_bijcItemExplorationConfig = a}) . _Default . _Map

instance FromJSON BatchInferenceJobConfig where
        parseJSON
          = withObject "BatchInferenceJobConfig"
              (\ x ->
                 BatchInferenceJobConfig' <$>
                   (x .:? "itemExplorationConfig" .!= mempty))

instance Hashable BatchInferenceJobConfig where

instance NFData BatchInferenceJobConfig where

instance ToJSON BatchInferenceJobConfig where
        toJSON BatchInferenceJobConfig'{..}
          = object
              (catMaybes
                 [("itemExplorationConfig" .=) <$>
                    _bijcItemExplorationConfig])
