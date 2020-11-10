{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.HPOConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.HPOConfig where

import Network.AWS.AmazonPersonalize.Types.HPOObjective
import Network.AWS.AmazonPersonalize.Types.HPOResourceConfig
import Network.AWS.AmazonPersonalize.Types.HyperParameterRanges
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties for hyperparameter optimization (HPO). For use with the bring-your-own-recipe feature. Do not use for Amazon Personalize native recipes.
--
--
--
-- /See:/ 'hPOConfig' smart constructor.
data HPOConfig = HPOConfig'{_hpocAlgorithmHyperParameterRanges
                            :: !(Maybe HyperParameterRanges),
                            _hpocHpoResourceConfig ::
                            !(Maybe HPOResourceConfig),
                            _hpocHpoObjective :: !(Maybe HPOObjective)}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HPOConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpocAlgorithmHyperParameterRanges' - The hyperparameters and their allowable ranges.
--
-- * 'hpocHpoResourceConfig' - Describes the resource configuration for HPO.
--
-- * 'hpocHpoObjective' - The metric to optimize during HPO.
hPOConfig
    :: HPOConfig
hPOConfig
  = HPOConfig'{_hpocAlgorithmHyperParameterRanges =
                 Nothing,
               _hpocHpoResourceConfig = Nothing,
               _hpocHpoObjective = Nothing}

-- | The hyperparameters and their allowable ranges.
hpocAlgorithmHyperParameterRanges :: Lens' HPOConfig (Maybe HyperParameterRanges)
hpocAlgorithmHyperParameterRanges = lens _hpocAlgorithmHyperParameterRanges (\ s a -> s{_hpocAlgorithmHyperParameterRanges = a})

-- | Describes the resource configuration for HPO.
hpocHpoResourceConfig :: Lens' HPOConfig (Maybe HPOResourceConfig)
hpocHpoResourceConfig = lens _hpocHpoResourceConfig (\ s a -> s{_hpocHpoResourceConfig = a})

-- | The metric to optimize during HPO.
hpocHpoObjective :: Lens' HPOConfig (Maybe HPOObjective)
hpocHpoObjective = lens _hpocHpoObjective (\ s a -> s{_hpocHpoObjective = a})

instance FromJSON HPOConfig where
        parseJSON
          = withObject "HPOConfig"
              (\ x ->
                 HPOConfig' <$>
                   (x .:? "algorithmHyperParameterRanges") <*>
                     (x .:? "hpoResourceConfig")
                     <*> (x .:? "hpoObjective"))

instance Hashable HPOConfig where

instance NFData HPOConfig where

instance ToJSON HPOConfig where
        toJSON HPOConfig'{..}
          = object
              (catMaybes
                 [("algorithmHyperParameterRanges" .=) <$>
                    _hpocAlgorithmHyperParameterRanges,
                  ("hpoResourceConfig" .=) <$> _hpocHpoResourceConfig,
                  ("hpoObjective" .=) <$> _hpocHpoObjective])
