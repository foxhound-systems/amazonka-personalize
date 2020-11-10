{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.TunedHPOParams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.TunedHPOParams where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | If hyperparameter optimization (HPO) was performed, contains the hyperparameter values of the best performing model.
--
--
--
-- /See:/ 'tunedHPOParams' smart constructor.
newtype TunedHPOParams = TunedHPOParams'{_thpopAlgorithmHyperParameters
                                         :: Maybe (Map Text Text)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TunedHPOParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thpopAlgorithmHyperParameters' - A list of the hyperparameter values of the best performing model.
tunedHPOParams
    :: TunedHPOParams
tunedHPOParams
  = TunedHPOParams'{_thpopAlgorithmHyperParameters =
                      Nothing}

-- | A list of the hyperparameter values of the best performing model.
thpopAlgorithmHyperParameters :: Lens' TunedHPOParams (HashMap Text Text)
thpopAlgorithmHyperParameters = lens _thpopAlgorithmHyperParameters (\ s a -> s{_thpopAlgorithmHyperParameters = a}) . _Default . _Map

instance FromJSON TunedHPOParams where
        parseJSON
          = withObject "TunedHPOParams"
              (\ x ->
                 TunedHPOParams' <$>
                   (x .:? "algorithmHyperParameters" .!= mempty))

instance Hashable TunedHPOParams where

instance NFData TunedHPOParams where
