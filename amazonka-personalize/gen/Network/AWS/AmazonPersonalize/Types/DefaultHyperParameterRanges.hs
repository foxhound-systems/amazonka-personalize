{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DefaultHyperParameterRanges
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DefaultHyperParameterRanges where

import Network.AWS.AmazonPersonalize.Types.DefaultCategoricalHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.DefaultContinuousHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.DefaultIntegerHyperParameterRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the hyperparameters and their default ranges. Hyperparameters can be categorical, continuous, or integer-valued.
--
--
--
-- /See:/ 'defaultHyperParameterRanges' smart constructor.
data DefaultHyperParameterRanges = DefaultHyperParameterRanges'{_dhprIntegerHyperParameterRanges
                                                                ::
                                                                !(Maybe
                                                                    [DefaultIntegerHyperParameterRange]),
                                                                _dhprCategoricalHyperParameterRanges
                                                                ::
                                                                !(Maybe
                                                                    [DefaultCategoricalHyperParameterRange]),
                                                                _dhprContinuousHyperParameterRanges
                                                                ::
                                                                !(Maybe
                                                                    [DefaultContinuousHyperParameterRange])}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'DefaultHyperParameterRanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhprIntegerHyperParameterRanges' - The integer-valued hyperparameters and their default ranges.
--
-- * 'dhprCategoricalHyperParameterRanges' - The categorical hyperparameters and their default ranges.
--
-- * 'dhprContinuousHyperParameterRanges' - The continuous hyperparameters and their default ranges.
defaultHyperParameterRanges
    :: DefaultHyperParameterRanges
defaultHyperParameterRanges
  = DefaultHyperParameterRanges'{_dhprIntegerHyperParameterRanges
                                   = Nothing,
                                 _dhprCategoricalHyperParameterRanges = Nothing,
                                 _dhprContinuousHyperParameterRanges = Nothing}

-- | The integer-valued hyperparameters and their default ranges.
dhprIntegerHyperParameterRanges :: Lens' DefaultHyperParameterRanges [DefaultIntegerHyperParameterRange]
dhprIntegerHyperParameterRanges = lens _dhprIntegerHyperParameterRanges (\ s a -> s{_dhprIntegerHyperParameterRanges = a}) . _Default . _Coerce

-- | The categorical hyperparameters and their default ranges.
dhprCategoricalHyperParameterRanges :: Lens' DefaultHyperParameterRanges [DefaultCategoricalHyperParameterRange]
dhprCategoricalHyperParameterRanges = lens _dhprCategoricalHyperParameterRanges (\ s a -> s{_dhprCategoricalHyperParameterRanges = a}) . _Default . _Coerce

-- | The continuous hyperparameters and their default ranges.
dhprContinuousHyperParameterRanges :: Lens' DefaultHyperParameterRanges [DefaultContinuousHyperParameterRange]
dhprContinuousHyperParameterRanges = lens _dhprContinuousHyperParameterRanges (\ s a -> s{_dhprContinuousHyperParameterRanges = a}) . _Default . _Coerce

instance FromJSON DefaultHyperParameterRanges where
        parseJSON
          = withObject "DefaultHyperParameterRanges"
              (\ x ->
                 DefaultHyperParameterRanges' <$>
                   (x .:? "integerHyperParameterRanges" .!= mempty) <*>
                     (x .:? "categoricalHyperParameterRanges" .!= mempty)
                     <*>
                     (x .:? "continuousHyperParameterRanges" .!= mempty))

instance Hashable DefaultHyperParameterRanges where

instance NFData DefaultHyperParameterRanges where
