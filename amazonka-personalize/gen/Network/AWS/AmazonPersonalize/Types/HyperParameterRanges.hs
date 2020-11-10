{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.HyperParameterRanges
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.HyperParameterRanges where

import Network.AWS.AmazonPersonalize.Types.CategoricalHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.ContinuousHyperParameterRange
import Network.AWS.AmazonPersonalize.Types.IntegerHyperParameterRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the hyperparameters and their ranges. Hyperparameters can be categorical, continuous, or integer-valued.
--
--
--
-- /See:/ 'hyperParameterRanges' smart constructor.
data HyperParameterRanges = HyperParameterRanges'{_hprIntegerHyperParameterRanges
                                                  ::
                                                  !(Maybe
                                                      [IntegerHyperParameterRange]),
                                                  _hprCategoricalHyperParameterRanges
                                                  ::
                                                  !(Maybe
                                                      [CategoricalHyperParameterRange]),
                                                  _hprContinuousHyperParameterRanges
                                                  ::
                                                  !(Maybe
                                                      [ContinuousHyperParameterRange])}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterRanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hprIntegerHyperParameterRanges' - The integer-valued hyperparameters and their ranges.
--
-- * 'hprCategoricalHyperParameterRanges' - The categorical hyperparameters and their ranges.
--
-- * 'hprContinuousHyperParameterRanges' - The continuous hyperparameters and their ranges.
hyperParameterRanges
    :: HyperParameterRanges
hyperParameterRanges
  = HyperParameterRanges'{_hprIntegerHyperParameterRanges
                            = Nothing,
                          _hprCategoricalHyperParameterRanges = Nothing,
                          _hprContinuousHyperParameterRanges = Nothing}

-- | The integer-valued hyperparameters and their ranges.
hprIntegerHyperParameterRanges :: Lens' HyperParameterRanges [IntegerHyperParameterRange]
hprIntegerHyperParameterRanges = lens _hprIntegerHyperParameterRanges (\ s a -> s{_hprIntegerHyperParameterRanges = a}) . _Default . _Coerce

-- | The categorical hyperparameters and their ranges.
hprCategoricalHyperParameterRanges :: Lens' HyperParameterRanges [CategoricalHyperParameterRange]
hprCategoricalHyperParameterRanges = lens _hprCategoricalHyperParameterRanges (\ s a -> s{_hprCategoricalHyperParameterRanges = a}) . _Default . _Coerce

-- | The continuous hyperparameters and their ranges.
hprContinuousHyperParameterRanges :: Lens' HyperParameterRanges [ContinuousHyperParameterRange]
hprContinuousHyperParameterRanges = lens _hprContinuousHyperParameterRanges (\ s a -> s{_hprContinuousHyperParameterRanges = a}) . _Default . _Coerce

instance FromJSON HyperParameterRanges where
        parseJSON
          = withObject "HyperParameterRanges"
              (\ x ->
                 HyperParameterRanges' <$>
                   (x .:? "integerHyperParameterRanges" .!= mempty) <*>
                     (x .:? "categoricalHyperParameterRanges" .!= mempty)
                     <*>
                     (x .:? "continuousHyperParameterRanges" .!= mempty))

instance Hashable HyperParameterRanges where

instance NFData HyperParameterRanges where

instance ToJSON HyperParameterRanges where
        toJSON HyperParameterRanges'{..}
          = object
              (catMaybes
                 [("integerHyperParameterRanges" .=) <$>
                    _hprIntegerHyperParameterRanges,
                  ("categoricalHyperParameterRanges" .=) <$>
                    _hprCategoricalHyperParameterRanges,
                  ("continuousHyperParameterRanges" .=) <$>
                    _hprContinuousHyperParameterRanges])
