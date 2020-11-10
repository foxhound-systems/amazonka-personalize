{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DefaultIntegerHyperParameterRange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DefaultIntegerHyperParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name and default range of a integer-valued hyperparameter and whether the hyperparameter is tunable. A tunable hyperparameter can have its value determined during hyperparameter optimization (HPO).
--
--
--
-- /See:/ 'defaultIntegerHyperParameterRange' smart constructor.
data DefaultIntegerHyperParameterRange = DefaultIntegerHyperParameterRange'{_dihprMaxValue
                                                                            ::
                                                                            !(Maybe
                                                                                Int),
                                                                            _dihprIsTunable
                                                                            ::
                                                                            !(Maybe
                                                                                Bool),
                                                                            _dihprName
                                                                            ::
                                                                            !(Maybe
                                                                                Text),
                                                                            _dihprMinValue
                                                                            ::
                                                                            !(Maybe
                                                                                Int)}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'DefaultIntegerHyperParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dihprMaxValue' - The maximum allowable value for the hyperparameter.
--
-- * 'dihprIsTunable' - Indicates whether the hyperparameter is tunable.
--
-- * 'dihprName' - The name of the hyperparameter.
--
-- * 'dihprMinValue' - The minimum allowable value for the hyperparameter.
defaultIntegerHyperParameterRange
    :: DefaultIntegerHyperParameterRange
defaultIntegerHyperParameterRange
  = DefaultIntegerHyperParameterRange'{_dihprMaxValue =
                                         Nothing,
                                       _dihprIsTunable = Nothing,
                                       _dihprName = Nothing,
                                       _dihprMinValue = Nothing}

-- | The maximum allowable value for the hyperparameter.
dihprMaxValue :: Lens' DefaultIntegerHyperParameterRange (Maybe Int)
dihprMaxValue = lens _dihprMaxValue (\ s a -> s{_dihprMaxValue = a})

-- | Indicates whether the hyperparameter is tunable.
dihprIsTunable :: Lens' DefaultIntegerHyperParameterRange (Maybe Bool)
dihprIsTunable = lens _dihprIsTunable (\ s a -> s{_dihprIsTunable = a})

-- | The name of the hyperparameter.
dihprName :: Lens' DefaultIntegerHyperParameterRange (Maybe Text)
dihprName = lens _dihprName (\ s a -> s{_dihprName = a})

-- | The minimum allowable value for the hyperparameter.
dihprMinValue :: Lens' DefaultIntegerHyperParameterRange (Maybe Int)
dihprMinValue = lens _dihprMinValue (\ s a -> s{_dihprMinValue = a})

instance FromJSON DefaultIntegerHyperParameterRange
         where
        parseJSON
          = withObject "DefaultIntegerHyperParameterRange"
              (\ x ->
                 DefaultIntegerHyperParameterRange' <$>
                   (x .:? "maxValue") <*> (x .:? "isTunable") <*>
                     (x .:? "name")
                     <*> (x .:? "minValue"))

instance Hashable DefaultIntegerHyperParameterRange
         where

instance NFData DefaultIntegerHyperParameterRange
         where
