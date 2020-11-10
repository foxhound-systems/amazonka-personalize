{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DefaultContinuousHyperParameterRange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DefaultContinuousHyperParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name and default range of a continuous hyperparameter and whether the hyperparameter is tunable. A tunable hyperparameter can have its value determined during hyperparameter optimization (HPO).
--
--
--
-- /See:/ 'defaultContinuousHyperParameterRange' smart constructor.
data DefaultContinuousHyperParameterRange = DefaultContinuousHyperParameterRange'{_dchprMaxValue
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Double),
                                                                                  _dchprIsTunable
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Bool),
                                                                                  _dchprName
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Text),
                                                                                  _dchprMinValue
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Double)}
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'DefaultContinuousHyperParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dchprMaxValue' - The maximum allowable value for the hyperparameter.
--
-- * 'dchprIsTunable' - Whether the hyperparameter is tunable.
--
-- * 'dchprName' - The name of the hyperparameter.
--
-- * 'dchprMinValue' - The minimum allowable value for the hyperparameter.
defaultContinuousHyperParameterRange
    :: DefaultContinuousHyperParameterRange
defaultContinuousHyperParameterRange
  = DefaultContinuousHyperParameterRange'{_dchprMaxValue
                                            = Nothing,
                                          _dchprIsTunable = Nothing,
                                          _dchprName = Nothing,
                                          _dchprMinValue = Nothing}

-- | The maximum allowable value for the hyperparameter.
dchprMaxValue :: Lens' DefaultContinuousHyperParameterRange (Maybe Double)
dchprMaxValue = lens _dchprMaxValue (\ s a -> s{_dchprMaxValue = a})

-- | Whether the hyperparameter is tunable.
dchprIsTunable :: Lens' DefaultContinuousHyperParameterRange (Maybe Bool)
dchprIsTunable = lens _dchprIsTunable (\ s a -> s{_dchprIsTunable = a})

-- | The name of the hyperparameter.
dchprName :: Lens' DefaultContinuousHyperParameterRange (Maybe Text)
dchprName = lens _dchprName (\ s a -> s{_dchprName = a})

-- | The minimum allowable value for the hyperparameter.
dchprMinValue :: Lens' DefaultContinuousHyperParameterRange (Maybe Double)
dchprMinValue = lens _dchprMinValue (\ s a -> s{_dchprMinValue = a})

instance FromJSON
           DefaultContinuousHyperParameterRange
         where
        parseJSON
          = withObject "DefaultContinuousHyperParameterRange"
              (\ x ->
                 DefaultContinuousHyperParameterRange' <$>
                   (x .:? "maxValue") <*> (x .:? "isTunable") <*>
                     (x .:? "name")
                     <*> (x .:? "minValue"))

instance Hashable
           DefaultContinuousHyperParameterRange
         where

instance NFData DefaultContinuousHyperParameterRange
         where
