{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.IntegerHyperParameterRange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.IntegerHyperParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name and range of an integer-valued hyperparameter.
--
--
--
-- /See:/ 'integerHyperParameterRange' smart constructor.
data IntegerHyperParameterRange = IntegerHyperParameterRange'{_ihprMaxValue
                                                              :: !(Maybe Int),
                                                              _ihprName ::
                                                              !(Maybe Text),
                                                              _ihprMinValue ::
                                                              !(Maybe Int)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'IntegerHyperParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ihprMaxValue' - The maximum allowable value for the hyperparameter.
--
-- * 'ihprName' - The name of the hyperparameter.
--
-- * 'ihprMinValue' - The minimum allowable value for the hyperparameter.
integerHyperParameterRange
    :: IntegerHyperParameterRange
integerHyperParameterRange
  = IntegerHyperParameterRange'{_ihprMaxValue =
                                  Nothing,
                                _ihprName = Nothing, _ihprMinValue = Nothing}

-- | The maximum allowable value for the hyperparameter.
ihprMaxValue :: Lens' IntegerHyperParameterRange (Maybe Int)
ihprMaxValue = lens _ihprMaxValue (\ s a -> s{_ihprMaxValue = a})

-- | The name of the hyperparameter.
ihprName :: Lens' IntegerHyperParameterRange (Maybe Text)
ihprName = lens _ihprName (\ s a -> s{_ihprName = a})

-- | The minimum allowable value for the hyperparameter.
ihprMinValue :: Lens' IntegerHyperParameterRange (Maybe Int)
ihprMinValue = lens _ihprMinValue (\ s a -> s{_ihprMinValue = a})

instance FromJSON IntegerHyperParameterRange where
        parseJSON
          = withObject "IntegerHyperParameterRange"
              (\ x ->
                 IntegerHyperParameterRange' <$>
                   (x .:? "maxValue") <*> (x .:? "name") <*>
                     (x .:? "minValue"))

instance Hashable IntegerHyperParameterRange where

instance NFData IntegerHyperParameterRange where

instance ToJSON IntegerHyperParameterRange where
        toJSON IntegerHyperParameterRange'{..}
          = object
              (catMaybes
                 [("maxValue" .=) <$> _ihprMaxValue,
                  ("name" .=) <$> _ihprName,
                  ("minValue" .=) <$> _ihprMinValue])
