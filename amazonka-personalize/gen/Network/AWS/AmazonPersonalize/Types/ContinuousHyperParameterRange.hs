{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.ContinuousHyperParameterRange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.ContinuousHyperParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name and range of a continuous hyperparameter.
--
--
--
-- /See:/ 'continuousHyperParameterRange' smart constructor.
data ContinuousHyperParameterRange = ContinuousHyperParameterRange'{_cMaxValue
                                                                    ::
                                                                    !(Maybe
                                                                        Double),
                                                                    _cName ::
                                                                    !(Maybe
                                                                        Text),
                                                                    _cMinValue
                                                                    ::
                                                                    !(Maybe
                                                                        Double)}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'ContinuousHyperParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cMaxValue' - The maximum allowable value for the hyperparameter.
--
-- * 'cName' - The name of the hyperparameter.
--
-- * 'cMinValue' - The minimum allowable value for the hyperparameter.
continuousHyperParameterRange
    :: ContinuousHyperParameterRange
continuousHyperParameterRange
  = ContinuousHyperParameterRange'{_cMaxValue =
                                     Nothing,
                                   _cName = Nothing, _cMinValue = Nothing}

-- | The maximum allowable value for the hyperparameter.
cMaxValue :: Lens' ContinuousHyperParameterRange (Maybe Double)
cMaxValue = lens _cMaxValue (\ s a -> s{_cMaxValue = a})

-- | The name of the hyperparameter.
cName :: Lens' ContinuousHyperParameterRange (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | The minimum allowable value for the hyperparameter.
cMinValue :: Lens' ContinuousHyperParameterRange (Maybe Double)
cMinValue = lens _cMinValue (\ s a -> s{_cMinValue = a})

instance FromJSON ContinuousHyperParameterRange where
        parseJSON
          = withObject "ContinuousHyperParameterRange"
              (\ x ->
                 ContinuousHyperParameterRange' <$>
                   (x .:? "maxValue") <*> (x .:? "name") <*>
                     (x .:? "minValue"))

instance Hashable ContinuousHyperParameterRange where

instance NFData ContinuousHyperParameterRange where

instance ToJSON ContinuousHyperParameterRange where
        toJSON ContinuousHyperParameterRange'{..}
          = object
              (catMaybes
                 [("maxValue" .=) <$> _cMaxValue,
                  ("name" .=) <$> _cName,
                  ("minValue" .=) <$> _cMinValue])
