{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.CategoricalHyperParameterRange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.CategoricalHyperParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name and range of a categorical hyperparameter.
--
--
--
-- /See:/ 'categoricalHyperParameterRange' smart constructor.
data CategoricalHyperParameterRange = CategoricalHyperParameterRange'{_chprValues
                                                                      ::
                                                                      !(Maybe
                                                                          [Text]),
                                                                      _chprName
                                                                      ::
                                                                      !(Maybe
                                                                          Text)}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'CategoricalHyperParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chprValues' - A list of the categories for the hyperparameter.
--
-- * 'chprName' - The name of the hyperparameter.
categoricalHyperParameterRange
    :: CategoricalHyperParameterRange
categoricalHyperParameterRange
  = CategoricalHyperParameterRange'{_chprValues =
                                      Nothing,
                                    _chprName = Nothing}

-- | A list of the categories for the hyperparameter.
chprValues :: Lens' CategoricalHyperParameterRange [Text]
chprValues = lens _chprValues (\ s a -> s{_chprValues = a}) . _Default . _Coerce

-- | The name of the hyperparameter.
chprName :: Lens' CategoricalHyperParameterRange (Maybe Text)
chprName = lens _chprName (\ s a -> s{_chprName = a})

instance FromJSON CategoricalHyperParameterRange
         where
        parseJSON
          = withObject "CategoricalHyperParameterRange"
              (\ x ->
                 CategoricalHyperParameterRange' <$>
                   (x .:? "values" .!= mempty) <*> (x .:? "name"))

instance Hashable CategoricalHyperParameterRange
         where

instance NFData CategoricalHyperParameterRange where

instance ToJSON CategoricalHyperParameterRange where
        toJSON CategoricalHyperParameterRange'{..}
          = object
              (catMaybes
                 [("values" .=) <$> _chprValues,
                  ("name" .=) <$> _chprName])
