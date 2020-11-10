{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DefaultCategoricalHyperParameterRange
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DefaultCategoricalHyperParameterRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the name and default range of a categorical hyperparameter and whether the hyperparameter is tunable. A tunable hyperparameter can have its value determined during hyperparameter optimization (HPO).
--
--
--
-- /See:/ 'defaultCategoricalHyperParameterRange' smart constructor.
data DefaultCategoricalHyperParameterRange = DefaultCategoricalHyperParameterRange'{_defIsTunable
                                                                                    ::
                                                                                    !(Maybe
                                                                                        Bool),
                                                                                    _defValues
                                                                                    ::
                                                                                    !(Maybe
                                                                                        [Text]),
                                                                                    _defName
                                                                                    ::
                                                                                    !(Maybe
                                                                                        Text)}
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'DefaultCategoricalHyperParameterRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'defIsTunable' - Whether the hyperparameter is tunable.
--
-- * 'defValues' - A list of the categories for the hyperparameter.
--
-- * 'defName' - The name of the hyperparameter.
defaultCategoricalHyperParameterRange
    :: DefaultCategoricalHyperParameterRange
defaultCategoricalHyperParameterRange
  = DefaultCategoricalHyperParameterRange'{_defIsTunable
                                             = Nothing,
                                           _defValues = Nothing,
                                           _defName = Nothing}

-- | Whether the hyperparameter is tunable.
defIsTunable :: Lens' DefaultCategoricalHyperParameterRange (Maybe Bool)
defIsTunable = lens _defIsTunable (\ s a -> s{_defIsTunable = a})

-- | A list of the categories for the hyperparameter.
defValues :: Lens' DefaultCategoricalHyperParameterRange [Text]
defValues = lens _defValues (\ s a -> s{_defValues = a}) . _Default . _Coerce

-- | The name of the hyperparameter.
defName :: Lens' DefaultCategoricalHyperParameterRange (Maybe Text)
defName = lens _defName (\ s a -> s{_defName = a})

instance FromJSON
           DefaultCategoricalHyperParameterRange
         where
        parseJSON
          = withObject "DefaultCategoricalHyperParameterRange"
              (\ x ->
                 DefaultCategoricalHyperParameterRange' <$>
                   (x .:? "isTunable") <*> (x .:? "values" .!= mempty)
                     <*> (x .:? "name"))

instance Hashable
           DefaultCategoricalHyperParameterRange
         where

instance NFData DefaultCategoricalHyperParameterRange
         where
