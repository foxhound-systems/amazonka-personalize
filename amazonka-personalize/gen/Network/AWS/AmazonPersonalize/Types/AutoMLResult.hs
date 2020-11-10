{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.AutoMLResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.AutoMLResult where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When the solution performs AutoML (@performAutoML@ is true in 'CreateSolution' ), specifies the recipe that best optimized the specified metric.
--
--
--
-- /See:/ 'autoMLResult' smart constructor.
newtype AutoMLResult = AutoMLResult'{_amlrBestRecipeARN
                                     :: Maybe Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlrBestRecipeARN' - The Amazon Resource Name (ARN) of the best recipe.
autoMLResult
    :: AutoMLResult
autoMLResult
  = AutoMLResult'{_amlrBestRecipeARN = Nothing}

-- | The Amazon Resource Name (ARN) of the best recipe.
amlrBestRecipeARN :: Lens' AutoMLResult (Maybe Text)
amlrBestRecipeARN = lens _amlrBestRecipeARN (\ s a -> s{_amlrBestRecipeARN = a})

instance FromJSON AutoMLResult where
        parseJSON
          = withObject "AutoMLResult"
              (\ x -> AutoMLResult' <$> (x .:? "bestRecipeArn"))

instance Hashable AutoMLResult where

instance NFData AutoMLResult where
