{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.HPOResourceConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.HPOResourceConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the resource configuration for hyperparameter optimization (HPO).
--
--
--
-- /See:/ 'hPOResourceConfig' smart constructor.
data HPOResourceConfig = HPOResourceConfig'{_hporcMaxNumberOfTrainingJobs
                                            :: !(Maybe Text),
                                            _hporcMaxParallelTrainingJobs ::
                                            !(Maybe Text)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HPOResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hporcMaxNumberOfTrainingJobs' - The maximum number of training jobs when you create a solution version. The maximum value for @maxNumberOfTrainingJobs@ is @40@ .
--
-- * 'hporcMaxParallelTrainingJobs' - The maximum number of parallel training jobs when you create a solution version. The maximum value for @maxParallelTrainingJobs@ is @10@ .
hPOResourceConfig
    :: HPOResourceConfig
hPOResourceConfig
  = HPOResourceConfig'{_hporcMaxNumberOfTrainingJobs =
                         Nothing,
                       _hporcMaxParallelTrainingJobs = Nothing}

-- | The maximum number of training jobs when you create a solution version. The maximum value for @maxNumberOfTrainingJobs@ is @40@ .
hporcMaxNumberOfTrainingJobs :: Lens' HPOResourceConfig (Maybe Text)
hporcMaxNumberOfTrainingJobs = lens _hporcMaxNumberOfTrainingJobs (\ s a -> s{_hporcMaxNumberOfTrainingJobs = a})

-- | The maximum number of parallel training jobs when you create a solution version. The maximum value for @maxParallelTrainingJobs@ is @10@ .
hporcMaxParallelTrainingJobs :: Lens' HPOResourceConfig (Maybe Text)
hporcMaxParallelTrainingJobs = lens _hporcMaxParallelTrainingJobs (\ s a -> s{_hporcMaxParallelTrainingJobs = a})

instance FromJSON HPOResourceConfig where
        parseJSON
          = withObject "HPOResourceConfig"
              (\ x ->
                 HPOResourceConfig' <$>
                   (x .:? "maxNumberOfTrainingJobs") <*>
                     (x .:? "maxParallelTrainingJobs"))

instance Hashable HPOResourceConfig where

instance NFData HPOResourceConfig where

instance ToJSON HPOResourceConfig where
        toJSON HPOResourceConfig'{..}
          = object
              (catMaybes
                 [("maxNumberOfTrainingJobs" .=) <$>
                    _hporcMaxNumberOfTrainingJobs,
                  ("maxParallelTrainingJobs" .=) <$>
                    _hporcMaxParallelTrainingJobs])
