{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.HPOObjective
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.HPOObjective where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metric to optimize during hyperparameter optimization (HPO).
--
--
--
-- /See:/ 'hPOObjective' smart constructor.
data HPOObjective = HPOObjective'{_hpooMetricName ::
                                  !(Maybe Text),
                                  _hpooType :: !(Maybe Text),
                                  _hpooMetricRegex :: !(Maybe Text)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HPOObjective' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpooMetricName' - The name of the metric.
--
-- * 'hpooType' - The type of the metric. Valid values are @Maximize@ and @Minimize@ .
--
-- * 'hpooMetricRegex' - A regular expression for finding the metric in the training job logs.
hPOObjective
    :: HPOObjective
hPOObjective
  = HPOObjective'{_hpooMetricName = Nothing,
                  _hpooType = Nothing, _hpooMetricRegex = Nothing}

-- | The name of the metric.
hpooMetricName :: Lens' HPOObjective (Maybe Text)
hpooMetricName = lens _hpooMetricName (\ s a -> s{_hpooMetricName = a})

-- | The type of the metric. Valid values are @Maximize@ and @Minimize@ .
hpooType :: Lens' HPOObjective (Maybe Text)
hpooType = lens _hpooType (\ s a -> s{_hpooType = a})

-- | A regular expression for finding the metric in the training job logs.
hpooMetricRegex :: Lens' HPOObjective (Maybe Text)
hpooMetricRegex = lens _hpooMetricRegex (\ s a -> s{_hpooMetricRegex = a})

instance FromJSON HPOObjective where
        parseJSON
          = withObject "HPOObjective"
              (\ x ->
                 HPOObjective' <$>
                   (x .:? "metricName") <*> (x .:? "type") <*>
                     (x .:? "metricRegex"))

instance Hashable HPOObjective where

instance NFData HPOObjective where

instance ToJSON HPOObjective where
        toJSON HPOObjective'{..}
          = object
              (catMaybes
                 [("metricName" .=) <$> _hpooMetricName,
                  ("type" .=) <$> _hpooType,
                  ("metricRegex" .=) <$> _hpooMetricRegex])
