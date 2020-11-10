{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DataSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the data source that contains the data to upload to a dataset.
--
--
--
-- /See:/ 'dataSource' smart constructor.
newtype DataSource = DataSource'{_dsDataLocation ::
                                 Maybe Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsDataLocation' - The path to the Amazon S3 bucket where the data that you want to upload to your dataset is stored. For example:  @s3://bucket-name/training-data.csv@ 
dataSource
    :: DataSource
dataSource = DataSource'{_dsDataLocation = Nothing}

-- | The path to the Amazon S3 bucket where the data that you want to upload to your dataset is stored. For example:  @s3://bucket-name/training-data.csv@ 
dsDataLocation :: Lens' DataSource (Maybe Text)
dsDataLocation = lens _dsDataLocation (\ s a -> s{_dsDataLocation = a})

instance FromJSON DataSource where
        parseJSON
          = withObject "DataSource"
              (\ x -> DataSource' <$> (x .:? "dataLocation"))

instance Hashable DataSource where

instance NFData DataSource where

instance ToJSON DataSource where
        toJSON DataSource'{..}
          = object
              (catMaybes [("dataLocation" .=) <$> _dsDataLocation])
