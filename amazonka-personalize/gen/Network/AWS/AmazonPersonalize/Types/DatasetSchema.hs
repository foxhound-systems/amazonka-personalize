{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.DatasetSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.DatasetSchema where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the schema for a dataset. For more information on schemas, see 'CreateSchema' .
--
--
--
-- /See:/ 'datasetSchema' smart constructor.
data DatasetSchema = DatasetSchema'{_dsLastUpdatedDateTime
                                    :: !(Maybe POSIX),
                                    _dsSchema :: !(Maybe Text),
                                    _dsSchemaARN :: !(Maybe Text),
                                    _dsName :: !(Maybe Text),
                                    _dsCreationDateTime :: !(Maybe POSIX)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsLastUpdatedDateTime' - The date and time (in Unix time) that the schema was last updated.
--
-- * 'dsSchema' - The schema.
--
-- * 'dsSchemaARN' - The Amazon Resource Name (ARN) of the schema.
--
-- * 'dsName' - The name of the schema.
--
-- * 'dsCreationDateTime' - The date and time (in Unix time) that the schema was created.
datasetSchema
    :: DatasetSchema
datasetSchema
  = DatasetSchema'{_dsLastUpdatedDateTime = Nothing,
                   _dsSchema = Nothing, _dsSchemaARN = Nothing,
                   _dsName = Nothing, _dsCreationDateTime = Nothing}

-- | The date and time (in Unix time) that the schema was last updated.
dsLastUpdatedDateTime :: Lens' DatasetSchema (Maybe UTCTime)
dsLastUpdatedDateTime = lens _dsLastUpdatedDateTime (\ s a -> s{_dsLastUpdatedDateTime = a}) . mapping _Time

-- | The schema.
dsSchema :: Lens' DatasetSchema (Maybe Text)
dsSchema = lens _dsSchema (\ s a -> s{_dsSchema = a})

-- | The Amazon Resource Name (ARN) of the schema.
dsSchemaARN :: Lens' DatasetSchema (Maybe Text)
dsSchemaARN = lens _dsSchemaARN (\ s a -> s{_dsSchemaARN = a})

-- | The name of the schema.
dsName :: Lens' DatasetSchema (Maybe Text)
dsName = lens _dsName (\ s a -> s{_dsName = a})

-- | The date and time (in Unix time) that the schema was created.
dsCreationDateTime :: Lens' DatasetSchema (Maybe UTCTime)
dsCreationDateTime = lens _dsCreationDateTime (\ s a -> s{_dsCreationDateTime = a}) . mapping _Time

instance FromJSON DatasetSchema where
        parseJSON
          = withObject "DatasetSchema"
              (\ x ->
                 DatasetSchema' <$>
                   (x .:? "lastUpdatedDateTime") <*> (x .:? "schema")
                     <*> (x .:? "schemaArn")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime"))

instance Hashable DatasetSchema where

instance NFData DatasetSchema where
