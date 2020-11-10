{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.FilterSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.FilterSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A short summary of a filter's attributes.
--
--
--
-- /See:/ 'filterSummary' smart constructor.
data FilterSummary = FilterSummary'{_fsFailureReason
                                    :: !(Maybe Text),
                                    _fsStatus :: !(Maybe Text),
                                    _fsLastUpdatedDateTime :: !(Maybe POSIX),
                                    _fsName :: !(Maybe Text),
                                    _fsFilterARN :: !(Maybe Text),
                                    _fsCreationDateTime :: !(Maybe POSIX),
                                    _fsDatasetGroupARN :: !(Maybe Text)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FilterSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsFailureReason' - If the filter failed, the reason for the failure.
--
-- * 'fsStatus' - The status of the filter.
--
-- * 'fsLastUpdatedDateTime' - The time at which the filter was last updated.
--
-- * 'fsName' - The name of the filter.
--
-- * 'fsFilterARN' - The ARN of the filter.
--
-- * 'fsCreationDateTime' - The time at which the filter was created.
--
-- * 'fsDatasetGroupARN' - The ARN of the dataset group to which the filter belongs.
filterSummary
    :: FilterSummary
filterSummary
  = FilterSummary'{_fsFailureReason = Nothing,
                   _fsStatus = Nothing,
                   _fsLastUpdatedDateTime = Nothing, _fsName = Nothing,
                   _fsFilterARN = Nothing,
                   _fsCreationDateTime = Nothing,
                   _fsDatasetGroupARN = Nothing}

-- | If the filter failed, the reason for the failure.
fsFailureReason :: Lens' FilterSummary (Maybe Text)
fsFailureReason = lens _fsFailureReason (\ s a -> s{_fsFailureReason = a})

-- | The status of the filter.
fsStatus :: Lens' FilterSummary (Maybe Text)
fsStatus = lens _fsStatus (\ s a -> s{_fsStatus = a})

-- | The time at which the filter was last updated.
fsLastUpdatedDateTime :: Lens' FilterSummary (Maybe UTCTime)
fsLastUpdatedDateTime = lens _fsLastUpdatedDateTime (\ s a -> s{_fsLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the filter.
fsName :: Lens' FilterSummary (Maybe Text)
fsName = lens _fsName (\ s a -> s{_fsName = a})

-- | The ARN of the filter.
fsFilterARN :: Lens' FilterSummary (Maybe Text)
fsFilterARN = lens _fsFilterARN (\ s a -> s{_fsFilterARN = a})

-- | The time at which the filter was created.
fsCreationDateTime :: Lens' FilterSummary (Maybe UTCTime)
fsCreationDateTime = lens _fsCreationDateTime (\ s a -> s{_fsCreationDateTime = a}) . mapping _Time

-- | The ARN of the dataset group to which the filter belongs.
fsDatasetGroupARN :: Lens' FilterSummary (Maybe Text)
fsDatasetGroupARN = lens _fsDatasetGroupARN (\ s a -> s{_fsDatasetGroupARN = a})

instance FromJSON FilterSummary where
        parseJSON
          = withObject "FilterSummary"
              (\ x ->
                 FilterSummary' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "filterArn")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn"))

instance Hashable FilterSummary where

instance NFData FilterSummary where
