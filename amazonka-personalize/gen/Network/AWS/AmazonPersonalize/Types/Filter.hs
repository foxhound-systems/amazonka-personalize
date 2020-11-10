{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.Filter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on a recommendation filter, including its ARN, status, and filter expression.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'{_fFailureReason ::
                      !(Maybe Text),
                      _fStatus :: !(Maybe Text),
                      _fFilterExpression :: !(Maybe (Sensitive Text)),
                      _fLastUpdatedDateTime :: !(Maybe POSIX),
                      _fName :: !(Maybe Text),
                      _fFilterARN :: !(Maybe Text),
                      _fCreationDateTime :: !(Maybe POSIX),
                      _fDatasetGroupARN :: !(Maybe Text)}
                deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFailureReason' - If the filter failed, the reason for its failure.
--
-- * 'fStatus' - The status of the filter.
--
-- * 'fFilterExpression' - Specifies the type of item interactions to filter out of recommendation results. The filter expression must follow the following format: @EXCLUDE itemId WHERE INTERACTIONS.event_type in ("EVENT_TYPE")@  Where "EVENT_TYPE" is the type of event to filter out. For more information, see <https://docs.aws.amazon.com/personalize/latest/dg/filters.html Using Filters with Amazon Personalize> .
--
-- * 'fLastUpdatedDateTime' - The time at which the filter was last updated.
--
-- * 'fName' - The name of the filter.
--
-- * 'fFilterARN' - The ARN of the filter.
--
-- * 'fCreationDateTime' - The time at which the filter was created.
--
-- * 'fDatasetGroupARN' - The ARN of the dataset group to which the filter belongs.
filter'
    :: Filter
filter'
  = Filter'{_fFailureReason = Nothing,
            _fStatus = Nothing, _fFilterExpression = Nothing,
            _fLastUpdatedDateTime = Nothing, _fName = Nothing,
            _fFilterARN = Nothing, _fCreationDateTime = Nothing,
            _fDatasetGroupARN = Nothing}

-- | If the filter failed, the reason for its failure.
fFailureReason :: Lens' Filter (Maybe Text)
fFailureReason = lens _fFailureReason (\ s a -> s{_fFailureReason = a})

-- | The status of the filter.
fStatus :: Lens' Filter (Maybe Text)
fStatus = lens _fStatus (\ s a -> s{_fStatus = a})

-- | Specifies the type of item interactions to filter out of recommendation results. The filter expression must follow the following format: @EXCLUDE itemId WHERE INTERACTIONS.event_type in ("EVENT_TYPE")@  Where "EVENT_TYPE" is the type of event to filter out. For more information, see <https://docs.aws.amazon.com/personalize/latest/dg/filters.html Using Filters with Amazon Personalize> .
fFilterExpression :: Lens' Filter (Maybe Text)
fFilterExpression = lens _fFilterExpression (\ s a -> s{_fFilterExpression = a}) . mapping _Sensitive

-- | The time at which the filter was last updated.
fLastUpdatedDateTime :: Lens' Filter (Maybe UTCTime)
fLastUpdatedDateTime = lens _fLastUpdatedDateTime (\ s a -> s{_fLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the filter.
fName :: Lens' Filter (Maybe Text)
fName = lens _fName (\ s a -> s{_fName = a})

-- | The ARN of the filter.
fFilterARN :: Lens' Filter (Maybe Text)
fFilterARN = lens _fFilterARN (\ s a -> s{_fFilterARN = a})

-- | The time at which the filter was created.
fCreationDateTime :: Lens' Filter (Maybe UTCTime)
fCreationDateTime = lens _fCreationDateTime (\ s a -> s{_fCreationDateTime = a}) . mapping _Time

-- | The ARN of the dataset group to which the filter belongs.
fDatasetGroupARN :: Lens' Filter (Maybe Text)
fDatasetGroupARN = lens _fDatasetGroupARN (\ s a -> s{_fDatasetGroupARN = a})

instance FromJSON Filter where
        parseJSON
          = withObject "Filter"
              (\ x ->
                 Filter' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "filterExpression")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "filterArn")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "datasetGroupArn"))

instance Hashable Filter where

instance NFData Filter where
