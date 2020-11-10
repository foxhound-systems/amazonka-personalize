{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.BatchInferenceJobSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.BatchInferenceJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A truncated version of the 'BatchInferenceJob' datatype. The 'ListBatchInferenceJobs' operation returns a list of batch inference job summaries.
--
--
--
-- /See:/ 'batchInferenceJobSummary' smart constructor.
data BatchInferenceJobSummary = BatchInferenceJobSummary'{_bijsFailureReason
                                                          :: !(Maybe Text),
                                                          _bijsStatus ::
                                                          !(Maybe Text),
                                                          _bijsJobName ::
                                                          !(Maybe Text),
                                                          _bijsLastUpdatedDateTime
                                                          :: !(Maybe POSIX),
                                                          _bijsBatchInferenceJobARN
                                                          :: !(Maybe Text),
                                                          _bijsCreationDateTime
                                                          :: !(Maybe POSIX),
                                                          _bijsSolutionVersionARN
                                                          :: !(Maybe Text)}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'BatchInferenceJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bijsFailureReason' - If the batch inference job failed, the reason for the failure.
--
-- * 'bijsStatus' - The status of the batch inference job. The status is one of the following values:     * PENDING     * IN PROGRESS     * ACTIVE     * CREATE FAILED
--
-- * 'bijsJobName' - The name of the batch inference job.
--
-- * 'bijsLastUpdatedDateTime' - The time at which the batch inference job was last updated.
--
-- * 'bijsBatchInferenceJobARN' - The Amazon Resource Name (ARN) of the batch inference job.
--
-- * 'bijsCreationDateTime' - The time at which the batch inference job was created.
--
-- * 'bijsSolutionVersionARN' - The ARN of the solution version used by the batch inference job.
batchInferenceJobSummary
    :: BatchInferenceJobSummary
batchInferenceJobSummary
  = BatchInferenceJobSummary'{_bijsFailureReason =
                                Nothing,
                              _bijsStatus = Nothing, _bijsJobName = Nothing,
                              _bijsLastUpdatedDateTime = Nothing,
                              _bijsBatchInferenceJobARN = Nothing,
                              _bijsCreationDateTime = Nothing,
                              _bijsSolutionVersionARN = Nothing}

-- | If the batch inference job failed, the reason for the failure.
bijsFailureReason :: Lens' BatchInferenceJobSummary (Maybe Text)
bijsFailureReason = lens _bijsFailureReason (\ s a -> s{_bijsFailureReason = a})

-- | The status of the batch inference job. The status is one of the following values:     * PENDING     * IN PROGRESS     * ACTIVE     * CREATE FAILED
bijsStatus :: Lens' BatchInferenceJobSummary (Maybe Text)
bijsStatus = lens _bijsStatus (\ s a -> s{_bijsStatus = a})

-- | The name of the batch inference job.
bijsJobName :: Lens' BatchInferenceJobSummary (Maybe Text)
bijsJobName = lens _bijsJobName (\ s a -> s{_bijsJobName = a})

-- | The time at which the batch inference job was last updated.
bijsLastUpdatedDateTime :: Lens' BatchInferenceJobSummary (Maybe UTCTime)
bijsLastUpdatedDateTime = lens _bijsLastUpdatedDateTime (\ s a -> s{_bijsLastUpdatedDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the batch inference job.
bijsBatchInferenceJobARN :: Lens' BatchInferenceJobSummary (Maybe Text)
bijsBatchInferenceJobARN = lens _bijsBatchInferenceJobARN (\ s a -> s{_bijsBatchInferenceJobARN = a})

-- | The time at which the batch inference job was created.
bijsCreationDateTime :: Lens' BatchInferenceJobSummary (Maybe UTCTime)
bijsCreationDateTime = lens _bijsCreationDateTime (\ s a -> s{_bijsCreationDateTime = a}) . mapping _Time

-- | The ARN of the solution version used by the batch inference job.
bijsSolutionVersionARN :: Lens' BatchInferenceJobSummary (Maybe Text)
bijsSolutionVersionARN = lens _bijsSolutionVersionARN (\ s a -> s{_bijsSolutionVersionARN = a})

instance FromJSON BatchInferenceJobSummary where
        parseJSON
          = withObject "BatchInferenceJobSummary"
              (\ x ->
                 BatchInferenceJobSummary' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "jobName")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "batchInferenceJobArn")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "solutionVersionArn"))

instance Hashable BatchInferenceJobSummary where

instance NFData BatchInferenceJobSummary where
