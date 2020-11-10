{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.CreateBatchInferenceJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a batch inference job. The operation can handle up to 50 million records and the input file must be in JSON format. For more information, see 'recommendations-batch' .
--
--
module Network.AWS.AmazonPersonalize.CreateBatchInferenceJob
    (
    -- * Creating a Request
      createBatchInferenceJob
    , CreateBatchInferenceJob
    -- * Request Lenses
    , cbijNumResults
    , cbijBatchInferenceJobConfig
    , cbijFilterARN
    , cbijJobName
    , cbijSolutionVersionARN
    , cbijJobInput
    , cbijJobOutput
    , cbijRoleARN

    -- * Destructuring the Response
    , createBatchInferenceJobResponse
    , CreateBatchInferenceJobResponse
    -- * Response Lenses
    , cbijrsBatchInferenceJobARN
    , cbijrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBatchInferenceJob' smart constructor.
data CreateBatchInferenceJob = CreateBatchInferenceJob'{_cbijNumResults
                                                        :: !(Maybe Int),
                                                        _cbijBatchInferenceJobConfig
                                                        ::
                                                        !(Maybe
                                                            BatchInferenceJobConfig),
                                                        _cbijFilterARN ::
                                                        !(Maybe Text),
                                                        _cbijJobName :: !Text,
                                                        _cbijSolutionVersionARN
                                                        :: !Text,
                                                        _cbijJobInput ::
                                                        !BatchInferenceJobInput,
                                                        _cbijJobOutput ::
                                                        !BatchInferenceJobOutput,
                                                        _cbijRoleARN :: !Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'CreateBatchInferenceJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbijNumResults' - The number of recommendations to retreive.
--
-- * 'cbijBatchInferenceJobConfig' - The configuration details of a batch inference job.
--
-- * 'cbijFilterARN' - The ARN of the filter to apply to the batch inference job. For more information on using filters, see Using Filters with Amazon Personalize.
--
-- * 'cbijJobName' - The name of the batch inference job to create.
--
-- * 'cbijSolutionVersionARN' - The Amazon Resource Name (ARN) of the solution version that will be used to generate the batch inference recommendations.
--
-- * 'cbijJobInput' - The Amazon S3 path that leads to the input file to base your recommendations on. The input material must be in JSON format.
--
-- * 'cbijJobOutput' - The path to the Amazon S3 bucket where the job's output will be stored.
--
-- * 'cbijRoleARN' - The ARN of the Amazon Identity and Access Management role that has permissions to read and write to your input and out Amazon S3 buckets respectively.
createBatchInferenceJob
    :: Text -- ^ 'cbijJobName'
    -> Text -- ^ 'cbijSolutionVersionARN'
    -> BatchInferenceJobInput -- ^ 'cbijJobInput'
    -> BatchInferenceJobOutput -- ^ 'cbijJobOutput'
    -> Text -- ^ 'cbijRoleARN'
    -> CreateBatchInferenceJob
createBatchInferenceJob pJobName_
  pSolutionVersionARN_ pJobInput_ pJobOutput_ pRoleARN_
  = CreateBatchInferenceJob'{_cbijNumResults = Nothing,
                             _cbijBatchInferenceJobConfig = Nothing,
                             _cbijFilterARN = Nothing, _cbijJobName = pJobName_,
                             _cbijSolutionVersionARN = pSolutionVersionARN_,
                             _cbijJobInput = pJobInput_,
                             _cbijJobOutput = pJobOutput_,
                             _cbijRoleARN = pRoleARN_}

-- | The number of recommendations to retreive.
cbijNumResults :: Lens' CreateBatchInferenceJob (Maybe Int)
cbijNumResults = lens _cbijNumResults (\ s a -> s{_cbijNumResults = a})

-- | The configuration details of a batch inference job.
cbijBatchInferenceJobConfig :: Lens' CreateBatchInferenceJob (Maybe BatchInferenceJobConfig)
cbijBatchInferenceJobConfig = lens _cbijBatchInferenceJobConfig (\ s a -> s{_cbijBatchInferenceJobConfig = a})

-- | The ARN of the filter to apply to the batch inference job. For more information on using filters, see Using Filters with Amazon Personalize.
cbijFilterARN :: Lens' CreateBatchInferenceJob (Maybe Text)
cbijFilterARN = lens _cbijFilterARN (\ s a -> s{_cbijFilterARN = a})

-- | The name of the batch inference job to create.
cbijJobName :: Lens' CreateBatchInferenceJob Text
cbijJobName = lens _cbijJobName (\ s a -> s{_cbijJobName = a})

-- | The Amazon Resource Name (ARN) of the solution version that will be used to generate the batch inference recommendations.
cbijSolutionVersionARN :: Lens' CreateBatchInferenceJob Text
cbijSolutionVersionARN = lens _cbijSolutionVersionARN (\ s a -> s{_cbijSolutionVersionARN = a})

-- | The Amazon S3 path that leads to the input file to base your recommendations on. The input material must be in JSON format.
cbijJobInput :: Lens' CreateBatchInferenceJob BatchInferenceJobInput
cbijJobInput = lens _cbijJobInput (\ s a -> s{_cbijJobInput = a})

-- | The path to the Amazon S3 bucket where the job's output will be stored.
cbijJobOutput :: Lens' CreateBatchInferenceJob BatchInferenceJobOutput
cbijJobOutput = lens _cbijJobOutput (\ s a -> s{_cbijJobOutput = a})

-- | The ARN of the Amazon Identity and Access Management role that has permissions to read and write to your input and out Amazon S3 buckets respectively.
cbijRoleARN :: Lens' CreateBatchInferenceJob Text
cbijRoleARN = lens _cbijRoleARN (\ s a -> s{_cbijRoleARN = a})

instance AWSRequest CreateBatchInferenceJob where
        type Rs CreateBatchInferenceJob =
             CreateBatchInferenceJobResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateBatchInferenceJobResponse' <$>
                   (x .?> "batchInferenceJobArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateBatchInferenceJob where

instance NFData CreateBatchInferenceJob where

instance ToHeaders CreateBatchInferenceJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateBatchInferenceJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBatchInferenceJob where
        toJSON CreateBatchInferenceJob'{..}
          = object
              (catMaybes
                 [("numResults" .=) <$> _cbijNumResults,
                  ("batchInferenceJobConfig" .=) <$>
                    _cbijBatchInferenceJobConfig,
                  ("filterArn" .=) <$> _cbijFilterARN,
                  Just ("jobName" .= _cbijJobName),
                  Just
                    ("solutionVersionArn" .= _cbijSolutionVersionARN),
                  Just ("jobInput" .= _cbijJobInput),
                  Just ("jobOutput" .= _cbijJobOutput),
                  Just ("roleArn" .= _cbijRoleARN)])

instance ToPath CreateBatchInferenceJob where
        toPath = const "/"

instance ToQuery CreateBatchInferenceJob where
        toQuery = const mempty

-- | /See:/ 'createBatchInferenceJobResponse' smart constructor.
data CreateBatchInferenceJobResponse = CreateBatchInferenceJobResponse'{_cbijrsBatchInferenceJobARN
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _cbijrsResponseStatus
                                                                        :: !Int}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'CreateBatchInferenceJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbijrsBatchInferenceJobARN' - The ARN of the batch inference job.
--
-- * 'cbijrsResponseStatus' - -- | The response status code.
createBatchInferenceJobResponse
    :: Int -- ^ 'cbijrsResponseStatus'
    -> CreateBatchInferenceJobResponse
createBatchInferenceJobResponse pResponseStatus_
  = CreateBatchInferenceJobResponse'{_cbijrsBatchInferenceJobARN
                                       = Nothing,
                                     _cbijrsResponseStatus = pResponseStatus_}

-- | The ARN of the batch inference job.
cbijrsBatchInferenceJobARN :: Lens' CreateBatchInferenceJobResponse (Maybe Text)
cbijrsBatchInferenceJobARN = lens _cbijrsBatchInferenceJobARN (\ s a -> s{_cbijrsBatchInferenceJobARN = a})

-- | -- | The response status code.
cbijrsResponseStatus :: Lens' CreateBatchInferenceJobResponse Int
cbijrsResponseStatus = lens _cbijrsResponseStatus (\ s a -> s{_cbijrsResponseStatus = a})

instance NFData CreateBatchInferenceJobResponse where
