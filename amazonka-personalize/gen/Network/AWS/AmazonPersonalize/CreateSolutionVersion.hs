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
-- Module      : Network.AWS.AmazonPersonalize.CreateSolutionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Trains or retrains an active solution. A solution is created using the 'CreateSolution' operation and must be in the ACTIVE state before calling @CreateSolutionVersion@ . A new version of the solution is created every time you call this operation.
--
--
-- __Status__ 
--
-- A solution version can be in one of the following states:
--
--     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
--
--
-- To get the status of the version, call 'DescribeSolutionVersion' . Wait until the status shows as ACTIVE before calling @CreateCampaign@ .
--
-- If the status shows as CREATE FAILED, the response includes a @failureReason@ key, which describes why the job failed.
--
-- __Related APIs__ 
--
--     * 'ListSolutionVersions' 
--
--     * 'DescribeSolutionVersion' 
--
--
--
--     * 'ListSolutions' 
--
--     * 'CreateSolution' 
--
--     * 'DescribeSolution' 
--
--     * 'DeleteSolution' 
--
--
--
module Network.AWS.AmazonPersonalize.CreateSolutionVersion
    (
    -- * Creating a Request
      createSolutionVersion
    , CreateSolutionVersion
    -- * Request Lenses
    , csvTrainingMode
    , csvSolutionARN

    -- * Destructuring the Response
    , createSolutionVersionResponse
    , CreateSolutionVersionResponse
    -- * Response Lenses
    , csvrsSolutionVersionARN
    , csvrsResponseStatus
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSolutionVersion' smart constructor.
data CreateSolutionVersion = CreateSolutionVersion'{_csvTrainingMode
                                                    :: !(Maybe TrainingMode),
                                                    _csvSolutionARN :: !Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'CreateSolutionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csvTrainingMode' - The scope of training to be performed when creating the solution version. The @FULL@ option trains the solution version based on the entirety of the input solution's training data, while the @UPDATE@ option processes only the data that has changed in comparison to the input solution. Choose @UPDATE@ when you want to incrementally update your solution version instead of creating an entirely new one. /Important:/ The @UPDATE@ option can only be used when you already have an active solution version created from the input solution using the @FULL@ option and the input solution was trained with the 'native-recipe-hrnn-coldstart' recipe.
--
-- * 'csvSolutionARN' - The Amazon Resource Name (ARN) of the solution containing the training configuration information.
createSolutionVersion
    :: Text -- ^ 'csvSolutionARN'
    -> CreateSolutionVersion
createSolutionVersion pSolutionARN_
  = CreateSolutionVersion'{_csvTrainingMode = Nothing,
                           _csvSolutionARN = pSolutionARN_}

-- | The scope of training to be performed when creating the solution version. The @FULL@ option trains the solution version based on the entirety of the input solution's training data, while the @UPDATE@ option processes only the data that has changed in comparison to the input solution. Choose @UPDATE@ when you want to incrementally update your solution version instead of creating an entirely new one. /Important:/ The @UPDATE@ option can only be used when you already have an active solution version created from the input solution using the @FULL@ option and the input solution was trained with the 'native-recipe-hrnn-coldstart' recipe.
csvTrainingMode :: Lens' CreateSolutionVersion (Maybe TrainingMode)
csvTrainingMode = lens _csvTrainingMode (\ s a -> s{_csvTrainingMode = a})

-- | The Amazon Resource Name (ARN) of the solution containing the training configuration information.
csvSolutionARN :: Lens' CreateSolutionVersion Text
csvSolutionARN = lens _csvSolutionARN (\ s a -> s{_csvSolutionARN = a})

instance AWSRequest CreateSolutionVersion where
        type Rs CreateSolutionVersion =
             CreateSolutionVersionResponse
        request = postJSON amazonPersonalize
        response
          = receiveJSON
              (\ s h x ->
                 CreateSolutionVersionResponse' <$>
                   (x .?> "solutionVersionArn") <*> (pure (fromEnum s)))

instance Hashable CreateSolutionVersion where

instance NFData CreateSolutionVersion where

instance ToHeaders CreateSolutionVersion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.CreateSolutionVersion" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSolutionVersion where
        toJSON CreateSolutionVersion'{..}
          = object
              (catMaybes
                 [("trainingMode" .=) <$> _csvTrainingMode,
                  Just ("solutionArn" .= _csvSolutionARN)])

instance ToPath CreateSolutionVersion where
        toPath = const "/"

instance ToQuery CreateSolutionVersion where
        toQuery = const mempty

-- | /See:/ 'createSolutionVersionResponse' smart constructor.
data CreateSolutionVersionResponse = CreateSolutionVersionResponse'{_csvrsSolutionVersionARN
                                                                    ::
                                                                    !(Maybe
                                                                        Text),
                                                                    _csvrsResponseStatus
                                                                    :: !Int}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'CreateSolutionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csvrsSolutionVersionARN' - The ARN of the new solution version.
--
-- * 'csvrsResponseStatus' - -- | The response status code.
createSolutionVersionResponse
    :: Int -- ^ 'csvrsResponseStatus'
    -> CreateSolutionVersionResponse
createSolutionVersionResponse pResponseStatus_
  = CreateSolutionVersionResponse'{_csvrsSolutionVersionARN
                                     = Nothing,
                                   _csvrsResponseStatus = pResponseStatus_}

-- | The ARN of the new solution version.
csvrsSolutionVersionARN :: Lens' CreateSolutionVersionResponse (Maybe Text)
csvrsSolutionVersionARN = lens _csvrsSolutionVersionARN (\ s a -> s{_csvrsSolutionVersionARN = a})

-- | -- | The response status code.
csvrsResponseStatus :: Lens' CreateSolutionVersionResponse Int
csvrsResponseStatus = lens _csvrsResponseStatus (\ s a -> s{_csvrsResponseStatus = a})

instance NFData CreateSolutionVersionResponse where
