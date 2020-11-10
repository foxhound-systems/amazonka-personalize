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
-- Module      : Network.AWS.AmazonPersonalize.DeleteSolution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of a solution and the @Solution@ object itself. Before deleting a solution, you must delete all campaigns based on the solution. To determine what campaigns are using the solution, call 'ListCampaigns' and supply the Amazon Resource Name (ARN) of the solution. You can't delete a solution if an associated @SolutionVersion@ is in the CREATE PENDING or IN PROGRESS state. For more information on solutions, see 'CreateSolution' .
--
--
module Network.AWS.AmazonPersonalize.DeleteSolution
    (
    -- * Creating a Request
      deleteSolution
    , DeleteSolution
    -- * Request Lenses
    , dSolutionARN

    -- * Destructuring the Response
    , deleteSolutionResponse
    , DeleteSolutionResponse
    ) where

import Network.AWS.AmazonPersonalize.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSolution' smart constructor.
newtype DeleteSolution = DeleteSolution'{_dSolutionARN
                                         :: Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSolution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSolutionARN' - The ARN of the solution to delete.
deleteSolution
    :: Text -- ^ 'dSolutionARN'
    -> DeleteSolution
deleteSolution pSolutionARN_
  = DeleteSolution'{_dSolutionARN = pSolutionARN_}

-- | The ARN of the solution to delete.
dSolutionARN :: Lens' DeleteSolution Text
dSolutionARN = lens _dSolutionARN (\ s a -> s{_dSolutionARN = a})

instance AWSRequest DeleteSolution where
        type Rs DeleteSolution = DeleteSolutionResponse
        request = postJSON amazonPersonalize
        response = receiveNull DeleteSolutionResponse'

instance Hashable DeleteSolution where

instance NFData DeleteSolution where

instance ToHeaders DeleteSolution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonPersonalize.DeleteSolution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSolution where
        toJSON DeleteSolution'{..}
          = object
              (catMaybes [Just ("solutionArn" .= _dSolutionARN)])

instance ToPath DeleteSolution where
        toPath = const "/"

instance ToQuery DeleteSolution where
        toQuery = const mempty

-- | /See:/ 'deleteSolutionResponse' smart constructor.
data DeleteSolutionResponse = DeleteSolutionResponse'
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DeleteSolutionResponse' with the minimum fields required to make a request.
--
deleteSolutionResponse
    :: DeleteSolutionResponse
deleteSolutionResponse = DeleteSolutionResponse'

instance NFData DeleteSolutionResponse where
