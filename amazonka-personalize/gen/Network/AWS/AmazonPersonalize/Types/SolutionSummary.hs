{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.SolutionSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.SolutionSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a solution. For a complete listing, call the 'DescribeSolution' API.
--
--
--
-- /See:/ 'solutionSummary' smart constructor.
data SolutionSummary = SolutionSummary'{_ssSolutionARN
                                        :: !(Maybe Text),
                                        _ssStatus :: !(Maybe Text),
                                        _ssLastUpdatedDateTime ::
                                        !(Maybe POSIX),
                                        _ssName :: !(Maybe Text),
                                        _ssCreationDateTime :: !(Maybe POSIX)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SolutionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSolutionARN' - The Amazon Resource Name (ARN) of the solution.
--
-- * 'ssStatus' - The status of the solution. A solution can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
--
-- * 'ssLastUpdatedDateTime' - The date and time (in Unix time) that the solution was last updated.
--
-- * 'ssName' - The name of the solution.
--
-- * 'ssCreationDateTime' - The date and time (in Unix time) that the solution was created.
solutionSummary
    :: SolutionSummary
solutionSummary
  = SolutionSummary'{_ssSolutionARN = Nothing,
                     _ssStatus = Nothing,
                     _ssLastUpdatedDateTime = Nothing, _ssName = Nothing,
                     _ssCreationDateTime = Nothing}

-- | The Amazon Resource Name (ARN) of the solution.
ssSolutionARN :: Lens' SolutionSummary (Maybe Text)
ssSolutionARN = lens _ssSolutionARN (\ s a -> s{_ssSolutionARN = a})

-- | The status of the solution. A solution can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED     * DELETE PENDING > DELETE IN_PROGRESS
ssStatus :: Lens' SolutionSummary (Maybe Text)
ssStatus = lens _ssStatus (\ s a -> s{_ssStatus = a})

-- | The date and time (in Unix time) that the solution was last updated.
ssLastUpdatedDateTime :: Lens' SolutionSummary (Maybe UTCTime)
ssLastUpdatedDateTime = lens _ssLastUpdatedDateTime (\ s a -> s{_ssLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the solution.
ssName :: Lens' SolutionSummary (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a})

-- | The date and time (in Unix time) that the solution was created.
ssCreationDateTime :: Lens' SolutionSummary (Maybe UTCTime)
ssCreationDateTime = lens _ssCreationDateTime (\ s a -> s{_ssCreationDateTime = a}) . mapping _Time

instance FromJSON SolutionSummary where
        parseJSON
          = withObject "SolutionSummary"
              (\ x ->
                 SolutionSummary' <$>
                   (x .:? "solutionArn") <*> (x .:? "status") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime"))

instance Hashable SolutionSummary where

instance NFData SolutionSummary where
