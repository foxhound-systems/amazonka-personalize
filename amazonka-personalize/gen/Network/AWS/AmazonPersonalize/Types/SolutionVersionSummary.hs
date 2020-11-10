{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.SolutionVersionSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.SolutionVersionSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a solution version. For a complete listing, call the 'DescribeSolutionVersion' API.
--
--
--
-- /See:/ 'solutionVersionSummary' smart constructor.
data SolutionVersionSummary = SolutionVersionSummary'{_svsFailureReason
                                                      :: !(Maybe Text),
                                                      _svsStatus ::
                                                      !(Maybe Text),
                                                      _svsLastUpdatedDateTime ::
                                                      !(Maybe POSIX),
                                                      _svsCreationDateTime ::
                                                      !(Maybe POSIX),
                                                      _svsSolutionVersionARN ::
                                                      !(Maybe Text)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'SolutionVersionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svsFailureReason' - If a solution version fails, the reason behind the failure.
--
-- * 'svsStatus' - The status of the solution version. A solution version can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- * 'svsLastUpdatedDateTime' - The date and time (in Unix time) that the solution version was last updated.
--
-- * 'svsCreationDateTime' - The date and time (in Unix time) that this version of a solution was created.
--
-- * 'svsSolutionVersionARN' - The Amazon Resource Name (ARN) of the solution version.
solutionVersionSummary
    :: SolutionVersionSummary
solutionVersionSummary
  = SolutionVersionSummary'{_svsFailureReason =
                              Nothing,
                            _svsStatus = Nothing,
                            _svsLastUpdatedDateTime = Nothing,
                            _svsCreationDateTime = Nothing,
                            _svsSolutionVersionARN = Nothing}

-- | If a solution version fails, the reason behind the failure.
svsFailureReason :: Lens' SolutionVersionSummary (Maybe Text)
svsFailureReason = lens _svsFailureReason (\ s a -> s{_svsFailureReason = a})

-- | The status of the solution version. A solution version can be in one of the following states:     * CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
svsStatus :: Lens' SolutionVersionSummary (Maybe Text)
svsStatus = lens _svsStatus (\ s a -> s{_svsStatus = a})

-- | The date and time (in Unix time) that the solution version was last updated.
svsLastUpdatedDateTime :: Lens' SolutionVersionSummary (Maybe UTCTime)
svsLastUpdatedDateTime = lens _svsLastUpdatedDateTime (\ s a -> s{_svsLastUpdatedDateTime = a}) . mapping _Time

-- | The date and time (in Unix time) that this version of a solution was created.
svsCreationDateTime :: Lens' SolutionVersionSummary (Maybe UTCTime)
svsCreationDateTime = lens _svsCreationDateTime (\ s a -> s{_svsCreationDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the solution version.
svsSolutionVersionARN :: Lens' SolutionVersionSummary (Maybe Text)
svsSolutionVersionARN = lens _svsSolutionVersionARN (\ s a -> s{_svsSolutionVersionARN = a})

instance FromJSON SolutionVersionSummary where
        parseJSON
          = withObject "SolutionVersionSummary"
              (\ x ->
                 SolutionVersionSummary' <$>
                   (x .:? "failureReason") <*> (x .:? "status") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "solutionVersionArn"))

instance Hashable SolutionVersionSummary where

instance NFData SolutionVersionSummary where
