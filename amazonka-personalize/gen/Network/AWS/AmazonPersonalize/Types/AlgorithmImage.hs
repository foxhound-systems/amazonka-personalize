{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.AlgorithmImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.AlgorithmImage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an algorithm image.
--
--
--
-- /See:/ 'algorithmImage' smart constructor.
data AlgorithmImage = AlgorithmImage'{_aiName ::
                                      !(Maybe Text),
                                      _aiDockerURI :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlgorithmImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiName' - The name of the algorithm image.
--
-- * 'aiDockerURI' - The URI of the Docker container for the algorithm image.
algorithmImage
    :: Text -- ^ 'aiDockerURI'
    -> AlgorithmImage
algorithmImage pDockerURI_
  = AlgorithmImage'{_aiName = Nothing,
                    _aiDockerURI = pDockerURI_}

-- | The name of the algorithm image.
aiName :: Lens' AlgorithmImage (Maybe Text)
aiName = lens _aiName (\ s a -> s{_aiName = a})

-- | The URI of the Docker container for the algorithm image.
aiDockerURI :: Lens' AlgorithmImage Text
aiDockerURI = lens _aiDockerURI (\ s a -> s{_aiDockerURI = a})

instance FromJSON AlgorithmImage where
        parseJSON
          = withObject "AlgorithmImage"
              (\ x ->
                 AlgorithmImage' <$>
                   (x .:? "name") <*> (x .: "dockerURI"))

instance Hashable AlgorithmImage where

instance NFData AlgorithmImage where
