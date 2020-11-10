{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.RecipeSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.RecipeSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a summary of the properties of a recipe. For a complete listing, call the 'DescribeRecipe' API.
--
--
--
-- /See:/ 'recipeSummary' smart constructor.
data RecipeSummary = RecipeSummary'{_rsStatus ::
                                    !(Maybe Text),
                                    _rsRecipeARN :: !(Maybe Text),
                                    _rsLastUpdatedDateTime :: !(Maybe POSIX),
                                    _rsName :: !(Maybe Text),
                                    _rsCreationDateTime :: !(Maybe POSIX)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecipeSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsStatus' - The status of the recipe.
--
-- * 'rsRecipeARN' - The Amazon Resource Name (ARN) of the recipe.
--
-- * 'rsLastUpdatedDateTime' - The date and time (in Unix time) that the recipe was last updated.
--
-- * 'rsName' - The name of the recipe.
--
-- * 'rsCreationDateTime' - The date and time (in Unix time) that the recipe was created.
recipeSummary
    :: RecipeSummary
recipeSummary
  = RecipeSummary'{_rsStatus = Nothing,
                   _rsRecipeARN = Nothing,
                   _rsLastUpdatedDateTime = Nothing, _rsName = Nothing,
                   _rsCreationDateTime = Nothing}

-- | The status of the recipe.
rsStatus :: Lens' RecipeSummary (Maybe Text)
rsStatus = lens _rsStatus (\ s a -> s{_rsStatus = a})

-- | The Amazon Resource Name (ARN) of the recipe.
rsRecipeARN :: Lens' RecipeSummary (Maybe Text)
rsRecipeARN = lens _rsRecipeARN (\ s a -> s{_rsRecipeARN = a})

-- | The date and time (in Unix time) that the recipe was last updated.
rsLastUpdatedDateTime :: Lens' RecipeSummary (Maybe UTCTime)
rsLastUpdatedDateTime = lens _rsLastUpdatedDateTime (\ s a -> s{_rsLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the recipe.
rsName :: Lens' RecipeSummary (Maybe Text)
rsName = lens _rsName (\ s a -> s{_rsName = a})

-- | The date and time (in Unix time) that the recipe was created.
rsCreationDateTime :: Lens' RecipeSummary (Maybe UTCTime)
rsCreationDateTime = lens _rsCreationDateTime (\ s a -> s{_rsCreationDateTime = a}) . mapping _Time

instance FromJSON RecipeSummary where
        parseJSON
          = withObject "RecipeSummary"
              (\ x ->
                 RecipeSummary' <$>
                   (x .:? "status") <*> (x .:? "recipeArn") <*>
                     (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime"))

instance Hashable RecipeSummary where

instance NFData RecipeSummary where
