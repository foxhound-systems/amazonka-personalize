{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmazonPersonalize.Types.Recipe
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AmazonPersonalize.Types.Recipe where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a recipe. Each recipe provides an algorithm that Amazon Personalize uses in model training when you use the 'CreateSolution' operation. 
--
--
--
-- /See:/ 'recipe' smart constructor.
data Recipe = Recipe'{_rStatus :: !(Maybe Text),
                      _rAlgorithmARN :: !(Maybe Text),
                      _rRecipeARN :: !(Maybe Text),
                      _rFeatureTransformationARN :: !(Maybe Text),
                      _rLastUpdatedDateTime :: !(Maybe POSIX),
                      _rName :: !(Maybe Text),
                      _rCreationDateTime :: !(Maybe POSIX),
                      _rRecipeType :: !(Maybe Text),
                      _rDescription :: !(Maybe Text)}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Recipe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rStatus' - The status of the recipe.
--
-- * 'rAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm that Amazon Personalize uses to train the model.
--
-- * 'rRecipeARN' - The Amazon Resource Name (ARN) of the recipe.
--
-- * 'rFeatureTransformationARN' - The ARN of the FeatureTransformation object.
--
-- * 'rLastUpdatedDateTime' - The date and time (in Unix format) that the recipe was last updated.
--
-- * 'rName' - The name of the recipe.
--
-- * 'rCreationDateTime' - The date and time (in Unix format) that the recipe was created.
--
-- * 'rRecipeType' - One of the following values:     * PERSONALIZED_RANKING     * RELATED_ITEMS     * USER_PERSONALIZATION
--
-- * 'rDescription' - The description of the recipe.
recipe
    :: Recipe
recipe
  = Recipe'{_rStatus = Nothing,
            _rAlgorithmARN = Nothing, _rRecipeARN = Nothing,
            _rFeatureTransformationARN = Nothing,
            _rLastUpdatedDateTime = Nothing, _rName = Nothing,
            _rCreationDateTime = Nothing, _rRecipeType = Nothing,
            _rDescription = Nothing}

-- | The status of the recipe.
rStatus :: Lens' Recipe (Maybe Text)
rStatus = lens _rStatus (\ s a -> s{_rStatus = a})

-- | The Amazon Resource Name (ARN) of the algorithm that Amazon Personalize uses to train the model.
rAlgorithmARN :: Lens' Recipe (Maybe Text)
rAlgorithmARN = lens _rAlgorithmARN (\ s a -> s{_rAlgorithmARN = a})

-- | The Amazon Resource Name (ARN) of the recipe.
rRecipeARN :: Lens' Recipe (Maybe Text)
rRecipeARN = lens _rRecipeARN (\ s a -> s{_rRecipeARN = a})

-- | The ARN of the FeatureTransformation object.
rFeatureTransformationARN :: Lens' Recipe (Maybe Text)
rFeatureTransformationARN = lens _rFeatureTransformationARN (\ s a -> s{_rFeatureTransformationARN = a})

-- | The date and time (in Unix format) that the recipe was last updated.
rLastUpdatedDateTime :: Lens' Recipe (Maybe UTCTime)
rLastUpdatedDateTime = lens _rLastUpdatedDateTime (\ s a -> s{_rLastUpdatedDateTime = a}) . mapping _Time

-- | The name of the recipe.
rName :: Lens' Recipe (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | The date and time (in Unix format) that the recipe was created.
rCreationDateTime :: Lens' Recipe (Maybe UTCTime)
rCreationDateTime = lens _rCreationDateTime (\ s a -> s{_rCreationDateTime = a}) . mapping _Time

-- | One of the following values:     * PERSONALIZED_RANKING     * RELATED_ITEMS     * USER_PERSONALIZATION
rRecipeType :: Lens' Recipe (Maybe Text)
rRecipeType = lens _rRecipeType (\ s a -> s{_rRecipeType = a})

-- | The description of the recipe.
rDescription :: Lens' Recipe (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a})

instance FromJSON Recipe where
        parseJSON
          = withObject "Recipe"
              (\ x ->
                 Recipe' <$>
                   (x .:? "status") <*> (x .:? "algorithmArn") <*>
                     (x .:? "recipeArn")
                     <*> (x .:? "featureTransformationArn")
                     <*> (x .:? "lastUpdatedDateTime")
                     <*> (x .:? "name")
                     <*> (x .:? "creationDateTime")
                     <*> (x .:? "recipeType")
                     <*> (x .:? "description"))

instance Hashable Recipe where

instance NFData Recipe where
