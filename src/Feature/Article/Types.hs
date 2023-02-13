{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DeriveGeneric #-}

module Feature.Article.Types where

import ClassyPrelude
import Feature.User.Types

import Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson.Types (FromJSON, ToJSON)

type Slug = Text

type Tag = Text

data ArticleFilter = ArticleFilter
  { tag :: Maybe Text
  , author :: Maybe Text
  , favoritedBy :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ArticleFilter
instance FromJSON ArticleFilter


data Article = Article
  { slug :: Slug
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Tag]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  } deriving (Eq, Show, Generic)

instance ToJSON Article
instance FromJSON Article

data CreateArticle = CreateArticle
  { title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Tag]
  } deriving (Eq, Show, Generic)

instance ToJSON CreateArticle
instance FromJSON CreateArticle

data UpdateArticle = UpdateArticle
  { title :: Maybe Text
  , description :: Maybe Text
  , body :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON UpdateArticle
instance FromJSON UpdateArticle

data ArticleError
  = ArticleErrorNotFound Slug
  | ArticleErrorNotAllowed Slug
  deriving (Eq, Show, Generic)

instance ToJSON ArticleError
instance FromJSON ArticleError

newtype ArticleWrapper a = ArticleWrapper { article :: a } deriving (Eq, Show, Generic) 
instance (ToJSON a) => ToJSON (ArticleWrapper a)
instance (FromJSON a) => FromJSON (ArticleWrapper a)

data ArticlesWrapper a = ArticlesWrapper { articles :: [a], articlesCount :: Int } deriving (Eq, Show, Generic)
instance (ToJSON a) => ToJSON (ArticlesWrapper a)
instance (FromJSON a) => FromJSON (ArticlesWrapper a)

newtype TagsWrapper a = TagsWrapper { tags :: a } deriving (Eq, Show, Generic)
instance (ToJSON a) => ToJSON (TagsWrapper a)
instance (FromJSON a) => FromJSON (TagsWrapper a)


instance FromRow Article where
  fromRow = Article 
    <$> field
    <*> field
    <*> field
    <*> field
    <*> (fromPGArray <$> field)
    <*> field
    <*> field
    <*> field
    <*> field
    <*> fromRow