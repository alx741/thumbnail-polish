-- | Create image thumbnails
--
-- Using the default configuration
--
-- >>> runResourceT $ createThumbnails def
--      [Size 512 512, Size 128 128]
--      "/opt/app/image.jpg"
--
-- Or specify a custom 'Configuration'

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Thumbnail where

import Control.Exception
import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (MonadResource, allocate, release)
import qualified Crypto.Nonce                 as Nonce
import           Data.ByteString              (hGetContents)
import           Data.Default                 (Default (..))
import           System.Directory             (getTemporaryDirectory)
import           System.IO                    (IOMode (..), hClose, hFileSize,
                                               openFile)
import           Vision.Image                 (RGB, crop, shape)
import qualified Vision.Image.Storage.DevIL   as Devil (Autodetect (..),
                                                        BMP (..), GIF (..),
                                                        JPG (..), PNG (..),
                                                        load, save)
import qualified Vision.Primitive             as P (Rect (..), Size, ix2)
import           Vision.Primitive.Shape       as PS

createThumbnails :: MonadResource m
 => Configuration -- ^ Use 'def' for default values
 -> [Size]        -- ^ Thumbnail sizes to create
 -> FilePath      -- ^ Input image
 -> m [Thumbnail]
createThumbnails config@Configuration{..} [sizes] inputFp = do
    imageResult <- liftIO $ Devil.load Devil.Autodetect inputFp
    (originalImage :: RGB) <- either
            (\_ -> liftIO $ throwIO FailedToLoadImage)
            pure
            imageResult
    let image = maybe originalImage (`cropImage` originalImage) cropFirst
    undefined

  where
    cropImage :: Rect -> RGB -> RGB
    cropImage (Rect x y (Size w h)) = crop (P.Rect x y w h)


data Configuration = Configuration
    { fileFormat :: ImageFileFormat
        -- ^ In which file format to encode the created thumbnails
        -- Default: JPG
    , cropFirst :: Maybe Rect
        -- ^ Whether the input image should be cropped to 'Rect' before creating
        -- thumbnails or left intact. The first thumbnail in the list will be
        -- the cropped input image. Default: Nothing
    , preserveAspectRatio :: Bool
        -- ^ Whether the created thumbnails should adhere to only one dimension
        -- of the requested size in order to preserve the original image aspect
        -- ratio or distort the image to make it fit. Default: True
    , upScaleOriginal :: Bool
        -- ^ Whether the original image should be up scaled to make it fit the
        -- requested thumbnail sizes when the input image is too small,
        -- otherwise ignore the requested thumbnails that are bigger than the
        -- input image. Default: False
    , namePrefix :: String
        -- ^ Created thumbnail files are named based on it's size, this option
        -- adds a prefix (e.g. "<prefix>512_512.jpg"). Default: "thumbnail-"
    , nonceSuffix :: Bool
        -- Whether to end thumbnail file names with a small nonce (i.e. a random
        -- string of characters). Useful for overwriting images that are prone
        -- to stay in cache. Default: False
    , dstDirectory :: IO FilePath
        -- ^ Where to put the created thumbnails.
        -- Default: 'getTemporaryDirectory'
    }

data Size = Size
    { width :: Int
    , height :: Int
    } deriving (Show, Read, Eq, Ord)

data Rect = Rect
    { rX :: Int  -- ^ X coordinate of the first pixel
    , rY :: Int  -- ^ Y coordinate of the first pixel
    , rSize :: Size -- ^ Size of the rectangle
    } deriving (Show, Read, Eq, Ord)

data ImageFileFormat
    = BMP
    | GIF
    | JPG
    | PNG
    deriving (Show, Read, Enum, Eq, Ord)

data Thumbnail = Thumbnail
    { thumbFp :: FilePath
    , thumbSize :: Size  -- ^ Actual size of the created thumbnail
    , thumbNonceSuffix :: Maybe String
    } deriving (Show, Eq)

instance Default Configuration where
    def = Configuration
        { fileFormat          = JPG
        , cropFirst           = Nothing
        , preserveAspectRatio = True
        , upScaleOriginal     = False
        , namePrefix          = "thumbnail-"
        , nonceSuffix         = False
        , dstDirectory        = getTemporaryDirectory
        }

data ThumbnailException
    = FailedToLoadImage
    deriving (Show, Eq)

instance Exception ThumbnailException
