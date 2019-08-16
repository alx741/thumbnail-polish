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
                                                        loadBS, save)
import qualified Vision.Primitive             as P (Rect (..), Size, ix2)
import           Vision.Primitive.Shape       as PS

createThumbnails :: MonadResource m
 => Configuration -- ^ Use 'def' for default values
 -> [Size]        -- ^ Thumbnail sizes to create
 -> FilePath      -- ^ Input image
 -> m (Either ThumbnailError [Thumbnail])
createThumbnails config@Configuration{..} [sizes] inputFp = do
    (imageReleaseKey, imageHandle) <- allocate (openFile inputFp ReadMode) hClose
    -- imageFileSize <- hFileSize imageHandle
    -- imageBytes <- hGetContents imageHandle
    -- imageLoadResult <- liftIO $ Devil.loadBS Devil.Autodetect imageBytes

    -- case imageLoadResult of
    --     Left _ -> pure $ Left FailedToReadImage
    --     Right image -> do

    --         let (originalImage :: RGB) = case cropFirst of
    --                 Nothing -> image
    --                 Just (Rect x y (Size w h)) ->
    --                     crop (P.Rect x y w h) (image :: RGB)

    imageResult <- liftIO $ loadImage config imageHandle
    release imageReleaseKey
    case imageResult of
        Left e -> pure $ Left e
        Right image -> do
            let (originalImage :: RGB) = case cropFirst of
                    Nothing -> image
                    Just (Rect x y (Size w h)) ->
                        crop (P.Rect x y w h) (image :: RGB)
            undefined

    undefined
  where
    loadImage Configuration{..} imageHandle = do
        imageFileSize <- hFileSize imageHandle
        if (imageFileSize > maxFileSize)
        then pure $ Left $ FileSizeTooLarge imageFileSize
        else do
            imageBytes <- hGetContents imageHandle
            case (Devil.loadBS Devil.Autodetect imageBytes) of
                Left _ -> pure $ Left FailedToReadImage
                Right (image :: RGB) -> do
                    let (PS.Z :. h :. w) = shape image
                    if (w > width maxImageSize || h > height maxImageSize)
                    then pure $ Left $ ImageSizeTooLarge $ Size w h
                    else pure $ Right image


data Configuration = Configuration
    { maxFileSize         :: Integer
        -- ^ Maximum input file size in bytes. Default = 5MiB
    , maxImageSize        :: Size
        -- ^ Maximum input image size. Default = 4096 x 4096 px
    , fileFormat          :: ImageFileFormat
        -- ^ In which file format to encode the created thumbnails
        -- Default: JPG
    , cropFirst           :: Maybe Rect
        -- ^ Whether the input image should be cropped to 'Rect' before creating
        -- thumbnails or left intact. The first thumbnail in the list will be
        -- the cropped input image. Default: Nothing
    , preserveAspectRatio :: Bool
        -- ^ Whether the created thumbnails should adhere to only one dimension
        -- of the requested size in order to preserve the original image aspect
        -- ratio or distort the image to make it fit. Default: True
    , upScaleOriginal     :: Bool
        -- ^ Whether the original image should be up scaled to make it fit the
        -- requested thumbnail sizes when the input image is too small,
        -- otherwise ignore the requested thumbnails that are bigger than the
        -- input image. Default: False
    , namePrefix          :: String
        -- ^ Created thumbnail files are named based on it's size, this option
        -- adds a prefix (e.g. "<prefix>512_512.jpg"). Default: "thumbnail-"
    , nonceSuffix         :: Bool
        -- Whether to end thumbnail file names with a small nonce (i.e. a random
        -- string of characters). Useful for overwriting images that are prone
        -- to stay in cache. Default: False
    , dstDirectory        :: IO FilePath
        -- ^ Where to put the created thumbnails.
        -- Default: 'getTemporaryDirectory'
    }

data Size = Size
    { width  :: Int
    , height :: Int
    } deriving (Show, Read, Eq, Ord)

data Rect = Rect
    { rX    :: Int  -- ^ X coordinate of the first pixel
    , rY    :: Int  -- ^ Y coordinate of the first pixel
    , rSize :: Size -- ^ Size of the rectangle
    } deriving (Show, Read, Eq, Ord)

data ImageFileFormat
    = BMP
    | GIF
    | JPG
    | PNG
    deriving (Show, Read, Enum, Eq, Ord)

data ThumbnailError
    = FileSizeTooLarge Integer -- ^ Input file is bigger than 'maxFileSize'
    | ImageSizeTooLarge Size   -- ^ Input image is bigger than 'maxImageSize'
    | UnrecognizedImageFormat
    | FailedToReadImage
    | FailedToSaveThumbnails
    deriving (Show, Eq)

data Thumbnail = Thumbnail
    { thumbFp          :: FilePath
    , thumbSize        :: Size  -- ^ Actual size of the created thumbnail
    , thumbFormat      :: ImageFileFormat
    , thumbNonceSuffix :: Maybe String
    } deriving (Show, Eq)

instance Default Configuration where
    def = Configuration
        { maxFileSize         = 5 * 1024 * 1024
        , maxImageSize        = Size 4096 4096
        , fileFormat          = JPG
        , cropFirst           = Nothing
        , preserveAspectRatio = True
        , upScaleOriginal     = False
        , namePrefix          = "thumbnail-"
        , nonceSuffix         = False
        , dstDirectory        = getTemporaryDirectory
        }
