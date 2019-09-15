-- | Create image thumbnails
--
-- Using the default configuration
--
-- >>> createThumbnails defaultConfig [Size 512 512, Size 128 128] "/opt/app/image.jpg"
--
-- Or specify a custom 'Configuration'

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Thumbnail
    ( createThumbnails
    , defaultConfig
    , imageSize
    , Thumbnail(..)
    , Configuration(..)
    , ImageFileFormat(..)
    , Rect(..)
    , Size(..)
    , ThumbnailException(..)
    ) where


import           Control.Exception
import qualified Crypto.Nonce               as Nonce
import           Data.Bool                  (bool)
import           Data.ByteString            (hGetContents)
import           Data.Char                  (toLower)
import           Data.Default               (Default (..))
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (unpack)
import           System.Directory           (getTemporaryDirectory)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (IOMode (..), hClose, hFileSize,
                                             openFile)
import           Vision.Image               (InterpolMethod (NearestNeighbor),
                                             RGB, crop, resize, shape)
import qualified Vision.Image.Storage.DevIL as Devil (Autodetect (..), BMP (..),
                                                      JPG (..), PNG (..),
                                                      SaveImageType, load, save)
import qualified Vision.Primitive           as P (Rect (..), Size, ix2)
import           Vision.Primitive.Shape     as PS

createThumbnails
 :: Configuration -- ^ Use 'defaultConfig' for default values
 -> [Size]        -- ^ Thumbnail sizes to create
 -> FilePath      -- ^ Input image
 -> IO [Thumbnail]
createThumbnails config@Configuration{..} reqSizes inputFp = do
    imageResult <- Devil.load Devil.Autodetect inputFp
    (originalImage :: RGB) <- either
            (\_ -> throwIO FailedToLoadImage)
            pure
            imageResult
    let image = maybe originalImage (`cropImage` originalImage) cropFirst
    let imageSize = size image
    let sizes = bool reqSizes (fitAspectRatio imageSize <$> reqSizes) preserveAspectRatio
    dstDir <- dstDirectory
    suffix <- bool (pure "") (('_' :) <$> nonce) nonceSuffix
    thumbnails <- catMaybes <$>
        traverse (createThumbnail config suffix dstDir image imageSize) sizes
    pure thumbnails
  where
    cropImage :: Rect -> RGB -> RGB
    cropImage (Rect x y (Size w h)) = crop (P.Rect x y w h)

    fitAspectRatio :: Size -> Size -> Size
    fitAspectRatio (Size iW iH) (Size oW oH)
        | iW > iH = Size oW $ round $ fromIntegral (oH * iH) / fromIntegral iW
        | iH > iW = Size (round $ fromIntegral (oW * iW) / fromIntegral iH) oH
        | otherwise = Size oW oH

    nonce :: IO String
    nonce = take 10 . unpack <$> (Nonce.new >>= Nonce.nonce128urlT)



createThumbnail :: Configuration -> String -> FilePath -> RGB -> Size -> Size -> IO (Maybe Thumbnail)
createThumbnail Configuration{..} suffix dstDir img imgSize size@(Size w h) = do
    let name = namePrefix
            <> show w <> "_" <> show h
            <> suffix
            <> "." <> (toLower <$> show fileFormat)

    if size > imgSize && not upScaleOriginal
    then pure Nothing
    else Just <$> do
        let thumbImg = makeThumb size img
        let filePath = dstDir </> name

        -- FIXME: Horrible hack, what to do?
        case fileFormat of
            BMP -> Devil.save Devil.BMP filePath thumbImg
            JPG -> Devil.save Devil.JPG filePath thumbImg
            PNG -> Devil.save Devil.PNG filePath thumbImg

        pure $ Thumbnail filePath size
  where
    makeThumb :: Size -> RGB -> RGB
    makeThumb (Size w h) = resize NearestNeighbor (ix2 h w)

imageSize :: FilePath -> IO Size
imageSize fp = do
    imgResult <- Devil.load Devil.Autodetect fp
    img <- either (\_ -> throwIO FailedToLoadImage) pure imgResult
    pure $ size img

size :: RGB -> Size
size img =
    let (Z :. h :. w) = shape img
    in Size w h


data Configuration = Configuration
    { fileFormat          :: ImageFileFormat
        -- ^ In which file format to encode the created thumbnails. Default: JPG
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
        -- requested thumbnail sizes when the input image is too small (nearest
        -- neighbor interpolation), otherwise ignore the requested thumbnails
        -- that are bigger than the input image. Default: False
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
    } deriving (Show, Read, Eq)

instance Ord Size where
    compare s1@(Size w1 h1) s2@(Size w2 h2)
        | s1 == s2 = EQ
        | w1 > w2 || h1 > h2 = GT
        | otherwise = LT

data Rect = Rect
    { rX    :: Int  -- ^ X coordinate of the first pixel
    , rY    :: Int  -- ^ Y coordinate of the first pixel
    , rSize :: Size -- ^ Size of the rectangle
    } deriving (Show, Read, Eq, Ord)

data ImageFileFormat
    = BMP
    | JPG
    | PNG
    deriving (Show, Read, Enum, Eq, Ord)

data Thumbnail = Thumbnail
    { thumbFp   :: FilePath
    , thumbSize :: Size
        -- ^ Actual size of the created thumbnail. Might differ from the
        -- requested size if the `preserveAspectRatio` option is used
    } deriving (Show, Eq)

defaultConfig = Configuration
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
