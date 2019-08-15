module Graphics.Thumbnail where

import Control.Monad.Trans.Resource (MonadResource)

createThumbnails :: MonadResource m
 => Configuration -- ^ Use 'def' for default values
 -> FilePath      -- ^ Input image
 -> m (Either ThumbnailError [Thumbnail])
createThumbnails = undefined

data Configuration = Configuration
    { maxFileSize      :: Integer -- ^ Maximum input file size in bytes. Default = 5MiB
    , maxImageSize     :: Size -- ^ Maximum input image size. Default = 3000 x 3000 px
    , cropFirst        :: Maybe Rect
        -- ^ Whether the input image should be cropped to 'Rect' before creating
        -- thumbnails or left intact. The first thumbnail will be the cropped
        -- input image. Default: Nothing
    , preserveAspectRatio :: Bool
        -- ^ Whether the created thumbnails should adhere to only one dimension
        -- of the requested size in order to preserve the original image aspect
        -- ratio or distort the image to make it fit. Default: True
    , upScaleOriginal :: Bool
        -- ^ Whether the original image should be up scaled to make it fit the
        -- requested thumbnail sizes when the image is too small, otherwise
        -- ignore the requested thumbnails that are bigger than the original
        -- input image. Default: False
    , reencodeOriginal :: Maybe Reencoding
        -- ^ Whether the input image should be reencoded or left intact.
        -- Default: Nothing
    , namePrefix :: String
        -- ^ Created thumbnail files are named based on it's size, this option
        -- adds a prefix (e.g. "<prefix>_512_512.jpg"). Default: ""
    , nonceSuffix :: Bool
        -- Whether to end thumbnail file names with a small nonce (i.e. a random
        -- string of characters). Useful for overwriting images that are prone
        -- to stay in cache. Default: False
    , dstDirectory :: IO FilePath
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

data Reencoding
    = SameFileFormat -- ^ Reencode using the original file format
    | NewFileFormat ImageFileFormat -- ^ Reencode using the given file format
    deriving (Show, Read, Eq, Ord)

data ImageFileFormat
    = GIF
    | JPG
    | PNG
    deriving (Show, Read, Enum, Eq, Ord)

data ThumbnailError
    = FileSizeTooLarge Integer -- ^ Input file is bigger than 'maxFileSize'
    | ImageSizeTooLarge Size   -- ^ Input image is bigger than 'maxImageSize'
    | UnrecognizedImageFormat  -- ^ Input image is not one of [GIF, JPG, PNG]
    deriving (Show, Eq)

data Thumbnail = Thumbnail
    { thumbFp          :: FilePath
    , thumbSize        :: Size  -- ^ Actual size of the created thumbnail
    , thumbFormat      :: ImageFileFormat
    , thumbNonceSuffix :: Maybe String
    } deriving (Show, Eq)
