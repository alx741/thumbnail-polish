module Graphics.Thumbnail where

import Control.Monad.Trans.Resource (MonadResource)

createThumbnails :: MonadResource m
 => Configuration -- ^ Use 'def' for default values
 -> FilePath      -- ^ Input image
 -> m (Either ThumbnailError [Thumbnail])
createThumbnails = undefined

data Configuration = Configuration
    { maxFileSize      :: Integer -- ^ Maximum input file size in bytes. Default = 5MiB
    , maxImageSize     :: Size    -- ^ Maximum input image size. Default = 3000 x 3000 px
    , reencodeOriginal :: Maybe Reencoding -- ^ Wheter the input image should be reencoded or not
    }

data Size = Size
    { width  :: Int
    , height :: Int
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
    { thumbFp     :: FilePath
    , thumbSize   :: Size -- ^ Actual size of the created thumbnail
    , thumbFormat :: ImageFileFormat
    } deriving (Show, Eq)
