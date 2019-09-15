# thumbnail-polish

Haskell image thumbnail creation library with cropping, upscaling, aspect ratio
preservation and nonce suffixing.

## Usage

```haskell
createThumbnails defaultConfig [Size 512 512, Size 128 128] "/opt/app/image.jpg"
```
