Preparing Images
================

Tile images which can be exported from Rallyman Track Editor for Windows have
PNG format and come with a size of 1654x1432 or 1654x1654 pixels. They are
stored in 8-bit RGBA colors which makes them suitable for print-out. For web
usage both, image size in pixels and the color format is reduced to save on
network transfer time.

## Medium Size Images

The original images are resized to 520x450 pixels using the [Unsharped Resizing]
feature of ImageMagick:

```
convert $INPUT -resize 520x -unsharp 0x0.75+0.75+0.008 $OUTPUT
```

Then each resized image is converted from full 8-bit RGBA colors to an indexed
palette of 180 different colors using [pngquant]:

```
pngquant --speed 1 --output $OUTPUT --strip --force 180 -- $INPUT
```

This will generate PNG files suitable for the tile details view in Rekee, or
for linking in exported SVG files. They are are about 65% smaller than the
previous non-indexed PNG files.

As an alternative for modern web browsers we will create WebP images from the
full 8-bit PNG files. This will create files that are about 86% smaller than the
full, non-indexed PNG files (or less than half the size of the indexed PNGs).

```
cwebp -q 80 -m 6 -o $OUTPUT -short -- $INPUT
```

## Thumbnail Size Images

For thumbnail images the original images are resized to 240x208 pixels using the [Unsharped Resizing] feature of ImageMagick:

```
convert $INPUT -resize 240x -unsharp 0x0.75+0.75+0.008 -strip $OUTPUT
```

The resized images are then converted to WebP format:

```
cwebp -q 80 -m 6 -o $OUTPUT -short -- $INPUT
```

[Unsharped Resizing]: https://legacy.imagemagick.org/Usage/resize/#resize_unsharp
[pngquant]: https://pngquant.org/
