Preparing Images
================

Tile images which can be exported from Rallyman Track Editor for Windows have
PNG format and come with a size of 1654x1432 or 1654x1654 pixels. They are
stored in 8-bit RGBA colors which makes them suitable for print-out. For web
usage both, image size in pixels and the color format, is reduced to save on
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

For thumbnail images the original images are resized to 240x208 pixels using the
[Unsharped Resizing] feature of ImageMagick:

```
convert $INPUT -resize 240x -unsharp 0x0.75+0.75+0.008 -strip $OUTPUT
```

The resized images are then converted to WebP format:

```
cwebp -q 80 -m 6 -o $OUTPUT -short -- $INPUT
```

## Appendix: How to extract tile images from the Windows Track Editor

The Rallyman Track Editor software for Windows can be downloaded from the
[webpage of the boardgame author][Rallyman GT]. After the software is unpacked
/ installed it contains a file named `Database1.2.db` in the target installation
folder. Use [sqlite-cli] to open this database file:

```
sqlite3 Database1.2.db
```

In the interactive prompt that is displayed enter the following command to
write each tile image into a file in the current folder:

```
select id, writefile('tile-' || lower(id) || '.png', image) from tuiles;
```

The SQLite command can be terminated by entering:

```
.quit
```

If everything went fine the current folder will contain all the high-res tile
images in PNG format.

[Unsharped Resizing]: https://legacy.imagemagick.org/Usage/resize/#resize_unsharp
[pngquant]: https://pngquant.org/
[Rallyman GT]: https://www.bouvier-international.com/index.php/rallyman/rallyman-gt
[sqlite-cli]: https://sqlite.org/cli.html
