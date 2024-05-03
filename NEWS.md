# kanjistat (development version)

## Bug

* Producing the point cloud representations of a few individual `kanjivec` objects produced an error. Fixed.  

-------------------------------

# kanjistat 0.12.0 (2024-05-02)

* kanjistat depends on R (>= 4.1) and transport (>= 0.15) now.

## New features

* Function `kanjidist` has a new argument `approx`, which specifies how the strokes are to be approximated for computing component distances. The three options "grid", "pc" or "pcweighted" work in any combination with the three options for the `type` argument (which now strictly specifies the type of distance used for the components).

* Function `kanjivec` has a new argument `bezier_discr`, which may be any of "svgparser", "eqtimed" and "eqspaced", specifing, for the discretization of the strokes in the `stroketree` component, which code is used and according to which strategy the points are placed.  

* Data set `pooled_similarity` contains the human similarity judgements of kanji from Yencken and Baldwin (2008). 

## Enhancements

* point cloud approximations ("pc" and "pcweighted") use (approximately) equispaced points on the Bézier curves now. 

* Various speed improvements to options "pc" and "pcweighted".

## Bugs

* Using `kanjidist` for compo_seg_depth1 >= 5 returned an error. Fixed. 

-------------------------------

# kanjistat 0.11.0 (2024-02-17)

* Lennart Finke is now a co-author.

## New features

* Function `kanjidist` accepts two new `type` arguments "pc" and "pcweighted" for computing component distances based on (weighted) point clouds rather than bitmap images.

* Data sets `dstrokedit` and `dyehli` added with stroke edit and Yeh-Li (bag-of-radicals) distances between Jouyou kanji and (usually a bit more than) their closest ten neighbors. Based on the PhD thesis by Lars Yencken (2010).

## User visible changes

* Previously, function `kanjimat` cut off part of the kanji under the default setting `marging = 0` on Windows. The algorithm for setting the effective margin in the bitmap representation has been improved.

-------------------------------

# kanjistat 0.10.0 (2024-01-01)

## New feature

* Function `read_kanjidic2`, which reads a KANJIDIC2 file and converts it to a list. All kanji information in the original file is retained, but the structure is simplified.

## Documentation

* Add contribution guidelines.

-------------------------------

# kanjistat 0.9.0 (2023-05-20)

## New feature

* Add function `cjk_escape`, which replaces CJK characters by their Unicode escape sequences in files.

## Documentation

* Improve the main package vignette and make it more versatile.

-------------------------------

# kanjistat 0.8.0 (2023-05-06)

## Documentation

* More extensive readme file and main package vignette.

* Add package website using [pkgdown](https://pkgdown.r-lib.org/).

## Improvements

* Increase functionality for `plotkanji`. This function now plots several kanji in possibly different fonts. A parameter `filename` was added for devices that plot to a file.

## Minor bug fixes

* Add `print.kanjivec()` to package exports.

-------------------------------

# kanjistat 0.7.0 (2023-04-07)

* First public release.
