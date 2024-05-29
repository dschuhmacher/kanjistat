# kanjistat 0.14.0 (2024-05-29)

## New features

* New function `convert_kanji` for universal conversion between kanji formats.  

* New function `sedist` for computing the stroke edit distance by Lars Yencken.

-------------------------------

# kanjistat 0.13.2 (2024-05-24)

## Bug fixes

* Properly set up integration of the new non-CRAN kanjistat.data package.  

-------------------------------

# kanjistat 0.13.1 (2024-05-17)

## Bug fixes

* `compare_neighborhoods` gave obscure errors when stroke edit distances involved kanji with index > 2133. Fixed by returning an explicit error if the key kanji has such an index and setting the corresponding return value to NA if any of the closest kanji in the kanji distance has such an index. 

-------------------------------

# kanjistat 0.13.0 (2024-05-10)

## Breaking changes

* Function `kanjidist` with `approx = "pc"` or `approx = "pcweighted"` now runs only for `kanjivec` objects generated with kanjistat 0.13.0 or newer. 

## New features

* The structure of `kanjivec` objects has been extended. Each strokes in the `stroketree` component now has an additional attribute `"beziermat"` which describes the Bézier curves of the stroke in a standardized 2 x (1+3n) matrix format (n = number of curves). The new structure is fully backward compatible. Whether a given kanjivec object `kan` follows the new structure can be tested by `attr(kan, "kanjistat_version") >= 0.13.0`. The `kvecjoyo` dataset on <https://github.com/dschuhmacher/kanjistat.data> has been updated accordingly.

* New function `compare_neighborhoods`, which currently compares stroke edit distances and kanji distances in a dstrokedit neighborhood of a given kanji and optionally extends the comparison to nearest neighbors in the kanji distance. This function is still somewhat experimental.

* `kanjidist` and `kanjidistmat` have a new parameter `minor_warnings` which toggles any warnings that can be ignored by most users. These warnings usually point to issues in the underlying `kanjivec` data or the `kanjidist` computation that are currently addressed by workarounds.

## Enhancements

* kanjidist with `approx = "pc"` or `approx = "pcweighted"` runs considerably faster with the new `kanjivec` objects, because the inefficient (multiple) parsing of `d` attributes from previous versions is now avoided.


## Bug fixes

* Producing the point cloud representations produced an error for some individual `kanjivec` objects. Fixed in the internal functions. Both `kanjivec` with non-default parameter `bezier_discr` and `kanjidist` with `approx = "pc"` or `approx = "pcweighted"` should run now in all cases without problems (tested for Jouyou kanji).

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
