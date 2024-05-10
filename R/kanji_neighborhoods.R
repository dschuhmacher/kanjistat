#' Compare distances of nearest kanji
#'
#' List distances to nearest neighbors of a given kanji in terms of a reference distance
#' (which is currently only the stroke edit distance) and compare with values in terms of
#' another distance (currently only the kanji distance).
#'
#' @param kan a kanji (currently only as a single UTF-8 character).
#' @param refdist the name of the reference distance (currently only "strokedit").
#' @param refnn the number of nearest neighbors in terms of the reference distance.
#' @param compdist a character vector. The name(s) of one or several other distances to compare with
#' (currently only "kanjidist").
#' @param compnn the number of nearest neighbors in terms of the other distance(s).
#' @param kvecdata a list of the 2136 Jouyou kanji in [`kanjivec`] format, as obtained e.g. from
#' <https://github.com/dschuhmacher/kanjistat.data>.
#' @param ... further parameters that are passed to [kanjidist()].
#'
#' @section Warning:
#'
#' `r lifecycle::badge("experimental")`\cr
#' This is only a first draft of the function and its interface and details may change considerably in the future.
#' As there is currently no precomputed kanjidist matrix, there is a huge difference in computation time between
#' setting `compnn = 0` (only kanji distances to the `refnn` nearest neighbors in terms of `refdist` have to be 
#' computed) and setting `compnn` to any value $> 0$ (kanji distances to all 2135 other Jouyou kanji have to be
#' computed in order to determine the `compnn` nearest neighbors; depending on the system and parameter settings
#' this can take (roughly) anywhere between 2 minutes and an hour).
#'
#' @return A matrix of distances with `refnn + compnn` columns named by the nearest neighbors of `kan` (first
#' in terms of the reference distance, then the other distances) and `1 + length(compdist)` rows named
#' by the type of distance.
#' 
#' @export
#'
#' @examples
#' # compare_neighborhoods("晴", refnn=5, compo_seg_depth=4, approx="pcweighted",
#' #                       compnn=0, minor_warnings=FALSE)
#
compare_neighborhoods <- function(kan, refdist="strokedit", refnn=10, compdist="kanjidist", compnn=0, kvecdata=kvecjoyo, ...) {
  refdist = match.arg(refdist, choices=c("strokedit"))
  compdist = match.arg(compdist, choices=c("kanjidist"))
  if (refdist != "strokedit" || compdist != "kanjidist") stop("combination of distances not (yet) implemented")
  
  ki <- which(kanjistat::kbase$kanji == kan)
  kj <- which(kanjistat::dstrokedit[ki,] > 0)  # the zeroes in the sparse matrix should be NAs (to be fixed?!)
  if (length(kj) == 0) {
    stop("no known stroke edit distances for kanji ", kan, " available")
  }
  if (refnn > 10) {
    stop("currently only the 10 nearest neighbors of each kanji are known for the stroke edit distance")
  }
  kjord <- order(kanjistat::dstrokedit[ki, kj])
  kj <- kj[kjord[seq_len(refnn)]]
  # dstrokedit[ki,kj]
  # kbase$kanji[kj]
  kan1 <- kvecdata[ki]   # needs to be a list even if it is just one kanji!
  if (compnn > 0) {
    warning("compnn > 0 currently means that 2135 kanji distances have to be computed. This will take a while
            (see help page for details).")
    kdall <- kanjidistmat(kan1, kvecdata, ...)
    # computing also the distance to kan1 itself costs nothing and keeps the indices intact.
    kdord <- order(kdall)  # still keep the indices intact
    kdord <- setdiff(kdord, c(ki, kj))
    # kdall[kdord][1:20]
    # kbase$kanji[kdord][1:20]
    kj2 <- kdord[1:compnn]
    kdrow <- kdall[ c(kj, kj2) ]
  } else {
    kan2 <- kvecdata[kj]
    kdrow <- kanjidistmat(kan1, kan2, ...)
    kj2 <- numeric(0)
  }
  
  sedrow <- kanjistat::dstrokedit[ki, c(kj, kj2)]
  sedrow[sedrow == 0] <- NA
  
  res <- rbind(sedrow, kdrow)
  rownames(res) <- c("strokedit", "kanjidist")
  colnames(res) <- kanjistat::kbase$kanji[c(kj, kj2)]
  attr(res, "key") <- kan
  
  return(res)
}