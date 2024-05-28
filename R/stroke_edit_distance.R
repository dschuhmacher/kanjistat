# replaced by convert_kanji:
# char2kanjivec <- function(char) {
#   lapply(char, \(x) {
#     kanjistat.data::kvecjoyo[[which(kbase$kanji == x)]]
#   })
# }


# using full info, without collapsing
strokesigraw <- function(kvec) {
  fullstrokes <- get_strokes(kvec)
  s <- sapply(fullstrokes, \(x){attr(x, "type")})
  s
}


# using full info
strokesignature <- function(kvec) {
  strokes <-
    c("\u31c0", "\u31c0/\u31cf", "\u31c0/\u31d0", "\u31c1", 
      "\u31c2", "\u31c3", "\u31c4", "\u31c4a", "\u31c5", "\u31c6", 
      "\u31c6/\u31da", "\u31c6a", "\u31c6v", "\u31c7", "\u31c7/\u31c6", 
      "\u31c7a", "\u31c8", "\u31c8a", "\u31c8b", "\u31c9", "\u31cb", 
      "\u31cf", "\u31cfa", "\u31d0", "\u31d0/\u31d2", "\u31d0/\u31d4", 
      "\u31d0a", "\u31d0b", "\u31d0b/\u31d4", "\u31d0c", "\u31d0c/\u31c0", 
      "\u31d0c/\u31d4", "\u31d1", "\u31d1/\u31d2", "\u31d1/\u31d9", 
      "\u31d1/\u31da", "\u31d1a", "\u31d1a/\u31d2", "\u31d2", 
      "\u31d2/\u31c0", "\u31d2/\u31d1", "\u31d2/\u31d4", "\u31d2/\u31da", 
      "\u31d3", "\u31d4", "\u31d4/\u31c0", "\u31d4/\u31cf", "\u31d4/\u31d0", 
      "\u31d4/\u31d1", "\u31d4/\u31d2", "\u31d4a", "\u31d5", 
      "\u31d5/\u31c6", "\u31d5a", "\u31d5a/\u31c6", "\u31d5b", 
      "\u31d5b/\u31c6", "\u31d5c", "\u31d6", "\u31d6a", "\u31d6b", 
      "\u31d6b/\u31c6", "\u31d7", "\u31d7/\u31db", "\u31d7a", 
      "\u31d9", "\u31d9/\u31cf", "\u31d9/\u31df", "\u31da", "\u31db", 
      "\u31dc", "\u31de", "\u31df", "\u31df/\u31cf", "\u31df/\u31d1", 
      "\u31dfa", "\u31dfa/\u31cf", "\u31dfb", "\u31e1")
  # lookup list to encode strokes for use with adist:
  # (using part of the visible ascii range; careful \\ is escaped \, i.e. 1 character)
  lookup <- as.list(intToUtf8(44:122, multiple=TRUE))
  names(lookup) <- strokes
  
  fullstrokes <- get_strokes(kvec, simplify=FALSE)
  s <- sapply(fullstrokes, \(x){attr(x, "type")})
  senc <- lookup[s]
  senc <- paste(senc, collapse="") 
  senc
}


# simple version using only the first character of each stroke
strokesignature1 <- function(kvec) {
  fullstrokes <- get_strokes(kvec, simplify=FALSE)
  s1 <- sapply(fullstrokes, \(x){substr( attr(x, "type"), 1, 1 )})
  s1 <- paste(s1, collapse="")
  s1
}

# using full stroke info except if / appears (then only part before /)
strokesignature2 <- function(kvec) {
  strokes2 <-
    c("\u31c0", "\u31c1", "\u31c2", "\u31c3", "\u31c4", "\u31c4a", 
      "\u31c5", "\u31c6", "\u31c6a", "\u31c6v", "\u31c7", "\u31c7a", 
      "\u31c8", "\u31c8a", "\u31c8b", "\u31c9", "\u31cb", "\u31cf", 
      "\u31cfa", "\u31d0", "\u31d0a", "\u31d0b", "\u31d0c", "\u31d1", 
      "\u31d1a", "\u31d2", "\u31d3", "\u31d4", "\u31d4a", "\u31d5", 
      "\u31d5a", "\u31d5b", "\u31d5c", "\u31d6", "\u31d6a", "\u31d6b", 
      "\u31d7", "\u31d7a", "\u31d9", "\u31da", "\u31db", "\u31dc", 
      "\u31de", "\u31df", "\u31dfa", "\u31dfb", "\u31e1")
  # lookup list to encode strokes for use with adist
  # (using part of the visible ascii range)
  lookup2 <- as.list(intToUtf8(44:90, multiple=TRUE))
  names(lookup2) <- strokes2
  
  fullstrokes <- get_strokes(kvec, simplify=FALSE)
  temp <- sapply(fullstrokes, \(x){attr(x, "type")})
  temp <- strsplit(temp, split="/")
  s2 <- sapply(temp, \(x) {x[[1]]}) # character vector containing only part before / (or whole info if no/)
  s2enc <- lookup2[s2]
  s2enc <- paste(s2enc, collapse="") 
  s2enc
}



#' Compute the stroke edit distances between two sets of kanji
#'
#' @description Variants of the stroke edit distance proposed by Yencken (2010).
#' Each kanji is encoded as sequence of stroke types according to 
#' its stroke order, using the type attribute from the kanjiVG data. Then the
#' edit distance (a.k.a.\ Levenshtein distance) between sequences is computed and
#' divided by the maximum of the number of strokes
#'
#' @param k1,k2 atomic vectors or lists of kanji in any format that can be treated by [convert_kanji()]
#' @param type the type of stroke edit distance to compute. See details.
#' 
#' @references Yencken, Lars (2010). Orthographic support for passing the reading hurdle in Japanese.<br>
#' PhD Thesis, University of Melbourne, Australia
#'
#' @details
#' The kanjiVG type attribute is a single string composed of a CJK strokes Unicode character, an optional
#' latin letter providing further information and possibly a variant (another CJK strokes character with optional
#' letter) separated by "/". If `type` is "full"` a match is only counted if two strings are exactly the
#' same, "before_slash" ignores any slashes and what comes after them, "first" only considers the first 
#' character of each string (so the first CJK stroke character) when counting matches.
#' 
#' The stroke edit distance used by Yencken (2010) is obtained by setting type = "all" (the default),
#' except that the underlying kanjiVG data has significantly changed since then. Comparing with the values
#' in [dstrokedit] we get an agreement of 96.3 percent, whereas the other distances disagree by 
#' a small amount (usually 1-2 edit operations).
#' 
#' @section Warning:
#' Requires kanjistat.data package.
#'
#' @return A `length(k1)` x `length(k2)` matrix of stroke edit distances.
#' @export
#'
#' @examples
#' ind1 <- 384  
#' k1 <- convert_kanji(ind1, "character")
#' ind2 <- which(dstrokedit[ind1,] > 0)  
#' # dstrokedit contains only the "closest" kanji
#' k2 <- convert_kanji(ind2, "character")
#' row_a <- dstrokedit[ind1, ind2]  
#' if (requireNamespace("kanjistat.data", quietly = TRUE)) {
#'   row_b <- sedist(k1, k2)  
#'   mat <- rbind(row_a, row_b)
#'   rownames(mat) = c(k1, k1)
#'   colnames(mat) = k2
#'   mat
#' }
#' 
sedist <- function(k1, k2, type = c("full", "before_slash", "first")) {
  type <- match.arg(type)
  kvec1 <- convert_kanji(k1, "kanjivec", simplify=FALSE)
  if (is(kvec1, "kanjivec")) {  # if k1 was just a single kanji specified in atomic form
    kvec1 <- list(kvec1)
  }
  kvec2 <- convert_kanji(k2, "kanjivec", simplify=FALSE)
  if (is(kvec2, "kanjivec")) {  # if k1 was just a single kanji specified in atomic form
    kvec2 <- list(kvec2)
  }
  if (type == "first") {
    sig1 <- sapply(kvec1, strokesignature1)
    sig2 <- sapply(kvec2, strokesignature1)
  } else if (type == "before_slash") {
    sig1 <- sapply(kvec1, strokesignature2)
    sig2 <- sapply(kvec2, strokesignature2)
  } else if (type == "full") {
    sig1 <- sapply(kvec1, strokesignature)
    sig2 <- sapply(kvec2, strokesignature)
  }
  rawdmat <- adist(sig1, sig2)  
  denom <- outer(nchar(sig1), nchar(sig2), pmax)
  return(rawdmat/denom)
}



if (FALSE) {
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use stroke signature via kanjiVG "type" attribute
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# taking full type info into account
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allthestrokes <- character()
for (i in 1:2136) {
  cat(i, ", ", sep="")
  fullstrokes <- get_strokes(kanjistat.data::kvecjoyo[[i]], simplify=FALSE)
  s1 <- sapply(fullstrokes, \(x){attr(x, "type")})
  stopifnot(is(s1, "character"))
  # if ("㇐b" %in% s1 && i %in% joyo1981ind) stop("reverse engineer")
  allthestrokes <- c(allthestrokes, s1)
}
str(allthestrokes)
unique(allthestrokes)
counts <- table(allthestrokes)
strokes <- dimnames(counts)[[1]]
attributes(counts) <- NULL
data.frame(strokes, counts)
dump("strokes", file="")

# strokes <-
#   c("㇀", "㇀/㇏", "㇀/㇐", "㇁", "㇂", "㇃", "㇄", "㇄a", 
#     "㇅", "㇆", "㇆/㇚", "㇆a", "㇆v", "㇇", "㇇/㇆", "㇇a", 
#     "㇈", "㇈a", "㇈b", "㇉", "㇋", "㇏", "㇏a", "㇐", "㇐/㇒", 
#     "㇐/㇔", "㇐a", "㇐b", "㇐b/㇔", "㇐c", "㇐c/㇀", "㇐c/㇔", 
#     "㇑", "㇑/㇒", "㇑/㇙", "㇑/㇚", "㇑a", "㇑a/㇒", "㇒", 
#     "㇒/㇀", "㇒/㇑", "㇒/㇔", "㇒/㇚", "㇓", "㇔", "㇔/㇀", 
#     "㇔/㇏", "㇔/㇐", "㇔/㇑", "㇔/㇒", "㇔a", "㇕", "㇕/㇆", 
#     "㇕a", "㇕a/㇆", "㇕b", "㇕b/㇆", "㇕c", "㇖", "㇖a", 
#     "㇖b", "㇖b/㇆", "㇗", "㇗/㇛", "㇗a", "㇙", "㇙/㇏", 
#     "㇙/㇟", "㇚", "㇛", "㇜", "㇞", "㇟", "㇟/㇏", "㇟/㇑", 
#     "㇟a", "㇟a/㇏", "㇟b", "㇡")

# lookup list to encode strokes for use with adist
# (using part of the visible ascii range; careful \\ is escaped \, i.e. 1 character)
lookup <- as.list(intToUtf8(44:122, multiple=TRUE))
names(lookup) <- strokes


# taking only first character of type info into account
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allthestrokes1 <- character()
for (i in 1:2136) {
  cat(i, ", ", sep="")
  fullstrokes <- get_strokes(kanjistat.data::kvecjoyo[[i]], simplify=FALSE)
  s1 <- sapply(fullstrokes, \(x){substr( attr(x, "type"), 1, 1 )})
  stopifnot( is(s1, "character") )
  allthestrokes1 <- c(allthestrokes1, s1)
}
str(allthestrokes1)
unique(allthestrokes1)
counts1 <- table(allthestrokes1)
strokes1 <- dimnames(counts1)[[1]]
attributes(counts1) <- NULL
data.frame(strokes1, counts1)
dump("strokes1", file="")

# strokes1 <-
#   c("㇀", "㇁", "㇂", "㇃", "㇄", "㇅", "㇆", "㇇", "㇈", 
#     "㇉", "㇋", "㇏", "㇐", "㇑", "㇒", "㇓", "㇔", "㇕", 
#     "㇖", "㇗", "㇙", "㇚", "㇛", "㇜", "㇞", "㇟", "㇡"
#   )


# taking only part before / into account (otherwise full;
# suspected Yencken-Baldwin approach)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allthestrokes2 <- character()
for (i in 1:2136) {
  cat(i, ", ", sep="")
  fullstrokes <- get_strokes(kanjistat.data::kvecjoyo[[i]], simplify=FALSE)
  temp <- sapply(fullstrokes, \(x){attr(x, "type")})
  stopifnot(is(temp, "character"))
  temp <- strsplit(temp, split="/")
  s2 <- sapply(temp, \(x) {x[[1]]}) # character vector containing only part before / (or whole info if no/)
  allthestrokes2 <- c(allthestrokes2, s2)
}
str(allthestrokes2)
unique(allthestrokes2)
counts2 <- table(allthestrokes2)
strokes2 <- dimnames(counts2)[[1]]
attributes(counts2) <- NULL
data.frame(strokes2, counts2)
dump("strokes2", file="")

# strokes2 <-
#   c("㇀", "㇁", "㇂", "㇃", "㇄", "㇄a", "㇅", "㇆", "㇆a", 
#     "㇆v", "㇇", "㇇a", "㇈", "㇈a", "㇈b", "㇉", "㇋", "㇏", 
#     "㇏a", "㇐", "㇐a", "㇐b", "㇐c", "㇑", "㇑a", "㇒", 
#     "㇓", "㇔", "㇔a", "㇕", "㇕a", "㇕b", "㇕c", "㇖", "㇖a", 
#     "㇖b", "㇗", "㇗a", "㇙", "㇚", "㇛", "㇜", "㇞", "㇟", 
#     "㇟a", "㇟b", "㇡")

# lookup list to encode strokes for use with adist
# (using part of the visible ascii range)
lookup2 <- as.list(intToUtf8(44:90, multiple=TRUE))
names(lookup2) <- strokes2

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# code used to replace the UTF-8 strings by \u (and possibly \U) escapes
repl_fun <- function(x) {
  hexchar <- kanjiToCodepoint(x, character=TRUE)
  prefix <- ifelse(nchar(hexchar) <= 4, "\\u", "\\U")
  # \u works only for up to 4 hex digits, but is more common
  # \U would work up to 8 hex digits (including for 4)
  paste0(prefix, hexchar)
}

strokesa <- gsubfn::gsubfn(pattern = "(\\P{ASCII})",
                 # Everything that is not in the basic_latin codeblock (i.e. is not ASCII)
               replacement = repl_fun,
               x = strokes, perl = TRUE, useBytes = FALSE)

# oh dear, the following achieves the same for free:
stringi::stri_escape_unicode(strokes)
}