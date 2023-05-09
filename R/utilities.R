#' Kanjistat Options
#'
#' Set or examine global kanjistat options.
#'
#' @param ... any number of options specified as \code{name = value}
#'
#' @param x name of an option given as character string.
#'
#' @return \code{kanjistat_options} returns the list of all set options if there
#'   is no function argument. Otherwise it returns list of *all* old options. 
#'   \code{get_kanjistat_option} returns the current value set for option \code{x}
#'   or NULL if the option is not set.
#'
#' @name options
NULL


#' @rdname options
#' @export
# this behavior is quite different from ?options and not in a good way --> should be fixed
kanjistat_options <- function(...) {
  ll <- list(...)
  if (length(ll) == 0) {
    return(kanjistat_env$kanjistatOptions)
  } else {
    oldopts <- kanjistat_env$kanjistatOptions
    kanjistat_env$kanjistatOptions[names(ll)] <- ll
    invisible(oldopts)
  }
}


#' @rdname options
#' @export
get_kanjistat_option <- function(x) {
  return(kanjistat_env$kanjistatOptions[[x]])
}


#' Convert between Unicode codepoint and kanji
#' 
#' Given codepoints \code{cp}, the function `codepointToKanji` transforms
#' to UTF-8, which will typically show as the actual character the codepoints stands for.
#' Vice versa, given (UTF-8 encoded) kanjis \code{kan}, the function `kanjiToCodepoint` transforms
#' to unicode codepoints.
#'
#' @param cp a vector of character strings or objects of class \code{hexmode}, representing
#'   hexadecimal numbers.
#' @param concat logical. Shall the returned characters be concatenated?
#' @param kan a vector of kanjis (strings of length 1) or a single string of length >= 1 
#'   of kanjis.
#' @param character logical. Shall the returned codepoints be of class "character" or hexmode.
#'
#' @name codepoint
#'
#' @examples
#' codepointToKanji(c("51b7", "6696", "71b1"))
#' kanjiToCodepoint("冷暖熱")
#' 


#' @rdname codepoint
#' @export
codepointToKanji <- function(cp, concat=FALSE) {   # x can be (vector of) hexmode int or a character string
  res <- intToUtf8(as.hexmode(cp))
  if (concat) {
    return(res)
  } else {
    return(unlist(strsplit(res, "")))
  }
}


#' @rdname codepoint
#' @export
kanjiToCodepoint <- function(kan, character=FALSE) {   # x should be vector of individual kanji characters 
                                                    # or a string of kanjis
  temp <- paste(kan, collapse="")
  stopifnot(nchar(temp) >= 1)
  res <- as.hexmode(utf8ToInt(temp))
  if (character) {
    return(as.character(res))
  } else {
    return(res)
  }
}


#' Sample kanji from a set
#'
#' @param set a character string specifying the set of kanjis to sample from.
#' @param size a positive number, the number of samples.
#' @param replace logical. Sample with replacement?
#' @param prob currently without effect.
#'
#' @return a vector of length \code{size} containing the individual characters
#' @export
#'
#' @examples
#' (sam <- samplekan(size = 10))
#' lookup(sam)
#
# In a later version we should allow (additionally/alternatively) for a user-specified condition
# not quite clear how to pass prob in either case.
samplekan <- function(set = c("kyouiku", "jouyou", "jinmeiyou", "kanjidic"), size = 1, replace = FALSE, prob = NULL) {
  set <- match.arg(set)
  ind <- switch(set,
                kyouiku = which(kanjistat::kbase$class == "kyouiku"),
                jouyou = which(kanjistat::kbase$class == "kyouiku" | kanjistat::kbase$class == "jouyou"),
                jinmeiyou = which(kanjistat::kbase$class == "jinmeiyou"),
                kanjidic = 1:dim(kanjistat::kbase)[1]
                )
  indsample <- sample(ind, size, replace = replace)
  res <- kanjistat::kbase$kanji[indsample]
  attr(res, "index") <- indsample
  return(res)
}


# Transforms an object of class "xml_document" obtained by reading an
# svg from https://github.com/KanjiVG/kanjivg with help of xml2::read_xml
# to a "reasonable" list in R
# Essentially: the viewBox and the style is dropped, the second g-tag is taken as new root (svg and first g-tag dropped)
# Then we keep all xml-attributes as list attributes, but also use the gxxx, or syyy endings of the id-elements as names
# of sublists and instead of innermost empty lists (at the leaves) we put interpolations of the strokes (given as Bézier3-
# curves in the d argument)
# The main work is done with code from the non-CRAN package svgparser v0.1.2 (MIT license, see svgparser_lite.R)
# Code had to be extracted because kanjistat wants to be a CRAN package when it's big...
.kanjivg_to_list <- function(xml, padhex, char, flatten_inner=TRUE, flatten_leaves=TRUE) {  # padhex and char are for verification purposes
  # check if everything is ok with xml
  # (it is not impossible that some of the kanjivg-files do not pass these tests!)
  if (!is(xml,"xml_document")) {
    stop("Function expects the unaltered object read with xml2::read_xml")
  }
  if (xml2::xml_name(xml) != "svg") {
    stop("xml is not a pure SVG file")
  }
  strokes <- xml2::xml_child(xml2::xml_child(xml,1),1)
  test <- xml2::xml_attrs(strokes)
  if (test["id"] != paste0("kvg:", padhex) || test["element"] != char) {
    stop("Mismatch of kanji id or character. Found ", paste(test, collapse=", "), 
         "; expected ", paste0("kvg:", padhex), ", ", char)
  }
  
  strokelist <- xml2::as_list(strokes)
  #str(strokelist, give.attr=FALSE)
  
  # the following subfunction goes through the list created by xml2::as_list depth first and 
  # fixes "nodes" and "leaves" in the way we want to.
  # We expect the kanjivg data to be such that every leaf is in fact an empty list (otherwise print the contents and stop)
  # 230120: The Bezier curves are still clearly visible in the stroke data and it is really
  #          a bit unfortunate that actually the beginning and end of each curve share a point
  #         (and it seems each B-curve is encoded with exactly 30 points)
  fix_xml_list <- function(li, root = FALSE) {
    if (!is.list(li)) {
      print(li)
      stop("Non-list encountered. Something is most probably wrong.")
    } 
    if ( length(li) == 1 && !root && ((flatten_inner && length(li[[1]]) > 0) ||  # we're at a parent with only-child that is NOT a leaf
                                      (flatten_leaves && length(li[[1]]) == 0)) ) { # or that IS a leaf (and we want to flatten accordingly)
      outerattr <- attributes(li)   
      outerattr <- outerattr[names(outerattr) != "names"]   # removes the one name "g"
      li <- fix_xml_list(li[[1]])   # this fuses only-child to parent
      innerattr <- attributes(li)
      # innerattr <- innerattr[names(innerattr) != "names"] 
        # only-child attributes become the "main" attributes 
      temp <- regexpr("^(p\\.)+", names(innerattr))
      repe <- max(c(0, attr(temp, "match.length")))/2 # 0 because if there is no match everything is -1
      names(outerattr) <- paste0( paste0(rep("p.",repe+1), collapse=""), names(outerattr))
      attributes(li) <- c(outerattr, innerattr)
        # 221128: minor bugfix the names are passed via innerattr (before: missing --> missing names with inner node flattening)
        # the fix is a bit sneaky: it works although leaves don't get their names assigned: If there
        # really is a straight line down to the next leaf, innerattr contains no names, but the first higher node that
        # combines something (or the root if there really can be a straight line to the root) takes the id of the leaf
        # and assigns it as its name. And if there are any combinations done further below innerattr contains the right names.
    } else if (length(li) != 0) {  # we're at a parent with multiple children (that may or may not be leaves)
      for (i in seq_along(li)) { 
        li[[i]] <- fix_xml_list(li[[i]])
      }
      names(li) <- substr(sapply(li, attr, "id"), 11, 14)
      # reads the ending g1, g2, ... s1, s2, ... of the xml-attribute "id" and assigns them as names
      # (14 is to make sure we can even cover even 100+ strokes, which is not necessary at the moment)
      # (has to come *after* the loop because flatten might adapt id of some li[[i]] to that of its only child)
    } else {  # # we're at a leaf
      path_d <- attr(li, "d")   # we save this because the following code remove it
      id <- attr(li, "id")
      type <- attr(li, "type")
      path_list <- parse_svg_path_d(path_d)
      points_df <- path_list_to_df(path_list, state=list(x=0,y=0,npoints=30))
      # npoints is how many interpolation points per bezier segment. 30 is the default for svgparser:::read_svg
      x <- points_df$x/109
      y <- 1-points_df$y/109
      li <- cbind(x[-1],y[-1])    # the first coordinates are from the move instruction, hence the same as the second coords.
      attr(li, "id") <- id
      attr(li, "type") <- type
      attr(li, "d") <- path_d # we save the original Bézier curve as attribute
    }
    li
  }
  strokelist <- fix_xml_list(strokelist, root = TRUE)  # suppress fusing of only-children to root
  
  # extract the positioned stroke numbers and save them as additional attribute to the whole strokelist
  numbers <- xml2::xml_child(xml,2)
  test <- xml2::xml_attrs(numbers)
  if (test["id"] != paste0("kvg:StrokeNumbers_", padhex)) {
    stop("Mismatch of id of presumed stroke number node. Found ", test["id"], 
         "; expected ", paste0("kvg:StrokeNumbers_", padhex))
  }
  temp <- xml2::as_list(numbers)
  temp <- lapply(temp, \(x) {attr(x, "transform")})
  
  read_coords <- function(string) {
    coords <- unlist(strsplit(string," "))
    coords <- as.numeric(stringr::str_sub(coords[5:6],c(1,1),c(-1,-2)))
    coords
  }
  numbermat <- t(sapply(temp, read_coords))
  numbermat[,1] <- numbermat[,1]/109
  numbermat[,2] <- 1-numbermat[,2]/109
  
  attr(numbermat, "dimnames") <- list(1:dim(numbermat)[1], c("x","y"))
  attr(strokelist, "strokenum_coords") <- numbermat
  
  return(strokelist)
}


# function to do some more intelligent post-flattening
# (input: stroketree obtained from .kanjivg_to_list without any flattening!)
flatten_intel <- function(stree) {
  #
  flattenstep <- function(st) {
    if (is.list(st)) {  # st is not a leaf
      if(length(st) == 1) {  # we are at a node with an only-child (inner-node or leaf)
        # browser()
        # the rules for fusing the only-child to the node:
        intel1 <- is.null(attr(st, "element"))   # the node has no element attribute
        #
        intel2 <- (!is.null(attr(st, "element")) && !is.null(attr(st[[1]], "element")) && (attr(st, "element") == attr(st[[1]], "element")) &&    
          (!is.null(attr(st, "part")) && !is.null(attr(st[[1]], "part")) && (attr(st, "part") == attr(st[[1]], "part"))) &&
          (is.null(attr(st, "number")) && is.null(attr(st[[1]], "number"))))
            # this rule is basically targeted at 巨 = kbase[1111,]
            # we need to be so careful because it might be otherwise (without weird repeated parts)
            # that the parent is just a simpified representation of a kanji that has more strokes than the child
            # but with the other strokes in another part of the tree (like 果 = kbase[475,])
        #
        if (intel1 || intel2) {
          outerattr <- attributes(st)   
          outerattr <- outerattr[names(outerattr) != "names"]   # removes the one name "g"
          st <- flattenstep(st[[1]])    # fusing only-child to parent
          innerattr <- attributes(st)   # compared to .kanjivg_to_list we do not have to call 
          # innerattr <- innerattr[names(innerattr) != "names"] 
          # only-child attributes become the "main" attributes 
          temp <- regexpr("^(p\\.)+", names(innerattr))
          repe <- max(c(0, attr(temp, "match.length")))/2 # 0 because if there is no match everything is -1
          names(outerattr) <- paste0( paste0(rep("p.",repe+1), collapse=""), names(outerattr))
          attributes(st) <- c(outerattr, innerattr)
        } else {
          st[[1]] <- flattenstep(st[[1]])  # no fusing
        }
      } else { # is.list(st) but length(st) != 1
        n <- length(st)
        stopifnot(n > 1)
        for (i in 1:n) {
          st[[i]] <- flattenstep(st[[i]])  # no fusing
        }
      }
    }  # eof is.list(st) otherwise do nothing (leaf)
    return(st)
  }
  res <- flattenstep(stree)
}


# function tries to get a working function for use in plots (used in kanjimat.plot and kanjivec.plot with type="dend")
handle_font <- function(family) {   # currently only called for family = NULL. In the long run we can maybe alternatively
                                    # pass a character vector that is checked in sequence until a usable font is found
  if (is.null(family)) {
    default_font <- get_kanjistat_option("default_font")
    if (is.null(default_font)) {
      rlang::warn("No font family specified. Characters will be represented in the CJK font WenQuanYi Micro Hei
that is included in the package showtext. The font targets Chinese writing and some strokes will
therefore look odd for Japanese kanji. It is strongly advised that you register a Japanese font.
See help with Japanese font.", .frequency = "regularly", .frequency_id = "no_font_family")
      family <- "wqy-microhei"
    } else {
      family <- default_font
    }
  } else if (!(family %in% sysfonts::font_families())) {
    stop("Specified font family ", family, " not found. Make sure to add it first with sysfonts::font_add")
  }
  return(family)
}


# given a (n x 2)-matrix of x and y coordinates compute the length of the traverse (Streckenzug)
strokelength <- function(smat) {
  dcoord <- diff(smat)
  length <- sum(apply(dcoord, 1, \(d) {sqrt(d[1]^2 + d[2]^2)}))
  length
}


