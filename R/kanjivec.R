#' Create kanjivec objects from kanjivg data
#'
#' Create a (list of) kanjivec object(s). Each object is a representation of the kanji as a tree of strokes 
#' based on .svg files from the KanjiVG database containing further, derived information.
#' 
#' @param kanji a (vector of) character string(s) of one or several kanji. 
#' @param database the path to a local copy of (a subset of) the KanjiVG database. It is expected
#'        that the svg files reside at this exact location (not in a subdirectory). If \code{NULL},
#'        an attempt is made to read the svg file(s) from the KanjiVG GitHub repository (after
#'        prompting for confirmation, which can be switched off via the \link[=kanjistat_options]{option}
#'        \code{ask_github}).
#' @param flatten logical. Should nodes that are only-children be fused with their parents?
#'        Alternatively one of the strings "intelligent", "inner" or "leaves". Although the first is the default
#'        it is experimental and the precise meaning will change in the future; see details.
#' @param bezier_discr character. How to discretize the Bézier curves describing the strokes. One of
#'        "svgparser" uses code from the svgparser package for discretizing at equal time steps (this was
#'        the standard prior to kanjistat 0.12). "eqtimed" and "eqspaced" use new code that allows for more
#'        customization. The former creates discretization points at equal time steps, the latter at (to a
#'        good approximation) equal distance steps. At a later point, a vector of several choices will be
#'        possible (not yet implemented).
#' @param save logical or character. If FALSE return the (list of) kanjivec object(s). Otherwise save the result
#' as an rds file in the working directory (as kvecsave.rds) or under the file path provided.
#' @param overwrite logical. If FALSE return an error (before any computations are done) if the designated 
#' file path already exists. Otherwise an existing file is overwritten.
#' @param simplify logical. Shall a single kanjivec object be returned (instead a list of one) if \code{kanji} 
#' is a single kanji?
#'
#' @details A kanjivec object contains detailed information on the strokes of which an individual kanji
#'          is composed including their order, a segmentation into reasonable components ("radicals" in a
#'          more general sense of the word), classification of individual strokes, and both 
#'          vector data and interpolated points to recreate the actual stroke in a Kyoukashou style font.
#'          For more information on the original data see \url{http://kanjivg.tagaini.net/}. That data
#'          is licenced under Creative Commons BY-SA 3.0 (see licence file of this package).
#'
#' @details The original .svg files sometimes contain additional `<g>` elements that provide
#'          information about the current group of strokes rather than establishing a new subgroup
#'          of its own. This happens typically for information that establishes coherence with another
#'          part of the tree (by noting that the current subgroup is also part 2 of something else), 
#'          but also for variant information. With the option \code{flatten = TRUE} the extra hierarchy
#'          level in the tree is avoided, while the original information in the KanjiVG file is kept.
#'          This is achieved by fusing only-children to their parents, giving the new node the name
#'          of the child and all its attributes, but prefixing \code{p.} to the attribute names
#'          of the parent (the parents' "names" attribute is discarded, but can be reconstructed from
#'          the parents' id). Removal of several hierarchies in sequence can lead to attribute names
#'          with multiple \code{p.} in front. Fusing to parents is suppressed if the parent is the
#'          root of the hierarchy (typically for one-stroke kanji), as this could lead to confusing
#'          results. 
#'
#' @details The options \code{flatten = "inner"} and \code{flatten = "leaves"} implement the above behavior 
#'          only for the corresponding type of node (inner nodes or leaves). The option
#'          \code{flatten = "intelligent"} tries to find out in more sophisticated ways which flattening
#'          is desirable and which is not (it will flatten rather conservatively). Currently nodes without
#'          an element attribute that have only one child are flattened away (one example where this is
#'          reasonable is in kanji \code{kbase[187, ]}), as are nodes with an element attribute and only
#'          one child if this child is also an inner node and has the same element and part attribute as the
#'          parent, but both have no number (this would be problematic for any component-building code
#'          in the particular case of kanji \code{kbase[1111, ]}). 
#'          
#' @details A \code{kanjivec} object has components 
#'          \describe{
#'            \item{\code{char}}{the kanji (a single character)}
#'            \item{\code{hex}}{its Unicode codepoint (integer of class \code{hexmode})}
#'            \item{\code{padhex}}{the Unicode codepoint padded with zeros to five digits (mode character)}
#'            \item{\code{family}}{the font on which the data is based. Currently only "schoolbook" (to be extended with "kaisho" at some point)}
#'            \item{\code{nstrokes}}{the number of strokes in the kanji}
#'            \item{\code{ncompos}}{a vector of the number of components at each depth of the tree}
#'            \item{\code{nveins}}{the number of veins in the component structure}
#'            \item{\code{strokedend}}{the decomposition tree of the kanji as an object of class \code{dendrogram}}
#'            \item{\code{components}}{the component structure by segmentation depth (components can overlap) in terms
#'                              of KanjiVG elements and their depth-first tree coordinates}
#'            \item{\code{veins}}{the veins in the component structure. Each vein is represented as a two-column matrix
#'                              that lists in its rows the indices of \code{components} (starting at the root,
#'                              which in the component indexing is \code{c(1,1)})}
#'            \item{\code{stroketree}}{the decomposition tree of the kanji, a list containing the full information of the
#'                              the KanjiVG file (except some top level attributes)}
#'          }
#'
#' @details \code{stroketree} is a close representation of the KanjiVG svg file as list object with
#'          some serious nesting of sublists. The XML attributes become attributes of the list and its elements. 
#'            The user will usually not have to look at or manipulate \code{stroketree} directly, but 
#'            \code{strokedend} and \code{compents} are derived from it and other functions may process it
#'            further.
#'          
#' @details The main differences to the svg file are 
#'          \enumerate{
#'            \item the actual strokes are not only given as d-attributes describing Bézier curves
#'                      but also as two-column matrices describing discretizations of these curves. These matrices
#'                      are the actual contents of the innermost lists in \code{stroketree}, but are more conveniently
#'                      accessed via the function \code{\link{get_strokes}}.
#'            \item The positions of the stroke numbers (for plotting) are saved as an attribute strokenum_coords
#'                      to the entire stroke tree rather than a separate element.
#'          }
#'
#' @details \code{strokedend} is more easy to examine and work with due to various convenience functions for
#'          dendrograms in the packages \code{stats} and \code{\link{dendextend}}, including \code{\link[utils]{str}}
#'          and \code{\link[stats]{plot.dendrogram}}. The function \code{\link{plot.kanjivec}} with option
#'          \code{type = "dend"} is a wrapper for \code{\link[stats]{plot.dendrogram}} with reasonable presets
#'          for various options.
#'          
#' @details The label-attributes of the nodes of \code{strokedend} are taken from the element (for inner nodes)
#'          and type (for leaves) attributes of the .svg files. They consist of UTF-8 characters representing
#'          kanji parts and a combination of UTF-8 characters for representing strokes and may not represent
#'          well in all CJK fonts (see details of \code{\link{plot.kanjivec}}). If element and type are missing
#'          in the .svg file, the label assigned is the second part of the id-attribute, e.g. g5 or s9.
#'          
#' @details The \code{components} at a given level can be plotted, see \code{\link{plot.kanjivec}} with
#'          \code{type = "kanji"}. Both \code{components} and \code{veins} serve mainly for the computation
#'          of \link[=kanjidist]{kanji distances}.
#'          
#' @return A list of objects of class \code{kanjivec} or, if only one kanji was specified and
#' \code{simplify} is \code{TRUE}, a single objects of class \code{kanjivec}. If \code{save = TRUE},
#' the same is (saved and) still returned invisibly.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Try to load the svg file for the kanji from GitHub.
#'   res <- kanjivec("藤", database=NULL)
#'   str(res)
#' }
#'
#' fivebetas  # sample kanjivec data
#' str(fivebetas[[1]])
#' 
#' @seealso \code{\link{plot.kanjivec}}, \code{\link{str.kanjivec}}
#' 
kanjivec <- function(kanji, database=NULL, flatten="intelligent", bezier_discr=c("svgparser", "eqtimed", "eqspaced"),
                     # default for flatten went from TRUE in 2022, to FALSE in Jan 2023
                     # to "intelligent" in Feb 2023 (about when components and veins where
                     # added to kanjivec objects)
                     # TRUE is mainly interesting for getting a simplified overview when 
                     # of the KanjiVG structure when plotting with type="dend", but in
                     # complicated kanji with many split components plots are confusing.
                     save=FALSE, overwrite=FALSE, simplify=TRUE) {
  
  bezier_discr <- match.arg(bezier_discr, choices=c("svgparser", "eqtimed", "eqspaced") , several.ok = TRUE)
  if (length(bezier_discr) != 1) stop("Multiple bezier_discr arguments not yet implemented")
  callstring <- paste(deparse(sys.call(), width.cutoff = 100L), collapse = "")
  
  if (is.null(database)) {
    github_ok <- FALSE
    if (isFALSE(get_kanjistat_option("ask_github"))) {  # if ask_github is weird, do not just download (but prompt below)
      github_ok <- TRUE
    } else if (interactive()) {
      ans <- readline("No database supplied. Is it OK to read svg files for the specified kanji from the KanjiVG database on GitHub? (y/n) ")
      if (ans == "y" || ans == "Y" || ans == "yes" || ans == "Yes") {
        github_ok <- TRUE
      }
    }
    
    if (github_ok) {
      database <- "https://raw.githubusercontent.com/KanjiVG/kanjivg/master/kanji/"
      # the directory is blocked and works only as soon as we paste an actual filename
    } else {  
      stop("No database supplied. Consider downloading the desired svg files from https://github.com/KanjiVG/kanjivg and supply the path via the parameter `database`")
    }
  }

  if (stringr::str_sub(database, -1, -1) != "/")  database <- paste0(database, "/")  
  # if there is no trailing /, add one --> apparently (on most systems?) a double "/" is just ignored 
  # database <- gsub("/$", "", database)  # remove trailing / if there is one
  
  stopifnot(is.logical(flatten) || (is.character(flatten) && flatten %in% c("intelligent", "none", "inner", "leaves", "leaf", "outer", "all")) )
  flatten_inner <- FALSE
  flatten_leaves <- FALSE
  if (isTRUE(flatten) || flatten %in% c("inner", "all")) {
    flatten_inner <- TRUE
  }
  if (isTRUE(flatten) || flatten %in% c("leaves", "leaf", "outer", "all")) {
    flatten_leaves <- TRUE
  }
  # "intelligent" and "none" are the same for now ("intelligent" does post-flattening below)

  # if save is anything else than FALSE, construct and check savepath 
  if (!isFALSE(save)) {
    if (isTRUE(save)) {
      savepath = file.path(getwd(), paste0("kvecsave", ".rds"))
    } else {
      #if (grepl("/", save, fixed = TRUE)) 
      if (stringr::str_sub(save, -4, -1) != ".rds") save <- paste0(save, ".rds")
      savepath = save
    }
    if (!overwrite && file.exists(savepath)) {
      stop("File ", savepath, " already exists.")
    } 
  }
  
  temp <- paste(kanji, collapse="")
  nkan <- nchar(temp)
  stopifnot(nkan >= 1)
  kan <- as.list(strsplit(temp, split="")[[1]])
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  kanjivec1 <- function(kan1) {
    hex <- kanjiToCodepoint(kan1)
    padhex <- stringr::str_pad(as.character(hex), width=5, pad="0")
    # it seems there are exactly 2 codepoints in kanjivg with 5 digits (in hex)
    # intToUtf8(as.hexmode("26951")) and intToUtf8(as.hexmode("27491"))
    # the second one does not display in my console (nor anywhere else I tried ;-)
    # Terminology: "Unicode code point / codepoint", but "utf-8 (or utf-16 a.s.o.) code units" 
    # see e.g. https://exploringjs.com/impatient-js/ch_unicode.html
    res1 <- list(char=kan1, hex=hex, padhex=padhex, family="schoolbook")
    
    xmldat <- xml2::read_xml(paste0(database, padhex, ".svg"))
    # it seems using paste0 rather than filepath is the easiest way(?) to account for the fact
    # that the user might have a another path separator than "/" on her system (does that exist?? apparently not even under windows)
    # but that when loading data from kanjivg on github we have to use "/".
    res1$stroketree <- .kanjivg_to_list(xmldat, padhex=padhex, char=kan1, bezier_discr = bezier_discr,
                                        flatten_inner=flatten_inner, flatten_leaves=flatten_leaves)
    if (flatten == "intelligent") {
      res1$stroketree <- flatten_intel(res1$stroketree)
    }
    res1$nstrokes <- dim(attr(res1$stroketree, "strokenum_coords"))[1]
    
    res1$strokedend <- stroketree_to_dend(res1$stroketree)
    temp <- stroketree_to_components(res1$stroketree, cover=TRUE)
    res1$components <- temp$compos
    res1$ncompos <- vapply(res1$components, length, 0)  # vapply because res1$components can be list()
      # (in fact no longer true as we added the kanji itself to components; I still find the vapply good style)
    res1$veins <- temp$veins
    res1$nveins <- length(res1$veins)
        
    res1 <- res1[c(1,2,3,4,6,9,11,7,8,10,5)]  # this removes all the attributes of res1 (--> we add attributes below)
    
    attr(res1, "call") <- callstring
    attr(res1, "kanjistat_version") <- packageVersion("kanjistat")
    class(res1) <- "kanjivec"
    
    return(res1)
  }
  
  res <- lapply(kan, kanjivec1)
  
  if (nkan == 1 && isTRUE(simplify)) {
    res <- res[[1]]
  } else {
    padhex <- sapply(res, \(x) {x$padhex})
    names(res) <- paste0("kvec", padhex)
  }
  
  if (isFALSE(save)) {
    return(res)
  } else {
    saveRDS(res, file=savepath)
    invisible()
  }
}



#' Plot kanjivec objects
#'
#' @param x an object of class \code{\link{kanjivec}}
#' @param type either "kanji" or "dend". Whether to plot the actual kanji, coloring strokes
#'        according to levels of segmentation, or to plot a representation of the tree structure
#'        underlying this segmentation. Among the following named parameters, only \code{family} is
#'        for use with \code{type = "dend"}; all others are for \code{type = "dend"}.
#' @param seg_depth an integer. How many steps down the segmentation hierarchy we use
#'        different colors for different groups. If zero (the default), only one color is used
#'        that can be specified with \code{col} passed via \code{...} as usual
#' @param palette a valid name of a hcl palette (one of \code{hcl.pals()}). Used for coloring the 
#'        components if \code{seg_depth} is \eqn{>0}. 
#' @param pal.extra an integer. How many extra colors are picked in the specified palette.
#'        If this is 0 (the default), palette is used with as many colors as we have components.
#'        Since many hcl palettes run from dark to light colors, the last (few) components may
#'        be too light. Increasing pal.extra then makes the component colors somewhat more similar,
#'        but the last component darker.
#' @param numbers logical. Shall the stroke numbers be displayed. 
#' @param offset the (x,y)-offset for the numbers relative to the positions from kanjivg saved
#'        in the kanjivec object. Either a vector of length 2 specifying some fixed offset for
#'        all numbers or a matrix of dimension kanjivec$nstrokes times 2.
#' @param family the font-family for labeling the nodes if \code{type = dend}. See details.
#' @param lwd the usual line width graphics parameter.
#' @param ... further parameters passed to \code{lines} if \code{type = "kanji"} and to \code{plot.dendrogram}
#'        if \code{type = "dend"}.
#'        
#' @details Setting up nice labels for the nodes if \code{type = "dend"} is not easy. For many 
#'          font families it appears that some "kanji components" cannot be displayed in plots
#'          even with the help of package \code{showtext} and if the
#'          font contains glyphs for the corresponding codepoints that display correctly in text documents.
#'          This concerns in increasing severity of the problem Unicode blocks 2F00–2FDF (Kangxi Radicals),
#'          2E80–2EFF (CJK Radicals Supplement) and 31C0–31EF (CJK Strokes). For the strokes it seems
#'          nearly impossible which is why leaves are simply annotated with the number of the strokes.
#'
#' @details For the other it is up to the user to find a suitable font and pass it via the argument
#'          font family. The default \code{family = NULL} first tries to use \code{default_font}
#'          if this option has been set (via \code{\link{kanjistat_options}}) and otherwise
#'          uses \code{wqy-microhei}, the Chinese default font that comes with package \code{showtext}
#'          and cannot display any radicals from the supplement.
#'          
#' @details On a Mac the experience is that "hiragino_sans" works well. In addition there is the issue of 
#'          font size which is currently not judiciously set and may be too large for some (especially
#'          on-screen) devices. The parameter \code{cex} (via \code{...}) fixes this.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' kanji <- fivebetas[[2]]
#' plot(kanji, type = "kanji", seg_depth = 2)
#' plot(kanji, type = "dend")  
#'   # gives a warning if get_kanjistat_option("default_font") is NULL
#' 
# For colors we just pick a default from grDevices::hcl.pals()
# plot(kanji, 2, col=list("darkblue",c("darkgreen", "chartreuse")))}
plot.kanjivec <- function(x, type=c("kanji", "dend"), seg_depth=0, palette = "Dark 3", pal.extra = 0,
                          numbers=FALSE, offset=c(0.025,0), family=NULL, lwd=8, ...) {
  kanji <- x
  type <- match.arg(type)
  
  if (type == "dend") {
    plotdend(kanji$strokedend, family = family, ...)
    return(invisible())
  }
  
  plot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1), axes=FALSE, type="n", asp=1, xaxs="i", yaxs="i", xlab="", ylab="")
  plotcompos(kanji, components=NULL, seg_depth=seg_depth, add=TRUE, palette = palette, 
             pal.extra = pal.extra, pal.random = FALSE, lwd=lwd, ...)

  if (numbers) {
    coords <- attr(kanji$stroketree,"strokenum_coords")
    if (length(offset) == 2) {
      coords <- coords + cbind(rep(offset[1], kanji$nstrokes), rep(offset[2], kanji$nstrokes))
    } else {
      stopifnot(dim(offset) == c(kanji$nstrokes, 2))
      coords <- coords + offset
    }
    text(coords, cex=1.2)
    # as a general rule the stroke numbers are not at their most aesthetic positions  
    # we fix this *somewhat* by moving them all by 0.025 to the right
  }
  invisible()
}


#' Print basic information about a kanjivec object
#'
#' @param x an object of class \code{kanjivec}.
#' @param dend whether to print the structure of the \code{strokedend} component.
#' @param ... further parameters passed to \code{print.default}.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
print.kanjivec <- function(x, dend=FALSE, ...) {
  cat("Kanjivec representation of ", x$char, " (Unicode ", as.character(x$hex), ")\n", sep="")
  # cat(x$nstrokes, " vector graphics in ", x$family, " font with ", attr(x$strokedend, "height"), " decomposition", sep="")
  cat(x$nstrokes, " stroke vector graphics with depth ", attr(x$strokedend, "height") - 1, " decomposition\n", sep="")
    # we do -1 because we call it decomposition (e.g. one would usually expect that a level 1 decomposition does indeed decompose once
    # more importantly the number here (with -1) corresponds then to the highest reasonable number to choose for
    # seg depth when plotting
  if (dend) str(x$strokedend)
  # family here declared as font, because for practical reason we often save the
  # concrete fontface in family, so it is good/ok to use a bit a blurry term here
  # but currently no font family
  invisible()
}


#' Compactly display the structure of a kanjivec object
#'
#' @param object an object of class \code{kanjivec}.
#' @param ... further parameters passed to \code{str} for all but the \code{stroketree} component 
#'   of \code{object}.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
str.kanjivec <- function(object, ...) {
  n <- length(object)
  nam <- names(object)
  ll <- list(...)    # the arguments in ... *cannot* be used without this (only passed on)  
  if (length(ll) && any("nest.lev" == names(ll))) 
    nest.lev <- ll$nest.lev else nest.lev <- 0
  if (length(ll) && any("indent.str" == names(ll))) 
    indent.str <- ll$indent.str else indent.str <- paste(rep.int(" ", max(0, nest.lev + 1)), collapse = "..")
  if (length(ll) && any("comp.str" == names(ll))) 
    comp.str <- ll$comp.str else comp.str <- "$"

  if (!(n == 11 && all(nam == c("char", "hex", "padhex", "family", "nstrokes", "ncompos", "nveins",
                               "strokedend", "components", "veins", "stroketree")))) {
    warning("kanjivec objects is incomplete or corrupted")
  }

  test <- nam %in% c("components", "veins", "stroketree")
  
  cat("'kanjivec':\t", n, " components:", "\n", sep = "")
  invisible(str(object[!test], no.list=TRUE, ...))
  # the order of the following output will not be in the right order if the object is malformed, but it shouldn't be
  if ("components" %in% nam) {
    cat(indent.str, comp.str, " components: ", "(list of components organized by level)\n", sep = "")
  }
  if ("veins" %in% nam) {
    cat(indent.str, comp.str, " veins: ", "(list of component coordinates from root to last inner node)\n", sep = "")
  }
  if ("stroketree" %in% nam) {
    cat(indent.str, comp.str, " stroketree: ", "(nested list with original kanjivg info)\n", sep = "")
  }
  
  invisible()
}


#' Get the strokes of a kanjivec object
#' 
#' The strokes are the leaves of the kanjivec \code{stroketree}. They consist of
#' a two-column matrix giving a discretized path for the stroke in the unit square \eqn{[0,1]^2}
#' with further attributes.
#'
#' @param kvec an object of class \code{kanjivec}
#' @param which a numeric vector specifying the numbers of the strokes that are to be returned.
#'    Defaults to all strokes.
#' @param simplify logical. Shall only the stroke be returned if \code{which} has length 1? 
#'
#' @return Usually a list of strokes with attributes. Regardless of
#' whether \code{which} is ordered or contains duplicates, the returned list will always contain
#' the strokes in their natural order without duplicates. If \code{which} has length 1 and
#' \code{simplified = TRUE}, the list is avoided, and only the single stroke is returned.
#' @export
#'
#' @examples
#' kanji <- fivebetas[[5]]
#' get_strokes(kanji, c(3,10))    # the two long vertical strokes in 陣
#' 
#' @seealso \code{\link{get_strokes_compo}}
#'
get_strokes <- function(kvec, which=1:kvec$nstrokes, simplify=TRUE) {
  stopifnot(is(kvec, "kanjivec"))
  which <- sort(which)
  L <- length(which)
  stopifnot(L == 0 || all(which >= 1 & which <= kvec$nstrokes))
  # L=0 is handled correctly by st_get_strokes
  
  res <- st_get_strokes(kvec$stroketree, which=which, check_order=TRUE)
  
  if (simplify && L==1) {
    return(res[[1]])
  } else {
    return(res)
  }
}


#' Get the strokes of a specific component of a kanjivec object
#'
#' The strokes are the leaves of the kanjivec \code{stroketree}. They consist of
#' a two-column matrix giving a discretized path for the stroke in the unit square \eqn{[0,1]^2}
#' with further attributes.
#'
#' @param kvec an object of class \code{kanjivec}
#' @param which a vector of length 2 specifing the index of the component, i.e. the component used
#'              is \code{pluck(kvec$components, !!!which)}. The default \code{c(1,1)} refers
#'              to the root component (full kanji), so all strokes are returned.
#'
#' @return A list of strokes with attributes. 
#'
#' @export
#'
#' @examples
#' kanji <- fivebetas[[5]]
#' # get the three strokes of the component⻖ in 陣
#' rad <- get_strokes_compo(kanji, c(2,1))    
#' plot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1), type="n", asp=1, xaxs="i", yaxs="i", xlab="", ylab="")
#' invisible(lapply(rad, lines, lwd=4))
#' 
#' @seealso \code{\link{get_strokes}}
#'
# (needed for kanjidist and appears more generally useful)
get_strokes_compo <- function(kvec, which = c(1,1)) {  
  if (length(which) != 2) {
    stop("Which should be a vector of length 2 specifying level and number of the component to choose.")
  }
  
  coordlist <- pluck(kvec$components, !!! which)[-c(1,2)]
  
  res <- list()
  for (coord in coordlist) {
    subtree <- pluck(kvec$stroketree, !!! coord)
    res <- c(res, st_get_strokes(subtree))
  }

  names(res) <- sapply(res, \(x) {as.numeric(substr(attr(x, "id"), 12, 14))})
  
  return(res)
}


# gets strokes from stroketree (or a subtree)
# (this will probably not get used unless which = "all" or stree is the full stroketree of a kanji
# (helper for get_strokes_compo, just above)
st_get_strokes <- function(stree, which = "all", check_order = FALSE) {  
  if (length(which) == 0) {
    return(list())
  }
  res <- list()
  k <- 1  # k counts from 1 to no of leaves in stree (leaves encountered)
  l <- 1  # l counts from 1 to which (pos in res filled)
  L <- length(which)  # only used for which != "all" 
  
  read_leaves <- function(st) {
    n <- length(st)
    if (is.list(st) && n > 0) {
      for (i in 1:n) {
        read_leaves(st[[i]])
      }
    } else {
      # if (check_order && k != as.numeric(substr(attr(st, "id"), 12, 14))) 
      #   warning("id tags of strokes are not in depth-first order")
      # due to malformed svg or bug in kanjivec, but neither should happen
      if (all(which == "all") || (l <= L && k == which[l])) {   # both 'which == "all"' and 'l <= L' are needed
                                                      # because later parts of the condition complain otherwise
        res[[l]] <<- st
        l <<- l + 1
      }
      k <<- k + 1
    }
    invisible()
  } 
  read_leaves(stree)
  
  return(res)
}



#' Transform a stroke tree to a stroke dendrogram
#' 
#' The original svg data contained in a kanjivec object
#' has the form of a nested list with a lot of attributes. This function converts this list in an object
#' of class \code{dendrogram}, dropping most of the attributes and the matrices at the lowest list levels.
#'
#' @param stroketree the stroketree component of a kanjivec object or any of its sublists.
#'
#' @details This function is used internally for computing the \code{strokedend} component of a
#'   kanjivec object. It may be useful to the user for investigating the subtrees produced by 
#'   \code{\link{get_subtrees}} via the functions \code{\link{plotdend}} and \code{\link{str.dendrogram}}.
#'   .
#' @return An object of class \code{dendrogram}.
#' 
#' @noRd
#'
# (needed for kanjivec)
stroketree_to_dend <- function(stroketree) {
  maxdepth <- -1  # counts number of nestings 
  prepare_all1 <- function(st, ndepth) {
    element <- attr(st, "element") # is NULL for leaves and sometimes in other circumstances (e.g. g5 in 0969c.svg)
    type <- attr(st, "type")       # is NULL for non-leaves and maybe in other circumstances (?)
    id <- strsplit(attr(st, "id"), "-")  # reads out the s1, s2, ... and g1, g2, ... on from id
    id <- id[[1]][2]             # uppermost list has NA here, but we don't care 
    if (is.null(element)) element <- id   # functions that operate on dendrograms do not react well to missing element or type
    if (is.null(type)) type <- id         # we fill in both although only one is used per node (to easily spot further problems)
    attributes(st) <- NULL
    n <- length(st)
    if (is.list(st) && n > 0) {
      for (i in 1:n) {
        st[[i]] <- prepare_all1(st[[i]], ndepth+1)
      }
      attr(st, "label") <- element
      attr(st, "members") <- sum(sapply(st, \(x) {attr(x, "members")}))
      attr(st, "depth") <- ndepth # to be corrected in second go
    } else {
      st <- as.numeric(substr(id, 2, 4))  # remove initial s
      # cat(st, ", ", sep="") # just as a check for now (outputs should be ordered) --> checked out always
      maxdepth <<- max(maxdepth, ndepth)
      attr(st, "label") <- type   # 
      attr(st, "members") <- 1
      attr(st, "depth") <- ndepth # to be corrected in second go
      attr(st, "leaf") <- TRUE
    }
    attr(st, "maxdepth") <- maxdepth
    st
  } 
  res1 <- prepare_all1(stroketree, ndepth=0)   # all the data wrangling except for
  
  # we need to pass the nested a second time to transform depth to height (with the help of maxdepth)
  maxdepth <- attr(res1, "maxdepth")
  prepare_all2 <- function(st) {
    n <- length(st)
    if (is.list(st) && n > 0) {
      for (i in 1:n) {
        st[[i]] <- prepare_all2(st[[i]])
      }
      attributes(st) <- list(label = attr(st, "label"), members = attr(st, "members"), height = maxdepth - attr(st, "depth"))
      # depth -> height and remove attribute maxdepth (which need not have the value of the global one anyway)
    } else {
      attributes(st) <- list(label = attr(st, "label"), members = attr(st, "members"), 
                            height = maxdepth - attr(st, "depth"), leaf = attr(st, "leaf"))
      # depth -> height and remove attribute maxdepth (which need not have the value of the global one anyway)
    }
    st
  } 
  res2 <- prepare_all2(res1)
  
  class(res2) <- "dendrogram"
  return(res2)
}



#'
#' The output gives an element-centered view of the stroketree data. The elements are the inner nodes
#' of the stroketree, which ultimately come from the element attribute in the kanjivg data. In the
#' stroketree elements may be split (according to stroke order) over different parts of the tree. The 
#' output organizes elements according to their depth in the tree and contains the (depth first) tree
#' coordinates of each of its parts. 
#'
#' @param stree a stroketree, usually the component a kanjivec object by that name.
#' @param cover logical. Should the elements cover (essentially) all strokes at all depths of the tree.
#'   This will copy elements whose decomposition into further \emph{inner nodes} is finished to all
#'   higher depths (except the one corresponding to its \code{coord1} value if a later part is finished).
#'
#' @details Note that elements at the a given level may overlap (this happens sometimes at higher depths)
#'   and will usually not cover all strokes. If \code{cover=FALSE}, all strokes below an inner node on that level
#'   are covered. If \code{cover=TRUE}, in addition all strokes below a last inner node at a higher level
#'   (lower depth) are covered. In particular, if an inner node at a higher level has leaves and further
#'   inner nodes, these leaves are not covered.
#'
#' @return A list with as many components as there are depths with inner nodes in the stroketree. Each
#' component is itself a list of all the elements at the respective depth (and the last inner node of
#' discontinued branches at lower depth if \code{cover=TRUE}). Each such element is a list with components
#'          \describe{
#'            \item{\code{elid}}{the element as represented in the stroketree (or the attribute element of kanjivg).}
#'            \item{\code{parts}}{the number of parts of the element in the stroketree}
#'            \item{\code{coord1,coord2,...}}{the depth-first coordinates of the root of the subtree describing the element. 
#'                                            There are as many coordX entries as their are parts}
#'          }
#'          
#' @examples
#'   # To do
#'
#' @noRd
# (needed for kanjivec)
stroketree_to_components <- function(stree, cover=TRUE) { # assumed to be unflattened/intelliflattened
  get_element <- attr_getter("element")
  get_id <- attr_getter("id")
  get_part <- attr_getter("part")
  get_number <- attr_getter("number")
  get_real <- attr_getter("real")
  get_original <- attr_getter("original")
  
  last_inner_ind <- numeric()  # collects indices in terms of res of nodes that only have leaves as children (for cover=TRUE)
  last_inner_coord <- list()  # we also save the coords because it is not uniquely identified for the fused entries in res
  last_iforgotten_coord <- list()  # the coords of inner nodes that have "forgotten leaves" attached to them
                                   # this happens either if there is a mix of leaf children and inner node childrens
                                   # or if a higher part has (only) leaf children, but the first part of the same
                                   # element has (only or also) inner node children (the higher parts are not conisdered in
                                   # last_inner_ind, because this would lead to adding the same element multiple times if cover=TRUE)
  raw_labels <- character()  # collects all component labels without numbers as they appear in res
  # (in order to achieve uniqueness of the labels in the final component list) 
  res <- list()   # collects elements
  veins <- list()   # veins are in terms of the index l for res (have to transform them afterwards (or not))
    # everybody has local version of vein (called loc_vein below), as soon as a leaf is reached the finalized vein is added to the general list 
  forgottenveins <- list()  # veins that end in components made up of one to several leaves that are children of a node which also has inner nodes as children
                            # these veins cannot be tracked in veins as their final component is only generated in the
                            # second "if (cover)" below (outside read_elid) 
  k <- 1  # counter for last_inner
  kf <- 1 # counter for last_iforgotten
  l <- 1  # counter for res
  
  read_elid <- function(coord, loc_vein) {
    cur_tree <- pluck(stree, !!! coord)
    n <- length(cur_tree)
    if (is.list(cur_tree) && n > 0) { # if we are not at a leaf (is.list = TRUE and n=0 should not be possible);
      label <- pluck(stree, !!! coord, get_element)
      num <- pluck(stree, !!! coord, get_number)
      if (!is.null(num)) {
        label <- paste0(label, "n", num)
      }
      # from kanjiVG website: "number: This relatively rare attribute allows an element of a kanji to be identified
      # when it is both represented several times in the kanji, and, due to the stroke order, more than one of these
      # representations is broken into parts, so that the part attribute has to be used for more than one element.
      # In other words, the number attribute is a way to uniquely identify the part when it becomes ambiguous." 
      # The first part is not entirely accurate it seems, e.g. 歌 (kbase[187, ]) has no number attribute although
      # it has two "丁"s each in two parts. It seems the numbers argument is only present if the parts of one element
      # are not finished when the other one starts.
      # We solve this as follows. If a number argument is present turn label X in Xn1, Xn2 and so on. By this parts
      # are completely identified. Otherwise we assign all following parts to the first element that is there
      # and open up a second one, relabeling the former X to X1 (if it was the first) and the current X to X2 only
      # if another part 1 occurs (for later parts we proceed in the analog way).
      
      if (length(label) == 0) {  # it is not 100% clear whether we shouldn't fuse (i.e. flatten)
        # elements without names (they often are a bit strange/arbitrary, but may 
        # sometimes be useful; at the very least those that have an only, see e.g.
        # the g1 in 歌 = kbase[187, ]
        id <- pluck(stree, !!! coord, get_id)
        temp <- strsplit(id, "-")  # reads out the g1, g2, ... on from id
        label <- temp[[1]][2]
      }
      
      part <- as.numeric(pluck(stree, !!! coord, get_part))
      part <- ifelse(is.null(part), NA, part)
      
      ind <- which(label == raw_labels)   # where has label already occurred (in terms of current res)
      # note: I think one could always just take the previous occurrence if a higher part comes up
      # (repeated occurrences will be numbered in cases where this would otherwise not work)
      # However we check for previous occurrences in order to assign extra numbers of the form X1, X2
      # for repetitions of elements without number attribute
      nind <- length(ind)
      if (nind == 0) {  # label has not occurred
        res[[l]] <<- list(label=label, parts=part, coord1=coord)
        last_updated <- l
        loc_vein <- c(loc_vein, l)
        if(all(sapply(cur_tree, \(x) {!is.list(x)}))) {  # only leafs at the next level
          veins <<- c(veins, list(loc_vein))  # add current vein (giving seq of l-indices from root to last inner node) to veins 
        }
        l <<- l + 1
        raw_labels <<- c(raw_labels, label)
      } else if (is.na(part) || (!is.na(part) && part == 1)) {  
        # label has occurred, but this is standalone element or a first part
        # ---> no joining of coordinates needed, but increase number in the label
        # (to also assign a 1 to the first label we relabel every existing label 
        # (in most cases only one anyway))
        invisible(lapply(1:nind, \(x) {res[[ind[x]]]$label <<- paste0(label,x)}))
        rawlab <- label   # the one that is added to raw_labels below
        label <- paste0(label, nind+1)
        res[[l]] <<- list(label=label, parts=part, coord1=coord)
        last_updated <- l
        loc_vein <- c(loc_vein, l)
        if(all(sapply(cur_tree, \(x) {!is.list(x)}))) {  # only leafs at the next level
          veins <<- c(veins, list(loc_vein))  # add current vein (giving seq of l-indices from root to last inner node) to veins 
        } 
        l <<- l + 1
        raw_labels <<- c(raw_labels, rawlab)
      } else {
        stopifnot(part >= 2)   # just in case
        former_parts <- sapply(res[ind], \(x) {x$parts})  
        # we obtain NAs here if elements occur multiple times for each (may be more than one ?!)
        # previous occurrence that was not split into parts; we need the last one that has parts
        temp <- ind[!is.na(former_parts)]
        ind0 <- max(temp)  # largest index for which former_parts is not NA
        stopifnot( part == res[[ind0]]$parts + 1 )
        loc_vein <- c(loc_vein, ind0)  # otherwise veins that separate later (i.e. have extra strokes, such as
                                       # kanjivec[[287]]) have gaps
        if(all(sapply(cur_tree, \(x) {!is.list(x)}))) {  # only leaves at the next level
          veins <<- c(veins, list(loc_vein))  # add current vein (giving seq of l-indices from root to last inner node) to veins 
        } # this should not be necessary, but is for safety (no problem if a vein occurs twice I guess)
        res[[ind0]]$parts <<- part
        res[[ind0]] <<- c(res[[ind0]], list(coord))   # concatenation of lists (means just we add the coord)
        last_updated <- ind0
        m <- length(res[[ind0]])
        coordname <- paste0("coord", part)
        names(res[[ind0]])[m] <<- coordname
        # check if we lose any leaves in the process (i.e. leaves that are children of the
        # current later part that are not accounted for because the first part still has inner node children
        # e.g. a node marking shared strokes):
        # if (length(coord) == 4 && all(coord == c(1,3,1,1))) browser()   # kvec[[287]]
        if (all(!sapply(cur_tree, is.list)) && !(ind0 %in% last_inner_ind)) {
            # all children are leaves, but the res-entry is not already flagged for being last_inner
          last_iforgotten_coord[[kf]] <<- coord
          forgottenveins[[kf]] <<- loc_vein  # we need to add the leaves component created below
          kf <<- kf + 1
            # the leaves will be added as a separate component in the if (cover=TRUE) part below
            # that will be put onto last_inner_ind then in order to be inherited to lower levels
            # we treat this together with branching-off strokes that are collected into a component
            # as well.
        } 
      }
      
      if (cover) { # to make sure we keep track of all the leaves
        if (all(!sapply(cur_tree, is.list))) { # all children are leaves
          if (is.na(part) || (!is.na(part) && part == 1)) {  # taking up the first *two* cases above, we take out 
            # part >= 2, because otherwise elements with several parts would be added several times per level if cover=TRUE
            # This is not exactly the right thing to do. In a few cases the first part is 
            # itself succeeded by a later part of another component. Then the other component gets added,
            # but not the component with its second part here, e.g. kvec[[287]], 感, level 6. As a consequence
            # e.g. the compoweights_ink (bei strokelength) on this level (6 for 感) will not add up to one
            # SHOULD NOW BE SOLVED FURTHER BELOW
            last_inner_ind[k] <<- last_updated
            last_inner_coord[[k]] <<- coord
            k <<- k + 1
          }
        } else if (any(!sapply(cur_tree, is.list))) { # some but not all children are leaves --> forgotten leaves
          last_iforgotten_coord[[kf]] <<- coord
          forgottenveins[[kf]] <<- loc_vein  # we need to add the leaves component created below
          kf <<- kf + 1
        }
      }
      
      for (i in 1:n) { # if c(coord, i) is a leaf nothing further will happen, so this can be safely called for everybody
        read_elid(c(coord, i), loc_vein)
      }
      
    } 

    invisible()
  } 
  read_elid(coord=integer(0), loc_vein=numeric())
  # up to here we get all the elements as in the element ids of the svg

  if (length(res) == 0) {  # shortcut for very simple kanji that have only leaves (would give problem with sapply below) 
    return(list())
  }
  
  if (cover) {
    # first we add components to res2 made up of "forgotten leaves"
    # it seems easiest to treat each leaf of a single extra component (1 extra component =
    # all the direct leaves of a single last_iforgotten_coord entry) as a part. 
    # It might be that older more special functions that deal with parts will do something
    # weird, but the important point is reflected with this structure, namely that
    # that exactly the strokes of the component are subsumed under the part coordinates
    # (if we take coord last_iforgotten_coord[i], we get too many strokes in general)
    for (i in seq_len(kf-1)) {  # kf-1 = number of forgotten components (before copying)
      coord <- last_iforgotten_coord[[i]]
      children <- chuck(stree, !!!coord)
      leaves <- unname(which(sapply(children, \(x) {!is.list(x)})))
      nleaves <- length(leaves)
      res[[l]] <- list(label="gextra", parts=ifelse(nleaves > 1, nleaves, NA))
        # the label has to start with "g" because later code will never try to match labels
        # that start with g (originally gX labels come from a missing element attribute)
      for (j in seq_along(leaves)) {
        pluck(res, l, j+2) <- c(coord, leaves[j])
        names(res[[l]])[j+2] <- paste0("coord", j)
      }
      
      last_inner_ind[k] <- l
      last_inner_coord[[k]] <- c(coord, 1)  # 1 or any j does not matter
                      # only sapply(last_inner_coord, length) is used for sorting when adding to the linear
                      # res2 list (seems not important) but also to avoid that we copy beyond
                      # the deepest level with inner nodes (also not so important)
      
      loc_vein <- c(forgottenveins[[i]], l)
      veins <- c(veins, list(loc_vein))
      
      k <- k + 1
      l <- l + 1
    }
  }
  
  res <- lapply(res, \(x) {attr(x, "real") <- TRUE; x}) 
  nreal <- l-1   # number of real components
  
  coords2level <- function(x) {
    nn <- length(x)
    temp <- sapply(x[3:nn], length)
    return(min(temp))
  }
  alevel <- sapply(res, coords2level)  
  # levels of the elements, this is needed because with cover=TRUE, the assigned level
  # of the copied elements is no longer the minimal coord length
  aind <- length(alevel)
  mm <- max(alevel)  # = maximal level of an inner node 
  
  res2 <- res
  
  if (cover) {
    # "extend" discontinued branches to lowest level that has an inner node.
    # More precisely, elements that are not further decomposed on lower levels
    # get copied once for each lower level where there are still ANY inner nodes
    # (if the element comes in several parts, start from the top level, where decomposition
    # for that element stops and add a copy at every level except on the level that corresponds
    # to length(coord1) because there the element gets taken into account anyway
    # In a way this elements are now better taken care of than elements in parts that get (partly) forgotten
    # because they still have inner nodes farther down the tree, but only for part of their
    # strokes, whereas the other strokes are assigned to later parts of the element that are not in the list
    # 
    compolen <- sapply(last_inner_coord, length)
    wh <- which(compolen < mm)
    nn <- length(wh)
    compolen <- compolen[wh] 
    ordcl <- order(compolen)
    # spawn_coord <- last_inner_coord[wh][ordcl]   # coords of last inner sorted by length and without the ones with maximal length
    spawn_ind <- last_inner_ind[wh][ordcl]
    #
    for (i in seq_len(nn)) {  # nn may be 0
      newel <- res[[spawn_ind[i]]]     # which entry to copy
      attr(newel, "real") <- FALSE
      attr(newel, "original") <- spawn_ind[i]  # this will have to be changed when we sort the entries below (-> res3)
      ll <- length(newel)
      coordlevels <- sapply(newel[3:ll], length)
      whmin <- which.min(coordlevels)   # need not be unique but first min good enough
      mincoordlev <- coordlevels[whmin]
      mincoord <- newel[[whmin+2]]
      for (j in (mincoordlev+1):mm) {   # min(coordlevel)+1 > mm excluded above (each element has a compo with fewer than mm coords)
        if (j != length(newel$coord1)) {  # if there is no coord at this level 
          #  newel[[whmin+2]] <- c(mincoord, rep(NA, j-mincoordlev))   # not needed anymore
          res2 <- c(res2, list(newel))     # careful: res2 <- c(res2, newel) does *not* append newel to the list
          # because newel is a list itself R will fuse the two lists
          aind <- aind + 1
          alevel[aind] <- j
        }
      }
    }
  }
  
  # first sort according to alevel then sort within each level by depth first order
  nlist <- as.list(seq_along(res2)) # to keep track of order of the res2 elements
  res3 <- split(res2, alevel)
  res3 <- unname(res3)
  nlist <- split(nlist, alevel)
  
  depthfirstkey <- function(x) {
    nn <- length(x)
    temp <- sapply(x[3:nn], \(y) {as.numeric(stringr::str_pad(paste(y, collapse = ""), width=mm, side="right", pad="0"))})
    # turns coordinates of a specific elements into single numbers padded to the right with zeros
    # up to the maximal depth of an inner node (canonic order of these numbers = depth first order of coordinates)
    return(min(temp))
  }
  
  for (i in 1:(mm+1)) {
    key <- sapply(res3[[i]], depthfirstkey)
    ord <- order(key)
    res3[[i]] <- res3[[i]][ord]
    nlist[[i]] <- unlist(nlist[[i]][ord])
    # get correct "original" attribute for unreal components
    for (j in seq_len(length(res3[[i]]))) { # possible within the i loop because the "original" reference of an unreal compo can only be to a lower depth
      if (!pluck(res3, i, j, get_real)) {
        ntemp <- pluck(res3, i, j, get_original)
        major <- which(sapply(nlist, \(x) {ntemp %in% x}))
        stopifnot(length(major) == 1)
        minor <- which(nlist[[major]] == ntemp)
        stopifnot(length(minor) == 1)
        attr(pluck(res3, i, j), "original") <- unname(c(major, minor))
      }
    }
  }
  
  # "invert" nlist to translate veins entries from (res-order to res3-order, i.e. compo-order)
  # could of course be done in a more efficient way, see code just above
  major <- rep(0, nreal)
  minor <- rep(0, nreal)
  for (i in seq_len(nreal)) {
    major[i] <- which(sapply(nlist, \(x) {i %in% x}))
    stopifnot(length(major[i]) == 1)
    minor[i] <- which(nlist[[major[i]]] == i)
    stopifnot(length(minor[i]) == 1)
  }
  veins2 <- lapply(veins, \(x) { cbind(major[x], minor[x]) })
  
  return(list(compos=res3, veins=veins2))  
}
# WARNING about veins2: in multipart kanjis it can happen that the major coordinate is NOT QUITE of the form 1, 2, ... n
# This makes sense and is just due to the tracking of the veins (it happens when two components overlap,
# for the (in strokeorder) later component as soon as it is referring back to the former component IF
# that component is on a lower depth). Example: kvec[[287]], vein 2
# The veins seem to work also for kanji with multiparts, the only small "bug" I found so far is that
# we can create veins that end in inner nodes that have leaves and further inner nodes as children
# (normally veins should only end in inner nodes that have only leaf children)
# However, this is just a matter of efficiency in the matching algo (i.e. in rare cases a
# superfluous side constraint that is in fact already part of the other side constraints is enforced)



#' Plot a representation of a kanjivec dendrogram
#'
#' Depict the tree structure underlying the kanjivec component \code{strokedend}. The main purpose
#' of this function on the user level is to plot kanji dendrograms obtained by other means, such as
#' the function \code{\link{stroketree_to_dend}}.
#'
#' @param dend the component \code{strokedend} of a kanjivec object or the output of the
#' function \code{\link{stroketree_to_dend}}.
#' @param family the font-family for labeling the nodes if \code{type = dend}. See the details for
#' \code{\link{plot.kanjivec}}.
#' @param ... further parameters passed to \code{plot.dendrogram}.
#'
#' @details For a kanjivec object called \code{kvec}, saying \code{plotdend(kvec$strokedend)} is exactly equivalent
#'   to \code{plot(kvec, type="dend")}.
#'
#' @return None (invisible NULL).
#'
#' @noRd
# internal
# (called from plot.kanjivec with type="dend")
plotdend <- function(dend, family = "hiragino_sans", ...) {
  family <- handle_font(family)
  showtext::showtext_auto()  # call before opening plot, so this will not work if we do not do a new plot
  plot(dend, type="triangle", center=TRUE, horiz=FALSE, nodePar=list(pch=c(19,19), cex=c(4,4), col=c(1,gray(0.4))),
       leaflab="none", ylim=c(-0.1, attr(dend, "height") + .1), axes=FALSE, ...) # whatever is left here for ...
  pos <- dendextend::get_nodes_xy(dend, type="triangle", center=TRUE, horiz=FALSE)
  char <- dendextend::get_nodes_attr(dend, "label")
  which_leaf <- is.na(dendextend::get_nodes_attr(dend, "label", include_leaves=FALSE))
  char[which_leaf] <- 1:(dendextend::nleaves(dend))
  text(x=pos, labels=char, col="white", cex=1.2, offset = 0, family=family)
  # with showtext_begin this does not work anymore in the R-studio window nor in quartz!!! it seems they circumvent sysfonts
  # and divert family to another path (works for other devices though)
  # hiragino is the only one showing beta on the rhs in the devices, otherwise family does not matter
  # stroke types are not shown anyway. Could be mapped to code as in http://kanjivg.tagaini.net/stroke-types.html
  # but probably using just the number is more straight forward here
  invisible()
} 


# internal
# called from plot.kanjivec with type="kanji"
plotcompos <- function(kanji, components=NULL, seg_depth=2, add=FALSE, palette = "Dark 3", 
                       pal.extra = 0, pal.random = FALSE, lwd=8, ...) { 
  
  if (seg_depth == 0) {
    if (!add) {
      plot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1), axes=FALSE, type="n", asp=1, xaxs="i", yaxs="i", xlab="", ylab="")
    }
    strokes <- get_strokes(kanji, simplify=FALSE)   # simplify = FALSE is for 1-stroke kanji
    return(invisible(lapply(strokes, lines, lwd=lwd, ...)))
      # plotted in black, but other colors can be passed via ...
  }
  
  if (is.null(components)) {
    components <- kanji$components
    if (is.null(components)) stop("kanji does not contain components and no components were specified.")
  }
  
  compo_seg_depth <- seg_depth + 1 
  if (compo_seg_depth > length(components)) {
    warning("No components at seg_depth ", seg_depth, ". Using components at seg_depth ", length(components)-1)
    compo_seg_depth <- length(components)
  }
  compos <- components[[compo_seg_depth]]
  ncols <- length(compos)
  ord <- if (pal.random) sample(ncols) else 1:ncols
  cols <- hcl.colors(ncols+pal.extra, palette=palette)[ord]
  if (!add) {
    plot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1), axes=FALSE, type="n", asp=1, xaxs="i", yaxs="i", xlab="", ylab="")
  }
  strokes_drawn <- numeric(0)
  for (i in 1:length(compos)) {
    element <- compos[[i]]
    for (j in 3:length(element)) {
      st <- pluck(kanji$stroketree, !!!element[[j]])
      strokes <- st_get_strokes(st)
      stroke_nums <- sapply(strokes, \(x) { as.numeric(stringr::str_sub(attr(x, "id"), 12, -1)) })
      newstroke_ind <- !(stroke_nums %in% strokes_drawn)
      invisible(lapply(strokes[newstroke_ind], lines, col=cols[i], lwd=lwd))
      invisible(lapply(strokes[!newstroke_ind], lines, col=cols[i], lwd=lwd*0.5))
        # I guess it is very rare that we draw over previous strokes more than once so this should be ok for now
      strokes_drawn <- c(strokes_drawn, stroke_nums)
    }
  }
  stroke_nums_left <- setdiff(1:kanji$nstrokes, strokes_drawn)
  strokes_left <- get_strokes(kanji, which=stroke_nums_left, simplify=FALSE)
  invisible(lapply(strokes_left, lines, col=gray(0.8), lwd=lwd))
}
