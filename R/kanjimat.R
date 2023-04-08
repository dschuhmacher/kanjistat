#' Create kanjimat objects
#'
#' Create a (list of) kanjimat object(s), i.e. bitmap representations of a kanji using a certain font-family
#' and other typographical parameters.
#' 
#' @param kanji a (vector of) character string(s) containing kanjis.
#' @param family the font-family to be used. For details see vignette.
#' @param size the sidelength of the (square) bitmap
#' @param margin extra margin to around the character. Defaults to 0 which leaves a relatively slim margin.
#' Can be negative, but risks cutting off parts of the character. Units are relative to size in steps of 1/32.
#' @param antialias logical. Shall antialiasing be performed?
#' @param save logical or character. If FALSE return the (list of) kanjimat object(s). Otherwise save the result
#' as an rds file in the working directory (as kmatsave.rds) or under the file path provided.
#' @param overwrite logical. If FALSE return an error (before any computations are done) if the designated 
#' file path already exists. Otherwise an existing file is overwritten.
#' @param simplify logical. Shall a single kanjimat object be returned (instead a list of one) if \code{kanji} 
#' is a single kanji?
#' @param ... futher arguments passed to \link[grDevices]{png}. This is for extensibility. The only argument
#' that may currently be used is \code{type}. Trying to change sizes, units, colors or fonts by this argument
#' results in an error or an undesirable output.
#'
#' @return A list of objects of class \code{kanjimat} or, if only one kanji was specified and
#' \code{simplify} is \code{TRUE}, a single objects of class \code{kanjimat}. If \code{save = TRUE},
#' the same is (saved and) still returned invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- kanjimat(kanji="藤", family="klee", size = 128)}
#' 
kanjimat <- function(kanji, family=NULL, size=NULL, margin=0, antialias=TRUE,
                     save=FALSE, overwrite=FALSE, simplify=TRUE, ...) {
  # I guess subpixel antialiasing does not make sense (we only use 1 channel in the end)
  callstring <- paste(deparse(sys.call(), width.cutoff = 100L), collapse = "")
  
  # if save is anything else than FALSE, construct and check savepath 
  if (!isFALSE(save)) {
    if (isTRUE(save)) {
      savepath = file.path(getwd(), paste0("kmatsave", ".rds"))
    } else {
      #if (grepl("/", save, fixed = TRUE)) 
      if (stringr::str_sub(save, -4, -1) != ".rds") save <- paste0(save, ".rds")
      savepath = save
    }
    if (!overwrite && file.exists(savepath)) {
      stop("File ", savepath, " already exists.")
    } 
  }
  
  if (is.null(size)) {
    default_size <- get_kanjistat_option("default_bitmap_size")
    if (is.null(default_size)) size <- 64 else size <- default_size
  }
  margin <- margin * size / 32  # our margin is actually a relative margin (how many 1/32-th of pixel size)
  # of course margin can still be non-integer if you need fine control
  factor <- (size-2*margin)/12
  # 5*12 = 60 points kanji  (12 is the default pointsize)
  # 60 * 1/72 = 0.8333333 inches is size of kanji
  # 64 * 1/72 size of window
  # user sets size and margin we compute factor
  # (size-2*margin)/12 = factor
 
  family <- handle_font(family)  # get from NULL or a maybe incomplete (not yet implemented)
                                 # specification of a font family to the closest suitable font
  
  temp <- paste(kanji, collapse="")
  nkan <- nchar(temp)
  stopifnot(nkan >= 1)
  kan <- as.list(strsplit(temp, split="")[[1]])
  
  if (!hasArg("type")) {   # if type was not specified in ... (which most users will not do anyway)
    cairo <- capabilities("cairo")
    attributes(cairo) <- NULL
    type <- ifelse(cairo, "cairo", "Xlib")  # this is what png sets according to the help page
  } 
  
  #
  kanjimat1 <- function(kan1) {
    hex <- kanjiToCodepoint(kan1)
    padhex <- stringr::str_pad(as.character(hex), width=5, pad="0")
    res1 <- list(char=kan1, hex=hex, padhex=padhex, family=family, size=size, margin=margin, antialias=antialias)
    
    attr(res1, "call") <- callstring
    attr(res1, "kanjistat_version") <- packageVersion("kanjistat")
    attr(res1, "Rversion") <- R.version$version.string
    attr(res1, "platform") <- R.version$platform
    attr(res1, "png_type") <- type
    class(res1) <- "kanjimat"
    
    fname <- tempfile("kanji", fileext=".png")
    png(filename=fname, width = size, height = size, res=72, ...)
    # type = c("cairo", "Xlib", "quartz"), at least on my system (cairo I pressume)
    # gray-antialiasing is the standard (I guess subpixel aa (if possible for png),
    # would make things difficult if we extract (only) the grayscales)
    # saving as grayscale png would save memory (and time presumably), but
    # I do not seem to get antialiasing to work in that case:
    # bitmap(file=fname, type="pnggray", width=size, height=size, res=72, pointsize=12, units="px", taa=4)
    par(mai=rep(0,4))
    plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
    text(x=0.5, y=0.5, kan1, family=family, cex=factor)
    dev.off()
    
    temp<- png::readPNG(fname)
    # on my system identical(res[,,1], res[,,2]) and identical(res[,,1], res[,,3])
    # return true, but you never know so we take an unweighted average
    # (note that rgb to grayscale algos would usually take a weighted average with more than
    # 0.5 weight on green, e.g. opencv 0.299*R + 0.587*G + 0.114*B)
    res1$matrix <- 1-apply(temp[,,1:3], c(1,2), mean)
    # we invert the image (1-... above) mainly for the mass transport algorithm
    # and of course for a cool darkmode style :-)
    unlink(fname)
    
    return(res1)
  }
  #
  showtext::showtext_auto()
  res <- lapply(kan, kanjimat1)
  showtext::showtext_auto(enable = FALSE)
  
  if (nkan == 1 && isTRUE(simplify)) {
    res <- res[[1]]
  } else {
    padhex <- sapply(res, \(x) {x$padhex})
    names(res) <- paste0("kmat", padhex)
  }
  
  if (isFALSE(save)) {
    return(res)
  } else {
    saveRDS(res, file=savepath)
    invisible()
  }
}


#' Plot kanjimat object
#'
#' @param x object of class kanjimat. 
#' @param mode character string. If "dark" the original grayscale values are used,
#'   if "light" they are inverted. With the default grayscale color scheme the kanji is
#'   plotted white-on-black for "dark" and black-on-white for "light".
#' @param col a vector of colors. Typically 256 values are enough to keep the full
#'   information of an (antialiased) kanjimat object.
#' @param ... further parameters passed to \code{\link[graphics]{image}}.
#' 
#' @export
#'
# the export above is just for registtering S3 method, exporting plot.kanjimat would require
# another @export plot.kanjimat afaics (which we don't to)
plot.kanjimat <- function(x, mode=c("dark","light") , col=gray(seq(0,1,length.out=256)), ...) {
  # 256 grayscales is (on my system) exactly what we have from kanjimat
  mode <- match.arg(mode)
  z <- x$matrix 
  nx <- dim(z)[1]  
  ny <- dim(z)[2]
  xx <- seq(1/(2*nx), 1-1/(2*nx), 1/nx)
  yy <- seq(1/(2*ny), 1-1/(2*ny), 1/ny)
  rotclock <- function(m) t(m)[, nrow(m):1]
  par(mai=rep(0,4))
  # plot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1), axes=F, type="n", asp=1, xaxs="i", yaxs="i", xlab="", ylab="")
  if (mode == "dark") {
    image(yy, xx, rotclock(z), col=col, axes=FALSE, main="", asp=1, ...)
  } else {
    image(yy, xx, 1-rotclock(z), col=col, axes=FALSE, main="", asp=1, ...)
  }
}


#' @export
#'
print.kanjimat <- function(x, ...) {
  cat("Kanjimat representation of ", x$char, " (Unicode ", as.character(x$hex), ")\n", sep="")
  cat(x$size, "x", x$size, " bitmap in ", x$family, " font with ", x$margin/32, " margin", sep="")
  if (x$antialias) cat(", antialiased\n") else cat("\n")
  # family here declared as font, because for practical reason we often save the
  # concrete fontface in family, so it is good/ok to use a bit a blurry term here
}


