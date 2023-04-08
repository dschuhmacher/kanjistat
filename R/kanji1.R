# Functions in this file operate on individual kanji.
# They may take vectors of kanji as input, but generate output per kanji.

#' Look up readings and meanings of kanji
#'
#' @param kanji a (vector of) character strings containing kanji. 
#' @param what the sort of information to display.
#'
#' @details This is a very basic interface for a quick lookup information based on exact
#'          knowledge of the kanji (provided by a Japanese input method or its UTF-8 code).
#'          Most of the information is based on the KANJIDIC2 file by EDRDG (see thank you page)
#'          Please use one of the many excellent online kanji dictionaries (see e.g.) more
#'          sophisticated lookup methods and more detailed results.
#'
#' @return If \code{what} is "readmean" the information is output with cat and there is no return value (invisible NULL)
#'         In the other cases the appropriate subsets of the tables kbase and kmorph are returned
#' @export
#'
#' @author Dominic Schuhmacher \email{schuhmacher@math.uni-goettingen.de}
#'
#' @examples
#' # kanji can be directly entered by Japanese input system
#' # R package check does not allow UTF-8 characters in examples, so
#' # we do it in a more complicated way
#' lookup(c("晴", "曇", "雨"))  
#' lookup("晴曇雨")   # same
#'
# most kanji are \u{4e00 to 9faf}, the notorious 𠮟 is the only 常用 kanji that isn't (just saying)
lookup <- function(kanji, what=c("readmean", "basic", "morphologic")) {
  what <- match.arg(what)
  temp <- paste(kanji, collapse="")
  kanji <- unlist(strsplit(temp, split=""))
  stopifnot(all(is.character(kanji)) && length(kanji) >= 1)
  for (i in 1:length(kanji)) {
    if (nchar(kanji[i]) != 1) stop("expected vector of individual kanji")
    if (what == "readmean") {
      item <- kanjistat::kreadmean[[kanji[i]]]
        # apparently we *must* add kanjistat:: in front of data
      if (is.null(item)) {
        message(crayon::red(kanji[i], "was not found!\n")) 
      } else {
      cat(kanji[i], " --> ON: ", paste(item$read_on, collapse=", "), " | kun: ", paste(item$read_kun, collapse=", "),
          " | nanori: ", paste(item$nanori, collapse=", "), "\n       meaning: ", paste(item$mean, collapse=", "), "\n", sep="")
      }
      invisible()
    } else if (what == "basic") {
      return(kanjistat::kbase[match(kanji, kanjistat::kbase$kanji),])
    } else if (what == "morphologic") {
      return(kanjistat::kmorph[match(kanji, kanjistat::kmorph$kanji),])
    }
  }
}   



#' Plot a kanji
#' 
#' @param kanji a single character, string or numeric specifying a single kanji.
#' @param device the type of graphics device where the kanji is plotted. Defaults to the
#'        user's default type according to \code{getOption("device")}. Can be set to
#'        "current", in which case the kanji is plotted on the currently active device.
#' @param family the font family used for writing the kanji. Make sure to add the font
#'        first by using \code{\link[sysfonts]{font_add}}. See details.
#' @param factor a maginification factor applied to the font size (typically 12 points).
#' @param width,height the dimensions of the device.
#' @param ... further parameters passed to the function opening the device (such as a
#'        file name for devices that create a file).
#'
#' @details This function allows to "draw" a kanji on a number of possible graphics devices
#'          in an arbitrary font that is installed on the user's system and has been added
#'          to the database in package \code{sysfonts}. For the latter \code{\link[sysfonts]{font_add}} or
#'          \code{\link[sysfonts]{font_families}} to verify what fonts are available. Locating user installed
#'          fonts varies from system to system and finding the path needed for \code{\link[sysfonts]{font_add}}
#'          can sometimes be tricky. The suggested package \code{systemfonts} (NOT the same as \code{sysfonts})
#'          offers the helpful command \code{\link[systemfonts]{match_font}}, which provides the font path based
#'          on the name of the font family (and if desired the font face).
#'          
#'          The function uses then the package \code{showtext} to write the kanji in a large font at the
#'          center of a new device of the specified type. If no font family is provided, the CJK font 
#'          WenQuanYi Micro Hei that that comes with the package showtext is used. This font targets
#'          Chinese writing and some strokes will therefore look odd for Japanese characters. We strongly
#'          advised that a Japanese font is used.
#'          
#'          For further details on CJK characters and computers, see the corresponding vignette (TO DO). 
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plotkanji("滝")   # uses Chinese font!}
#' 
# most kanji are \u{4e00 to 9faf}, the notorious 𠮟 is the only 常用 kanji that isn't (just saying)
plotkanji <- function(kanji, device="default", family=NULL, factor=10, width=NULL, height=NULL, ...) {
  showtext::showtext_auto()
  if (device == "default") {
    device <- getOption("device")
    dev.new(...)
  } else if (device == "png") {
    if (is.null(width) & is.null(height)) {
      width <- 128
      height <- 128
    } else if (is.null(width)) {
      width <- height
    } else {
      height <- width
    }
    png(width = width, height = height, ...)
    on.exit(dev.off())
  } else if (device == "tiff") {
    if (is.null(width) & is.null(height)) {
      width <- 128
      height <- 128
    } else if (is.null(width)) {
      width <- height
    } else {
      height <- width
    }
    tiff(width = width, height = height, ...)  
    on.exit(dev.off())
  } else if (device == "pdf") {
    if (is.null(width) & is.null(height)) {
      width <- 1.8
      height <- 1.8
    } else if (is.null(width)) {
      width <- height
    } else {
      height <- width
    }
    pdf(width = width, height = height, ...)
    on.exit(dev.off())
  } else if (device == "svg") {
    stopifnot(capabilities("cairo"))
    if (is.null(width) & is.null(height)) {
      width <- 1.8
      height <- 1.8
    } else if (is.null(width)) {
      width <- height
    } else {
      height <- width
    }
    svg(width = width, height = height, onefile=TRUE, ...)
    on.exit(dev.off())
  } else if (device != "current") {
    eval(parse(text=paste(device,"(width=width, height=height, ...)",sep="")))
  }
  if (is.null(family)) {
    default_font <- get_kanjistat_option("default_font")
    if (is.null(default_font)) {
      rlang::warn("No font family specified. Kanji will be represented in the CJK font WenQuanYi Micro Hei
that is included in the package showtext. The font targets Chinese writing and some strokes will
therefore look odd for Japanese characters. It is strongly advised that you register a Japanese font.
See help with Japanese font.", 
        .frequency = "regularly", .frequency_id = "no_font_family")
      family <- "wqy-microhei"
    } else {
      family <- default_font
    }
  }
  par(mai=rep(0,4))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(x=0.5, y=0.5, kanji, family=family, cex=factor)
  showtext::showtext_auto(enable=FALSE)
  invisible()
}
