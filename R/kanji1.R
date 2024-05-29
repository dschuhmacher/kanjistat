# Functions in this file operate on individual kanji.
# They may take vectors of kanji as input, but generate output per kanji.


# Convert between index number, character and kanjivec object (if kvecjoyo is available)
# for now we assume a single kanji as key 
convert_1kanji <- function(key, output=c("all", "index", "character", "hexmode", "kanjivec")) {
  output <- match.arg(output)
  
  # kanjivec input
  if (is(key, "kanjivec")) {
    key <- key$char
  }
  
  # hex number of any form 
  if (is.character(key) && str_sub(key, end = 2) %in% c("0x", "0X")) {
    key <- as.hexmode(key)
  }
  if (is(key, "hexmode")) {
    key <- intToUtf8(key)
  }
  
  # character (by here, the only other possibility is index or something illegal)
  if (is.character(key)) {
    ind <- which(kanjistat::kbase$kanji == key)
    if (length(ind) == 0) {
      stop("kanji ", key, " not found in kbase")
    }
  } else {
    ind <- key
  }
  
  if (output == "index")  return(ind)
  
  char <- kanjistat::kbase$kanji[ind]
  if (output == "character") return(char)
  
  hex <- as.hexmode(utf8ToInt(char))
  if (output == "hexmode") return(hex)
  
  if (output == "kanjivec") {
    check_for_data()
    if (ind %in% 1:2136) {
      return(kanjistat.data::kvecjoyo[[ind]])
    } else {
      stop("kanjivec objects are only available for jouyou kanji")
    }
  }
    
  # only "all" remains
  if (requireNamespace("kanjistat.data", quietly = TRUE)) {
    kvec <- kanjistat.data::kvecjoyo[[ind]]
  } else {
    kvec <- NA
  }
  
  return(list(index=ind, character=char, hexmode=hex, kanjivec=kvec))
}


#' Convert between kanji formats
#' 
#' Accept any interpretable representation of kanji in terms of index numbers,
#' UTF-8 character strings of length 1, UTF-8 codepoints
#' or [`kanjivec`] objects and convert it to all or any of these
#' formats.
#'
#' @param key an atomic vector or list of kanji in any combination of formats.
#' @param output a string describing the desired output.
#' @param simplify logical. Whether to simplify the output to an atomic vector
#' or keep the structure of the original vector. In either case it depends on
#' output whether this is possible.
#'
#' @details
#' Index numbers are in terms of the order in [`kbase`]. UTF-8 codepoints are
#' usually of class "hexmode", but character strings starting 
#' with "0x" or "0X" are also accepted in the `key`.
#'
#' For `output = "kanjivec"`, the GitHub package kanjistat.data has to be available or
#' an error is returned. For `output = "all"`, component kanjivec is set to NA if
#' kanjistat.data is not available.
#'
#' @return A vector of the same length as key. If `simplify` is `TRUE`, this is an
#' atomic vector for output = "index", "character" or "hexmode", and a list
#' for output = "kanjivec" or "all" a list. If `simplify` is `FALSE`, the original
#' structure (atomic or list) kept whenever possible.
#' @export
#'
#' @examples
#' convert_kanji(as.hexmode("99ac"))
#' convert_kanji("0x99ac")  # same
#' convert_kanji(500, "character") == kbase$kanji[500]  # TRUE
#
convert_kanji <- function(key, output=c("all", "index", "character", "hexmode", "kanjivec"), simplify=TRUE) {
  output <- match.arg(output)
  
  input_simple <- (is.atomic(key) || is(key, "kanjivec"))  # atomic or a single kanjivec object
  
  # the following seems to be necessary since as.list (also if implicitly used by lapply)
  # drops the hexmode class (as would c)
  if (is(key, "hexmode")) {  # key is atomic and has class "hexmode"
    key <- as.list(key)
    key <- lapply(key, \(x) {as.hexmode(x)}) 
  } else if (is(key, "kanjivec")) {  # the special case of a single kanjivec (which is a list)
    key <- list(key) 
  } else {
    key <- as.list(key)   # explicit conversion to list for better maintainability
  }

  res <- lapply(key, convert_1kanji, output)
  
  # output (and therefore input) is just one object (input could be a single kanjivec)
  if (simplify || input_simple) {
    if (length(res) == 1) { # output (and therefore input) is just one object (input could be a single kanjivec)
      res <- res[[1]]
    } else if (output %in% c("index", "character", "hexmode")) {
      res <- purrr::list_simplify(res)
    }
    if (output == "hexmode") {  # the simplification drops the hexmode class again
      res <- as.hexmode(res)
    }
  }

  return(res)
}


#' Look up kanji
#' 
#' Return readings and meanings or information from `kbase` or `kmorph`.
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



#' Plot kanji
#' 
#' Write kanji to a graphics device.
#' 
#' @param kanji a vector of class character specifying one or several kanji to be plotted.
#' @param device the type of graphics device where the kanji is plotted. Defaults to the
#'        user's default type according to \code{getOption("device")}. 
#' @param family the font family or families used for writing the kanji. Make sure to add the font(s)
#'        first by using \code{\link[sysfonts]{font_add}}; see details. If `family` is
#'        a vector of several font families they are matched to the characters in `kanji`
#'        (and possibly recycled).
#' @param factor a maginification factor applied to the font size (typically 12 points).
#' @param width,height the dimensions of the device.
#' @param ... further parameters passed to the function opening the device (such as a
#'        file name for devices that create a file).
#'
#' @details
#' This function writes one or several kanji to a graphics device
#' in an arbitrary font that has been registered, i.e., added
#' to the database in package `sysfonts`. For the latter say \code{\link[sysfonts]{font_add}} or
#' \code{\link[sysfonts]{font_families}} to verify what fonts are available. 
#'          
#' For further information see _Working with Japanese fonts_ in `vignette("kanjistat", package = "kanjistat")`.
#' `plotkanji` uses the package `showtext` to write the kanji in a large font at the
#' center of a new device of the specified type.
#' specify `device = "current"` to write the kanji to the current device. It is now recommended
#' to simply use `graphics::text` in combination with `showtext::showtext_auto` instead.
#'          
#' @section Warning:
#' If no font family is provided, the default **Chinese** font WenQuanYi Micro Hei that comes with the package showtext is used. 
#' This means that the characters will typically be recognizable, but quite often look odd as Japanese characters.
#' We strongly advised that a Japanese font is used as detailed above.
#'
#' @return No return value, called for side effects.
#'
#' @export
#'
#' @examples
#' plotkanji("滝")   
#' plotkanji("犬猫魚")
#' 
# most kanji are \u{4e00 to 9faf}, the notorious 𠮟 is the only 常用 kanji that isn't (just saying)
plotkanji <- function(kanji, device="default", family=NULL, factor=10, width=NULL, height=NULL, ...) {
  
  temp <- paste(kanji, collapse="")
  kanji <- unlist(strsplit(temp, split=""))
  n <- length(kanji)
  nfam <- length(family)
  
  # if (is.null(filename)) {
  #   filename <- paste0("kanji.", device) 
  # }
  
  showtext::showtext_auto()
  if (device == "default") {
    # device <- getOption("device")
    if (is.null(width) && is.null(height)) {
      dev.new(...)
    } else {
      if (is.null(width)) {
        width <- height * n
      } else {
        height <- width / n
      }
      dev.new(width=width, height=height, ...)  # set noRStudioGD = TRUE via ... if needed
    }
  } else if (device == "png") {
    
    if (is.null(width) && is.null(height)) {
      width <- 128
      height <- 128
    } else if (is.null(width)) {
      width <- height * n
    } else {
      height <- width/n
    }
    
    png(width = width, height = height, ...)
    on.exit(dev.off())
  } else if (device == "tiff") {
    
    if (is.null(width) && is.null(height)) {
      width <- 128
      height <- 128
    } else if (is.null(width)) {
      width <- height * n
    } else {
      height <- width / n
    }

    tiff(width = width, height = height, ...)  
    on.exit(dev.off())
  } else if (device == "pdf") {
    
    if (is.null(width) && is.null(height)) {
      width <- 1.8
      height <- 1.8
    } else if (is.null(width)) {
      width <- height * n
    } else {
      height <- width / n
    }

    pdf(width = width, height = height, ...)
    on.exit(dev.off())
  } else if (device == "svg") {
    
    stopifnot(capabilities("cairo"))
    if (is.null(width) && is.null(height)) {
      width <- 1.8
      height <- 1.8
    } else if (is.null(width)) {
      width <- height * n
    } else {
      height <- width / n
    }

    svg(width = width, height = height, onefile=TRUE, ...)
    on.exit(dev.off())
  } else {
    
    if (is.null(width) && is.null(height)) {
      eval(parse(text=paste(device, "(...)", sep="")))
    } else {
      if (is.null(width)) {
        width <- height * n
      } else {
        height <- width / n
      }
      eval(parse(text=paste(device, "(width=width, height=height, ...)", sep="")))
    }
  }
  
  if (is.null(family)) {
    default_font <- get_kanjistat_option("default_font")
    if (is.null(default_font)) {
      rlang::warn("No font family specified. Kanji will be represented in the Chinese font WenQuanYi Micro Hei
that is included in the package showtext. It is strongly advised that you register a Japanese font.
See vignette(\"kanjistat\").", 
        .frequency = "regularly", .frequency_id = "no_font_family")
      family <- "wqy-microhei"
    } else {
      family <- default_font
    }
  }
  
  oldpar <- par(mai=rep(0,4), mfrow=c(1,n))
  on.exit(par(oldpar), add = TRUE, after = FALSE) # we already had on.exit above
  for (i in seq_len(n)) {
    plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
    text(x=0.5, y=0.5, kanji[i], family=family[(i-1) %% nfam + 1], cex=factor)
  }
  showtext::showtext_auto(enable=FALSE)
  invisible()
}
