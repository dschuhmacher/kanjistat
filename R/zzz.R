.onLoad <- function(libname, pkgname) {
  assign("kanjistat_env", new.env(parent = emptyenv()), envir = topenv())
  # could also do kanjistat_env <- new.env(parent = emptyenv()) in any of the .R files
  # (but not inside .onLoad and apparently not even with <<-)
  kanjistat_env$kanjistatOptions <- list(ask_github=TRUE,     # ask before reading svg data from github
                                         default_bitmap_size=64,   # default side length of bitmap (also for kanjidist)
                                         default_font=NULL,  # fonts have to be set by user (a default_font is not needed)
                                         verbose=getOption("verbose"))  # we take this from the overall options if not specified in the kanjistat file
  # for easy reference we list all options here that are understood by kanjistat
  locpath <- file.path(getwd(), ".Rkanjistat-profile") 
  locpathR <- file.path(getwd(), ".Rkanjistat-profile.R")
  homepath <- path.expand(file.path("~", ".Rkanjistat-profile"))
  homepathR <- path.expand(file.path("~", ".Rkanjistat-profile.R"))
  if (file.exists(locpath)) {
    source(locpath, local=TRUE)
    kanjistat_env$sourcedProfile <- locpath
  } else if (file.exists(locpathR)) {
    source(locpathR, local=TRUE)
    kanjistat_env$sourcedProfile <- locpathR
  } else if (file.exists(homepath)) {
    source(homepath, local=TRUE)
    kanjistat_env$sourcedProfile <- homepath
  } else if (file.exists(homepathR)) {
    source(homepathR, local=TRUE)
    kanjistat_env$sourcedProfile <- homepathR 
  }
  # print(kanjistat_env$kanjistatOptions)  
  # print(sysfonts::font_families())       
}


.onAttach <- function(libname, pkgname) {
  if (!is.null(kanjistat_env$sourcedProfile)) {
    packageStartupMessage(".Rkanjistat-profile sourced from ", kanjistat_env$sourcedProfile)
  }
}
