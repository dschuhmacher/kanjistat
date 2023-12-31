#' Read a KANJIDIC2 file
#'
#' Perform basic validity checks and transform data to a standardized list or keep as an object of
#' class [`xml_document`][`xml2::read_xml`] (package `xml2`).
#' 
#' @param fpath the path to a local KANJIDIC2 file. If `NULL` (the default) the most recent
#'        KANJIDIC2 file is downloaded from \url{https://www.edrdg.org/kanjidic/kanjidic2.xml.gz} after asking for confirmation. 
#' @param output one of `"list"` or `"xml"`. The desired type of output.
#'
#' @details KANJIDIC2 contains detailed information on all of the 13108 kanji in three main Japanese standards (JIS X 0208, 0212 and 0213).
#'          The KANJIDIC files have been compiled and maintained by Jim Breen since 1991, with the help of various
#'          other people. The copyright is now held by the Electronic Dictionary Research and Development Group (EDRDG).
#'          The files are made available under the Creative Commons BY-SA 4.0 license.
#'          See \url{https://www.edrdg.org/wiki/index.php/KANJIDIC_Project} for details on the contents of the files
#'          and their license.
#'          
#' @details If `output = "xml"`, some minimal checks are performed (high level structure and 
#'          total number of kanji). 
#'
#' @details If `output = "list"`, additional validity checks of the lower level structure are performed.
#'          Most are in accordance with the file's Document Type Definition (DTD).
#'          Some additional check concern some common patterns that are true about the current
#'          KANJIDIC2 file (as of December 2023) and seem unlikely to change in the near future.
#'          This includes that there is always at most one `rmgroup` entry in `reading_meaning`.
#'          Informative warnings are provided if any of these additional checks fail.
#'          
#' @return If `output = "xml"`, the exact XML document obtained from [xml2::read_xml]. If `output = "list"`, a list of
#'         lists (the individual kanji), each with the following seven components. 
#' * `literal`: a single UTF-8 character representing the kanji.
#' * `codepoint`: a named character vector giving the available codepoints in the unicode and jis standards.
#' * `radical`: a named numeric vector giving the radical number(s), in the range 1 to 214.
#'              The number named `classical` is as recorded in the *KangXi Zidian* (1716); if there is a number
#'              named `nelson_c`, the kanji was reclassified in Nelson's *Modern Reader's Japanese-English Character
#'              Dictionary* (1962/74).
#' * `misc`: a list with six components
#'   - `grade`: the kanji grade level. 1 through 6 indicates a kyouiku kanji and the grade in which the
#'      kanji is taught in Japanese primary school. 8 indicates one of the remaining jouyou kanji learned 
#'      in junior high school, and 9 or 10 are jinmeiyou kanji. The remaining (hyougai) kanji have `NA`
#'      as their entry.
#'   - `stroke_count`: The stroke count of the kanji, including the radical. If more than 
#'      one, the first is considered the accepted count, while subsequent ones are common miscounts.
#'   - `variant`: a named character vector giving either a cross-reference code to another kanji,
#'      usually regarded as a variant, or an alternative indexing code for the current kanji.
#'      The type of variant is given in the name.
#'   - `freq`: the frequency rank (1 = most frequent) based on newspaper data.
###########(2501 most frequent kanji over four years in the Mainichi Shimbun?) 
#'      `NA` if not among the 2500 most frequent.
#'   - `rad_name`: a character vector. For a kanji that is a radical itself, the name(s) of the radical, 
#'      otherwise of length 0.
#'   - `jlpt`: The Japanese Language Proficiency Test level according to the old four-level system
#'     that was in place before 2010. A value from 4 (most elementary) to 1 (most advanced). 
#' * `dic_number`: a named character vector (possibly of length 0) giving the index numbers (for some
#'      kanji with letters attached) of the kanji in various dictionaries,
###########  conretely there is a P added for some of the dr_type="moro" entries (e.g. 7022, 7765, etc.)
#'      textbooks and flashcard collections (specified by the name). For Morohashi's *Dai Kan-Wa Jiten*,
#'      the volume and page number is also provided in the format moro.VOL.PAGE.
#' * `query_code`: a named character vector giving the codes of the kanji in various query systems
#'      (specified by the name). For Halpern's SKIP code, possible misclassifications (if any) of the kanji
#'      are also noted in the format mis.skip.TYPE, where TYPE indicates the type of misclassification.
#' * `reading_meaning`: a (possibly empty) list containing zero or more `rmgroup` components creating
#'      groups of readings and meanings (in practice there is never more than one `rmgroup` currently)
#'      as well as a component `nanori` giving a character vector (possibly of length 0) of readings only
#'      associated with names. Each `rmgroup` is a list with entries:
#'      - `reading`: a (possibly empty) list of entries named from among `pinyin`, `korean_r`, `korean_h`,
#'          `vietnam`, `ja_on` and `ja_kun`, each containing a character vector of the corresponding readings
#'      - `meaning`: a (possibly empty) list of entries named with two-letter (ISO 639-1) language codes, 
#'          each containing a character vector of the corresponding meanings.
#'      
#' @export
#'
#' @examples
#' if (interactive()) {
#'   read_kanjidic2("kanjidic2.xml")
#' }
#' 
#' @seealso [kanjidata], [kreadmean]
#' 
read_kanjidic2 <- function(fpath=NULL, output=c("list","xml")) {
  output <- match.arg(output)
  verb <- get_kanjistat_option("verbose")
  if (is.null(verb)) verb <- FALSE  # I wonder if this is wise, but is.null(verb) should not happen anyway
  
  if (is.null(fpath)) {
    if (interactive()) {
      ans <- readline("No directory supplied. Download latest KANJIDIC2 file from www.edrdg.org? (y/n) ")
      if (ans == "y" || ans == "Y" || ans == "yes" || ans == "Yes") {
        fpath <- "https://www.edrdg.org/kanjidic/kanjidic2.xml.gz"
      }
    } else {
      stop("No access to KANJIDIC2 file. The latest KANJIDIC2 file can be downloaded manually at http://www.edrdg.org/wiki/index.php/KANJIDIC_Project")
    }
  }
  
  xml <- xml2::read_xml(fpath) # works both with .xml or .xml.gz file (fpath can be local path or url)
  header <- xml2::xml_find_first(xml, "/kanjidic2/header")
  hchildren <- xml2::xml_children(header)
  vnum <- as.numeric(xml2::xml_text(hchildren[1]))
  if (vnum != 4) {
    warning("This function is designed for kanjidic2 structure version 4. File is version ", vnum, ".
            Please check output carefully and send bug report in case of problems.")
  }
  
  if (verb) {
    cat("comment:", xml2::xml_text(xml2::xml_find_first(header, ".//comment()")), sep="")
    for (i in seq_along(hchildren)) {
      cat(xml2::xml_name(hchildren[i]), ": ", xml2::xml_text(hchildren[i]), "\n" , sep="")
    }
  }
  
  characters <- xml2::xml_find_all(xml, "/kanjidic2/character")
  charnum <- length(characters)
  if (charnum != 13108) {    
    # 6355 kanji from JIS X 0208 
    # 5801 kanji from JIS X 0212
    #  952 kanji from JIS X 0213 (out of 3693) that are not already in JIS X 0212
    warning("File contains information about ", charnum, " kanji. This differs from the 13,108 kanji 
            traditionally contained in the KANJIDIC2 file.")
  }
  
  if (output == "xml") {
    return(xml)
  } else {
    if (verb) {
      cat("\nCreation of simplified list from xml may take up to a minute ...\n")
    }
    
    dat <- xml2::as_list(characters)
    
    # allnames <- c("literal", "codepoint", "radical", "misc", "dic_number", "query_code", "reading_meaning")
    # allnames <- names(dat[[1]])
    # [1] "literal" "codepoint" "radical" "misc" "dic_number" "query_code" "reading_meaning"
    # walk(seq_along(dat), \(x) {if (length(names(dat[[x]])) < 7)
    #   cat(x, ": ", setdiff(allgroups, names(dat[[x]])), "\n")})
    # "dic_number" and "reading_meaning" are the only names which are missing (no earlier then Kanji 12158)
    dat2 <- lapply(dat, transform_kanjidic2_entry)  
  }
  
}



# x is a list for an individual kanji, obtained from 
# read_xml |> xml_find_all(xpath = "/kanjidic2/character") |> as_list
# the comments below are from kanjidic dtd Version 1.6 - April 2008
transform_kanjidic2_entry <- function(x) {
  
  # cat(x$literal[[1]], " ")
  # ---------
  #  literal
  # ---------
  # The character itself in UTF8 coding.
  x$literal <- unlist(x$literal)
  
  # -----------
  #  codepoint
  # -----------
  # The codepoint element states the code of the character in the various character set standards.
  # -- cp_value contains the codepoint of the character in a particular
  #    standard. The standard will be identified in the cp_type attribute.
  # -* cp_type attribute states the coding standard applying to the
  #    element. The values assigned so far are:
  #    jis208 - JIS X 0208-1997 - kuten coding (nn-nn)
  #    jis212 - JIS X 0212-1990 - kuten coding (nn-nn)
  #    jis213 - JIS X 0213-2000 - kuten coding (p-nn-nn)
  #    ucs - Unicode 4.0 - hex coding (4 or 5 hexadecimal digits)
  cp_type <- unname( unlist(sapply(x$codepoint, attributes)) )
  cp_value <- unname( unlist(x$codepoint) )
  stopifnot(length(cp_type) == length(cp_value))
  # we do not transform the ucs values to hexmode because we would need to have two sublists then
  x$codepoint <- setNames(cp_value, cp_type)
  
  # ---------
  #  radical
  # ---------
  # -- rad_value contains the radical number, in the range 1 to 214. The particular 
  #    classification type is stated in the rad_type attribute.
  # -* rad_type attribute states the type of radical classification.
  #        classical - as recorded in the KangXi Zidian.
  #        nelson_c - as used in the Nelson "Modern Japanese-English 
  #        Character Dictionary" (i.e. the Classic, not the New Nelson).
  #        This will only be used where Nelson reclassified the kanji.
  rad_type <- unname( unlist(sapply(x$radical, attributes)) )
  rad_value <- unname( unlist(x$radical) )
  stopifnot(length(rad_type) == length(rad_value))
  x$radical <- setNames(as.numeric(rad_value), rad_type)
  
  # ------
  #  misc (list of lists)
  # ------
  # misc (grade?, stroke_count+, variant*, freq?, rad_name*,jlpt?)>
  # -- grade: The kanji grade level. 1 through 6 indicates a Kyouiku kanji
  #      and the grade in which the kanji is taught in Japanese schools. 
  #      8 indicates it is one of the remaining Jouyou Kanji to be learned 
  #      in junior high school, and 9 or 10 indicates it is a Jinmeiyou (for use 
  #      in names) kanji. [G]
  # -- stroke_count: The stroke count of the kanji, including the radical. If more than 
  #      one, the first is considered the accepted count, while subsequent ones 
  #      are common miscounts. (See Appendix E. of the KANJIDIC documentation
  #      for some of the rules applied when counting strokes in some of the 
  #      radicals.) [S]
  # -- variant: Either a cross-reference code to another kanji, usually regarded as a 
  #      variant, or an alternative indexing code for the current kanji.
  #      The type of variant is given in the var_type attribute.
  # -* var_type: The var_type attribute indicates the type of variant code. The current
  #      values are: 
  #             jis208 - in JIS X 0208 - kuten coding
  #             jis212 - in JIS X 0212 - kuten coding
  #             jis213 - in JIS X 0213 - kuten coding
  #               (most of the above relate to "shinjitai/kyuujitai"
  #               alternative character glyphs)
  #             deroo - De Roo number - numeric
  #             njecd - Halpern NJECD index number - numeric
  #             s_h - The Kanji Dictionary (Spahn & Hadamitzky) - descriptor
  #             nelson_c - "Classic" Nelson - numeric
  #             oneill - Japanese Names (O'Neill) - numeric
  #             ucs - Unicode codepoint- hex
  # -- freq: A frequency-of-use ranking. The 2,500 most-used characters have a 
  #      ranking; those characters that lack this field are not ranked. The 
  #      frequency is a number from 1 to 2,500 that expresses the relative 
  #      frequency of occurrence of a character in modern Japanese. This is
  #      based on a survey in newspapers, so it is biassed towards kanji
  #      used in newspaper articles. The discrimination between the less
  #      frequently used kanji is not strong.
  # -- rad_name: When the kanji is itself a radical and has a name, this element
  #      contains the name (in hiragana.) [T2]
  # -- jlpt: The (former) Japanese Language Proficiency test level for this kanji. 
  #      Values range from 1 (most advanced) to 4 (most elementary). This field 
  #      does not appear for kanji that were not required for any JLPT level.
  #      Note that the JLPT test levels changed in 2010, with a new 5-level
  #      system (N1 to N5) being introduced. No official kanji lists are
  #      available for the new levels. The new levels are regarded as
  #      being similar to the old levels except that the old level 2 is
  #      now divided between N2 and N3.
  
  if (is.null(x$misc)) {
    warning('Kanji ', x, ' does not have any misc entries. Check original file and output.')
    x$misc <- list()
  } else {
    tempmisc <- list()
    
    temp <- pluck(x, "misc", "grade")  # 0 or 1 values
    if (is.null(temp)) {
      tempmisc$grade <- NA
    } else {
      tempmisc$grade <- as.numeric(unlist(temp))
    }
  
    # one or more values (without attribute), according to dtd first one is always the correct one
    # (indeed currently 504 kanji have two and 21 kanji have three stroke counts) 
    # e.g. kanji 9 has three, kanji 27 has 2 stroke counts
    tempbool <- (names(x$misc) == "stroke_count")
    tempcount <- sum(tempbool)  
    temp <- x$misc[tempbool]
    if (tempcount == 0) {
      warning('Kanji ', x, ' does not have any stroke_count information. Check original file and output.')
      tempmisc$stroke_count <- numeric(0)
    } else {
      tempmisc$stroke_count <- as.numeric(unlist(temp))
    }
  
    # variant has to be treated differently as there may be several entries with
    # name (i.e. tag in the xml) "variant" (pluck, etc., only look for the first)  
    # variant are 0 or more values with attributes
    tempbool <- (names(x$misc) == "variant")
    tempcount <- sum(tempbool)  
    temp <- x$misc[tempbool]
    if (tempcount == 0) {
      tempmisc$variant <- character(0)
    } else {
      var_type <- unname( unlist(sapply(temp, attributes)) )
      variant <- unname( unlist(temp) )
      stopifnot(length(var_type) == length(variant))
      tempmisc$variant <- setNames(variant, var_type)
    }
  
    temp <- pluck(x, "misc", "freq")  # 0 or 1 values
    if (is.null(temp)) {
      tempmisc$freq <- NA
    } else {
      tempmisc$freq <- as.numeric(unlist(temp))
    }
  
    # 1 or more values (without attribute)
    tempbool <- (names(x$misc) == "rad_name")
    tempcount <- sum(tempbool)  
    temp <- x$misc[tempbool]
    if (tempcount == 0) {
      tempmisc$rad_name <- character(0)
    } else {
      tempmisc$rad_name <- unname( unlist(temp) )
    }
  
    temp <- pluck(x, "misc", "jlpt")  # 0 or 1 values
    if (is.null(temp)) {
      tempmisc$jlpt <- NA
    } else {
      tempmisc$jlpt <- as.numeric(unlist(temp))
    }
  
    x$misc <- tempmisc
  }
  # eof misc
  
  # ------------
  #  dic_number
  # ------------
  # This element contains the index numbers and similar unstructured
  # information such as page numbers in a number of published dictionaries,
  # and instructional books on kanji.
  # -- dic_ref contains an index number. The particular dictionary,
  #    etc. is defined by the dr_type attribute
  # -* dr_type defines the dictionary or reference book, etc. to which
  #    dic_ref element applies. See kanjidic2 dtd for possible names
  # -* m_vol: for some kanji with dr_type="moro", the volume
  # -* m_page: for some kanji with dr_type="moro", the page number in the volume
  
  if (is.null(x$dic_number)) { # happens for the very rare kanji
    x$dic_number <- list()
  } else { 
    attrlist <- lapply(x$dic_number, attributes)
    indlist <- which(sapply(attrlist, length) > 1) # should be only moro
    for (i in indlist) {
      dic <- attrlist[[i]]
      if (dic$dr_type != "moro") {
        warning('Multivariate attribute found in kanji ', x$literal, ', dic_number for dictionary ',
                dic$dr_type, '. This should only be possible for dictionary "moro". Check original file and output.')
      }
      attrlist[[i]] <- list(dr_type = do.call(\(...) paste(..., sep="."), args=dic))
    }
    dr_type <- unname( unlist(attrlist) ) 
    dic_ref <- unname( unlist(x$dic_number) )   # don't say as.numeric, e.g. 7022 and 7765 have a single moro entry with a final P here
    stopifnot(length(dr_type) == length(dic_ref))
    x$dic_number <- setNames(dic_ref, dr_type)
  }
    
  # ------------
  #  query_code
  # ------------  
  # These codes contain information relating to the glyph, and can be used
  # for finding a required kanji. The type of code is defined by the
  # qc_type attribute.
  # -- q_code contains the actual query-code value, according to the
  #    qc_type attribute.
  # -* The qc_type attribute defines the type of query code. The current values
  #    are:
  #    skip -  Halpern's SKIP (System  of  Kanji  Indexing  by  Patterns) 
  #   	code. The  format is n-nn-nn.  See the KANJIDIC  documentation 
  #   	for  a description of the code and restrictions on  the 
  #   	commercial  use  of this data. [P]  There are also
  # 	  a number of misclassification codes, indicated by the
  # 	  "skip_misclass" attribute.
  #   sh_desc - the descriptor codes for The Kanji Dictionary (Tuttle 
  #   	1996) by Spahn and Hadamitzky. They are in the form nxnn.n,  
  #   	e.g.  3k11.2, where the  kanji has 3 strokes in the 
  #   	identifying radical, it is radical "k" in the SH 
  #   	classification system, there are 11 other strokes, and it is 
  #   	the 2nd kanji in the 3k11 sequence. (I am very grateful to 
  #   	Mark Spahn for providing the list of these descriptor codes 
  #   	for the kanji in this file.) [I]
  #   four_corner - the "Four Corner" code for the kanji. This is a code 
  #   	invented by Wang Chen in 1928. See the KANJIDIC documentation 
  #   	for  an overview of  the Four Corner System. [Q]
  #   deroo - the codes developed by the late Father Joseph De Roo, and 
  #   	published in his book "2001 Kanji" (Bonjinsha). Fr De Roo 
  #   	gave his permission for these codes to be included. [DR]
  #   !!! misclass - a possible misclassification of the kanji according
  # 	to one of the code types. (See the "Z" codes in the KANJIDIC
  # 	documentation for more details.)  !!! this is *not* true; these cases
  #     have qc_type "skip" and then the additional attribute skip_misclass below
  # -* skip_misclass contains the type if there is a skip misclassification 
  #    (it currently there are no other misclassifications noted it seems)
  #   - posn - a mistake in the division of the kanji
  #   - stroke_count - a mistake in the number of strokes
  #   - stroke_and_posn - mistakes in both division and strokes
  #   - stroke_diff - ambiguous stroke counts depending on glyph
  #
  # we treat this much the same way as dic_number, but add also a "mis." in front
  # to avoid accidents
  
  if (is.null(x$query_code)) {
    warning('kanji ', x, ' does not have any query codes. Check original file and output.')
    x$query_code <- list()
  } else { 
    attrlist <- lapply(x$query_code, attributes)
    indlist <- which(sapply(attrlist, length) > 1) # should be only skip
    for (i in indlist) {
      query <- attrlist[[i]]
      if (query$qc_type != "skip") {
        warning('Multivariate attribute found in kanji ', x$literal, ', query_code for qc_type ', query$qc_type,
                '. This should only be possible for qc_type "skip". Check original file and output.')
      }
      if (length(query) > 2 || names(query)[2] != "skip_misclass") {
        warning('Multivariate attribute in kanji ', x$literal, ', $query_code[[', i, ']] has unexpected format. ',
                'In addition to "qc_type" only a single "skip_misclass" attribute should be present.')
      }
      attrlist[[i]] <- list(qc_type = do.call(\(...) paste("mis", ..., sep="."), args=query))
    }
    qc_type <- unname( unlist(attrlist) ) 
    q_code <- unname( unlist(x$query_code) )
    stopifnot(length(qc_type) == length(q_code))
    x$query_code <- setNames(q_code, qc_type)
  }
  
  # -----------------
  #  reading_meaning
  # -----------------
  # reading_meaning (rmgroup*, nanori*)
  # The readings for the kanji in several languages, and the meanings, also
  # in several languages. The readings and meanings are grouped to enable
  # the handling of the situation where the meaning is differentiated by 
  # reading. [T1]
  # !!! In reality there is currently only at most one rmgroup in each kanji
  # (it's usually one, except for 316 kanji with very high number)
  
  # rmgroup (reading*, meaning*)
  # ----------------------------
  # -- reading: the reading or pronunciation of the kanji.
  # -* r_type defines the type of reading in the reading element. The current values are:
  #    pinyin - the modern PinYin romanization of the Chinese reading 
  #      of the kanji. The tones are represented by a concluding 
  #      digit. [Y]
  #    korean_r - the romanized form of the Korean reading(s) of the 
  #      kanji.  The readings are in the (Republic of Korea) Ministry 
  #      of Education style of romanization. [W]
  #    korean_h - the Korean reading(s) of the kanji in hangul.
  #    ja_on - the "on" Japanese reading of the kanji, in katakana. 
  #      !!! Another attribute r_status, if present, will indicate with
  #      a value of "jy" whether the reading is approved for a
  #      "Jouyou kanji".
  #      !!! A further attribute on_type, if present,  will indicate with 
  #      a value of kan, go, tou or kan'you the type of on-reading.
  #   ja_kun - the "kun" Japanese reading of the kanji, usually in 
  #      hiragana. 
  #      Where relevant the okurigana is also included separated by a 
  #      ".". Readings associated with prefixes and suffixes are 
  #      marked with a "-". 
  #      !!! A second attribute r_status, if present, 
  #      will indicate with a value of "jy" whether the reading is 
  #      approved for a "Jouyou kanji".
  # -* reading on_type: See under ja_on above.    # practically does not occur (searched in orginal xml)
  # -* reading r_status: See under ja_on and ja_kun above.  # practically does not occur (searched in orginal xml)
  #
  # -- meaning: the meaning associated with the kanji.
  # -* m_lang: defines the target language of the meaning. It 
  #    will be coded using the two-letter language code from the ISO 639-1 
  #    standard. When absent, the value "en" (i.e. English) is implied. [{}]
  
  # nanori
  # ------
  # Japanese readings that are now only associated with names.
  
  if (is.null(x$reading_meaning)) { # happens for the very rare kanji
    x$reading_meaning <- list()
  } else {   
    nam <- names(x$reading_meaning)
    whrm <- which(nam == "rmgroup")  # in practice either the first or none (but the dtd allows for several groups)
    whnano <- which(nam == "nanori")
    stopifnot(all(sort(c(whrm, whnano)) == 1:length(nam)))   # there should be nothing except rmgroup and nanori
 
    list_of_rmgroups <- vector("list")
    
    for (i in whrm) {  # if there is a rmgroup (currently always at most one)
      temp <- pluck(x, "reading_meaning", i)
      namrm <- names(temp)
      wh_read <- which(namrm == "reading")
      wh_mean <- which(namrm == "meaning")
      stopifnot(all(sort(c(wh_read, wh_mean)) == 1:length(namrm)))
      
      # reading:
      if (length(wh_read) == 0) {
        reading <- list()    # empty list if there are no readings
      } else {
        if (any( sapply(temp[wh_read], \(x) {length(attributes(x))}) != 1)) {
          warning('More than one attribute for kanji ', x$literal, ', in a $reading_meaning$rmgroup$reading entry. ',
                  'Everything but the attribute "r_type" is lost.')
          # safety check to be aware if one day there really are attributes r_status or on_type added
        }
        fac_rtype <- factor( sapply(temp[wh_read], attr, "r_type"),
                             levels=c("pinyin", "korean_r", "korean_h", "vietnam", "ja_on", "ja_kun") ) 
          # "vietnam" is missing from the dtd the rest seems to be all there currently is
        reading <- split(unname(unlist(temp[wh_read])), fac_rtype, drop=TRUE)
      }
      
      # meaning:
      if (length(wh_mean) == 0) {
        meaning <- list()    # empty list if there are no readings
      } else {
        wh_mean_en <- which(sapply(temp[wh_mean], \(x) {length(attributes(x)) == 0 }))
        wh_en <- wh_mean[wh_mean_en]
        temp[wh_en] <- lapply(temp[wh_en], \(x) {attr(x, "m_lang") <- "en"; x})
        if (any( sapply(temp[wh_mean], \(x) {length(attributes(x))}) != 1)) {
          warning('More than one attribute for kanji ', x$literal, ', in a $reading_meaning$rmgroup$meaning entry. ',
                  'There should be only an "m_lang" attribute. Check original file and output.')
        }
        fac_mlang <- as.factor( sapply(temp[wh_mean], attr, "m_lang") )
                     # it's not so clear what the current languages are, basically any ISO 639-1 code is admissible
                     # so we accept everything
        meaning <- split(unname(unlist(temp[wh_mean])), fac_mlang, drop=TRUE)
      }
        
      list_of_rmgroups <- c(list_of_rmgroups, list(rmgroup=list(reading=reading, meaning=meaning)))
    }
    
    # if there are any nanori (there are zero to many but we summarize them as a single list element)
    if (length(whnano) == 0) {
      list_of_nanori <- list( nanori=character(0))
    } else {
      list_of_nanori <- list( nanori=unname(unlist(x$reading_meaning[whnano])) )
    }

    x$reading_meaning <- c(list_of_rmgroups, list_of_nanori)  # either may be empty
  }
  
  return(x)
}


# Some of the checks of assumptions about the kanjidic2 format
# 
#
# $dic_number and $reading_meaning are the only entries that are missing in the kanjidic file
# a <- b <- numeric(0)
# for (i in 1:13108) {
#   if (is.null(dat[[i]]$dic_number)) a <- c(a,i)
#   if (is.null(dat[[i]]$reading_meaning)) b <- c(b,i)
# }
#
# sc <- numeric()
# for (i in 1:13108) {
#   nam <- names(dat[[i]]$misc)
#   snam <- sum(nam == "stroke_count")
#   sc <- c(sc, snam)
# }
# # sc
# # 1     2     3 
# # 12583   504    21 
#
# for (i in 1:13108) {
#   nlist <- names(dat2[[i]]$misc)
#   nradname <- sum(nlist == "rad_name")
#   if (nradname > 1) {
#     print(i); print(dat2[[i]]$misc)
#   }
# }
# 
# for (i in 1:13108) {
#   attrlist <- lapply(dat2[[i]]$query_code, attributes)
#   indlist <- which(sapply(attrlist, length) > 1)
#   if (length(indlist) > 0) {
#     tempquery <- dat2[[i]]$query_code[indlist]
#     for (j in seq_along(tempquery)) {
#       if (is.null(attr(tempquery[[j]], "qc_type")) || attr(tempquery[[j]], "qc_type") != "skip" ||
#           is.null(attr(tempquery[[j]], "skip_misclass")) || nchar(tempquery[[j]]) < 6) {
#         print(i); print(tempquery)
#       }
#     }
#   }
# }
# 
# rmg <- nano <- oth <- numeric(0)
# for (i in 1:13108) {
#   rmnan <- dat2[[i]]$reading_meaning
#   names <- names(rmnan)
#   wh <- which(names == "rmgroup")
#   nwh <- which(names == "nanori")
#   rmg <- c(rmg, length(wh))
#   nano <- c(nano, length(nwh))
#   oth <- c(oth, sum(!(names %in% c("rmgroup", "nanori"))))
#   if (length(wh) > 1) {
#     print(i); print(rmnan[wh])
#   }
#   if (length(wh) == 0) {
#     print(i)
#   }
#   # message("Above are all kanji with several rmgroups")
#   reme <- rmnan$rmgroup
#   rmnames <- names(reme)
#   if (any(sort(rmnames, decreasing=TRUE) != rmnames)) print(rmnames)
# }
# 
# for (i in 1:13108) {
#   attrlist <- lapply(dat2[[i]]$reading_meaning$rmgroup, attributes)
#   if ("r_status" %in% attrlist) {
#     dat2[[i]]$reading_meaning$rmgroup
#   }
# }



# if (FALSE) {
#   xml <- read_kanjidic2("../kanjidic2.xml", output="xml")
#   characters <- xml_find_all(xml, "/kanjidic2/character")
#   system.time( dat <- as_list(characters) )
#   
#   system.time( kdic <- read_kanjidic2("../kanjidic2.xml") )
#   
#   library(listviewer)
#   library(rlist)
#   list.filter(kdic, as.numeric(misc$stroke_count) > 32)
#   walk(1:13108, \(x) {if (kdic[[x]]$literal == "䯂") print(x)}) # max stroke count of 34
#   kdic[[12614]]
#   jsonedit(kdic)
#   
#   # testing (looked at for impression and comparison to KANJIDIC2 xml-file)
#   # (former mainly for spotting remaining unnecessary hierarchies
#   # latter mainly for loss (and mixup) of information)
#   
#   # classics
#   kdic[[9]]   # multiple stroke counts
#   jsonedit(dat[[9]])
#   kdic[[6371]]   # no meaning
#   kdic[[76]]  
#   kdic[[123]]
#   kdic[[2557]]
#   kdic[[12158]]
#   
#   # random
#   (selection <- sort(sample(13108,4)))
#   for (i in 1:4) print(kdic[[selection[i]]])
#   
#   # search kanji xml with certain features then check, e.g.
#   walk(1:13108, \(x) {if (kdic[[x]]$literal == "圧") print(x)})
#   kdic[[21]]   # checked against original xml
# }
