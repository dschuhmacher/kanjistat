# The code in this file consists of five files from the non-CRAN package
# svgparser v0.1.2 by mikefc <mikefc@coolbutuseless.com>
# as covered under the MIT license below. 

# The files are:
#~~~~~~~~~
# lex.R
# svg-path.R
# svg-path-arc.R
# svg-path-bezier.R
# utils.R
#~~~~~~~~~

# Remarks:
# 1. svg-path-arc.R is included for local completeness of dependencies
#    it is very unlikely that kanjiVG data contains any arcs.
# 2. The @noRd tag has been added to all roxygen2 doc in this file
#    (only parse_N_numbers and bezier3_to_df_loop had one before).
# 3. "@importFrom stats setNames" has been moved from function parse_N_numbersto
#    to kanjistat-package.R and imports from stringi have been declared in DESCRIPTION
#    (although it appears we could replace the two occurences of stri_match_all below
#    by str_match_all from stringr: both call stringi::stri_match_all_regex, but
#    with slightly different parameters, so I leave it for now).


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MIT License
# 
# Copyright (c) 2021 mikefc@coolbutuseless.com
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~







# === lex.R =================================================================================


# nocov start

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Break a string into labelled tokens based upon a set of patterns
#'
#' @param text a single character string
#' @param regexes a named vector of regex strings.  Each string represents
#'                a regex to match a token, and the name of the string is the
#'                label for the token.  Each regex can contain an explicit
#'                captured group using the standard \code{()} brackets. If a regex
#'                doesn't not define a captured group then the entire regex will
#'                be captured. The regexes
#'                will be processed in order such that an early match takes
#'                 precedence over any later match.
#' @param verbose print more information about the matching process. default: FALSE
#'
#' @return a named character vector with the names representing the token type
#'         with the value being the element extracted by the corresponding
#'         regular expression.
#'
#' @examples
#' \dontrun{
#' lex("hello there 123.45", regexes=c(number=re$number, word="(\\w+)", whitespace="(\\s+)"))
#' }
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lex <- function(text, regexes, verbose=FALSE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # disallow multiple capture groups in a single pattern.
  # i.e. regexes = c("(a|b)", "(c)|(d)")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  captured_groups <- stringi::stri_match_all(regexes, regex = "(?<!\\\\)\\([^?]")
  
  n_captured_groups <- vapply(captured_groups, FUN = function(x) {
    if (anyNA(x)) {
      0L
    } else {
      nrow(x)
    }
  }, integer(1))
  if (any(n_captured_groups > 1)) {
    stop("Regexes can define at most only a single capture group. Patterns which need fixing",
         deparse(regexes[n_captured_groups > 1]))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Any regex that has 0 capture groups has its whole regex become the
  # capture group
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idx <- n_captured_groups == 0
  regexes[idx] <- paste0("(", regexes[idx], ")")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Insert a default pattern to match anything missed by the provided regexes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regexes        <- c(regexes, .missing="(.)")
  regex_labels   <- names(regexes)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All regexes must be named
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(!anyNA(regex_labels))
  stopifnot(!any(regex_labels == ''))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine all the patterns into a single regex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regex <- paste(regexes, collapse='|')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Match all regex against the text
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  token_matching  <- stringi::stri_match_all(text, regex = regex)[[1]]
  
  if (verbose) {
    colnames(token_matching) <- c("all", regex_labels)
    print(token_matching)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the actual token and the regex which matched the token
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regex_idx      <- apply(token_matching[, -1, drop=FALSE], 1, function(x) {  which(!is.na(x))})
  tokens         <- apply(token_matching[, -1, drop=FALSE], 1, function(x) {x[which(!is.na(x))]})
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if 'regex_idx' is a list, then a location was matched by multiple regexes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.list(regex_idx)) {
    lens <- lengths(regex_idx)
    idx  <- which(lens > 1)
    stop("lex issues at the following locations within 'text': ", deparse(idx))
  }
  
  
  names(tokens)  <- regex_labels[regex_idx]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If any tokens were captured by the '.missing' regex, then show
  # a warning message
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (verbose && any(names(tokens) == '.missing')) {
    not_captured <- sort(unique(tokens[names(tokens) == '.missing']))
    warning("The following characters were not captured: ", deparse(not_captured))
  }
  
  tokens
}

# nocov end







# === svg-path.R =================================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The patters to tokenise a path string
# The number regex is a bit of work.
# in SVG paths, 2 numbers don't need a comma between them if the context
# indicates they're 2 numbers.
#  e.g. "1-2" is the number sequence c(1, -2)
#
# Second number regex
# From: https://stackoverflow.com/questions/46340156/svg-path-data-regex-c-sharp
# [\+\-]?            // an optional sign
# (\d*\.\d+|\d+\.?)  // an integer or a decimal number
# ([eE][\+\-]?\d+)*  // the exponential part of the scientific notation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# number              = "[+\\-]?(?:0|[\\.0-9]\\d*)(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?",
number_pattern <- "[\\+\\-]?(?:\\d*\\.\\d+|\\d+\\.?)(?:[eE][\\+\\-]?\\d+)*"

path_patterns <- c(
  number              = number_pattern,
  comma               = ",",
  move_abs            = "MM", # workaound foSuperTinyIcons-master/images/svg/linux_mint.svg
  move_abs            = 'M',
  move_rel            = 'm',
  lineto_abs          = 'L',
  lineto_rel          = 'l',
  horiz_lineto_abs    = 'H',
  horiz_lineto_rel    = 'h',
  vert_lineto_abs     = 'V',
  vert_lineto_rel     = 'v',
  bezier3_abs         = 'C',
  bezier3_rel         = 'c',
  bezier3_reflect_abs = 'S',
  bezier3_reflect_rel = 's',
  bezier2_abs         = 'Q',
  bezier2_rel         = 'q',
  bezier2_reflect_abs = 'T',
  bezier2_reflect_rel = 't',
  arc_abs             = 'A',
  arc_rel             = 'a',
  close_path_abs      = 'Z|z',
  whitespace          = "\\s+"
)


path_patterns2 <- c(
  number              = "number",
  comma               = ",",
  move_abs            = 'M',
  move_rel            = 'm',
  lineto_abs          = 'L',
  lineto_rel          = 'l',
  horiz_lineto_abs    = 'H',
  horiz_lineto_rel    = 'h',
  vert_lineto_abs     = 'V',
  vert_lineto_rel     = 'v',
  bezier3_abs         = 'C',
  bezier3_rel         = 'c',
  bezier3_reflect_abs = 'S',
  bezier3_reflect_rel = 's',
  bezier2_abs         = 'Q',
  bezier2_rel         = 'q',
  bezier2_reflect_abs = 'T',
  bezier2_reflect_rel = 't',
  arc_abs             = 'A',
  arc_rel             = 'a',
  close_path_abs      = 'Z',
  whitespace          = "\\s+"
)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The names of the parameters associated with each path element
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path_param_names <- list(
  move            =  c('x', 'y'),                         # 'M', 'm'
  lineto          =  c('x', 'y'),                         # 'L', 'l'
  horiz_lineto    =  c('x'),                              # 'H', 'h'
  vert_lineto     =  c('y'),                              # 'V', 'v'
  bezier3         =  c('x1', 'y1', 'x2', 'y2', 'x', 'y'), # 'C', 'c'
  bezier3_reflect =  c(            'x2', 'y2', 'x', 'y'), # 'S', 's'
  bezier2         =  c('x1', 'y1',             'x', 'y'), # 'Q', 'q'
  bezier2_reflect =  c(                        'x', 'y'), # 'T', 't'
  arc             =  c('rx', 'ry', 'rot', 'arcflag', 'sweepflag', 'x', 'y'), # 'A', 'a'
  close_path      =  c()  # 'Z|z'
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse N numbers from a token stream and assign the given param names
#'
#' @param tokens named character vector
#' @param idx the index at which to start extracting numeric values
#' @param param_names character vector of names for the extracted values.
#' @param instruction path instruciton
#'
#' @return named list of numeric values (name = param_names)
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_N_numbers <- function(tokens, idx, param_names, instruction) {
  res <- vector('list', length(param_names))
  for (i in seq_along(param_names)) {
    res[[i]] = as.numeric(tokens[idx + i - 1L])
    
    if (is.na(res[[i]])) {
      message("------------------------ path parsing failure")
      print(instruction)
      print(param_names)
      print(i)
      print(param_names[i])
      print(tokens[idx + i - 1L])
      print(tokens[idx + seq_along(param_names) - 1L])
      stop("path parsing failure")
    }
  }
  
  setNames(res, param_names)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Arc numbers can be tricky because SVG is allowed to lax about spacing
# if the context makes it clear.
#
# Arc params:  rx, ry, rot, arcflag, sweepflag, x , y
#   e.g.       10  20   0       1          0  12  28
#
# But since 'arcflag' and 'sweepflag' can only be 0/1 it is possible to write
# the above path as   "10 20 0 1012 20"
#
# This isn't able to be tokenised by lex, so I'm going to have to do a manual
# hack for 'arc' parameters.
#
# Thais is going to get pretty gnarly!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_arc_numbers <- function(tokens, idx, param_names, instruction) {
  
  param_names <- c("rx", "ry", "rot", "arcflag", "sweepflag", "x", "y")
  
  res <- list()
  
  # The first 3 tokens are always sane.
  res[['rx']]        = as.numeric(tokens[idx + 0L]);
  res[['ry']]        = as.numeric(tokens[idx + 1L]);
  res[['rot']]       = as.numeric(tokens[idx + 2L]);
  
  # Now I have to work out whether the arcflag and sweepflag tokens
  # are compacted together i.e. "01" representes arc=0, sweep=1, etc
  tok <- tokens[[idx + 3L]]
  if (nchar(tok) == 1L) {
    # easy case - 1 character for arcflag, 1 character for sweepflag
    res[['arcflag']]   = as.numeric(tokens[idx + 3L])
    res[['sweepflag']] = as.numeric(tokens[idx + 4L])
    res[['x']]         = as.numeric(tokens[idx + 5L])
    res[['y']]         = as.numeric(tokens[idx + 6L])
    consumed <- 7L
  } else if (nchar(tok) == 2L) {
    # two chars mashed together!
    bits <- strsplit(tok, "")[[1]]
    res[['arcflag']]   = as.numeric(bits[1])
    res[['sweepflag']] = as.numeric(bits[2])
    res[['x']]         = as.numeric(tokens[idx + 4L])
    res[['y']]         = as.numeric(tokens[idx + 5L])
    consumed <- 6L
  } else if (nchar(tok) > 2L) {
    # two chars mashed together!
    bits <- strsplit(tok, "")[[1]]
    res[['arcflag']]   = as.numeric(bits[1])
    res[['sweepflag']] = as.numeric(bits[2])
    res[['x']]         = as.numeric(paste0(bits[-(1:2)], collapse = ""))
    res[['y']]         = as.numeric(tokens[idx + 4L])
    consumed <- 5L
  } else {
    stop("parse_arc_numbers(): ambiguous encoding of path arc parameters. Please report this error.")
  }
  
  list(
    consumed = consumed,
    params   = res
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse path into a list of parameters and meta-information
#'
#' @param svg_path SVG path string (single character string)
#'
#' @return list of lists of information - 1 list per element in the path.
#'         each element is itself a list of parameters and meta-information
#'         about the path element e.g. index of the element in the path
#'         
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_svg_path_d <- function(svg_path) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the path into tokens and remove the junk tokens
  #  i.e. whitespace and commas
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tokens <- lex(svg_path, path_patterns)
  tokens <- tokens[!names(tokens) %in% c('whitespace', 'comma')]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialise the parsing
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  path  <- list()
  idx   <- 1L
  prev  <- "unknown"
  prev_raw <- "unknown"
  
  inst_idx <- 0L
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Process the entire stream
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  while (idx <= length(tokens)) {
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get the name of the instruction.
    # If the instruction is a number, it means that this is a repeat of
    # the prior instruction.
    # Whether it's a repeat or not will determine whether the token for the
    # actual instruciton needs to be stepped over or not.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    raw_instruction <- instruction <- names(tokens)[idx]
    if (instruction == 'number') {
      step_over   <- FALSE
      instruction <- prev
      raw_instruction <- prev_raw
      abs         <- prev_abs
    } else {
      step_over   <- TRUE
      instruction <- substr(instruction, start = 1, stop = nchar(instruction) - 4)
      abs         <- endsWith(raw_instruction, "abs")
      inst_idx    <- inst_idx + 1L
    }
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep track of this instruction which may be needed for a repeated
    # instruction next.
    # Get the parameter names for this instruction and parse out that many
    # numbers from the stream
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prev        <- instruction
    prev_raw    <- raw_instruction
    prev_abs    <- abs
    param_names <- path_param_names[[instruction]]
    
    if (instruction == 'arc') {
      params_consumed <- parse_arc_numbers(tokens, idx + step_over, param_names, instruction)
      params   <- params_consumed$params
      consumed <- params_consumed$consumed
      # print("ARC UP")
      # print(unlist(params))
      # print(consumed)
      # print(tokens[idx + seq(consumed) - 1])
      idx <- idx + consumed + step_over
    } else {
      params <- parse_N_numbers(tokens, idx + step_over, param_names, instruction)
      idx <- idx + length(param_names) + step_over
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Construct the return object and add to the list of all path objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cmd <- paste(c(path_patterns2[[raw_instruction]], params), collapse = " ")
    elem <- list(
      params    = params,
      name      = instruction,
      abs       = abs,
      raw       = raw_instruction,
      cmd       = cmd,
      inst_idx  = inst_idx
    )
    
    path <- c(path, list(elem))
  }
  
  path
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert path relative coordinates to absolute coordinates
#'
#' @param path_list path as returned by \code{parse_svg_path_d()}
#' @param state named list of current state. Default \code{list(x=0, y=0)}
#'
#' @return \code{path_list} with only absolute elements
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path_list_to_abs_path_list <- function(path_list, state = list(x=0, y=0)) {
  stopifnot(is.list(path_list))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 'state' may be an environment.  Want to just make an editable
  # copy that won't affect the master state
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  init_state <- as.list(state)
  prev_elem <- list(name = "XXX")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # look at teach element in the path
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (idx in seq_along(path_list)) {
    # cat(sprintf("%i: (%.1f, %.1f)\n", idx, state$x, state$y))
    elem        <- path_list[[idx]]
    param_names <- path_param_names[[elem$name]]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update the coordinates from relative to absolute if required
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (elem$abs) {
      # do nothing
    } else {
      for (param_name in param_names) {
        if (startsWith(param_name, 'x')) {
          elem$params[[param_name]] <- elem$params[[param_name]] + state$x
        } else if (startsWith(param_name, 'y')) {
          elem$params[[param_name]] <- elem$params[[param_name]] + state$y
        }
      }
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The "moveto" commands (M or m) must establish a new initial point and a
    # new current point.
    # https://www.w3.org/TR/SVG/paths.html#TermInitialPoint
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (elem$name == 'move' && prev_elem$name != 'move') {
      init_state$x <- elem$params$x
      init_state$y <- elem$params$y
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Per: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d
    # Multiple move command. e.g. M 1,2 3,4, 5,6 should actually be interpreted
    # as "M 1,2 L 3,4 5,6"
    #
    # i.e. "Move the current point to the coordinate x,y.
    #      Any subsequent coordinate pair(s) are interpreted as parameter(s)
    #      for implicit absolute LineTo (L) command(s) (see below)."
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Store the coords at the start of this instruction
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    elem$params$x0 <- state$x
    elem$params$y0 <- state$y
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # For ease of access later, ensure that all commands have
    # (x,y) coordinates.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (elem$name == 'horiz_lineto') {
      elem$params$y <- state$y
    } else if (elem$name == 'vert_lineto') {
      elem$params$x <- state$x
    } else if (elem$name == 'close_path') {
      elem$params$x <- init_state$x
      elem$params$y <- init_state$y
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Mark this element as not in 'absolute' units and save back into
    # the complete path list
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    elem$abs <- TRUE
    path_list[[idx]] <- elem
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The current (x,y) at the end of this part of the part are now the
    # starting state for the next part
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    state$x <- elem$params$x
    state$y <- elem$params$y
    
    prev_elem <- elem
  }
  
  path_list
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Prepare complete list of control point coordinates for all beziers
#'
#'
#' @param path_list path as returned by parse_svg_path_d
#' @param state named list of current state. Default list(x=0, y=0)
#'
#' @return \code{path_list} with all bezier elements now having 'x' and 'y'
#'         vectors with coords of the control points.
#'         
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path_list_expand_beziers <- function(path_list, state) {
  stopifnot(is.list(path_list))
  
  for (idx in seq_along(path_list)) {
    elem   <- path_list[[idx]]
    
    if (!elem$name %in% c('bezier3', 'bezier3_reflect',
                          'bezier2', 'bezier2_reflect')) {
      next
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Cubic Bezier
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (elem$name == 'bezier3') {
      elem$x <- with(elem$params, c(x0, x1, x2, x))
      elem$y <- with(elem$params, c(y0, y1, y2, y))
    } else if (elem$name == 'bezier3_reflect') {
      p <- elem$params
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # If prior point was bezier, then use the prior control point - but
      # reflect it w.r.t. the starting point for this bezier.
      #
      # Todo: add checks for edge cases
      # https://www.w3.org/TR/SVG/paths.html
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      elem_prev <- path_list[[idx - 1L]]
      
      if (elem_prev$name %in% c('bezier3', 'bezier3_reflect')) {
        x2_prev <- elem_prev$params$x2
        y2_prev <- elem_prev$params$y2
        
        x1 <- 2 * p$x0 - x2_prev
        y1 <- 2 * p$y0 - y2_prev
        
      } else {
        x1 <- p$x0
        y1 <- p$y0
      }
      
      elem$x <- c(p$x0, x1, p$x2, p$x)
      elem$y <- c(p$y0, y1, p$y2, p$y)
    }
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Quadratic Beziers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (elem$name == 'bezier2') {
      elem$x <- with(elem$params, c(x0, x1, x))
      elem$y <- with(elem$params, c(y0, y1, y))
    } else if (elem$name == 'bezier2_reflect') {
      p <- elem$params
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # If prior point was bezier, then use the prior control point - but
      # reflect it w.r.t. the starting point for this bezier.
      #
      # Todo: add checks for edge cases
      # https://www.w3.org/TR/SVG/paths.html
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      elem_prev <- path_list[[idx - 1L]]
      
      if (elem_prev$name %in% c('bezier2', 'bezier2_reflect')) {
        x1_prev <- elem_prev$params$x1
        y1_prev <- elem_prev$params$y1
        
        x1 <- 2 * p$x0 - x1_prev
        y1 <- 2 * p$y0 - y1_prev
        
      } else {
        x1 <- p$x0
        y1 <- p$y0
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Save the computed (x1, y1) in case the next path segment is a
      # bezier reflect which needs this point
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      elem$params$x1 <- x1
      elem$params$y1 <- y1
      
      elem$x <- c(p$x0, x1, p$x)
      elem$y <- c(p$y0, y1, p$y)
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Store the adjusted elem back into the path_list
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    path_list[[idx]] <- elem
  }
  
  
  path_list
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a path list to a data.frame
#'
#' @param path_list path as returned by parse_svg_path_d
#' @param state named list of current state. Default list(x=0, y=0)
#'
#' @return data.frame of x,y coordinates
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path_list_to_df <- function(path_list, state = list(x=0, y=0)) {
  stopifnot(is.list(path_list))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert all path_list segments to absolute value (rather than relative
  # Expand all beziers so they're easier to process later.)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  path_list <- path_list_to_abs_path_list(path_list)
  path_list <- path_list_expand_beziers(path_list)
  
  npoints <- state$npoints %||% 15
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Each time there is a close path, start a new sub-segment
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idx <- 1L
  last_elem <- list(name = 'close_path')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each element in the path, get its data.frame representation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- lapply(seq_along(path_list), function(path_idx) {
    elem <- path_list[[path_idx]]
    
    if (elem$name %in% c('bezier3', 'bezier3_reflect')) {
      res <- bezier3_to_df(elem$x, elem$y, npoints)
    } else if (elem$name %in% c('bezier2', 'bezier2_reflect')) {
      # Quadratic beziers not tested much. be careful!
      res <- bezier2_to_df(elem$x, elem$y, npoints)
    } else if (elem$name == 'arc') {
      res <- arc_to_df(elem$params, npoints)
    } else if (elem$name == 'move') {
      if ( (last_elem$name != 'close_path' && last_elem$name != 'move') ||
           (last_elem$name == 'move' && last_elem$inst_idx != elem$inst_idx)) {
        # message("Move without prior closepath! : ", last_elem$name)
        # res0 <- data_frame(
        #   x    = elem$params$x0,
        #   y    = elem$params$y0,
        #   name = 'close_path',
        #   idx  = idx,
        #   path_idx = path_idx,
        #   inst_idx = elem$inst_idx,
        #   path = "(implicit close path)"
        # )
        res <- data_frame(
          x        = elem$params$x,
          y        = elem$params$y,
          name     = elem$name,
          idx      = idx + 1L,
          path_idx = path_idx,
          inst_idx = elem$inst_idx,
          path     = elem$cmd
        )
        idx <<- idx + 1L
        # res <- rbind(res0, res1)
      } else {
        res <- data_frame(x = elem$params$x, y = elem$params$y)
      }
    } else {
      res <- data_frame(x = elem$params$x, y = elem$params$y)
    }
    
    # If meta-info not yet added to data.frame, then do so now.
    if (!'name' %in% colnames(res)) {
      res$name     <- elem$name
      res$idx      <- idx
      res$path_idx <- path_idx
      res$inst_idx <- elem$inst_idx
      res$path     <- elem$cmd
    }
    
    idx       <<- idx + (elem$name %in% c('close_path'))
    last_elem <<- elem
    
    res
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bind all the data.frames into a single data.frame for the path
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  do.call(rbind, coords)
}







# === svg-path-arc.R =================================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert \code{<path>} arc to a data.frame of coordinates
#'
#' Reference \url{https://svgwg.org/svg2-draft/implnote.html#ArcConversionEndpointToCenter}
#'
#' ToDo: actual rotation of arc coords by 'phi'
#'
#' @param params named list of arc parameters from the path segment i.e. xo, y0,
#'        x, y, arcflag, sweepflag, rx, ry, rot (degrees)
#' @param npoints number of points along arc
#'
#' @return data.frame of coodinates of arc
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
arc_to_df <- function(params, npoints) {
  
  if (npoints < 4) {
    npoints <- 4
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract convert values from the path 'd' parameters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x1  <- params$x0
  y1  <- params$y0
  x2  <- params$x
  y2  <- params$y
  fa  <- params$arcflag
  fs  <- params$sweepflag
  rx  <- params$rx
  ry  <- params$ry
  phi <- params$rot * pi/180
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Section B.2.5 "Correction of out-of-range radii
  # https://svgwg.org/svg2-draft/implnote.html#ArcCorrectionOutOfRangeRadii
  #
  # Step 1: Ensure radii are non-zero
  #   - if either is zero, then assume a straight line between start/end
  # Step 2: Ensure radii are positive
  #   - just call abs()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (rx == 0 || ry == 0) {
    return(
      data_frame(
        x = c(x1, x2),
        y = c(y1, y2)
      )
    )
  }
  
  rx <- abs(rx)
  ry <- abs(ry)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x1p <-  cos(phi) * ((x1 - x2)/2) + sin(phi) * ((y1 - y2)/2)
  y1p <- -sin(phi) * ((x1 - x2)/2) + cos(phi) * ((y1 - y2)/2)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Section B.2.5 "Correction of out-of-range radii
  # https://svgwg.org/svg2-draft/implnote.html#ArcCorrectionOutOfRangeRadii
  #
  # Step 3: Ensure radii are large enough
  #
  # It is possible (due to say, rounding), that the given rx and ry
  # can't actually span the gap between the start and the end of the arc.
  # In thise case, just tweak the radii so that they do actually
  # span the start/end gap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gap <- x1p^2/rx^2 + y1p^2/ry^2
  if (gap > 1) {
    rx <- sqrt(gap) * rx
    ry <- sqrt(gap) * ry
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  det_numer <- rx^2 * ry^2 - rx^2 * y1p^2 - ry^2 * x1p^2
  det_denom <- rx^2 * y1p^2 + ry^2 * x1p^2
  
  if (det_numer < 0) {
    if (abs(det_numer) < 1e-4) { # floating point precision. basically zero.
      det_numer <- 0
    } else {
      warning("det_number < 0.  replacing with 0. strangeness will ensue")
      det_number <- 0
    }
  }
  
  det <- sqrt(det_numer/det_denom)
  
  if (fa == fs) {
    det <- -det
  }
  
  cxp <- det *  rx * y1p / ry
  cyp <- det * -ry * x1p / rx
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cx <- cos(phi) * cxp - sin(phi) * cyp + (x1 + x2) / 2
  cy <- sin(phi) * cxp + cos(phi) * cyp + (y1 + y2) / 2
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Eqn 5.5, 5.6
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ref1 <- c(
    (x1p - cxp) / rx,
    (y1p - cyp) / ry
  )
  
  ref2 <- c(
    (-x1p - cxp) / rx,
    (-y1p - cyp) / ry
  )
  
  theta1 <- angle(c(1, 0), ref1)
  dtheta <- angle(ref1   , ref2)
  
  if (fs == 0 && dtheta > 0) {
    dtheta <- dtheta - 2 * pi
  } else if (fs == 1 && dtheta < 0) {
    dtheta <- dtheta + 2 * pi
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create some points along the arc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  theta <- seq(theta1, theta1 + dtheta, length.out = npoints)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Points around the arc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xoff <- rx * cos(theta)
  yoff <- ry * sin(theta)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rotate them around the centre of the arc
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xoff2 <- xoff * cos(phi) - yoff * sin(phi)
  yoff2 <- xoff * sin(phi) + yoff * cos(phi)
  
  res <- data_frame(
    x = cx + xoff2,
    y = cy + yoff2
  )
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Eqn 5.4 - angle between 2 vectors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
angle <- function(u, v){
  atan2(v[2], v[1]) - atan2(u[2], u[1])
}







# === svg-path-bezier.R =================================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-calculate the basis functions for different N points:
# Cubic Beziers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bf3s <- lapply(1:100, function(N) {
  t  <- seq.int(0, 1, length.out = N)
  
  matrix(c(
    1 * t^0 * (1 - t)^3,
    3 * t^1 * (1 - t)^2,
    3 * t^2 * (1 - t)^1,
    1 * t^3 * (1 - t)^0
  ), ncol = 4
  )
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-calculate the basis functions for different N points:
# Quadratic Beziers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bf2s <- lapply(1:100, function(N) {
  t  <- seq.int(0, 1, length.out = N)
  
  matrix(c(
    1 * t^0 * (1 - t)^2,
    2 * t^1 * (1 - t)^1,
    1 * t^2 * (1 - t)^0
  ), ncol = 3
  )
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert cubic and quadratic beziers to data.frame of coordinates along curve
#'
#' @param x,y coords of control points
#' @param N Number of output points
#'
#' @return data.frame of coords
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bezier3_to_df <- function(x, y, N = 5) {
  stopifnot(length(x)== 4, length(y) == 4)
  data_frame(
    x = rowSums(bf3s[[N]] * matrix(x, nrow = N, ncol = 4, byrow = TRUE)),
    y = rowSums(bf3s[[N]] * matrix(y, nrow = N, ncol = 4, byrow = TRUE))
  )
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname bezier3_to_df
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bezier2_to_df <- function(x, y, N = 5) {
  stopifnot(length(x)== 3, length(y) == 3)
  data_frame(
    x = rowSums(bf2s[[N]] * matrix(x, nrow = N, ncol = 3, byrow = TRUE)),
    y = rowSums(bf2s[[N]] * matrix(y, nrow = N, ncol = 3, byrow = TRUE))
  )
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert bezier control points to data.frame of coordinates along curve
#'
#' This is an old version - just keeping around for sanity checking.  For small
#' N this is the same speed as the current 'bezier_to_df', but for larger N
#' it is slower.
#'
#' @param x,y coords of control points
#' @param N Number of output points
#'
#' @return data.frame of coords along cubc bezier
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bezier3_to_df_loop <- function(x, y, N = 5) { #nocov start
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Allocate space for result
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xs <- numeric(N)
  ys <- numeric(N)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bezier param 't' at which to evaluate points
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ts <- seq.int(0, 1, length.out = N)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate bezier at N locations
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq.int(1, N, 1)) {
    t  <- ts[[i]]
    nt <- 1 - t
    bf <- c(
      1 * t^0 * nt^3,
      3 * t^1 * nt^2,
      3 * t^2 * nt^1,
      1 * t^3 * nt^0
    )
    xs[[i]] <- sum(bf * x)
    ys[[i]] <- sum(bf * y)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Faster data.frame creation than using 'data.frame(x=xs, y=ys)'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data_frame(x = xs, y = ys)
} #nocov end







# === utils.R =================================================================================


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL or NA operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%||%" <- function(x, y) {
  if (is.null(x) || is.na(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Trim the end off a word.
#'
#' @param x string
#' @param len number of characters to trim
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
trim <- function(x, len) {
  substr(x, 1, max(nchar(x) - len, 1))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simpler version of 'modifyList()'.  Idea borrowed from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modify_list <- function (base, new) {
  for (i in names(new)) base[[i]] <- new[[i]]
  base
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' data.frame creation
#'
#' 20x Faster than 'data.frame()', but with *ZERO* sanity checking and
#' no support for row.names
#'
#' @param ... named arguments
#'
#' @return data.frame
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_frame <- function(...) {
  ll <- list(...)
  structure(
    ll,
    class     = 'data.frame',
    row.names = .set_row_names(length(ll[[1]]))
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get an element from an XML document by ID. Return NA if ID does not exist.
#'
#' This function is called when a \code{<use>} tag references another element
#' or a a colour tag references a gradient element.
#'
#' @param svg xml document containing svg
#' @param id id, #id, url(#id), url('#id')
#'
#' @return SVG element with the given id, otherwise NA
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_element_by_id <- function(svg, id) {
  
  # Sanity check that 'svg' and 'id' are valid
  if (is.null(svg) || is.null(id) || is.na(id)) {
    return(NA)
  }
  
  # Convert 'url(#id)' referees to just the 'id' part which is all
  # xml2 wants.
  id    <- gsub("url\\('#|url\\(#|'\\)|\\)|#", "", id)
  
  # Convert the id to an xpath
  xpath <- paste0("//*[@id='", id, "']")
  
  # featch the element in the svg
  xml   <- xml2::xml_find_first(svg, xpath)
  
  xml
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal function for perfroming do.call(rbind...)
#'
#' This function expands all data.frames to have the same columns, whereas
#' base R just complains a throws an error
#'
#' @param dfs list of data.frames
#'
#' @return single data.frame
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rbind_dfs <- function(dfs) {
  
  # Drop NULLs
  dfs <- Filter(Negate(is.null), dfs)
  
  # Compile full list of all column names in all data.frames.
  # TODO: Could do something tricky here to try and retain column ordering?
  all_names <- unique(unlist(lapply(dfs, names)))
  
  # Expand all data.frames to include all column names
  dfs <- lapply(dfs, function(df) {
    new_names <- setdiff(all_names, names(df))
    if (length(new_names)) {
      print(new_names)
    }
    for (name in new_names) {
      df[[name]] <- NA
    }
    df
  })
  
  # rbind should complain now as all data.frames are guaranteed to have the
  # same column names
  do.call(rbind, dfs)
}


