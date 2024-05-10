# Functions in this file deal with Bézier curves, specifically for generating point clouds from SVG data.

# Point on a cubic Bézier curve. Numerical stability could be improved with a Horner scheme.
cubic_bezier_point <- function(t, p0, p1, p2, p3) {
  ((1 - t)^3 * p0 + 3 * (1 - t)^2 * t * p1 + 3 * (1 - t) * t^2 * p2 + t^3 * p3)
}

# Spatial derivative in curve time for a cubic Bézier curve
cubic_bezier_derivative <- function(t, p0, p1, p2, p3) {
  (3 * (1 - t)^2 * (p1 - p0) + 6 * (1 - t) * t * (p2 - p1) + 3 * t^2 * (p3 - p2))
}

# Euclidean differential Arc Length of a Cubic Bézier curve
cubic_arc_length_diff <- function(t, p0, p1, p2, p3) {
  v <- cubic_bezier_derivative(t, p0, p1, p2, p3)
  sqrt(sum(v * v))
}

# Total arc length of a cubic Bézier curve
cubic_bezier_arc_length <- function(p0, p1, p2, p3, num_points = 100) {
  t_values <- seq(0, 1, length.out = num_points)
  speed_values <- sapply(t_values, function(t) cubic_arc_length_diff(t, p0, p1, p2, p3))
  
  # Numerically integrating with Simpson's rule, accuracy could be improved
  sum(diff(t_values) * (speed_values[-length(speed_values)] + speed_values[-1]) / 2)
}

# Parses a string of SVG curves, as they occur in kanjivec, to a list.
# For a complete specification of SVG commands, see
# https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d#path_commands
parse_svg_path <- function(path, factors=c(1,1)) {
  # Insert comma before negative numbers
  path <- gsub("-", ",-", path)
  
  # Insert space before MmLlHhVvCcSsQqTtAaZz
  path <- gsub("([MmLlHhVvCcSsQqTtAaZz])", " \\1", path)
  # Remove a comma before a negative number if it is first parameter of an SVG command
  path <- gsub("([A-Za-z]),", "\\1", path)
  commands <- stringr::str_split_1(path, " ")
  commands <- commands[commands != ""] # Remove empty elements

  # Extract parameters
  parsed_commands <- lapply(commands, function(cmd) {
    cmd_letter <- substr(cmd, 1, 1)
    params <- stringr::str_split_1(stringr::str_sub(cmd, 2), "[ ,]")
    params <- matrix(as.numeric(params), ncol=2, byrow=TRUE)
    # Rescaling X
    params[,1] <- params[,1] * factors[1]
    # Rescaling Y
    params[,2] <- params[,2] * factors[2]

    list(command = cmd_letter, params = params)
  })
  
  return(parsed_commands)
}


# Given an SVG string and a parameter adjusting the number of points, 
# returns a point cloud as a 2xn matrix described by that SVG string.
# If eqspaced, a boolean, is true, the number of points per curve will be divided
# by the length of the curve.
# We expect strings with an M in the beginning and a number of CcSc afterwards
# otherwise the behaviour of the following function is a bit strange ...
points_from_svg <- function(svg_str, point_density, eqspaced, factors = c(1,1)) {
  parsed_path <- parse_svg_path(svg_str, factors)
  # list_of_points <- list()
  # current_point <- c(0, 0)  # DS: removed to ensure the function fails if something is weird with the string
  
  # Iterate through each command
  for (command in parsed_path) {
    if (command$command == "M") { # to do Move makes first point rest does without first point, maybe shift last point to real last point
      # The 'Move' command sets the current point *and* a first point of the list
      # (although this is not strictly what the M says, it allows us to easily do without (quasi-)double points
      current_point <- command$params
      list_of_points <- list(current_point)   # is matrix and direct cbind really slower here?
    } else {
      if (command$command == "C" || command$command == "c") {
        # The "Cubic Bézier" command draws the eponymous curve
        p0 <- current_point
      
        if (command$command == "C") {
          # 'C' -> Absolute Coordinates
          p1 <- command$params[1,]
          p2 <- command$params[2,]
          p3 <- command$params[3,]
        } else {
          # 'c' -> Relative Coordinates
          p1 <- command$params[1,] + current_point
          p2 <- command$params[2,] + current_point
          p3 <- command$params[3,] + current_point
        }
      } else if (command$command == "S" || command$command == "s") {
        # Smooth Cubic Bézier command
        p0 <- current_point
        
        if (command$command == "S") {
          # 'S' -> Absolute coordinates
          # Reflect previous control point for smooth transition
          if (length(previous_control_point) > 0) {
            p1 <- current_point + (current_point - previous_control_point)
          } else {
            # Assume a starting control point at current_point
            p1 = current_point
          }
          
          p2 <- command$params[1,]
          p3 <- command$params[2,]
        } else {
          # 's' -> Relative coordinates
          # Reflect previous control point for smooth transition
          if (length(previous_control_point) > 0) {
            p1 <- current_point + (current_point - previous_control_point)
          } else {
            # Assume a starting control point at current_point 
            p1 = current_point
          }
          
          p2 <- command$params[1,] + current_point
          p3 <- command$params[2,] + current_point
        }

      } # Let's hope that no other SVG commands are used.
    
      if (eqspaced) {
        curve_points <- cubic_bezier_curve_eqspaced_cpp(point_density, 25, p0, p1, p2, p3)
          # 25 "check points" is fully sufficient (so let's keep this hard wired):
          # when point_density is just around 20-30, the
          # computed theoretical distance between different strokes will vary a lot due to rounding
          # (so errors in no of check point is not seen), when point_density is high (e.g. 100),
          # the distances turn pretty perfect even with only 25 check points.
      } else {
        d <- p3 - p0
        approxlen <- sqrt(d[1]^2 + d[2]^2)
        numpoints <- 2 + round(point_density * approxlen);  # to match bezier.cpp l.83, but with 2+ instead
                                # of 1+ to slightly help the short very curved lines, where we get the length
                                # very wrong with our approximate method (creates slight inefficiency for 
                                # short but more or less straight lines)
        curve_points <- cubic_bezier_curve_cpp(seq(0,1, length.out=numpoints), p0, p1, p2, p3)
      }
      # Add curve points to the list (removing the point at the beginning, which we already have) and update the current point
      list_of_points <- c(list_of_points, list(curve_points[-1,]))
      # Keep track for the next 'S' or 's' command
      previous_control_point <- p2
      current_point <- p3 
      # DS: last line was only there for C and c in versions prior to 24/04/21 but I think this was a mistake
    }
  }
    
  do.call(rbind, list_of_points)
}


# A helper function to calculate the distance to the points before and after
# in a polygonal line representing an SVG curve.
average_distances <- function(coords) {
  n <- dim(coords)[1]
  if (n < 2) {
    return(rep(0.1, ceiling(n/2))) # Dealing with one-row matrices
  }
  diffs <- diff(coords)
  distances <- sqrt(diffs[, 1]^2 + diffs[, 2]^2)
  
  distance_average <- (distances[-(n-1)] + distances[-1]) / 2
  distance_average <- c(distances[1]/2, distance_average, distances[n-1]/2)
  # except for the /2 in the previous line, this code does the same as the old code
  return(distance_average)
}




# alternative pipeline for svg-strings to point clouds
#
# parses a wellformed d-string composed of cubic Beziér curves
# (only letters M, m, C, c, S, s allowed, must start with M or m and have no other M/m)
# and transforms it into a strict MCCC...C format (a list of a 2-vector followed
# by any number of 3x2 matrices containing in the rows the two control points and the
# endpoint for a C-representation)
strictformat_bezier <- function(path) {
  stopifnot(stringr::str_sub(path,1,1) %in% c("M","m"))
  path <- stringr::str_sub(path,2,-1)
  if (grepl("[MmLlHhVvQqTtAaZz]", path)) {
    stop("unexpected letters found in ", path)
  }
  
  # after that "," is our separator for splitting 
  path <- gsub("-", ",-", path)   # insert comma before negative numbers
  path <- gsub("([CcSs])", ",\\1,", path)
  path <- gsub("\\s+", ",", path) # replace any sequence of whitespaces by a comma
  path <- gsub(",+", ",", path) # replace multiple by single commas
  
  path <- gsub("^[,\\s]+", "", path, perl=TRUE)   # remove any whitespaces or commas at the beginning
  # (I don't think comma would be legal there; perl=TRUE since POSIX does apparently not
  # allow/understand \s in a character set)
  path <- gsub("[,\\s]+$", "", path, perl=TRUE)   # and at the end (just to be sure)
  
  atoms <- stringr::str_split_1(path, ",")
  
  current_point <- as.numeric(atoms[1:2])
  current_command <- NA
  previous_control_point <- current_point  # for the S/s commands: if there is no 
  # previous control point its assumed to be current point
  bezierlist <- list(current_point)  # make matrix in the end (seems faster)
  
  atoms <- atoms[-(1:2)]
  natoms <- length(atoms)
  athead <- 1
  while (athead <= natoms) {
    # if we do not have one of the following letter, the letter stored in 
    # current_command is relevant (if NA an error is returned)
    if (atoms[athead] %in% c("C", "c", "S", "s")) { 
      current_command <- atoms[athead]
      athead <- athead + 1
    }
    if (current_command  == "C") {
      bmat <- matrix(as.numeric(atoms[athead+0:5]), 2, 3)
      athead <- athead + 6
    } else if (current_command  == "c") {
      bmat <- matrix(current_point, 2, 3) + matrix(as.numeric(atoms[athead+0:5]), 2, 3)
      athead <- athead + 6
    } else if (current_command == "S") {
      bmat <- matrix(as.numeric(atoms[athead+0:3]), 2, 2)
      bmat <- cbind(2 * current_point - previous_control_point, bmat)  # reflection
      athead <- athead + 4
    } else if (current_command == "s") {
      bmat <- matrix(current_point, 2, 2) + matrix(as.numeric(atoms[athead+0:3]), 2, 2)
      bmat <- cbind(2 * current_point - previous_control_point, bmat)  # reflection
      athead <- athead + 4
    }
    
    previous_control_point <- bmat[,2]  # keep track for future S/s commands
    current_point <- bmat[,3]           # keep track für future C/c commands
    
    bezierlist <- c(bezierlist, list(bmat))
  } 
  
  beziermat <- do.call(cbind, bezierlist)
  return(beziermat)
}


scale_svg_path <- function(beziermat, factors){
  beziermat[1,] <- beziermat[1,] * factors[1]
  beziermat[2,] <- beziermat[2,] * factors[2]
  beziermat
}


# will be replaced by a function that directly takes an unscaled beziermat
# once this is included in the kanjivec format
points_from_svg2 <- function(beziermat, point_density, eqspaced, factors = c(1,1)) {
  # beziermat <- strictformat_bezier(svg_str)
  beziermat <- scale_svg_path(beziermat, factors)
  ncurves <- (dim(beziermat)[2]-1)/3
  stopifnot(all.equal(round(ncurves), ncurves))
  
  curve_points <- bezier_curve_cpp(beziermat, ncurves, point_density, eqspaced)
  curve_points
}

if (FALSE) {
  # kan <- fivebetas[[2]]
  kk <- samplekan("jouyou", 1)
  print(kk)
  wh <- which(kbase$kanji == kk)
  # wh <- 2121  # works for points_from_svg2, but not points_from_svg1
  kan <- kvecjoyo[[wh]]
  strokes <- get_strokes(kan, simplify=FALSE)
  bez <- sapply(as.list(strokes), \(x) {attr(x, "d")})
  plot(0,0, xlim=c(0,1), ylim=c(0,1), asp=1, xaxs="i", yaxs="i", type="n")
  allpoints1 <- matrix(0,0,2)
  allpoints2 <- matrix(0,0,2)
  cat(wh, " ")
  system.time(for (st in bez) {
    #print(st)
    temp1 <- points_from_svg(st, point_density=30/109, eqspaced=TRUE, factors = c(1,1))
    temp1 <- rescale_points(temp1, a=c(1,-1)/109, b=c(0,1))
    allpoints1 <- rbind(allpoints1, temp1)
    points(allpoints1, cex=0.6, pch=16)
  })
  system.time(for (st in bez) {
    #print(st)
    temp2 <- points_from_svg2(st, point_density=30/109, eqspaced=TRUE, factors = c(1,1))
    temp2 <- rescale_points(temp2, a=c(1,-1)/109, b=c(0,1))
    allpoints2 <- rbind(allpoints2, temp2)
    points(allpoints2, cex=0.8, col=2, lwd=1)
  })
  stopifnot(all.equal(allpoints1,allpoints2))  # only the last stroke
}

# points_from_svg2 fixes errors for kanji 2, 235, 359, 507, 633, 658, 809, 823, 1035, 1044, 1085
#   1306, 1556, 1651, 1798, 1815, 1824, 2033, 2058, 2121 (among 1:2136, all other kanji give
#   the matrix of points as points_from_svg1)

# current problems in points_from_svg: cannot treat spaces, implicitly repeated commands ("missing letters"), small m at
# beginning of string

# A combination of spaces and missing letters is often in (particularly long) 左はらい. But also elsewhere (perhaps
# these strokes have been manually corrected at some point) 

# small m at the beginning is e.g. in kanji 809, 1085, 1306; what to do with that is not everywhere well documented
# but according to some sources and visual inspection it should clearly be interpreted as M (absolute move)




