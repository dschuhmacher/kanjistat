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
    params <- as.numeric(params)
    
    # params <- rescale_points(params, a=factors)
    # Rescaling X
    params[seq(1, length(params), by = 2)] <- (params[seq(1, length(params), by = 2)]) * factors[1]
    # Rescaling Y
    params[seq(2, length(params), by = 2)] <- (params[seq(2, length(params), by = 2)]) * factors[2]
    
    list(command = cmd_letter, params = params)
  })
  
  return(parsed_commands)
}

# A function for keeping track of relative points given a current point
adjust_relative_points <- function(relative_points, current_point) {
  adjusted_points <- matrix(relative_points, ncol = 2, byrow = TRUE) + current_point
  return(as.vector(adjusted_points))
}

# Given an SVG string and a parameter adjusting the number of points, 
# returns a point cloud as a 2xn matrix described by that SVG string.
# If eqspaced, a boolean, is true, the number of points per curve will be divided
# by the length of the curve.
# We expect strings with an M in the beginning and a number of CcSc afterwards
# otherwise the behaviour of the following function is a bit strange ...
points_from_svg <- function(svg_str, point_density, eqspaced, factors = c(1,1)) {
  # DS to do: we need factor back, but not for parse_svg_path
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
          p1 <- command$params[1:2]
          p2 <- command$params[3:4]
          p3 <- command$params[5:6]
        } else {
          # 'c' -> Relative Coordinates
          p1 <- adjust_relative_points(command$params[1:2], current_point)
          p2 <- adjust_relative_points(command$params[3:4], current_point)
          p3 <- adjust_relative_points(command$params[5:6], current_point)
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
          
          p2 <- command$params[1:2]
          p3 <- command$params[3:4]
        } else {
          # 's' -> Relative coordinates
          # Reflect previous control point for smooth transition
          if (length(previous_control_point) > 0) {
            p1 <- current_point + (current_point - previous_control_point)
          } else {
            # Assume a starting control point at current_point 
            p1 = current_point
          }
          
          p2 <- adjust_relative_points(command$params[1:2], current_point)
          p3 <- adjust_relative_points(command$params[3:4], current_point)
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
