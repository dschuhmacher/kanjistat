# Functions in this file deal with Bézier curves, specifically for generating point clouds from SVG data.

# Point on a cubic Bézier curve. Numerical stability could be improved with a Horner scheme.
# The scaling factor one hundreth is due to the fact that the kvec SVG commands assume a range
# between 0 and 100, while the kvec point clouds are normalised between 0 and 1, which we reproduce here.
cubic_bezier_point <- function(t, p0, p1, p2, p3) {
  ((1 - t)^3 * p0 + 3 * (1 - t)^2 * t * p1 + 3 * (1 - t) * t^2 * p2 + t^3 * p3)/100
}

# Spatial derivative in curve time for a cubic Bézier curve
cubic_bezier_derivative <- function(t, p0, p1, p2, p3) {
  (3 * (1 - t)^2 * (p1 - p0) + 6 * (1 - t) * t * (p2 - p1) + 3 * t^2 * (p3 - p2))/100
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
parse_svg_path <- function(path, factor) {
  # Insert delimiter before negative numbers
  path <- gsub("-", ",-", path)
  
  # Insert delimiter
  path <- gsub("([MmLlHhVvCcSsQqTtAaZz])", " \\1", path)
  # Remove a comma before a negative number as the first parameter of an SVG command
  path <- gsub("([A-Za-z]),", "\\1", path)
  commands <- unlist(strsplit(path, " "))
  commands <- commands[commands != ""] # Remove empty elements

  # Extract parameters
  parsed_commands <- lapply(commands, function(cmd) {
    cmd_letter <- substr(cmd, 1, 1)
    params <- strsplit(substr(cmd, 2, nchar(cmd)), "[ ,]")[[1]]
    params <- as.numeric(params)
    
    # Check for relative commands to determine whether to use offset
    off <- offset
    if (grepl("[a-z]", cmd_letter)) {
      off <- c(0,0)
    }
    
    # Rescaling X
    params[seq(1, length(params), by = 2)] <- (params[seq(1, length(params), by = 2)]) * factor[1]
    # Rescaling Y
    params[seq(2, length(params), by = 2)] <- -(params[seq(2, length(params), by = 2)]) * factor[2]
    

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
# If spaced, a boolean, is true, the number of points per curve will be divided
# by the length of the curve.
points_from_svg <- function(svg_str, point_density, eqspaced, factor) {
  parsed_path <- parse_svg_path(svg_str, factor=factor)
  list_of_points <- list()
  current_point <- c(0, 0)
  k <- 0
  
  # Iterate through each command
  for (command in parsed_path) {
    k <- k + 1
    if (command$command == "M") {
      # The 'Move' command sets the current point
      current_point <- command$params
    } else if (command$command == "C" || command$command == "c") {
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
      
      if (eqspaced) {
        curve_points <- cubic_bezier_curve_eqspaced_cpp(point_density, 25, p0, p1, p2, p3)
      } else {
        curve_points <- cubic_bezier_curve_cpp(seq(0,1, length.out=floor(10*point_density)), p0, p1, p2, p3)
      }
      curve_points <- curve_points
      # Add curve points to the list and update the current point
      list_of_points <- c(list_of_points, list(curve_points))
      previous_control_point <- p2
      current_point <- p3
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
      
      if (eqspaced) {
        curve_points <- cubic_bezier_curve_eqspaced_cpp(point_density, 25, p0, p1, p2, p3)
      } else {
        curve_points <- cubic_bezier_curve_cpp(seq(0,1, length.out=floor(10*point_density)), p0, p1, p2, p3)
      }
      curve_points <- curve_points
      list_of_points <- c(list_of_points, list(curve_points))
      # Keep track for the next 'S' or 's' command
      previous_control_point <- p2
      
    } # Let's hope that no other SVG commands are used.
  }
  
  do.call(rbind, list_of_points)
}


# A helper function to calculate the distance to the points before and after
# in a polygonal line representing an SVG curve.
average_distances <- function(coords) {
  if (length(coords) < 3) {
    return(rep(0.1, length(coords)/2)) # Dealing with one-row matrices
  }
  diffs <- diff(matrix(as.numeric(rbind(coords[2,], coords, coords[nrow(coords)-1,])), ncol=2))
  distances <- sqrt(diffs[, 1]^2 + diffs[, 2]^2)
  
  distance_average <- (distances[-length(distances)] + distances[-1]) / 2
  return(distance_average)
}
