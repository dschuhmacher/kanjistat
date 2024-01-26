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
parse_svg_path <- function(path) {
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
    
    # SVG follows a different convention from R, so we should negate the y-coordinates. 
    params[seq(2, length(params), by = 2)] <- -params[seq(2, length(params), by = 2)]
    
    list(command = cmd_letter, params = params)
  })
  
  return(parsed_commands)
}

# A function for keeping track of relative points given a current point
adjust_relative_points <- function(relative_points, current_point) {
  adjusted_points <- matrix(relative_points, ncol = 2, byrow = TRUE) + current_point
  return(as.vector(adjusted_points))
}

# Given an SVG string and a parameter adjusting the number of points per space
# unit, returns a point cloud as a 2xn matrix described by that SVG string.
points_from_svg <- function(svg_str, total_points=100, equidistant=TRUE) {
  parsed_path <- parse_svg_path(svg_str)
  arc_lengths <- list()
  parameters  <- matrix(nrow = 0, ncol = 8)
  
  list_of_points <- list()
  current_point <- c(0, 0)
  
  # Iterate through each command
  for (command in parsed_path) {
    if (command$command == "M") {
      # Move command sets the current point
      current_point <- command$params
    } else if (command$command == "C" || command$command == "c") {
      p0 <- current_point
      
      if (command$command == "C") {
        p1 <- command$params[1:2]
        p2 <- command$params[3:4]
        p3 <- command$params[5:6]
      } else {
        p1 <- adjust_relative_points(command$params[1:2], current_point)
        p2 <- adjust_relative_points(command$params[3:4], current_point)
        p3 <- adjust_relative_points(command$params[5:6], current_point)
      }
      
      parameters  <- rbind(parameters, c(p0,p1,p2,p3))
      arc_length <- 1
      if (equidistant) {
        arc_length <- cubic_bezier_arc_length(p0, p1, p2, p3)
      }
      
      arc_lengths <- c(arc_lengths, arc_length)
      
      current_point <- p3
    } # Let's hope that no other SVG commands are used ;)
  }
  
  total_arc_length <- sum(unlist(arc_lengths))
  used_points <- 0
  
  for (i in 1:nrow(parameters)) {
    p0 <- unlist(parameters[i,1:2])
    p1 <- unlist(parameters[i,3:4])
    p2 <- unlist(parameters[i,5:6])
    p3 <- unlist(parameters[i,7:8])
    num_points <- max(2, ceiling(total_points*arc_lengths[[i]]/total_arc_length))
    num_points <- min(num_points, total_points - used_points)
    used_points <- used_points + num_points
    if (num_points > 0) {
      # Sample points along the curve
      t_values <- seq(1/(2*num_points),(2*num_points-1)/(2*num_points), length.out = num_points)
      curve_points <- sapply(t_values, function(t) cubic_bezier_point(t, p0, p1, p2, p3))
      # Add curve points to the list and update the current point
      list_of_points <- c(list_of_points, list(t(curve_points)))
    }
  }
  do.call(rbind, list_of_points)
}

# A duplicate from the above, both should become one function eventually.
# Below uses a point density, above uses a fixed number of points.
spaced_points_from_svg <- function(svg_str, point_density) {
  parsed_path <- parse_svg_path(svg_str)
  list_of_points <- list()
  current_point <- c(0, 0)
  # Iterate through each command
  for (command in parsed_path) {
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
      
      # Determine the number of points based on arc length
      arc_length <- cubic_bezier_arc_length(p0, p1, p2, p3)
      num_points <- max(2, ceiling(point_density*arc_length))
      
      # Sample points along the curve
      t_values <- seq(1/(2*num_points),(2*num_points-1)/(2*num_points), length.out = num_points)
      curve_points <- sapply(t_values, function(t) cubic_bezier_point(t, p0, p1, p2, p3))
      
      # Add curve points to the list and update the current point
      list_of_points <- c(list_of_points, list(t(curve_points)))
      current_point <- p3
    } # Let's hope that no other SVG commands are used.
  }
  
  do.call(rbind, list_of_points)
}

