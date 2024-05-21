# A script to convert precomuted kanjistat distance data for use in the 
# visualisation tool found at 
# https://github.com/file-acomplaint/kanjidist-visualiser
#
# Presupposes remotes::install_github("dschuhmacher/kanjistat.data")

library(jsonlite)
library(kanjistat)
library(kanjistat.data)

description <- "The Optimal-Transport Kanji Distance as computed by Kanjistat."
title <- "Kanjistat Distance"

output_list <- list(nearest = list(), inverted=FALSE, scaleBy=0.5, description=description, title=title)

n_neighbors <- 20

# Iterate over matrix elements, finding the closest kanji and adding them to an output dictionary
for(k in 1:nrow(dct)) {
  closest <- order(dct[k,])[2:(2+n_neighbors)]
  
  k_name <- rownames(dct)[k]
  
  if (is.null(output_list$nearest[[k_name]])) {
    output_list$nearest[[k_name]] <- list()
  }
  
  for (j in 1:n_neighbors) {
    j_name <- rownames(dct)[closest[[j]]]
    value <- dct[k, closest[[j]]]
    if (value != 0.25) {
      output_list$nearest[[k_name]][[j_name]] <- value
      output_list$nearest[[j_name]][[k_name]] <- value
    }
  }
}

# Convert the list to JSON and write to disk
json_output <- toJSON(output_list, auto_unbox = TRUE)
write(json_output, file = "dkanjistat.json")