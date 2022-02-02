#' Takes in a string and creates a new folder with that name.
#'
#' String path can contain multiple folders that do not exist yet, separated by "/".
#' It returns the new path name, in case it needs to be assigned to a variable.
#' @param folder Path (string) of the new directory to be created.
#' @return The input parameter "folder" that indicates the path of the new folder.
#' @examples
#' create_folder("Sub1/Sub2")
#' @export
create_folder <- function(folder) {
  # Get a vector of all parent folders
  folders <- split_one.by.one(folder, "/")

  # Create folders
  invisible(lapply(folders, function(f) {
    if (!dir.exists(f)) {
      dir.create(f)
    }
  }))

  return(folder)
}

#' Read tabular data
#'
#' @description Read tabular data - file input can only be tab-delimited text or csv.
#'
#' @param filename String with path to file
#' @param sep The field separator character. Values on each line of the file are separated by this character.
#' @param ... Additional parameters passed to \code{\link[utils]{read.table}}
#' @return The data frame stored in file
#' @export
read_file <- function(filename, sep = "\t") {
  # Replace single/double slashes with forward slash for reading into R
  filename <- filename %>% gsub("\\\\", "/", .)

  # Get part after last period in file name, e.g. "txt" in "____.txt"
  file_type <- gsub(".*\\.", "", filename)
  if (file_type == "txt") {
    read.delim(filename, row.names = 1, sep = sep, stringsAsFactors = F)
  }
  if (file_type == "csv" | file_type == "tsv") {
    read.csv(filename, row.names = 1, stringsAsFactors = F)
  }
}

#' Search file names
#'
#' Provide a list of file names and regular expression to get the file name
#'
#' @param file_list A character vector of file names.
#' @param regex Regular expression pattern. See \code{\link[base]{regex}} for more info.
#' @return The name(s) of file(s) that match the regex
#' @export
get_file <- function(file_list, regex) {
  file_list[grep(regex, file_list)]
}


#' Customize name of comparison / output folder depending on inclusion/exclusion criteria and presence of row annotation 2
#'
#' @param current_dir The name of the output directory
#' @param filters A string in the form of filters delimited by default ";". Each filter has 3 parts: 1) column name in df, 2) operator either != or ==, 3) value in column to exclude/include
#' @param delim A string/character to seperate individuals filter by, default is ";"
#' @param all_out_dirs A character vector of directory names, if the current_dir name exists, the algorithm will append "1", "2", and so on until the dir name is unique.
#' @param rowAnn2 Name of row annotation 2 if applicable, otherwise NA.
#' @return New name of output folder
#' @example subset_by_filters(df, "Smoker==Yes;Cancer.subtype!=NA") # positively select for smokers and remove NA from Cancer.subtype column
#' @export
get_comparison_name <- function(current, filters, delim = ";", all_out_dirs = NULL, rowAnn2 = NA) {
  # Retrieve individual filters as elements in a vector
  filters <- filters %>%
    gsub("\"", "", x = .) %>% # Remove quotes
    strsplit(split = delim) %>% # Split by delimeter
    unlist() # Unlist result

  # Loop through filters
  for (filt in filters) {
    # Which operator?
    operator <- ifelse(grepl("!=", filt), "!=", ifelse(grepl("==", filt), "==", NA))

    # Get first part (column name) and second part of filter (value to keep/exclude)
    part1 <- get_nth_part(filt, operator, 1)
    part2 <- get_nth_part(filt, operator, 2)

    # Now depending on equality or inequality, perform correct filtering
    if (operator == "==") { # inclusion/keep
      current <- paste(current, part2)
      # e.g. c(NA, "1", "@3", NA) %in% "1" returns F,T,F,F
    }
    if (operator == "!=") { # exclude/remove
      current <- paste(current, paste("excl", part2, sep = "_"))
      # e.g. !c(NA, "1", "@3", NA) %in% NA returns F,T,T,F
    }
  }

  # Add color code
  if (!is.na(rowAnn2)) {
    col_label <- sprintf("dots %s", rowAnn2)
    current <- paste(current, col_label)
  }
  # In case a folder of the same name exists, append a number to the end of it
  i <- 1
  while (current %in% all_out_dirs) {
    current <- paste0(current, i)
    i <- i + 1
  }

  # Return the name of the output folder
  return(current)
}
