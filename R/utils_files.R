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


#' Read in custom analysis file
#'
#' @param file_xl The name of file path to an Excel file for custom analyses
#' @section Custom analysis file description:
#'
#' For custom/handpicked parameters for each strata, keys and values worksheets need to be specified.
#'
#' Worksheet Names:
#' I. "Keys"
#' Table with 3 columns:
#' Analysis_Name	Names of custom analyses
#' Group	The classifications
#' Group.Numbers	The numbers belonging to each classification
#'
#' II. "Values"
#' Table with atleast 3 columns:
#' Column 1 must share a name with a column annotation
#' Optional: next n columns can be additional column annotations e.g. Stroma or Tumor, Group..
#' For each "Group" in key, 2 columns are needed:
#'   1) x_y, where x is the group name in keys and y is a column in column annotations
#'   2) x_Group.Numbers, where x is the group name
#' @return List of 2 data frames: 1) keys, 2) values
#' @export
import_customAn_file <- function(file_xl) {
  # If file not found
  if (length(file_xl) == 0) {
    return(NULL)
  }

  library(openxlsx)
  # return a list, where each element is a relevant worksheet
  list(
    keys = read.xlsx(file_xl, sheet = "Keys"),
    values = read.xlsx(file_xl, sheet = "Values")
  )
}

#' PDAC-specific - Customize output folder name depending on exclusion criteria and presence of row annotation 2
#'
#' @param current_dir The name of the output directory
#' @param all_out_dirs A character vector of directory names, if the current_dir name exists, the algorithm will append "1", "2", and so on until the dir name is unique.
#' @param rowAnn2 Name of row annotation 2 if applicable, otherwise NA.
#' @param EXC_HRD Logical, include "excl HRD" label?
#' @param EXC_NEO Logical, include "excl neo" label?
#' @return New name of output folder
#' @export
get_out_dir <- function(current_dir, all_out_dirs = NULL, rowAnn2 = NA, EXC_HRD = F, EXC_NEO = F) {
  # Now append labels based on whether HRD/neo cases and color codes are included
  if (EXC_HRD) {
    current_dir <- paste(current_dir, "excl HRD")
  }
  if (EXC_NEO) {
    current_dir <- paste(current_dir, "excl neo")
  }
  if (!is.na(rowAnn2)) {
    col_label <- sprintf("dots %s", rowAnn2)
    current_dir <- paste(current_dir, col_label)
  }
  # In case a folder of the same name exists, append a number to the end of it
  i <- 1
  while (current_dir %in% all_out_dirs) {
    current_dir <- paste0(current_dir, i)
    i <- i + 1
  }

  # Return the name of the output folder
  return(current_dir)
}
