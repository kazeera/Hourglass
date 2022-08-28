#' Subset data frame to the rows and/or columns specified
#'
#' @param df The data frame or matrix to subset
#' @param rows_to_keep Numeric, logical or character vectors of rows to keep in subset. Note: character for named rows only
#' @param cols_to_keep Numeric, logical or character vectors of columns to keep in subset. Note: character for named columns only
#' @return A data frame subsetted to specified rows/columns.
#' @export
subset_dataframe <- function(df, rows_to_keep = NULL, cols_to_keep = NULL) {
  # keep all rows_to_keep/cols_to_keep if not indicated
  if (is.null(rows_to_keep)) {
    rows_to_keep <- 1:nrow(df)
  }

  if (is.null(cols_to_keep)) {
    cols_to_keep <- 1:ncol(df)
  }

  # subset and return
  data.frame(df[rows_to_keep, cols_to_keep,drop=FALSE])
}

#' Subset dataset list object to the rows and/or columns specified
#'
#' @param ds A dataset object (a list with any of vals, rowAnn, colAnn)
#' @param rows_to_keep Numeric, logical or character vectors of rows to keep in subset. Note: character for named rows only
#' @param cols_to_keep Numeric, logical or character vectors of columns to keep in subset. Note: character for named columns only
#' @return A dataset subsetted to specified rows/columns
#' @export
subset_dataset <- function(ds, rows_to_keep = NULL, cols_to_keep = NULL) {
  # keep all rows_to_keep/cols_to_keep if not indicated
  if (!is.null(ds$vals)) {
    ds$vals <- subset_dataframe(ds$vals, rows_to_keep, cols_to_keep)
  }

  if (!is.null(ds$rowAnn)) {
    ds$rowAnn <- subset_dataframe(ds$rowAnn, rows_to_keep = rows_to_keep)
  }

  if (!is.null(ds$colAnn)) {
    ds$colAnn <- subset_dataframe(ds$colAnn, rows_to_keep = cols_to_keep)
  } # this is right

  # return
  return(ds)
}

#' Subsets a dataframe rows based on filters
#'
#' @param df A dataframe
#' @param filters A string in the form of filters delimited by default ";". Each filter has 3 parts: 1) column name in df, 2) operator either != or ==, 3) value in column to exclude/include
#' @param delim A string/character to seperate individuals filter by, default is ";"
#' @return logical vector of length = nrow(df) indicating rows to keep with respect to inclusion/exclusion criteria
#' @details subset_by_filters(df, "Smoker==Yes;Cancer.subtype!=NA") # positively select for smokers and remove NA from Cancer.subtype column
#' @export
subset_by_filters <- function(df, filters, delim = ";") {
  # Initialize a vector of rows to keep
  keep <- rep(TRUE, nrow(df))

  # Return if NA
  if(is.na(filters)){
    return(keep)
  }

  # Retrieve individual filters as elements in a vector
  filters <- filters %>%
    gsub("\"", "", x = .) %>% # Remove quotes
    strsplit(split = delim) %>% # Split by delimeter
    unlist() # Unlist result

  # Loop through filters
  for (filt in filters) {
    rows_to_keep <- evaluate_filter(df, filt)
    keep <- keep & rows_to_keep
  }
  return(keep)
}

#' Subsets a dataframe based on filters
#'
#' @param df A dataframe
#' @param filters A string with 3 parts: 1) column name in df, 2) operator either != or ==, 3) value in column to exclude/include
#' @details evaluate_filter(df, "Smoker==Yes") # positively select for smokers
#' @export
evaluate_filter <- function(df, filt) {
  # Which operator?
  operator <- ifelse(grepl("!=", filt), "!=", ifelse(grepl("==", filt), "==", NA))

  # Get first part (column name) and second part of filter (value to keep/exclude)
  part1 <- get_nth_part(filt, operator, 1)
  part2 <- get_nth_part(filt, operator, 2)


  if (part1 %in% colnames(df)) {
    if (part2 %in% df[, part1]) {
      # Now depending on equality or inequality, perform correct filtering
      if (operator == "==") { # inclusion/keep
        # e.g. c(NA, "1", "@3", NA) %in% "1" returns F,T,F,F
        return(as.character(df[, part1]) %in% part2)
      }
      if (operator == "!=") { # exclude/remove
        # e.g. !c(NA, "1", "@3", NA) %in% NA returns F,T,T,F
        return(!(as.character(df[, part1]) %in% part2))
      }
    }
  }

  # Return all TRUE if there's an error -- if (is.na(operator))
  print(sprintf("Error in filter: %s", filt))
  return(rep(TRUE, nrow(df)))
}
