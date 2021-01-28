#' Subset data frame to the rows and/or columns specified
#'
#' @param df The data frame or matrix to subset
#' @param rows_to_keep Numeric, logical or character vectors of rows to keep in subset. Note: character for named rows only
#' @param cols_to_keep Numeric, logical or character vectors of columns to keep in subset. Note: character for named columns only
#' @return A data frame subsetted to specified rows/columns
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
  data.frame(df[rows_to_keep, cols_to_keep])
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

#' Subset PDAC-specific row annotation table based on selection criteria, provided as arguments.
#'
#' @param rowAnn where row names are unique ids
#' @param EXC_HRD A logical, T will remove all HRD cases
#' @param EXC_neo A logical, T will remove all neo-adjuvant cases
#' @param LIMIT_TO_PANC A logical, T removes all non-pancreas organ controls (and missing cores) in Pancreas.tissue
#' @param LIMIT_TO_PDAC A logical, T positively selects for PDAC tissue in Pancreas.tissue
#' @return A character vector of rownames of rowAnn to keep in analysis
#' @export
subset_PDAC_rowAnn <- function(rowAnn, EXC_HRD = F, EXC_NEO = F, LIMIT_TO_PANC = F, LIMIT_TO_PDAC = F) {
  print(sprintf("Original rowAnn has %s rows.", nrow(rowAnn)))

  # Throw error if global variables do not exist
  if (any(!c("PANC.TISSUE", "NEO", "HRD") %in% ls(envir = .GlobalEnv))) {
    errorCondition("Ensure PANC.TISSUE, NEO, and HRD global variables are defined.")
  }

  # Subset PDAC-specific row annotation table based on selection criteria, provided as arguments.
  if (LIMIT_TO_PANC) {
    cores <- !is.na(rowAnn[, PANC.TISSUE])
    print(sprintf("Keep %s pancreas cores.", sum(cores, na.rm = T)))
    rowAnn <- rowAnn[cores, ]
  }
  if (LIMIT_TO_PDAC) {
    cores <- grep("PDAC", rowAnn[, PANC.TISSUE])
    print(sprintf("Keep %s PDAC cores.", length(cores)))
    rowAnn <- rowAnn[cores, ]
  }
  if (EXC_NEO) {
    cases <- rowAnn[, NEO] == "neo"
    print(sprintf("Remove %s neo cases.", sum(cases, na.rm = T)))
    rowAnn <- rowAnn[!cases, ]
  }
  if (EXC_HRD) {
    cases <- rowAnn[, HRD] == "HRD"
    print(sprintf("Next remove %s HRD cases.", sum(cases, na.rm = T)))
    rowAnn <- rowAnn[!cases, ]
  }
  print(sprintf("Final rowAnn has %s rows.", nrow(rowAnn)))

  # Return unique ids to keep
  rownames(rowAnn)
}
