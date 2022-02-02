#' Sort a dataframe by the contents of its row (sort columnwise) or columns (sort row-wise)
#'
#' @param df A dataframe or matrix
#' @param row.or.col Either "row" or "column" specify what is being sorted
#' @param by The name of the column/row to sort by.
#' @return A data frame sorted to specified rows/columns order
#' @export
sort_dataframe <- function(df, row.or.col = "row", by) {
  if (row.or.col == "row") {
    df <- df[order(df[, by]), ]
  } else if (row.or.col == "column") {
    df[, order(df[by, ])]
  } else {
    NULL
  }
}

#' Sort dataset list object to specified rows and/or columns order
#'
#' @param ds A dataset object (a list with vals (required), rowAnn, colAnn)
#' @param row_order Numeric, logical or character vectors of row order. Note: character for named rows only
#' @param col_order Numeric, logical or character vectors of column order. Note: character for named columns only
#' @return A dataset sorted to specified rows/columns order
#' @export
sort_dataset <- function(ds, row_order = NULL, col_order = NULL) {
  # Keep all rows_to_keep/cols_to_keep if not indicated
  if (is.null(row_order)) {
    row_order <- 1:nrow(ds$vals)
  }

  if (is.null(col_order)) {
    col_order <- 1:ncol(ds$vals)
  }

  # Sort ds data frames depending on what changed
  ds$vals <- ds$vals[row_order, col_order]
  if (!is.null(ds$rowAnn)) {
    ds$rowAnn <- ds$rowAnn[row_order, ]
  }
  if (!is.null(ds$colAnn)) {
    ds$colAnn <- ds$colAnn[col_order, ]
  } # this is right

  return(ds)
}
