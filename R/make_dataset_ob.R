#' Initialize a dataset object
#'
#' Make a dataset object by specifying 3 main components: 1) values matrix (required), 2) column annotations, 3) row annotations
#'
#' @param vals Numeric matrix or data frame of values
#' @param colAnn Column annotations. Data frame that specifies the annotations for each column of vals. Each row defines features for a specific column. The row names should match the column names of vals.
#' @param rowAnn Row annotations. Data frame that specifies the annotations for each row of vals. Each row defines features for a specific row The row names should match the row names of vals.
#' @param name Name of dataset that will be used in plots and so on.
#' @return A dataset object (essentially a list with 4 elements: vals, colAnn, rowAnn, name)
#' @export
make_dataset_ob <- function(vals, colAnn = NULL, rowAnn = NULL, name = "") {
  # Check if row names match up
  if (!is.null(rowAnn) & all(rownames(vals) == rownames(rowAnn))) {
    errorCondition("Row names of annotation row and values matrix do not match.")
  }
  if (!is.null(colAnn) & all(colnames(vals) == rownames(colAnn))) {
    errorCondition("Row names of annotation column and column names values matrix do not match.")
  }
  # Return data set object
  list(
    vals = vals,
    colAnn = colAnn,
    rowAnn = rowAnn,
    name = name
  )
}
