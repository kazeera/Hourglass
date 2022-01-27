#' Functions defined in this file:
#'   impute_w_mean
#'   impute_w_mean_df
#'   impute_ds

#' Imputes NA/missing values in a vector with random values plus/minus the percent around the mean of the values in vector
#'
#' @param v A numeric vector containing NAs
#' @param p the percent around the mean, e.g. p=5%=5
#' @return The input vector with NAs imputed.
#' @export
impute_w_mean <- function(v, p = 5) {
  # Percent to decimal
  p <- p / 100
  # find mean
  m <- mean(v, na.rm = T)
  # Impute NAs vith random value between mean +/-  percent
  v[is.na(v)] <- runif(sum(is.na(v)), m * (1 - p), m * (1 + p))
  return(v)
}

#' Imputes NA/missing values in a data frame with random values plus/minus the percent around the mean across rows/columns
#'
#' @param df Numeric data frame or matrix
#' @param row.or.col Number indicating direction to apply function - either 1 (rows) or 2 (columns, default)
#' @param p the percent around the mean, e.g. p=5%=5
#' @return The input data frame with NAs imputed.
#' @export
impute_w_mean_df <- function(df, row.or.col = 2, p = 5) {
  # Impute and return
  apply(df, row.or.col, function(y) impute_w_mean(y, p)) %>% data.frame()
}

#' Impute numeric matrix in dataset object
#'
#' @param ds Dataset list object with vals, rowAnn, colAnn, name
#' @param p the percent around the mean to impute, default: p=5%=5
#' @param add_to_name String to add to the ds$name for new imputed dataset, default: "imputed"
#' @return Dataset object where NA in vals has been imputed with random values around mean (default is 5%)
#' @export
impute_ds <- function(ds, p = 5, add_to_name = "imputed") {
  make_dataset_ob(
    vals = ds$vals %>% impute_w_mean_df(p = p),
    rowAnn = ds$rowAnn,
    colAnn = ds$colAnn,
    name = paste(ds$name, add_to_name)
  )
}
