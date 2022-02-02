#' Remove outliers outside of 1st and 3rd quartile by making the values NA
#'
#' @param x A numeric vector, can contain NA.
#' @return The numeric vector without outliers (points outside 1st/3rd quartiles)
#' @export
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

#' Removes outliers using boxplot statistics
#'
#' @param x A numeric vector, can contain NA.
#' @return The numeric vector without outliers
#' @export
remove_outliers2 <- function(x) {
  x[!x %in% boxplot.stats(x)$out]
}


#' Remove outliers outside of 1st and 3rd quartile by making the values NA
#'
#' @param df Numeric data frame or matrix
#' @param row.or.col A number indicating direction to apply function: either 1 for row-wise, 2 (default) for column-wise.
#' @return The data frame without outliers
#' @export
remove_outliers_df <- function(df, row.or.col = 2) {
  apply(df, row.or.col, remove_outliers) %>% data.frame()
}

#' Identify outliers outside of 1st and 3rd quartile by making the values "lower" and "upper" respectively
#'
#' @param x A numeric vector, can contain NA.
#' @return The numeric vector where outliers have the values "lower" and "upper" (points outside 1st/3rd quartiles respectively)
#' @export
get_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- "lower"
  y[x > (qnt[2] + H)] <- "upper"
  return(y)
}
