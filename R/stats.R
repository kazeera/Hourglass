#' Functions defined in this file:
#'   pval_to_stars
#'   means_of_groups
#'   medians_of_groups
#'   perform_t.test
#'   perform_wilcox

#' Convert p-values to stars
#'
#' Convert p-values to stars according to map legend p <= 0.001'****', 0.001 '***', 0.01 '**', 0.05 '*'
#'
#' @param pvals A numeric vector of pvals
#' @examples
#' runif(10,min = 0, 0.06)
#' pval_to_stars(c(0.0001, 0.00012, 0.002, 0.049, 0.05, 0.06))
#' Output: '**** ***  **   *    *'
#' @return A character vector with stars representing p-values
pval_to_stars <- function(pvals) {
  symnum(
    as.numeric(pvals),
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
    symbols = c("****", "***", "**", "*", "")
  ) %>%
    as.character()
}

#' Calculates means of a vector with discrete levels
#'
#' @param vals A numeric vector of values
#' @param groups A character vector specifying the respective group of each value
#' @return A data frame with 2 columns: 1) group name and 2) median value of group
#' @export
means_of_groups <- function(vals, groups) {
  df <- vals %>%
    aggregate(by = list(groups), mean, na.rm = TRUE) %>% # find median per group
    # Group.1        x
    # 1     immature 6201.351
    # 2 intermediate 4569.160
    # 3       mature 3995.845
    rename(c(Group.1 = Var, x = "value")) # rename columns
  # Rename rownames with group and return
  rownames(df) <- df[, 1]
  return(df)
}

#' Calculates medians of a vector with discrete levels
#'
#' @param vals A numeric vector of values
#' @param groups A character vector specifying the respective group of each value
#' @return A data frame with 2 columns: 1) group name and 2) median value of group
#' @export
medians_of_groups <- function(vals, groups) {
  df <- vals %>%
    aggregate(by = list(groups), median, na.rm = TRUE) # find median per group
  # Group.1        x
  # 1     immature 6201.351
  # 2 intermediate 4569.160
  # 3       mature 3995.845
  # Rename rownames with group and return
  rownames(df) <- df[, 1]
  return(df)
}

#' Computes t-test between 2 groups
#'
#' @param vals A numeric vector of values
#' @param groups A character vector specifying the respective group of each value
#' @param ... Additional parameters passed to ?pairwise.t.test
#' @return A number indicating p-value from statistical test
#' @export
perform_t.test <- function(vals, groups, ...) {
  # Perform 2-tailed t-test for pair-wise comparisons
  test <- pairwise.t.test(
    as.numeric(vals), as.character(groups), ...
  )
  # Return p-value
  print(test$p.value)
}

#' Computes Wilcoxon test between 2 groups
#'
#' @param vals A numeric vector of values
#' @param groups A character vector specifying the respective group of each value
#' @param ... Additional parameters passed to ?pairwise.wilcox.test.
#' @return A number indicating p-value from statistical test
#' @export
perform_wilcox <- function(vals, groups, p.adjust.method = "none", paired = F, ...) {
  # Perform wilcoxin test for pair-wise comparisons
  test <- suppressWarnings(pairwise.wilcox.test(
    as.numeric(vals), as.character(groups),
    paired = paired,
    p.adjust.method = p.adjust.method, ...
  ))
  # Return p value but don't print to console
  invisible(test$p.value)
}
