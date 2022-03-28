#' Plot overview correlation scatter plot
#'
#' @family plotting
#' @param mat Numeric data frame or matrix
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param corr_method Method for correlation (one of "pearson","spearman","kendall").
#' @param hist.col Histogram color on the diagonal.
#' @param save.to.file Logical indicating whether to save to out_dir (TRUE) or print to panel (FALSE).
#' @param compute_w_NA If TRUE, consider columns and rows with at least 7 values (default = FALSE).
#' @param ... Additional parameters passed to \code{\link[psych]{pairs.panels}}. e.g. rug = T, scale = T (scale font by size of correlation), jiggle = T (jitter points).
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_overview_corr_scatt <- function(mat, out_dir = ".", labels = "", corr_method = "pearson", hist.col = "#92C5DE", save.to.file = T, compute_w_NA = F, ...) {
  # Only consider columns and rows with at least 7 values (not default)
  if (compute_w_NA) { # Prevent this error: "Error in r.con(r12, n): number of subjects must be greater than 3\n"
    if (ncol(mat) >= 7) {
      mat <- mat[, has_at.least_n.vals(mat, "column", 7)]
    }
    if (nrow(mat) >= 7) {
      mat <- mat[has_at.least_n.vals(mat, "row", 7), ]
    }
  }

  # Make plot
  p <- pairs.panels(mat,
    method = corr_method,
    density = T, # density curve on histogram
    cor = T, # correlations for regressions
    rug = F,
    lm = T, # linear regressions instead of LOESS smoothed fits
    ci = T, # confidence interval of regressions (shaded area)
    alpha = 0.05, # confidence levels alpha
    stars = T, # significance of linear models
    hist.col = hist.col,
    ellipses = F
  )
  title(main = paste(labels, collapse = "_"))

  # Graphing params
  if (save.to.file) {
    # Print to file
    grid_l <- ncol(mat) / 1.5 # file width
    pdf(sprintf("%s/%s_cor_scatter.pdf", out_dir, paste(labels, collapse = "_")), height = grid_l, width = grid_l) #
    print(p)
    dev.off()
  } else {
    # Print to graphic panel
    print(p)
  }
}


#' Plot all correlation scatter plots for pairwise variables
#'
#'
#' @family plotting
#' @param df A data frame with the first column as discrete values to group by (i.e. rowAnn_col), and the rest of the columns are numeric variables to plot.
#' @param rowAnn_col A column index (numeric) or name in df indicating which groups to stratify by.
#' @param cor.method Method for correlation (one of "pearson","spearman","kendall").
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @section Warning:
#' This function will take a long time to run depending on the number of variables.
#' @export
plot_indiv_corrscatt <- function(df, rowAnn_col = 1, cor.method = "spearman", out_dir = ".", labels = "") {
  # Get column name of rowAnn if it's an index
  if (is.numeric(rowAnn_col)) {
    rowAnn_col <- colnames(df)[rowAnn_col]
  }
  # Get all permutations
  perm <- permutations(n = ncol(df), r = 2, colnames(df), repeats.allowed = F) # gtools
  label <- paste(labels, collapse = "_")
  # For each permutation, make a scatter plot
  lapply(1:nrow(perm), function(i) {
    # Get columns of interest
    v1 <- perm[i, 1]
    v2 <- perm[i, 2]
    # Make plot and save
    p <- ggscatter(df,
      x = v1, y = v2, # color = rowAnn_col, palette = c("red", "orange", "blue"),
      add = "reg.line", conf.int = TRUE,
      cor.coef = TRUE, cor.method = cor.method, title = sprintf("%s-%s, %s", v1, v2, label),
      xlab = v1, ylab = v2
    )
    ggsave(sprintf("%s/%s.%s_%s_corr_regression2.png", out_dir, v1, v2, cor.method), plot = p)
  })
}
