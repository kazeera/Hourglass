#' Functions defined in this file:
#'   plot_heatmap

#' Plots a heatmap
#'
#' Transforms a numeric data frame/matrix and plots corresponding heatmap using the pheatmap function and saves to file
#'
#' @family plotting
#' @param mat Numeric matrix or data frame directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name.
#' @param ann_col A data frame that defines annotations or features for columns of the data matrix. The row names should the same as the column names of the data matrix.
#' @param ann_row A data frame that defines annotations or features for rows of the data matrix. The row names should the same as the row names of the data matrix.
#' @param ann_colors A list that specifies annotation colors (each element is a named vector, where names are unique groups in annotations), names of list are the annotation names.
#' @param out_dir The output directory where the plot will be saved, default is current working
#' @param log10 Logical indicating whether to log10-transform values before plotting
#' @param z_score Logical indicating whether to plot z_scores
#' @param show_colnames Logical indicating whether to show column names on heatmap. See ?pheatmap
#' @param show_rownames Logical indicating whether to show row names on heatmap. See ?pheatmap
#' @param clust_row Logical indicating whether to cluster rows
#' @param clust_col Logical indicating whether to cluster rows
#' @param clustering_distance_rows Distance measure to be used for row clustering. Accepts the same values as method parameter in ?dist.
#' @param clustering_distance_cols Distance measure to be used for column clustering. Accepts the same values as method parameter in ?dist.
#' @param clustering_method The agglomeration/clustering method to be used. Accepts the same values as method parameter in ?hclust.
#' @param gradient_palette RColorBrewer palette. See RColorBrewer::display.brewer.all() for all options.
#' @param pheatmap_scale String describing which direction around the mean to scale by. Accepts the same values ("none", "row", "column") as scale parameter in ?pheatmap
#' @param scale_func Logical indicating whether to scale matrix using scale()
#' @param man_scale Logical indicating whether to manually scale each row/column using rescale() from scales package
#' @param man_scale_range Numeric vector of length 2 indicating desired range of values e.g. c(-4,4), applied when man_scale is TRUE.
#' @param col.or.row numeric - 1 for row, 2 for column. Only applicable when z_score, scale_func, man_scale are TRUE.
#' @param ... Additional plotting parameters. Parameters passed to pheatmap, see ?pheatmap.
#' @export
plot_heatmap <- function(mat, ann_row = NA, ann_col = NA, ann_colors = NA, plot_title = "", out_dir = ".", labels = "",
                         clust_row = F, clust_col = F, fontsize_col = 10, log10 = F, z_score = F, man_scale = T, man_scale_range = c(-2, 2), scale_func = F, col.or.row = 2, pheatmap_scale = "none",
                         clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean", clustering_method = "complete",
                         show_colnames = T, show_rownames = F, gradient_palette = "RdBu", border_color = NA, ...) {
  # Perform log10
  if (isTRUE(log10)) {
    mat <- log10(mat + 1)
    labels <- c(labels, "log10")
  }

  ## Scaling
  # Compute z scores (across cores for each stain)
  if (isTRUE(z_score)) {
    mat <- apply(mat, col.or.row, function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
    labels <- c(labels, "z")
  }

  # Scale - center around mean
  if (isTRUE(scale_func)) {
    if (col.or.row) { # column wise
      mat <- scale(mat, )
    } else {
      # Scale by rows
      mat <- t(mat) %>%
        scale() %>%
        t()
    }
  }

  ## Colors
  # Gradient for heatmap
  pal_grad <- get_col_palette(gradient_palette, rev = T) %>% get_col_gradient(100)

  # Rescale manually
  if (isTRUE(man_scale)) {
    # Scale each column (stain) so range is forced between 2 values {-4, 4}
    mat <- apply(mat, col.or.row, function(x) {
      scales::rescale(x, to = man_scale_range) # scales pkg
    })
  }
  if (isFALSE(clust_row)) {
    clustering_distance_rows <- F
  }
  if (isFALSE(clust_col)) {
    clustering_distance_cols <- F
  }

  # Make heatmap
  tryCatch(
    {
      # Make heatmap
      p <- pheatmap::pheatmap(mat,
        scale = pheatmap_scale,
        show_rownames = show_rownames,
        show_colnames = show_colnames,
        fontsize_col = fontsize_col,
        color = pal_grad,
        cluster_rows = clust_row,
        cluster_cols = clust_col,
        clustering_distance_rows = clustering_distance_rows,
        clustering_distance_cols = clustering_distance_cols,
        clustering_method = clustering_method,
        annotation_row = ann_row,
        annotation_col = ann_col,
        annotation_colors = ann_colors,
        main = plot_title,
        na_col = "black",
        border_color = border_color
      )
      # Print to file
      pdf(sprintf("%s/%s_heatmap.pdf", out_dir, paste(labels, collapse = "_"))) # file
      print(p)
      dev.off()
    },
    error = function(err) {
      ## do something with 'err', then maybe throw it again stop(err)
      x <- sprintf("%s", err)
      print(x)
      return()
    }
  )
}
