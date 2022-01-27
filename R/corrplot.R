#' Functions defined in this file:
#'   run_corrplot_analysis
#'   plot_corrplotgg
#'   plot_corrplot

#' Run correlation plot analysis
#'
#' Makes multiple correlation plots in 1 PDF file, each group and all values (total plots >= 2)
#'
#' @family plotting
#' @param df A data frame - first column: rowAnnotation column with groups, the rest of the columns are values.
#' @param rowAnn_col A column index (numeric) or name in df indicating which groups to stratify by.
#' @param gradient_palette RColorBrewer palette. See RColorBrewer::display.brewer.all() for all options.
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param plot_gg Should we use the function \code{link{plot_corrplotgg}} from this package? Otherwise, it uses \code{link{plot_corrplot}}.
#' @param pval.label How to display p-values? Allowed values are "p.signif" (stars) and "p.format" (number).
#' @param corr_method A character vector of 2 that correspond to "use" and "method" parameters in \code{\link[stats]{cor}}. The first defaults to "pairwise.complete.obs" and second should be one of "pearson","spearman","kendall".
#' @export
run_corrplot_analysis <- function(df, rowAnn_col = 1, out_dir = ".", labels = "", gradient_palette = "RdBu", plot_gg = T, corr_method = c("pairwise.complete.obs", "spearman"), pval.label = "p.signif") {
  # Get column name of rowAnn if it's an index
  if (is.numeric(rowAnn_col)) {
    rowAnn_col <- colnames(df)[rowAnn_col]
  }
  val_cols <- !colnames(df) %in% rowAnn_col

  # if(any(is.na(unlist(df[, val_cols])))) return()

  # Get number of vars other than annotation row
  n_stains <- ncol(df) - 1

  # from trial and error #TODO find more efficient way
  x <- ifelse(n_stains < 5, 0.6 * n_stains, 0.4 * n_stains)
  x <- n_stains / 2
  # Get unique elements
  grps <- df[, rowAnn_col] %>%
    as.character() %>%
    unique() %>%
    .[!is.na(.)]

  # Keep elements of interest (no NAs in annotations)
  df <- df[, rowAnn_col] %in% grps %>% df[., ]

  # Don't continue if NAs exist
  if (any(is.na(df[, val_cols]))) {
    return()
  }

  # Specify point sizes
  text_size <- ifelse(n_stains < 20, 0.071 * n_stains, 0.035 * n_stains) # stain label size
  pch_size <- ifelse(n_stains < 20, 0.081 * n_stains, 0.04 * n_stains) # star size

  tryCatch(expr = {
    # Some may return NULL if there are NAs/not enough groups because:
    # [1] "Error in cor.test.default(x = mat[, i], y = mat[, j], ...): not enough finite observations\n"
    # Initialize file
    filename <- sprintf("%s/%s_corrplot.pdf", out_dir, paste(labels, collapse = "_"))

    # Create pdf file of all plots
    pdf(filename, onefile = TRUE) # , height = x, width = x)
    if (plot_gg) {
      # save.image("run_cor.RData")
      # For all data points regardless of stratification
      plot_corrplotgg(df[, val_cols], labels = c("All", labels), gradient_palette = gradient_palette, out_dir = out_dir, corr_method = corr_method, pval.label = pval.label)

      # For each group,
      for (group in grps) {
        # Get indices
        keep_rows <- as.character(df[, rowAnn_col]) == group
        plot_corrplotgg(df[keep_rows, val_cols], labels = c(group, labels), gradient_palette = gradient_palette, out_dir = out_dir, corr_method = corr_method, pval.label = pval.label)
      }
    } else {
      # For all grps in' scaffold column name regardless of subtype
      plot_corrplot(df[, val_cols], labels = c("All", labels), gradient_palette = gradient_palette)

      # For each group,
      for (group in grps) {
        # Get indices
        keep_rows <- as.character(df[, rowAnn_col]) == group
        plot_corrplot(mat = df[keep_rows, val_cols], labels = c(group, labels), text_size = text_size, pch_size = pch_size, gradient_palette = gradient_palette)
      }
    }
    dev.off()
  })
}


#' Creates correlation plot using ggplot2 package
#'
#' @family plotting
#' @param mat Numeric dataframe or matrix, where columns will be correlated. Must not contain any NAs; use complete.cases(mat) to remove rows with NAs.
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param corr_method A character vector of 2 that correspond to "use" and "method" parameters in \code{\link[stats]{cor}}. The first defaults to "pairwise.complete.obs" and second is one of "pearson","spearman","kendall".
#' @param pval_color The color of the significance stars or p-value text.
#' @param grid.fill.color The color for grid fill.
#' @param grid.line.color The color for grid line.
#' @param pval.label How p-values are represented. Allowed values are "p.signif" (stars) and "p.format" (number).
#' @param circ_max The maximum size of circle within cells in grid.
#' @param star_size The size of star labels on correlation plots.
#' @param gradient_palette RColorBrewer palette. See \code{\link[RColorBrewer]{display.brewer.all}} for all options.
#' @param font_size The size of text labels on correlation plots. legend title. The size of legend text and plot title is font_size / 1.5. The size of legend text and plot subtitle is font_size / 3.
#' @param line_size The thickness of grid lines.
#' @param out_dir The output directory where the plot will be saved when save.to.file is TRUE, default is current working directory.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_corrplotgg <- function(mat, xlab = "", ylab = "", labels = "", corr_method = c("pairwise.complete.obs", "spearman"), pval_color = "white",
                            grid.fill.color = "white", grid.line.color = "black", pval.label = "p.signif", circ_max = NULL, star_size = NULL,
                            gradient_palette = "RdBu", font_size = 15, line_size = 1, out_dir = ".", save.to.file = F) {
  # Don't continue if NAs exist
  if (any(is.na(mat))) {
    errorCondition("Cannot make correlation plots with NAs")
  }

  # Make color palette gradient
  pal_grad <- get_col_palette(gradient_palette, rev = T) %>% get_col_gradient(50)

  tryCatch(expr = {
    # Get correlation matrices
    corr_mat <- cor(mat, use = corr_method[1], method = corr_method[2])

    # Cluster correlation matrix
    corr_mat <- cluster_corrmat(corr_mat)

    # Rename columns
    mat2 <- melt(corr_mat)
    colnames(mat2) <- c("Var1", "Var2", "corr")

    # Add stars if applicable
    if (!isFALSE(pval.label)) {
      # Add p values
      sig_test <- cor.mtest(mat, method = corr_method[2], exact = F) # package corrplot
      # Get p value matrix from sig_test, melt, and bind "value"/3rd column to melted data frame
      mat2 <- cbind(mat2, p.value = round(melt(sig_test$p)[, 3], 2))
      mat2$p_stars <- pval_to_stars(mat2$p.value)
    }

    # Get circle size (so it doesn't go passed geom tile boundaries)
    # Get star size (geom_text, so it's not too small/big)
    x <- length(unique(mat2$Var1))
    circ_max <- ifelse(is.null(circ_max), 130 * (1 / x), circ_max) # 140.391*(1/x), circ_max)
    star_size <- ifelse(is.null(star_size), -0.25 * x + 8, star_size)
    if (star_size < 1) star_size <- 2

    # Font size
    if (x >= 10) font_size <- 10
    if (x >= 50) font_size <- 5

    # Plot
    # Create the heatmap
    p <- ggplot(mat2, aes(x = Var2, y = Var1)) +
      geom_tile(fill = grid.fill.color, color = grid.line.color) + # ,width = circ_max, height = circ_max) + #grid
      geom_point(aes(colour = corr, size = corr)) + # circles
      scale_size(guide = F, range = c(1, circ_max)) +
      scale_color_gradientn(colors = pal_grad, name = corr_method[2], guide = "colourbar", limits = c(-1, 1)) +
      scale_x_discrete(expand = c(0, 0)) + # remove space between grid and axes
      scale_y_discrete(expand = c(0, 0)) +
      coord_equal(ratio = 1)

    # Add stars if applicable
    if (!isFALSE(pval.label)) {
      if (pval.label == "p.signif") {
        p <- p +
          geom_text(aes(label = p_stars), size = star_size, color = pval_color, vjust = 0.8)
      }
      if (pval.label == "p.format") {
        p <- p +
          geom_text(aes(label = p.value), size = star_size, color = pval_color, vjust = 0.5)
      }
    }

    # Theme and labels
    p <- p +
      theme(
        panel.background = element_blank(), # remove background color and lines
        plot.title = element_text(colour = "black", size = font_size / 1.5),
        plot.subtitle = element_text(colour = "black", size = font_size / 3),
        axis.line = element_line(colour = "black", size = line_size), # increase the axis-line thickness and change the color to blac
        # Ticks
        axis.ticks = element_line(colour = "black", size = line_size), # increase the tick thickness)
        axis.ticks.length = unit(.25, "cm"),
        # Axes labels
        axis.text = element_text(colour = "black", size = font_size), # face = "bold"),
        axis.text.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0), angle = 45, vjust = 1, hjust = 1), # , angle = 90, vjust= 0.5), #increase space between x axis title and labels
        axis.text.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
        # axes tick labels
        axis.title = element_blank(), # axes title labels
        # legend
        legend.text = element_text(colour = "black", size = font_size / 1.5), # face = "bold"),
        legend.title = element_text(colour = "black", size = font_size)
      ) + # , face = "bold"))+
      labs(
        title = paste(labels, collapse = "_"),
        subtitle = out_dir,
        caption = sprintf("%s", ifelse(!isFALSE(pval.label), "p <= 0.001 '****', 0.001 '***', 0.01 '**', 0.05 '*'", "")),
        y = ylab,
        x = xlab
      )

    if (save.to.file) {
      # Graphing params
      file_h <- (length(unique(mat2$Var2)) + 7) / 4 + 2 # file width
      # Print to file
      filename <- sprintf("%s/%s_corrplot.pdf", out_dir, paste(labels, collapse = "_"))
      ggsave(filename, plot = p, width = file_h, height = file_h, units = "cm", limitsize = F)
    } else {
      # Print to image panel
      print(p)
    }
  }, error = function(err) {
    print(sprintf("%s", err))
    return()
  }) # end tryCatch
}


#' Creates correlation plot using corrplot package
#'
#' @family plotting
#' @param mat Numeric data frame or matrix, where columns will be correlated. Must not contain any NAs; use complete.cases(mat) to remove rows with NAs.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param text_size The size of text labels on correlation plots. Color legend size is text_size/1.5.
#' @param pch_size The size of star labels.
#' @param gradient_palette RColorBrewer palette. See \code{\link[RColorBrewer]{display.brewer.all}} for all options.
#' @param corr_method A character vector of 2 that correspond to "use" and "method" parameters in \code{\link[stats]{cor}}. The first defaults to "pairwise.complete.obs" and second is one of "pearson","spearman","kendall".
#' @return Plot object.
#' @export
plot_corrplot <- function(mat, labels = "", text_size = 0.5, pch_size = 0.5, gradient_palette = "RdBu", corr_method = c("pairwise.complete.obs", "spearman")) {
  # Make color palette gradient
  pal_grad <- get_col_palette(gradient_palette, rev = T) %>% get_col_gradient(50)

  tryCatch(expr = {
    # Get correlation matrices and significance tables
    sig_test <- corrplot::cor.mtest(mat, method = corr_method[2])
    corr_mat <- cor(mat, use = corr_method[1], method = corr_method[2])

    # Make plot
    corrplot(corr_mat,
      method = "circle", col = pal_grad, # gradient palette defined in constants.R
      # stain labels formatting
      tl.cex = text_size, tl.col = "black",
      # significance formatting
      # Note: p<.001 = 3 stars, .01 = 2 stars, .05 = 1 star)
      p.mat = sig_test$p, insig = "label_sig", sig.level = c(.001, .01, .05),
      # color legend options - text size and put on bottom
      cl.cex = text_size / 1.5, cl.pos = "b",
      # significance labels formatting
      pch.cex = pch_size, pch.col = "white", order = "AOE",
      # put number of cores in brackets in title
      title = sprintf("%s (%s)", paste(labels, collapse = "_"), nrow(df)),
      mar = c(0, 0, 1, 0)
    )
  }, error = function(err) {
    print(sprintf("%s", err))
    return()
  }) # end tryCatch
}
