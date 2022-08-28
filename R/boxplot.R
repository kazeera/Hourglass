
#' Creates overview box plots.
#'
#' @family plotting
#' @param df3 A data frame in long format with 3 columns: order must be: 1) variable to group by (groups of boxes) = factor/character, 2) levels within each group (boxes) = factor/character, 3) values = numeric
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param log10_y Logical (TRUE/FALSE) indicating whether to log10-transform y-axis.
#' @param lvl.colors A vector of colors to manually specify levels colors = box colors.
#' @param dot_color A string indicating what color to make points, if NA, points will not be shown.
#' @param gradient_palette RColorBrewer palette for variable colors if lvl.colors is NA. See RColorBrewer::display.brewer.all() for all options.
#' @param legend.title Title for legend.
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param font_size The size of axis.title, axis.text, plot.title, legend.text and legend.title. The size of plot.subtitle is font_size / 2.
#' @param line_size The thickness of axis lines.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_overview_boxplot <- function(df3, out_dir = ".", labels = "", log10_y = F, lvl.colors = NA, gradient_palette = "Spectral", legend.title = "Group", xlab = "variable",
                                  dot_color = "black", ylab = "value", font_size = 15, line_size = 1.3, save.to.file = T) {
  # Rename columns
  colnames(df3) <- c("variable", "level", "value")
  # Get colors
  if (any(is.na(lvl.colors))) {
    lvl.colors <- get_element_colors(unique(df3$level), colRamp = get_col_palette(gradient_palette))
  }
  # Extra step to prevent extra color codes - PDAC
  lvl.colors <- lvl.colors[names(lvl.colors) %in% df3$level]

  # Make plot
  p3 <- ggplot(df3, aes(x = variable, y = value, fill = level)) +
    geom_boxplot(aes(fill = level), outlier.color = NA) +
    geom_point(position = position_jitterdodge(), color = dot_color, size = 0.8, alpha = 0.5) +
    scale_fill_manual(values = lvl.colors) +
    labs(
      title = paste(labels, collapse = "_",
      y = ylab),
      x = xlab,
      fill = legend.title,
      subtitle = out_dir
    ) +
    theme(
      panel.background = element_blank(), # remove background color and lines
      plot.title = element_text(colour = "black", size = font_size),
      plot.subtitle = element_text(colour = "black", size = font_size / 2),
      axis.line = element_line(colour = "black", size = line_size), # increase the axis-line thickness and change the color to blac
      # Ticks
      axis.ticks = element_line(colour = "black", size = line_size), # increase the tick thickness)
      axis.ticks.length = unit(.25, "cm"),
      # Axes labels
      axis.text = element_text(colour = "black", size = font_size),
      axis.text.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0), hjust = 1, vjust = 1, angle = 45), # increase space between x axis title and labels
      axis.text.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
      # axes tick labels
      axis.title = element_text(colour = "black", size = font_size, face = "bold"), # axes title labels
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      # axis.text.x = element_text(angle = 45, hjust = 1),
      # legend
      legend.position = "top",
      legend.text = element_text(colour = "black", size = font_size),
      legend.title = element_text(colour = "black", size = font_size, face = "bold")
    )

  # Log10 the y-axis to show point spread better
  if (log10_y) {
    p3 <- p3 + scale_y_continuous(trans = "log10") # log transform
  }

  # Print to file
  # grid_h <- length(unique(df3$variable))+ 14 # file height
  grid_w <- length(unique(df3$variable)) + 6 # width

  if (save.to.file) {
    ggsave(
      file = sprintf("%s/%s_overview.pdf", out_dir, paste(labels, collapse = "_")), plot = suppressWarnings(p3),
      width = length(unique(df3$variable)) + 3, height = 7.5, limitsize = F
    )
  } else {
    print(p3)
  }
}


#' Creates single box plot.
#'
#' @family plotting
#' @param df A data frame in long format with 2-3 columns: 1) Box or level = factor/character, 2) Value = numeric, 3) Dots/points (color, optional) = factor/character.
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param log10_y Logical (TRUE/FALSE) indicating whether to log10-transform y-axis.
#' @param font_size The size of axis.text. The size of axis.title and plot.title is font_size / 1.3 and font_size / 2, respectively. The size of plot.subtitle, legend.text, and legend.title is font_size/3.
#' @param line_size The thickness of axis lines.
#' @param lvl.colors  A vector of colors to manually specify box colors.
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param rowAnns Optional. A character vector of 1-2 column names in ds$rowAnn.
#' @param alpha_dots Transparency (alpha) of dots. Accepted values in range 0-1.
#' @param alpha_box Transparency (alpha) of boxes. Accepted values in range 0-1.
#' @param point_size Size of dots.
#' @param jit_w Jitter width of dots.
#' @param show_stats Logical, if TRUE, will show statistics between all comparisons/boxes.
#' @param pval.test Only applies if show_stats is TRUE. String corresponding to method parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "t.test" and "wilcox.test".
#' @param pval.label Only applies if show_stats is TRUE. String corresponding to label parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "p.signif" (stars) and "p.format" (number).
#' @param trim_x Number of characters in x-axis labels.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_indiv_boxplot <- function(df, labels = "Group", out_dir = ".", log10_y = F, font_size = 25, show_stats = T, line_size = 1.3, lvl.colors = NA, xlab = "", ylab = "value", rowAnns = c(NA, NA), alpha_dots = 0.8, alpha_box = 1, point_size = 2, gradient_palette = "Spectral", jit_w = 0.1, pval.test = "t.test", pval.label = "p.signif", trim_x = 3, save.to.file = T, legend_position = "right") {
  #' @param df 2-3 columns. 1) Box or level, 2) Value 3) Dots (color)
  #' @param pval.label p-values on box plots, either "p.signif" (stars), "p.format" (numeric), etc.

  # Rename columns
  colnames(df)[1:2] <- c("box", "value")

  # Get palette for boxes if not specified
  if (any(is.na(lvl.colors))) {
    lvl.colors <- get_element_colors(unique(df$box), colRamp = get_col_palette(gradient_palette))
  }

  if (ncol(df) == 3) {
    colnames(df)[3] <- "dots"
  }

  # Make plot
  a <- ggplot(df, aes(box, value)) +
    geom_boxplot(aes(fill = box), width = 0.8, lwd = 1, color = "black", na.rm = T, outlier.color = NA) + # , alpha = alpha_box) +
    scale_fill_manual(values = lvl.colors)
  
  # Make list of unique elements
  ele <- unique(as.character(df$box))
  
  # Show statistics based on user specifications
  if(isTRUE(show_stats)){
    # Make list of combinations (order doesn't matter) for p-values
    comb <- combinations(n = length(ele), r = 2, v = ele, repeats.allowed = F) %>% # gtools
      split(., seq(nrow(.)))

    # Add significance levels
    # Star height relative to bars
    vjust <- ifelse(pval.label == "p.signif", 0.5, -0.1)
    a <- a + stat_compare_means(
      method = pval.test, comparisons = comb, na.rm = T, vjust = vjust, hide.ns = T,
      label = pval.label, size = font_size / 2.5, bracket.size = 1
    )
  }

  # Log scale
  if (log10_y) {
    a <- a + scale_y_continuous(trans = "log10") # log transform
  }

  # Account for secondary comparison (colors of dots)
  if (!is.null(df$dots)) {
    # Add the dots
    a <- a + geom_jitter(width = jit_w, pch = 16, aes(color = dots, fill = dots), size = point_size, alpha = alpha_dots, stroke = 0.9) +
      scale_color_manual(values = lvl.colors)
  } else {
    # a <- a + geom_jitter(width = jit_w, pch=21, fill="black", alpha=alpha_dots)
    # alpha_dots = 0.5
    a <- a + geom_jitter(width = jit_w, pch = 21, fill = "black", size = point_size, alpha = alpha_dots, stroke = 0.9, colour = "white")
  }

  # Trim x axis text to 3 characters
  a <- a + scale_x_discrete(labels = function(x) strtrim(x, trim_x))

  # If it belongs to a PDAC-specific analysis, reorder cols
  if ("PANC_TISS_ORDER" %in% ls(envir = .GlobalEnv)) {
    if (isTRUE(any(PANC_TISS_ORDER %in% ele) & length(ele) > 2)) { # if elements are just "adj_normal" and "PDAC" it'll mess up the order
      # Set subtype orders - PDAC
      panc_order <- PANC_TISS_ORDER[PANC_TISS_ORDER %in% ele] # PANC_TISS_ORDER <- c("adj_normal", "mature", "intermediate","immature") # in "1.import_data.R"
      a <- a + scale_x_discrete(limits = panc_order, labels = function(x) strtrim(x, trim_x))
    }
  }

  # Add labels to graph
  a <- a +
    labs(
      title = paste(labels, collapse = "_"),
      fill = ifelse(!is.na(rowAnns[1]), rowAnns[1], ""),
      color = ifelse(!is.na(rowAnns[2]), rowAnns[2], ""),
      caption = ifelse(show_stats, sprintf("%s%s", pval.test, ifelse(pval.label == "p.signif", ", p: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "")), ""),      subtitle = out_dir,
      y = ylab,
      x = xlab
    )

  # Add theme
  a <- a +
    theme(
      panel.background = element_blank(), # remove background color and lines
      plot.title = element_text(colour = "black", size = font_size / 2),
      plot.subtitle = element_text(colour = "black", size = font_size / 3),
      axis.line = element_line(colour = "black", size = line_size), # increase the axis-line thickness and change the color to blac
      # Ticks
      axis.ticks = element_line(colour = "black", size = line_size), # increase the tick thickness)
      # axis.ticks.x = element_line(margin = margin(t = 20, r = 0, b = 0, l = 0)), #increase space between x axis title and labels
      # axis.ticks.y = element_line(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.ticks.length = unit(.25, "cm"),
      # Axes labels
      axis.text = element_text(colour = "black", size = font_size),
      axis.text.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
      axis.text.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
      # axes tick labels
      axis.title = element_text(colour = "black", size = font_size / 1.3, face = "bold"), # axes title labels
      # axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), #increase space between x axis title and labels
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      # legend
      legend.text = element_text(colour = "black", size = font_size / 3),
      legend.title = element_text(colour = "black", size = font_size / 3),
      legend.position = legend_position)
  # guides(fill=guide_legend(nrow=ncol(df)-1,byrow=TRUE))# number of rows for legend

  if (save.to.file) {
    # Print to file
    filename <- sprintf("%s/%s_boxplot.png", out_dir, paste(labels, collapse = "_"))
    ggsave(filename, plot = a, width = length(ele) * 2, height = 7.5)
  } else {
    # Print to image panel
    print(a)
  }
}
