#' Makes discrete box plots for each parameter in column annotation
#'
#' @inheritParams run_comparison
#' @param rowAnn1 A column index (numeric) or name in ds$rowAnn indicating which groups to stratify by.
#' @param parameters Group names or parameters in colAnn1 that required its own barplot
#' @export
run_discrete_barplot_analysis <- function(ds, rowAnn1 = 2, colAnns = NA, parameters = "", out_dir = ".", gradient_palette = "RdBu") {
  if (is.numeric(rowAnn1)) {
    rowAnn1 <- colnames(df)[rowAnn1]
  }
  if (any(is.na(colAnns))) {
    return()
  }
  for (parameter in parameters) {
    # Define columns of interest if parameter matches
    keep_cols <- ds$colAnn[, colAnns[1]] == parameter # names

    # Get palette
    pal <- get_rowAnn_color_pal(ds, rowAnn1)
    pal <- pal$pal
    keep_rows <- ds$rowAnn[, rowAnn1] %in% names(pal)

    if ((sum(keep_cols,na.rm = T) < 1) | (sum(keep_rows,na.rm = T) < 2)) {
      next
    }
    # Make data frame where first column is row annotations
    df <- cbind(ds$rowAnn[keep_rows, rowAnn1], ds$vals[keep_rows, keep_cols])
    col_names <- ds$colAnn[keep_cols, colAnns[2]]
    colnames(df) <- c(rowAnn1, col_names)

    # Wide to long format
    df2 <- melt(df)

    # Make Het.Score stacked bar graphs
    plot_discrete_barplot(df2, out_dir = out_dir, plot_title = parameter, facet_by_var = T, legend_title = parameter, save.to.file = T)
  }
}


#' Plot discrete bar graph.
#'
#' @family plotting
#' @param df2 A data frame with 3 columns: 1) ID = discrete values to group by (i.e. rowAnn1) = bars, 2) variables on x-axis = group of bars, 3) value.
#' @param out_dir The output directory where the plots will be saved, default is current working directory.
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param facet_by_var If TRUE, facet groups of bars horizontally.
#' @param legend_title Title of legend.
#' @param pal Vector for colors of values.
#' @param plot_title Title of plot.
#' @param gradient_palette RColorBrewer palette. See RColorBrewer::display.brewer.all() for all options.
#' @param pos How bars should be stacked. Either "fill" (relative ratio, 100% bar) or "stack". See position parameter in \code{\link[ggplot2]{geom_bar}}
#' @param font_size The size of axis title on plots. The size of plot subtitle and caption is font_size / 2. The size of legend text and x axis text is font_size / 3 and font_size / 1.5.
#' @param line_size The thickness of axis lines.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_discrete_barplot <- function(df2, out_dir = ".", xlab = "", ylab = "", facet_by_var = F, legend_title = "", pal = NA, plot_title = "",
                                  gradient_palette = "RdBu", pos = "stack", font_size = 20, line_size = 2, save.to.file = F) {
  colnames(df2) <- c("ID", "variable", "value")

  # Remove Nas and convert value column to character
  df2 <- df2[!is.na(df2$value), ]
  df2$value <- as.factor(as.character(df2$value))

  if (is.na(pal)) {
    # Make color palette gradient
    pal <- get_col_palette(gradient_palette, rev = T) %>% get_element_colors(levels(df2$value), ., rearr = F)
  }

  tryCatch(
    {
      # Make plot
      g <- ggplot(df2, aes(x = ID, y = as.integer(value), fill = value)) +
        geom_bar(stat = "identity", position = "fill", width = .8) + # bars
        scale_y_continuous(labels = scales::percent) + # fill bars to 100%
        scale_fill_manual(name = legend_title, values = pal) +
        labs(
          title = plot_title,
          subtitle = out_dir,
          x = xlab,
          y = ylab
        ) +
        # Customize theme
        theme(
          panel.background = element_blank(), # remove background color and lines
          axis.line = element_line(colour = "black"), # increase the axis-line thickness and change the color to blac
          # Titles
          plot.subtitle = element_text(colour = "black", size = font_size / 2),
          plot.caption = element_text(colour = "black", size = font_size / 2),
          strip.text.x = element_text(size = font_size / 1.5),
          # Axes labels
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(size = font_size / 1.5, angle = 45, hjust = 1, margin = margin(t = 7, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
          axis.text.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
          # axes tick labels
          axis.title = element_text(colour = "black", size = font_size, face = "bold"), # axes title labels
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          # legend
          legend.text = element_text(colour = "black", size = font_size / 3, face = "bold"),
          legend.key.size = unit(1, "line")
        )
      # legend.position = "bottom")

      # Remove legend if it takes up too much space
      if (length(unique(df2$value)) > 30) {
        g <- g +
          theme(legend.position = "none")
      }

      # If we facet horizontally
      if (facet_by_var) {
        g <- g + facet_grid(~variable, scales = "free_x", space = "free")
      }
      # Save to file
      if (save.to.file) {
        # Print to file
        filename <- sprintf("%s/%s_discrete_barplot_full.pdf", out_dir, plot_title)
        ggsave(filename, plot = g, width = length(unique(df2$variable)) * 3 + 2, height = 7.5, limitsize = F)
      } else {
        # Print to image panel
        print(g)
      }
    },
    error = function(err) {
      print(sprintf("%s", err))
    }
  )
}
