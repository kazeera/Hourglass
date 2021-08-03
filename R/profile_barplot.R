#' Generates both fill and stacked bar plots and saves to file
#'
#' @param df A data frame - first column: rowAnnotation column with groups, the rest of the columns are values.
#' @param rowAnn_col A column index (numeric) or name in df indicating which groups to stratify by.
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param log_ Logical indicating whether to log2-transform values
#' @param gradient_palette RColorBrewer palette for variable colors if var_colors is NA. See RColorBrewer::display.brewer.all() for all options.
#' @export
run_profile_barplot <- function(df, rowAnn_col = 1, out_dir = ".", labels = "", log_ = F, gradient_palette = "RdBu") {
  # Get column name of rowAnn if it's an index
  if (is.numeric(rowAnn_col)) {
    rowAnn_col <- colnames(df)[rowAnn_col]
  }

  # Make 0 values NA so it doesn't mess with the factoring
  df[df == 0] <- NA

  # Columns with values
  val_cols <- !colnames(df) %in% rowAnn_col

  # Cluster and reorder rows based on dendogram
  tryCatch(
    {
      dend_row <- cluster_within_group(t(df[, val_cols]), factor(df[, rowAnn_col]))
      hclust_row <- as.hclust(dend_row)
      df <- df[hclust_row$order, ]
    },
    error = function(err) {
      print(sprintf("%s", err))
    }
  )

  # Add a new row with ID
  # Factor so rows are preservd
  df$ID <- df[, val_cols] %>%
    rownames() %>%
    factor(., levels = .)

  # Wide to long format
  df2 <- suppressMessages(melt(df, ID = "ID")) # reshape2

  # Log the values if necessary *note log of number <1 but >0 results in negative
  if (log_) {
    df2$value <- log2(df2$value)
    labels <- c(labels, "log")
  }

  # Make NA values 0 so it doesn't mess with the factoring
  df2$value[is.na(df2$value)] <- 0
  # Graphing params
  file_h <- ifelse(nrow(df2) < 24, 7, 15) # file height
  file_w <- (length(unique(df2$ID))) / 2 # file width
  # Initialize file
  filename <- sprintf("%s/%s_profile.pdf", out_dir, paste(labels, collapse = "_"))
  # Create pdf file of all plots
  pdf(filename, onefile = TRUE, width = file_w, height = file_h)

  # Plot for each bar graph type
  for (pos in c("fill", "stack")) {
    plot_profile_barplot(df2, legend_title = rowAnn_col, pos = pos, gradient_palette = gradient_palette, out_dir = out_dir, labels = labels, font_size = 30, save.to.file = F)
  }
  dev.off()
}

#' Plot profile bar graph
#'
#' @family plotting
#' @param df2 A long format data frame with 4 columns: 1) group (group of bars on x), 2) ID (individual bars on x), 3) variable (components within each bar), 4) value.
#' @section Example of input data frame:
#' group         ID        variable     value
#' 1       low 1_21043      CD3  273.2400
#' 2       low 1_36312      CD3  700.2100
#' 3       low 1_37265      CD3  931.1133
#' 4       low 1_39924      CD3 1503.2325
#' @param var_colors A vector of colors to manually specify variable colors.
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param gradient_palette RColorBrewer palette for variable colors if var_colors is NA. See RColorBrewer::display.brewer.all() for all options.
#' @param pos How bars should be stacked. Either "fill" (relative ratio, 100% bar) or "stack". See position parameter in \code{\link[ggplot2]{geom_bar}}
#' @param font_size The size of axis title on plots. The size of plot subtitle and caption is font_size / 2. The size of legend text and x axis text is font_size / 3 and font_size / 1.5.
#' @param line_size The thickness of axis lines.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#'
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_profile_barplot <- function(df2, pos = "stack", var_colors = NA, out_dir = ".", legend_title = "Group",
                                 labels = "", gradient_palette = "RdBu", line_size = 1, font_size = 10, save.to.file = F) {
  # Get colors for each variable (feature)
  if (is.na(var_colors)) {
    var_colors <- get_element_colors(df2$variable, get_col_palette(gradient_palette, rev = T))
    # Replace the index at the centre that is close to white with gray
    var_colors[ceiling(length(var_colors) / 2)] <- "gray"
  }

  # Make the first column "group"
  colnames(df2) <- c("group", "ID", "variable", "value")
  tryCatch(
    {
      # Initialize ggplot
      if (pos == "stack") {
        g <- ggplot(df2, aes(x = reorder(ID, -value), y = value, fill = variable))
      } else {
        g <- ggplot(df2, aes(x = ID, y = value, fill = variable))
      }
      # Add geom layers
      g <- g +
        geom_bar(stat = "identity", position = pos, width = .8, na.rm = T) + # bars
        scale_fill_manual(name = legend_title, values = var_colors) +
        labs(
          title = paste(labels, collapse = "_"), # labels
          subtitle = out_dir,
          x = "ID",
          ylab = "value"
        ) +
        # Customize theme
        theme(
          panel.background = element_blank(), # remove background color and lines
          axis.line = element_line(colour = "black"), # increase the axis-line thickness and change the color to blac
          # Titles
          plot.subtitle = element_text(colour = "black", size = font_size),
          plot.caption = element_text(colour = "black", size = font_size),
          strip.text.x = element_text(size = font_size),
          # Axes labels
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = 45, size = font_size / 3, hjust = 1, margin = margin(t = 7, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
          axis.text.y = element_text(size = font_size, margin = margin(t = 0, r = 7, b = 0, l = 0)),
          # axes tick labels
          axis.title = element_text(colour = "black", size = font_size, face = "bold"), # axes title labels
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          # legend
          legend.text = element_text(colour = "black", size = font_size / 3, face = "bold"),
          legend.title = element_blank(), # element_text(colour = "black", size = 7, face = "bold"),
          # legend.position="bottom",
          legend.key.size = unit(1, "line")
        ) + #+  facet_wrap(~group, scales = "free_x")
        facet_grid(~group, scales = "free_x", space = "free")
      # facet_grid(~vars(group), scales = "free", space = "free")
      if (pos == "fill") {
        g <- g + scale_y_continuous(labels = percent_format()) # scales
      }

      # Save to file
      if (save.to.file) {
        # Graphing params
        file_h <- ifelse(nrow(df2) < 20, 5, 7.5) # file height
        file_w <- (length(unique(df2$ID)) + 7) / 4 + 2 # file width
        # Print to file
        filename <- sprintf("%s/%s_bar_profile_%s.pdf", out_dir, paste(labels, collapse = "_"), pos)
        ggsave(filename, g, width = file_w, height = file_h, units = "cm", limitsize = F)
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
