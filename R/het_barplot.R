#' Functions defined in this file:
#'   run_het_analysis
#'   plot_het_barplot

#' Runs heterogeneity analysis for custom comparison, look at expression levels within samples of same patient
#'
#' @param ds A dataset object (a list with vals, rowAnn, colAnn, comparison, name).
#' @param rowAnn1 Column in ds$vals and was created in ds$rowAnn as a CustomComparison
#' @param pID short for patient ID, column name in ds$rowAnn to count samples for
#' @param out_dir The output folder path
#' @param var_colors A named vector with colors as values and annotations/groups as names.
#'
#' @export
run_het_analysis <- function(ds, rowAnn1, pID = 1, out_dir = ".", var_colors = NULL) {
  # If it is not a numeric variable, return nothing
  if (!rowAnn1 %in% colnames(ds$vals)) {
    return()
  }

  # Prepare dataframe with relevant info
  df <- data.frame(
    group = ds$rowAnn[, rowAnn1], # lo,med,high
    ID = rownames(ds$vals), # core ID
    variable = ds$rowAnn[, pID], # patient ID
    value = ds$vals[, rowAnn1]
  ) # IL6 expression

  # Count the number of samples belonging to each patient in each group e.g. 2 samples from patient 12341 is in "low"
  df2 <- aggregate(value ~ group + variable, data = df, FUN = length)
  #   group variable value
  # 1 intermed  1_21043     1
  # 2      low  1_21043     1
  # 3     high  1_33581     1

  # Initiate file
  pdf(file = sprintf("%s/%s_samples.pdf", out_dir, rowAnn1))

  # Plot all patients
  plot_het_barplot(df2, labels = c(rowAnn1, "all samples"), var_colors = var_colors, save.to.file = F)

  # Remove patients with just 1 sample
  x <- plyr::count(df2$variable) %>% data.frame()
  df2 <- df2[df2$variable %in% x$x[x$freq != 1], ]

  # Plot patients with >1 sample
  plot_het_barplot(df2, labels = c(rowAnn1, pID, "with 1 sample removed"), var_colors = var_colors, save.to.file = F)

  # Save file
  dev.off()
}

#' Plot variation bargraph
#'
#' @family plotting
#' @param df2 A long format data frame with 4 columns: 1) group (group of bars on x), 2) ID (individual bars on x), 3) variable (samples within each bar), 4) value.
#' @section Example of input data frame:
#' group         ID        variable     value
#' 1       low 1_21043      CD3  273.2400
#' 2       low 1_36312      CD3  700.2100
#' 3       low 1_37265      CD3  931.1133
#' 4       low 1_39924      CD3 1503.2325
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param pos How bars should be stacked. Either "fill" (relative ratio, 100% bar) or "stack". See position parameter in \code{\link[ggplot2]{geom_bar}}
#' @param var_colors A named vector with colors as values and annotations/groups as names.
#' @param font_size The size of axis title on plots. The size of plot subtitle and caption is font_size / 2. The size of legend text and x axis text is font_size / 3 and font_size / 1.5.
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#'
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_het_barplot <- function(df2, labels = "", pos = "stack", var_colors = NULL, font_size = 20, out_dir = ".", save.to.file = T) {

  # Initialize ggplot
  if (pos == "stack") {
    g <- ggplot(df2, aes(x = reorder(variable, -value), y = value, fill = group))
  } else {
    g <- ggplot(df2, aes(x = variable, y = value, fill = group))
  }
  # Add geom layers
  g <- g +
    geom_bar(stat = "identity", position = pos, width = .8, na.rm = T) + # bars
    labs(
      title = paste(labels, collapse = "_"),
      # subtitle = "Levels across patients",
      x = "Patient ID",
      y = "Number of samples"
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks()) + # integers on y axis
    # Customize theme
    theme(
      panel.background = element_blank(), # remove background color and lines
      axis.line = element_line(colour = "black"), # increase the axis-line thickness and change the color to blac
      # Titles
      # plot.subtitle = element_text(colour = "black", size = font_size),
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
    )

  if (!is.null(var_colors)) {
    # Colours for bars
    var_colors <- var_colors[unique(df2$group)] %>% unlist()
    # Add to graph
    g <- g + scale_fill_manual(values = var_colors)
  }

  # Convert y-axis to 0-100%
  if (pos == "fill") {
    g <- g + scale_y_continuous(labels = percent_format()) # scales
  }

  # Save to file
  if (save.to.file) {
    # # Graphing params
    # file_h <- ifelse(nrow(df2) < 20, 5, 7.5) # file height
    # file_w <- (length(unique(df2$ID)) + 7) / 4 + 2 # file width
    # Print to file
    filename <- sprintf("%s/%s_bar_profile_%s.pdf", out_dir, paste(labels, collapse = "_"), pos)
    ggsave(filename, g) # , width = file_w, height = file_h, units = "cm", limitsize = F)
  } else {
    # Print to image panel
    print(g)
  }
}
