#' Functions defined in this file:
#'   run_paired_analysis
#'   get_paired_df
#'   plot_indiv_paired

#' Makes multiple correlation plots in 1 PDF file
#'
#' @inheritParams run_comparison
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param pair_id Name or index of column in ds$rowAnn that count as patients or groups to pair by, i.e. which ID to average over, e.g. patient ID (if there are multiple rows per patient)
#' @export
run_paired_analysis <- function(ds, rowAnns, colAnns = NA, out_dir = ".", var_colors = NULL, pair_id = 1, pval.test = "t.test", pval.label = "p.signif") {
  # Get data frame with individual pairs
  ds_sub <- get_paired_df(ds, rowAnns[1], pair_id)
  # Get color palette for row annotations
  pal <- get_rowAnn_color_pal(ds_sub, rowAnns[1], var_colors) %>%
    .[["pal"]]

  # Get a vector of all the unique variables
  all_vars1 <- ds$colAnn[, colAnns[2]] %>% unique()

  for (var1 in all_vars1) {
    # Get the columns to plot and pdf filename
    pdf_filename <- sprintf("%s/%s_paired.pdf", out_dir, var1)
    col_names <- rownames(ds$colAnn)[ds$colAnn[, colAnns[2]] == var1]

    # Create pdf file of all plots
    pdf(pdf_filename, onefile = TRUE)

    for (col_name in col_names) {
      tryCatch(
        {
          # Subset data frame to only column of interest
          df <- data.frame(
            case = ds_sub$rowAnn[, pair_id],
            box = ds_sub$rowAnn[, rowAnns[1]],
            value = ds_sub$vals[, col_name],
            stringsAsFactors = F
          )
          df <- get_duplicated_cases(df, col = "case", rm.NA = "value")
          # In case there are a couple of cases or zero left, do not continue
          if (nrow(df) < 2) next
          # Create plot
          plot_indiv_paired(df,
                            labels = var1, color_pal = pal, pval.label = pval.label, pval.test = pval.test,
                            xlab = rowAnns[1], ylab = col_name, rowAnns = rowAnns, save.to.file = F
          )
        },
        finally = {
          next
        }
      )
    }
    dev.off()
  }
}

#' Finds means of pairs of duplicated ids in different groups
#'
#' Produces a dataset in which duplicated pair_id are averaged across different groups specified in
#'
#' @param ds A dataset object (a list with vals, rowAnn, colAnn, comparison, name).
#' @param pair_id Name or column index in ds$rowAnn that count as patients or groups to pair by.
#' @param rowAnn1 A column name in df indicating which groups to stratify by
#' @return A new dataset object averaged for each rowAnn pair_id
#' @export
get_paired_df <- function(ds, rowAnn1, pair_id = 1) {
  # Get name of column to pair by
  pair_id <- ifelse(is.numeric(pair_id), colnames(ds$rowAnn)[pair_id], pair_id)

  # Merge df with row annotations
  rowAnns <- c(pair_id, rowAnn1)
  df <- cbind(ds$rowAnn[, rowAnns], ds$vals)
  # Remove rows with missing annotations
  df <- subset(df, !is.na(df[, rowAnn1]))

  # Get means of rows base on pair_id duplicates, and different groups in scaffold column, remove NA values
  avg_df <- df[, !colnames(df) %in% rowAnns] %>%
    aggregate(
      by = list(df[, pair_id], df[, rowAnn1]),
      mean, na.rm = TRUE
    )
  avg_df2 <- get_duplicated_cases(avg_df, col = "Group.1")

  # Rename columns
  colnames(avg_df)[1:2] <- rowAnns

  # Print
  print(sprintf("Subset data only to 2+ cores that belong to same patient.
                Average across same %s for each patient.
                Cores dropped from %s to %s for %s unique %ss.", rowAnn1, nrow(df), nrow(avg_df), length(unique(avg_df[, 1])), pair_id))

  # Return subsetted and averaged dataset object
  list(
    vals = avg_df[, -c(1:2)],
    colAnn = ds$colAnn,
    rowAnn = avg_df[, rowAnns]
  )
}


#' Create paired plot
#'
#' @family plotting
#' @param df A data frame with 3 columns: 1) id = cases or groups to pair by, 2) box = dif levels in rowAnn, 3) value
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param log10 Logical indicating whether to log10-transform values before plotting
#' @param font_size The size of text labels on correlation plots. legend title. The size of legend text and plot title is font_size / 1.5. The size of legend text and plot subtitle is font_size / 3.
#' @param line_size The thickness of grid lines.
#' @param color_pal A named vector with colors, where names are box names (values in column 2 of df).
#' @param ylab Y axis label.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#' @param rowAnns A character vector of 1-2 column names in ds$rowAnn.
#' @param pval.test 2-sample test to use (paired only for 2 groups). String corresponding to method parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "t.test" and "wilcox.test".
#' @param pval.label p-value label. String corresponding to label parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "p.signif" (stars) and "p.format" (number).
#' @return Plot object if save.to.file is FALSE.
#' @export
plot_indiv_paired <- function(df, labels = "Group", out_dir = ".", log10 = F, font_size = 30, line_size = 1.3, color_pal = NA,
                              xlab = "variable", ylab = "value", rowAnns = c(NA, NA), pval.test = "t.test",
                              pval.label = "p.format", save.to.file = T) {
  # Rename columns
  colnames(df)[1:3] <- c("case", "box", "value")
  # Log scale
  if (log10) {
    df$value <- log10(df$value) # a <- a + scale_y_continuous(trans='log10') # log transform
  }

  # Make plot
  a <- ggplot(df, aes(x = box, y = value)) +
    geom_point(aes(color = box), size = 4, alpha = 0.5) +
    scale_color_manual(values = color_pal) +
    # geom_boxplot(aes(fill=box), width=0.8, lwd = 1, color="black", na.rm=T, outlier.color = NA) +
    geom_line(aes(group = case), size = 1, linetype = "solid", color = "gray38")

  # Make list of unique elements
  e <- unique(as.character(df$box))
  # Make list of combinations (order doesn't matter) for p-values
  comb <- combinations(n = length(e), r = 2, v = e, repeats.allowed = F) %>% # gtools package
    split(., seq(nrow(.)))

  # Paired 2 sample test only supported for 2 groups
  paired <- ifelse(length(e) == 2, T, F)

  # Add stats to plot using ggpubr
  tryCatch({
    a <- a + stat_compare_means(method = pval.test, comparisons = comb, na.rm = T, paired = paired, label = pval.label, size = font_size / 5, bracket.size = 1)
  })

  # Trim x labels to 3 characters
  a <- a + scale_x_discrete(labels = function(x) strtrim(x, 3))

  # Account for PDAC-specific analysis
  if ("PANC_TISS_ORDER" %in% ls(envir = .GlobalEnv)) {
    if (isTRUE(any(PANC_TISS_ORDER %in% ele) & length(ele) > 2)) { # if elements are just "adj_normal" and "PDAC" it'll mess up the order
      # Set subtype orders - PDAC
      panc_order <- PANC_TISS_ORDER[PANC_TISS_ORDER %in% e] # PANC_TISS_ORDER <- c("adj_normal", "mature", "intermediate","immature") # in "1.import_data.R"
      a <- a + scale_x_discrete(limits = panc_order, labels = function(x) strtrim(x, 3))
    }
  }

  # Add a line for mean
  a <- a + stat_summary(
    fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
    position = position_dodge(width = 0.3), width = 0.25, color = "black", size = line_size
  )

  # Add theme
  a <- a +
    # theme_classic()
    theme(
      panel.background = element_blank(), # remove background color and lines
      plot.title = element_text(colour = "black", size = font_size / 2),
      plot.subtitle = element_text(colour = "black", size = font_size / 3),
      axis.line = element_line(colour = "black", size = line_size), # increase the axis-line thickness and change the color to black
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
      axis.title = element_text(colour = "black", size = font_size / 2, face = "bold"), # axes title labels
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      # legend
      legend.text = element_text(colour = "black", size = font_size / 3),
      legend.title = element_text(colour = "black", size = font_size / 3)
    )
  # Add labels to graph
  a <- a +
    labs(
      title = paste(labels, collapse = "_"),
      color = ifelse(!is.na(rowAnns[1]), rowAnns[1], ""),
      caption = sprintf("%s%s", pval.test, ifelse(pval.label == "p.signif", ", p: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "")),
      subtitle = out_dir,
      y = sprintf("%s%s", ifelse(log10, "log10 ", ""), ylab),
      x = xlab
    )

  if (save.to.file) {
    # Print to file
    ggsave(sprintf("%s/%s_paired.png", out_dir, paste(labels, collapse = "_")), plot = a) # , width = length(elements)*2.5,height = 7.5)
  } else {
    # Print to pdf
    print(a)
  }
}
