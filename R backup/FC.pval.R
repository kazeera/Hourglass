#' Find p-values and fold-change (FC) for each group
#'
#' Find p-values and fold-change (FC) for each group specified in rowAnn_col across many columns of a data frame
#'
#' @param df A data frame - first column: rowAnnotation column with groups, the rest of the columns are values
#' @param rowAnn_col A column index (numeric) or name in df indicating which groups to stratify by
#' @param rev Logical indicating whether to reverse the order of the default FC (default is TRUE), ex. low-hig --> hig-low
#' @param group_name_sep A character indicating how the labels for groups should be presented, "/" in low/high
#' @param vs.other logical, indicating whether to compare all groups to other, eg. high vs (low+intermed)
#' @param FC.method Fold change computation method to use, either "divide" (for non-transformed values) or "subtract" (for log2-transformed values)
#' @param p.method Significance test to use, either "t.test" or "wilcox.test"
#' @return A data frame with p-vals and fold-changes for each group (rowAnn) in each variable (column)
#' @export
make_FC.pval_df <- function(df, rowAnn_col = 1, rev = T, vs.other = T, group_name_sep = "/", FC.method = "divide", p.method = "wilcox.test") {
  tryCatch(expr = {
    if (is.numeric(rowAnn_col)) {
      rowAnn_col <- colnames(df)[rowAnn_col]
    }

    # Get all variables stored in column names
    all_vars <- colnames(df)[!colnames(df) %in% rowAnn_col]

    l2 <- lapply(all_vars, function(v) {
      # Make a new data frame with row annotations and variable
      df3 <- df[, c(rowAnn_col, v)]

      # Remove variables that don't have enough values for a certain group by returning NULL
      df3 <- df3[!is.na(df3[, v]), ]
      counts <- table(df3[, rowAnn_col]) %>% data.frame()

      if (any(counts$Freq < 4)) {
        return(NULL)
      }

      # Get p-values and fold-change for each group
      df4 <- make_FC.pval_df_helper(df3, rowAnn_col = rowAnn_col, val_col = v, rev = rev, group_name_sep = group_name_sep, FC.method = FC.method, p.method = p.method)

      # Make "other" comparisons
      if (vs.other) {
        # Groups within annotation column e.g. low, high
        groups <- df[, rowAnn_col] %>%
          unique() %>%
          as.character()
        # Annotations in a vector
        x <- df3[, rowAnn_col] %>% as.character()

        # For each group, find p-values/FC, i.e. high vs other, low vs other
        for (group in groups) {
          y <- x
          y[x != group] <- "other"
          df3[, rowAnn_col] <- y
          df4 <- rbind(df4, make_FC.pval_df_helper(df3, rowAnn_col = rowAnn_col, val_col = v, rev = rev, group_name_sep = group_name_sep))
        }
      }
      return(df4)
    })
    names(l2) <- all_vars

    # Remove NULL list elements
    l2[sapply(l2, is.null)] <- NULL

    # List to data frame
    d7 <- do.call(rbind.data.frame, l2)
    d7$Var <- get_nth_part(rownames(d7), "\\.", 1)

    return(d7)
  }) # end try catch
}

#' Find p-values and fold-change (FC) for each group
#'
#' @param df3 A data frame with 2 columns: 1) row annotations (rowAnn), 2) numeric values
#' @param rowAnn_col Column in df3 with the groups
#' @param val_col Column in df3 with the numeric values
#' @param rev Logical indicating whether to reverse the order of the default FC (default is TRUE), ex. low-hig --> hig-low
#' @param group_name_sep A character indicating how the labels for groups should be presented, "/" in low/high
#' @param FC.method Fold change computation method to use, either "divide" (for non-transformed values) or "subtract" (for log2-transformed values)
#' @param p.method Significance test to use, either "t.test" or "wilcox.test"
#' @return A data frame with p-values and fold-changes
#' @section Output data frame:
#'          group    p.value Fold.change
#' 1 intermed/high 0.03457640   0.2216307
#' 2      low/high 0.02586801   0.5496807
#' 3  low/intermed 0.65911963   2.4801647
#' @export
make_FC.pval_df_helper <- function(df3, rowAnn_col = 1, val_col = 2, rev = F, group_name_sep = "/", FC.method = "divide", p.method = "wilcox.test") {
  # Get p values
  if (p.method == "wilcox.test") {
    df4 <- perform_wilcox(df3[, val_col], df3[, rowAnn_col]) %>%
      melt() %>%
      filter(!is.na(value))
  } else {
    df4 <- perform_t.test(df3[, val_col], df3[, rowAnn_col]) %>%
      melt() %>%
      filter(!is.na(value))
  }

  # If there are no values return
  if (nrow(df4) == 0) {
    return()
  }
  # Reverse order of factors
  if (rev) {
    df4[, c("Var1", "Var2")] <- df4[, c("Var2", "Var1")]
  }

  # Get median of each group
  df5 <- medians_of_groups(df3[, val_col], df3[, rowAnn_col])

  # Rename rows
  rownames(df5) <- df5[, 1]
  # > df5
  #              Var        value
  # high         high 1.942304e-05
  # intermed intermed 1.264453e-05
  # low           low 1.343557e-05

  # Calculate quotient (FC) of medians depending on permutations of groups specified in df4
  l <- lapply(1:nrow(df4), function(i) {
    if (FC.method == "divide") {
      df5[as.character(df4$Var1[i]), 2] / df5[as.character(df4$Var2[i]), 2]
    } else {
      df5[as.character(df4$Var1[i]), 2] - df5[as.character(df4$Var2[i]), 2]
    }
  })

  # Return data frame of combining FC and pvalue info for this iteration
  data.frame(
    group = factor(paste(df4$Var1, df4$Var2, sep = group_name_sep)), # paste(substring(df4$Var1,1,3), substring(df4$Var2,1,3), sep="-"),
    p.value = df4$value,
    Fold.change = do.call(rbind.data.frame, l) %>% unlist() %>% unname()
  )
}


#' Plot FC-p value heatmap
#'
#' Plot grid of FC values as fill (with color scale) and p-value as stars/numbers in the centre of each tile.
#'
#' @param df A data frame with these exact columns 1) group (ie. group comparisons on x-axis), 2) p.value, 3) Fold.change, 4) Var (y-axis), created using \code{\link{make_FC.pval_df}}.
#' @section Example of input data frame:
#' group   p.value       Fold.change      Var
#' neo/non 0.6747883 1.5617445    CD11B
#' neo/non 0.4820339 1.7443427      CD3
#' neo/non 0.9082967 1.0589647      CD4
#' neo/non 0.9691466 0.9896168     CD45
#' @param xlab X axis label.
#' @param ylab Y axis label.
#' @param plot_title Title of plot.
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param pval.label How p-values are represented. Allowed values are "p.signif" (stars) and "p.format" (number).
#' @param gradient_palette RColorBrewer palette for fold-change values. See RColorBrewer::display.brewer.all() for all options.
#' @param group_name_sep A character indicating how the labels for groups should be presented, "/" in low/high
#' @param trim_x Number, indicating the number of characters for each part (that is, length of truncated output string).
#' @param pval_size Size of p-values.
#' @param pval_color Color of p-values.
#' @param log2FC Logical (TRUE/FALSE). Should log2 transformation be applied to Fold.change column before plotting?
#' @param scale_FC Either "scale_column", "scale_row", "none", or "cap_outliers" (default). Should scale be applied to FC for each group (column), Var (row)? In "cap_outliers", non-outliers are unscaled, but upper and lower outliers (points outside 1st/3rd quartiles respectively) become upper and lower values of range of non-outliers.
#' @param rescale_to A numeric vector of length 2, indicating lower and upper limits of scale. Default is 0 to 1: c(0,1). Only applied if "scale_FC" parameter is not "none".
#' @param x_axis_angle Angle of the x-axis label. Default is 0 (horizontal), 90 is vertical.
#' @param save.to.file If TRUE, save plot to file in out_dir. If FALSE, print to panel.
#' @param font_size The size of text labels plot. legend title. The size of plot title, axis text, legend text is font_size. The size of plot subtitle is font_size / 1.5.
#' @param line_size The thickness of grid lines.
#' @param alphabetical_row Logical; should the y axis be sorted alphabetically or preserve the order of df$Var?
#' @return Plot object if save.to.file is FALSE.
#' @export
make_FC.pval_plot <- function(df, x_lab = "", y_lab = "", plot_title = "", out_dir = ".", pval.label = "p.signif", gradient_palette = "RdBu",
                              group_name_sep = "/", trim_x = 3, pval_size = 8, pval_color = "white", log2FC = F, scale_FC = "cap_outliers", rescale_to = c(0, 1),
                              x_axis_angle = 0, save.to.file = F, font_size = 10, line_size = 1, alphabetical_row = F) {
  # Error checking
  if (!scale_FC %in% c("scale_column", "scale_row", "none", "cap_outliers")) {
    errorCondition(message = "Ensure scale_FC parameter in make_FC.pval_plot has value of: 'scale_column', 'scale_row', 'none', or 'cap_outliers'")
    return()
  }

  # Apply log transformation
  if (log2FC) {
    df$Fold.change <- log2(df$Fold.change)
  }

  # Apply scale to each group/Variable
  if (scale_FC == "scale_column" | scale_FC == "scale_row") {
    scale_by <- ifelse(scale_FC == "scale_row", "Var", "group")
    # Apply scale
    df$Fold.change <- ave(as.numeric(df$Fold.change), df[, scale_by], FUN = function(x) {
      scales::rescale(x, to = rescale_to)
    })
  }

  # Apply limit to outliers, upper outliers become upper limit of scale and lower becomes lower limit
  if (scale_FC == "cap_outliers") {
    # Find outliers (logical)
    outliers <- get_outliers(df$Fold.change)
    # Rescale non-outliers
    unchanged <- !outliers %in% c("upper", "lower")
    # df$Fold.change[unchanged] <- scales::rescale(df$Fold.change[unchanged], to = rescale_to)
    # # Make outliers limit
    df$Fold.change[outliers == "lower"] <- df$Fold.change[unchanged] %>% min(na.rm = T) # rescale_to[1]
    df$Fold.change[outliers == "upper"] <- df$Fold.change[unchanged] %>% max(na.rm = T) # rescale_to[2]
  }

  # Add stars
  if (pval.label == "p.signif") {
    df$p_stars <- pval_to_stars(df$p.value)
  } else {
    df$p.value <- round(df$p.value, 3)
  }

  # Trim x axis # alt to scale_x_discrete(labels = function(x) strtrim(x, 3))
  if (!isFALSE(trim_x)) {
    n_orig <- df$group %>%
      unique() %>%
      length()
    x <- trim_each_part(df$group, trim_x = trim_x, split = group_name_sep)
    n_new <- x %>%
      unique() %>%
      length()
    if (n_orig != n_new) {
      x_axis_angle <- 45
    } else {
      df$group <- x
    }
  }

  # Manually order high/low/oth comparisons
  e <- unique(df$group)
  e <- e[!is.na(e)]
  lvl_order <- c("hig/low", "hig/int", "int/low", "hig/oth", "int/oth", "low/oth")

  if (all(e %in% lvl_order)) {
    # If all the elements are in the vector above, order x axis according to level order
    df$group <- factor(df$group, levels = lvl_order[lvl_order %in% e])
  } else {
    # Move "/other" to right of heatmap
    other <- ifelse(isFALSE(trim_x), "other", strtrim("other", trim_x)) %>% grepl(e)
    df$group <- factor(df$group, levels = c(as.character(e[!other]), as.character(e[other])))
  }

  # Color gradient for heatmap
  pal_grad <- get_col_palette(gradient_palette, rev = T) %>% get_col_gradient(100)

  # Should rows not be sorted?
  if (isFALSE(alphabetical_row)) {
    df$Var <- factor(df$Var, levels = unique(df$Var))
  }
  # Plot
  p <- ggplot(df, aes(x = group, y = Var, fill = Fold.change)) +
    geom_tile() +
    ggtitle(paste(plot_title)) +
    labs(
      title = plot_title,
      caption = ifelse(!isFALSE(pval.label), "p <= 0.001 '****', 0.001 '***', 0.01 '**', 0.05 '*'", ""),
      subtitle = out_dir,
      x = x_lab,
      y = y_lab
    ) +
    scale_fill_gradientn(colors = pal_grad, name = paste0(ifelse(log2FC, "log2 ", ""), "FC ", ifelse(scale_FC == "none", "", scale_FC))) +
    scale_x_discrete(expand = c(0, 0)) + # remove space between grid and axes
    scale_y_discrete(expand = c(0, 0)) +
    theme(
      panel.background = element_blank(), # remove background color and lines
      plot.title = element_text(colour = "black", size = font_size),
      plot.subtitle = element_text(colour = "black", size = font_size / 1.5),
      axis.line = element_line(colour = "black", size = line_size), # increase the axis-line thickness and change the color to blac
      # Ticks
      axis.ticks = element_line(colour = "black", size = line_size), # increase the tick thickness)
      axis.ticks.length = unit(.25, "cm"),
      # Axes labels
      axis.text = element_text(colour = "black", size = font_size), # face = "bold"),
      axis.text.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0), angle = x_axis_angle, vjust = 0.5), # increase space between x axis title and labels
      axis.text.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 0)),
      # axes tick labels
      axis.title = element_text(colour = "black", size = font_size, face = "bold"), # axes title labels
      # legend
      legend.text = element_text(colour = "black", size = font_size, face = "bold"),
      legend.title = element_text(colour = "black", size = font_size, face = "bold"),
      legend.position = "bottom", legend.box = "vertical"
    )

  # Add stars if applicable
  if (!isFALSE(pval.label)) {
    if (pval.label == "p.signif") {
      p <- p +
        geom_text(aes(label = p_stars), size = pval_size, color = pval_color, vjust = 0.8)
    }
    if (pval.label == "p.format") {
      p <- p +
        geom_text(aes(label = p.value), size = pval_size, color = pval_color, vjust = 0.5)
    }
  }

  # Save to file
  if (save.to.file) {
    # Graphing params
    file_h <- (length(unique(df$Var)) + 4) / 4 + 2 # file height
    ggsave(device = "pdf", height = file_h, limitsize = F, filename = sprintf("%s/%s_pval_FC.pdf", out_dir, plot_title), plot = p) # , height = nrow(df)*0.6, width = 4)
  } else {
    print(p)
  }
}
