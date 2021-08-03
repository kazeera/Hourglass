#' Create plots in a folder in streamlined approach
#'
#' @inheritParams run_comparison ds,rowAnns,colAnns,out_dir,customAn,global_palette,gradient_palette,corr_method,pval.test,pval.label,make.indiv.boxplot,make.overview.boxplot,make.heatmap,make.corrplot,make.overview.corrscatt,make.indiv.corrscatt,make.barplot,make.FC.pval.plot
#' @param labels A character vector of at least length 1 that will be collapsed for file name/plot titles.
#' @param also.complete Logical indicating whether to also make "complete cores" plots as a seperate folder, default FALSE.
#' @export
#' save.image("create.RData")
create_plots <- function(ds, rowAnns = 1, colAnns = NA, out_dir = ".", labels = "Sub1", global_palette = NULL, gradient_palette = NULL,
                         corr_method = "pearson", pval.test = "wilcox.test", pval.label = "p.signif",
                         make.indiv.boxplot = F, make.overview.boxplot = F, make.heatmap = F, make.corrplot = F, 
                         make.overview.corrscatt = F, make.indiv.corrscatt = F, make.barplot = F, make.FC.pval.plot = F, also.complete = F) {
  # # Create folder
  # out_dir <- create_folder(sprintf("%s/%s", out_dir, paste(labels, collapse = "_")))
  
  # Get rid of any NAs in rowAnn1
  ds <- subset_dataset(ds, rows_to_keep = !is.na(ds$rowAnn[, rowAnns[1]]))
  
  # Get color palette for row annotations
  p <- get_rowAnn_color_pal(ds, rowAnns, global_palette)
  ds <- p$ds
  # pal <- p$pal #TODO
  rm(p)
  
  # Depending on whether we're looking at a dataset with NAs or not
  if (!any(is.na(ds$vals))) { 
    tryCatch(
      {
        # Run all cores with NAs, unclustered # make all plots
        create_plots_helper(ds, rowAnns, colAnns, out_dir, labels, global_palette,
                            clust_row = T, gradient_palette = gradient_palette, 
                            corr_method = corr_method, pval.test = pval.test, pval.label = pval.label,
                            make.indiv.boxplot = make.indiv.boxplot, make.overview.boxplot = make.overview.boxplot, 
                            make.heatmap = make.heatmap, make.corrplot = make.corrplot, 
                            make.overview.corrscatt = make.overview.corrscatt, make.indiv.corrscatt = make.indiv.corrscatt, 
                            make.barplot = make.barplot, make.FC.pval.plot = make.FC.pval.plot
        )
      },
      error = function(err) {
        print(sprintf("%s", err))
        return()
      }
    )
  } else {
    # Else if dataset has NAs in it
    tryCatch(
      {
        # 1) Run all cores with NAs, unclustered  # don't make plots that require computing correlations
        create_plots_helper(ds, rowAnns, colAnns, out_dir, labels, global_palette, 
                            gradient_palette = gradient_palette, 
                            corr_method = corr_method, pval.test = pval.test, pval.label = pval.label,
                            make.indiv.boxplot = make.indiv.boxplot, make.overview.boxplot = make.overview.boxplot, 
                            make.heatmap = make.heatmap, make.corrplot = F, 
                            make.overview.corrscatt = F, make.indiv.corrscatt = F, 
                            make.barplot = make.barplot, make.FC.pval.plot = make.FC.pval.plot
        )
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
    if(also.complete){
      tryCatch(
        {
          # 2) Subset to only complete rows (no NAs)
          # ALT: rows_to_keep <- has_less.than.eq.to_NA.thres(ds$vals, col.or.row = "row", NA_thres = 0)
          ds_comp <- subset_dataset(ds, rows_to_keep = complete.cases(ds$vals))
          
          if (nrow(ds_comp$vals) > 2) {
            # Make a new out directory with 'complete" at end
            sub_out_dir <- create_folder(sprintf("%s complete", out_dir))
            
            # Run complete cores, clustered # make all plots
            create_plots_helper(ds_comp, rowAnns, colAnns, sub_out_dir,
                                labels = c(labels, "complete"), global_palette, clust_row = T, gradient_palette = gradient_palette,
                                corr_method = corr_method, pval.test = pval.test, pval.label = pval.label,
                                make.indiv.boxplot = make.indiv.boxplot, make.overview.boxplot = make.overview.boxplot, 
                                make.heatmap = make.heatmap, make.corrplot = make.corrplot, 
                                make.overview.corrscatt = make.overview.corrscatt, make.indiv.corrscatt = make.indiv.corrscatt, 
                                make.barplot = make.barplot, make.FC.pval.plot = make.FC.pval.plot)
          }
        },
        error = function(err) {
          print(sprintf("%s", err))
        }
      )
    }
  }
  turn_off_null_devices()
}

#' Create plots helper
#'
#' @inheritParams run_comparison ds,rowAnns,colAnns,out_dir,customAn,global_palette,gradient_palette,corr_method,pval.test,pval.label,make.indiv.boxplot,make.overview.boxplot,make.heatmap,make.corrplot,make.overview.corrscatt,make.indiv.corrscatt,make.barplot,make.FC.pval.plot
#' @param global_palette A named character vector of colors (R or hex), where the names are the groups in row annotations and column annotations
#' @param clust_row,clust_col Logicals indicating whether to cluster rows and columns in heatmap.
#' @param gradient_palette RColorBrewer palette name for gradients (e.g. heatmap, correlation plots). See RColorBrewer::display.brewer.all() for all options. 
#' @export
create_plots_helper <- function(ds, rowAnns = 1, colAnns = NA, out_dir = ".", labels = "", global_palette = NA, clust_row = F, clust_col = F, gradient_palette = "RdBu",
                                corr_method = "pearson", pval.test = "wilcox.test", pval.label = "p.signif",
                                make.indiv.boxplot = F, make.overview.boxplot = F, make.heatmap = F, make.corrplot = F, 
                                make.overview.corrscatt = F, make.indiv.corrscatt = F, make.barplot = F, make.FC.pval.plot = F) {
  # If there are no stains or rows to plot, return incomplete
  if (any(dim(ds$vals) < 3)) return()
  
  # - 1st df: col1 = rowAnn1, cols2:n = values
  # Make the first column the main row annotation/stratification variable
  df <- data.frame(ds$rowAnn[, rowAnns[1]], ds$vals)
  colnames(df)[1] <- rowAnns[1]
  
  ## Make fold-change (FC) p-value heatmap
  if (make.FC.pval.plot) {
    tryCatch(
      {
        # Get data frame with logFC and pvalues
        df_FCp <- make_FC.pval_df(df, rowAnn_col = 1, p.method = pval.test)
        # Make plot
        make_FC.pval_plot(df_FCp, x_lab = rowAnns[1], y_lab = colAnns[2], plot_title = labels, out_dir = out_dir, gradient_palette = gradient_palette, pval.label = pval.label, save.to.file = T)
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
  }
  
  ## Make correlation plots
  if (make.corrplot) {
    tryCatch(
      {
        run_corrplot_analysis(df, out_dir = out_dir, labels = labels, gradient_palette = gradient_palette, pval.label = pval.label, corr_method = c("pairwise.complete.obs", corr_method))
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
  }
  
  ## Make correlation scatter plots
  # Overview # When number of variables are over 100, it crashes
  if (make.overview.corrscatt & ncol(ds$vals) <= 50) {
    tryCatch(
      {
        grid_l <- (ncol(df) - 1) # file width
        # Initialize file
        filename <- sprintf("%s/%s_corrscatter.pdf", out_dir, paste(labels, collapse = "_"))
        # Create pdf file of all plots
        pdf(filename, onefile = TRUE, height = grid_l, width = grid_l)
        # All rows
        plot_overview_corr_scatt(df[, -1], out_dir, c(labels, "All"), corr_method = corr_method, save.to.file = F)
        # Plot for each group
        for (group in unique(df[, 1])) {
          if (is.na(group)) next
          df3 <- df[df[, 1] == group, -1]
          plot_overview_corr_scatt(df3, out_dir, c(labels, group), corr_method = corr_method, save.to.file = F)
        }
      },
      error = function(err) {
        print(sprintf("%s", err))
      },
      finally = {
        turn_off_null_devices()
      }
    )
  }
  
  # Individual pairwise comparisons (in a new folder)
  if (make.indiv.corrscatt) {
    tryCatch(
      {
        corr_out_dir <- create_folder(paste(out_dir, "Correlation Scatter", sep = "/"))
        plot_indiv_corrscatt(df[, -1], out_dir, labels, cor.method = corr_method)
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
  }
  
  ## Make stacked bar plots
  if (make.barplot) {
    tryCatch(
      {
        run_profile_barplot(df, rowAnn_col = 1, out_dir, labels, gradient_palette = gradient_palette)
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
  }
  
  # - 2nd df: Add an extra column
  if (!is.na(rowAnns[2])) {
    df <- cbind(ds$rowAnn[, rowAnns[2]], df)
    # Replace NA in rowAnn2 with _NA
    if (any(is.na(df[, 1]))) {
      df[is.na(df[, 1]) & !is.na(df[2, ]), ] <- "NA_"
    }
    colnames(df)[1] <- rowAnns[2]
  }
  # Wide to long data format
  df2 <- melt(df) # reshape2
  
  # TODO make color palette for rowAnn
  pal <- get_ann_colors(ds$rowAnn, rowAnns[!is.na(rowAnns)], global_palette) %>% unname %>% unlist
  
  ## Make overview boxplots
  if (make.overview.boxplot) {
    tryCatch(
      {
        plot_overview_boxplot(
          df3 = df2[, c("variable", rowAnns[1], "value")],
          out_dir, labels, lvl.colors = pal,
          legend.title = rowAnns[1], xlab = colAnns[2],
          log10 = T
        )
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
  }
  
  ## Make individual boxplots
  if (make.indiv.boxplot) {
    # Get a vector of all the unique stains
    all_vars <- df2$variable %>%
      as.character() %>%
      unique()
    # Get the columns to plot and pdf filename
    pdf_filename <- sprintf("%s/%s_boxplots.pdf", out_dir, paste(labels, collapse = "_"))
    # Create pdf file of all plots
    pdf(pdf_filename, onefile = TRUE)
    for (v in all_vars) {
      # Prepare data frame
      df3 <- df2[df2$variable == v, c(rowAnns[1], "value")]
      # Add extra column for color code dots if applicable
      if (!is.na(rowAnns[2])) {
        df3 <- data.frame(df3, dots = df2[df2$variable == v, rowAnns[2]])
      }
      # y-axis label
      # ylab <- ifelse(length(labels) == 1, labels, "")
      ylab <- ifelse(all(is.na(colAnns)), "",
                     ds$colAnn[ds$colAnn[, colAnns[2]] == v, colAnns[1]][1]
      )
      # Plot
      tryCatch(
        {
          plot_indiv_boxplot(df3,
                             labels = c(labels, v), out_dir, lvl.colors = pal, font_size = 30,
                             xlab = "", ylab = ylab, rowAnns = rowAnns, save.to.file = F, 
                             pval.label = pval.label, pval.test = pval.test 
          )
        },
        error = function(err) {
          print(sprintf("%s", err))
        }
      )
    }
    dev.off()
  }
  
  ## Make heatmap
  if (make.heatmap) {
    # Get annotations colors
    ann_colors <- NA
    tryCatch(
      {
        ann_colors <- c(get_ann_colors(ds$rowAnn, rowAnns[!is.na(rowAnns)], global_palette))
        # ann_colors <- c(ann_colors, get_ann_colors(ds$colAnn, colAnns, global_palette))
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
    
    # Order rows
    row_order <- sort_dataframe(ds$rowAnn, "row", rowAnns[1]) %>%
      rownames()
    ds <- sort_dataset(ds, row_order = row_order)
    #  Order columns
    tryCatch(
      {
        col_order <- sort_dataframe(ds$colAnn, "column", colAnns[1]) %>%
          rownames()
        ds <- sort_dataset(ds, col_order = col_order)
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
    
    # Make annotation column
    ann_col <- NA
    # tryCatch(
    #   {
    #     ann_col <- reform_ann_df(ds$colAnn, colAnns)
    #     rownames(ann_col) <- colnames(ds$vals) <- make.unique(colnames(ds$vals))
    #   },
    #   error = function(err) {
    #     print(sprintf("%s", err))
    #   }
    # )
    
    # Heatmap 1 - sorted, unclustered
    tryCatch(
      {
        plot_heatmap(
          mat = ds$vals,
          ann_row = reform_ann_df(ds$rowAnn, rowAnns),
          ann_col = ann_col,
          ann_colors = ann_colors,
          plot_title = sprintf("%s rows", nrow(ds$vals)),
          out_dir = out_dir,
          labels = labels,
          clust_row = clust_row,
          clust_col = clust_row,
          gradient_palette = gradient_palette
        )
      },
      error = function(err) {
        print(sprintf("%s", err))
      }
    )
    
    # Make "unclustered" heatmap    # cluster_within rowAnn1
    # if (isTRUE(clust_row)) {
    #   # Cluster rows
    #   dend_row <- cluster_within_group(t(ds$vals), factor(ds$rowAnn[, rowAnns[1]]))
    #   hclust_row <- as.hclust(dend_row)
    # 
    #   # Cluster columns
    #   hclust_col <- clust_row
    #   tryCatch(
    #     {
    #       dend_col <- cluster_within_group(ds$vals, factor(ds$colAnn[, colAnns[1]]))
    #       hclust_col <- as.hclust(dend_col)
    #     },
    #     error = function(err) {
    #       print(sprintf("%s", err))
    #     }
    #   )
    # 
    #   tryCatch(
    #     { # Get the new df and ann_row
    #       plot_heatmap(
    #         mat = ds$vals,  
    #         ann_col = reform_ann_df(ds$colAnn, colAnns),
    #         ann_row = reform_ann_df(ds$rowAnn, rowAnns),
    #         ann_colors = ann_colors,
    #         plot_title = sprintf("%s rows", nrow(ds$vals)),
    #         out_dir = out_dir,
    #         labels = c(labels, "2"),
    #         clust_row = hclust_row,
    #         clust_col = hclust_col,
    #         gradient_palette = gradient_palette
    #       )
    #     },
    #     error = function(err) {
    #       print(sprintf("%s", err))
    #     }
    #   )
    # }
  }
}
