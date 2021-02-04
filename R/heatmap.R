#' Runs main analysis for all parameter combos specified in colAnns and creates plots.
#'
#' @inheritParams run_comparisons ds,rowAnns,colAnns,heatmap_folder,customAn
#' @export
run_heatmap_analysis <- function(ds, rowAnns = 1, colAnns = NA, heatmap_folder = ".", customAn = NULL) {
  # Analysis 1: Make heatmaps for all parameters and stains
  if (all(!is.na(colAnns))) {
    # Create a directory in each comparison folder
    out_dir <- create_folder(sprintf("%s/By %s", heatmap_folder, colAnns[1]))

    # Parameters of column annotations to run
    params <- ds$colAnn[, colAnns[1]] %>% unique()

    # Considering all stains, make heatmaps
    for (param1 in params) {
      # Get logical of whether or not to keep columns in data table
      cols_to_keep <- ds$colAnn[, colAnns[1]] %in% param1
      if (sum(cols_to_keep) < 3) next
      # Subset dataset object accordingly
      ds_sub <- subset_dataset(ds, cols_to_keep = cols_to_keep)
      # Create plots
      create_heatmap_plots(ds_sub, rowAnns, colAnns, out_dir, labels = param1)
      # Turn off null devices
      turn_off_null_devices()
    }
  }

  # Analysis 2: Make biologically relevant heatmaps by combining stains from handpicked parameters #defined in main.script
  if (!is.null(customAn)) {
    # # Create a directory in each comparison folder
    out_dir <- create_folder(sprintf("%s/Custom", heatmap_folder))

    # Number of columns to annotate from Custom Analysis, ie, Stain, ECMorImmune, etc
    n <- unique(customAn$keys[, 2]) %>% length() # num col annotations
    new_colAnns <- ncol(customAn$values) - 2 * n # num of new colAnns, this is correct
    new_colAnns <- colnames(customAn$values)[2:new_colAnns] # name of col anns

    # ca = custom analysis
    for (i in 1:nrow(customAn$keys)) {
      tryCatch(
        {
          # Name of analysis
          customAn_name <- customAn$keys[i, 1]
          # This line splits the group numbers into a vector: eg. "1,2,3,4" turns into "1" "2" "3" "4"
          grp.num <- strsplit(customAn$keys[i, "Group.Numbers"], split = ",") %>% unlist()
          # Get type: ex. standard or PPC?
          grp <- customAn$keys[i, "Group"]
          # Get the rows in stain info
          rows_customAn <- customAn$values[, paste(grp, "Group.Numbers", sep = "_")] %in% grp.num
          # Which columns are we stratifying by in colAnn
          cn <- colnames(customAn$values)
          colAnn2 <- cn[1] # Stain, this is right
          # Get the column of interest, ex. "Parameter" from "PPC_Parameter"
          colAnn1 <- (grepl(grp, cn) & !grepl("Group.Numbers", cn)) %>%
            cn[.] %>%
            get_nth_part("_", 2)
          colAnn1_custom <- paste(grp, colAnn1, sep = "_")
          customAn$values[, colAnn1] <- customAn$values[, colAnn1_custom]
          # Get all new column annotations
          colAnns <- c(colAnn1, colAnn2, new_colAnns) %>% unique()

          # Check whether any colann1/colann2 combo is duplicated, e.g. TIMP1-Pos.Pix.Perc shows up in more than one place
          # Prevents error: duplication leads to incorrect dimensions for colAnn
          dup <- duplicated(customAn$values[rows_customAn, c(colAnn2, colAnn1_custom)])
          rows_customAn <- rows_customAn & !dup

          # Subset to columns in column annotation of interest
          cols_to_keep <- interaction(ds$colAnn[, c(colAnn2, colAnn1)]) %in%
            interaction(customAn$values[rows_customAn, c(colAnn2, colAnn1_custom)])
          # # Do not continue with analysis if the parameter and stains don't match the columns in the input data
          if (sum(cols_to_keep) < 3) next # next in loop

          # Subset dataset object accordingly
          ds_sub <- subset_dataset(ds, cols_to_keep = cols_to_keep)

          # Prevents error: column order of ds$vals and ds$colAnn are not the same
          colnames(ds_sub$vals) <- paste(ds_sub$colAnn[, colAnn2], ds_sub$colAnn[, colAnn1], sep = "_")
          # Make unique column to merge by
          df <- reform_ann_df(customAn$values[rows_customAn, ], new_colAnns)
          df$MergeID <- paste(customAn$values[rows_customAn, colAnn2], customAn$values[rows_customAn, colAnn1_custom], sep = "_")
          ds_sub$colAnn$MergeID <- paste(ds_sub$colAnn[, colAnn2], ds_sub$colAnn[, colAnn1], sep = "_")

          # Merge with annotations from values Excel sheet
          ds_sub$colAnn <- merge(x = ds_sub$colAnn, y = df[c("MergeID", new_colAnns)], by = "MergeID")
          # Rename rows
          rownames(ds_sub$colAnn) <- ds_sub$colAnn$MergeID
          ds_sub <- sort_dataset(ds_sub, col_order = colnames(ds_sub$vals))

          # Create plots
          create_heatmap_plots(ds_sub, rowAnns, colAnns, out_dir, labels = c(customAn_name))
        },
        error = function(err) {
          print(sprintf("%s", err))
          return()
        }
      )
    }
  }
  turn_off_null_devices()
}

#' Plots a heatmap
#'
#' Transforms a numeric data frame/matrix and plots corresponding heatmap using the pheatmap function and saves to file
#'
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
#' @param pal_brew RColorBrewer palette. See RColorBrewer::display.brewer.all() for all options. Note: Define PAL_BREWER global variable.
#' @param pheatmap_scale String describing which direction around the mean to scale by. Accepts the same values ("none", "row", "column") as scale parameter in ?pheatmap
#' @param scale_func Logical indicating whether to scale matrix using scale()
#' @param man_scale Logical indicating whether to manually scale each row/column using rescale() from scales package
#' @param man_scale_range Numeric vector of length 2 indicating desired range of values e.g. c(-4,4), applied when man_scale is TRUE.
#' @param col.or.row numeric - 1 for row, 2 for column. Only applicable when z_score, scale_func, man_scale are TRUE.
#' @param ... Additional plotting parameters. Parameters passed to pheatmap, see ?pheatmap.
#' @export
plot_heatmap <- function(mat, ann_row = NA, ann_col = NA, ann_colors = NA, plot_title = "", out_dir = ".", labels = "",
                         clust_row = F, clust_col = F, log10 = F, z_score = F, man_scale = T, man_scale_range = c(-2, 2), scale_func = F, col.or.row = 2, pheatmap_scale = "none",
                         clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean", clustering_method = "complete",
                         show_colnames = T, show_rownames = F, pal_brew = "RdBu", border_color = NA, ...) {
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
  # If brewer palette specified in global constants/variables, make it as default palette
  if (isTRUE("PAL_BREWER" %in% ls(envir = .GlobalEnv))) {
    pal_brew <- PAL_BREWER
  }
  # Gradient for heatmap
  pal_grad <- get_col_palette(pal_brew, rev = T) %>% get_col_gradient(100)

  # Rescale manually
  if (isTRUE(man_scale)) {
    # Scale each column (stain) so range is forced between 2 values {-4, 4}
    mat <- apply(mat, col.or.row, function(x) {
      scales::rescale(x, to = man_scale_range)
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
      pheatmap(mat,
        scale = pheatmap_scale,
        show_rownames = show_rownames,
        show_colnames = show_colnames,
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
        border_color = border_color,
        filename = sprintf("%s/%s_heatmap.pdf", out_dir, paste(labels, collapse = "_")),
        ...
      )
    },
    error = function(err) {
      ## do something with 'err', then maybe throw it again stop(err)
      x <- sprintf("%s", err)
      print(x)
      return()
    }
  )
}
