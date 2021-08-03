# save.image("run.RData")
# load("run.RData")
#' Run analysis in streamlined approach
#'
#' Create the plots for each comparison. Run the same analysis on dataset object (ds parameter) and transformed values (ds.imp parameter, optional).
#'
#' @param ds A dataset object (a list with vals, rowAnn, colAnn, comparison, name).
#' @param rowAnns A character vector of 1-2 column names in ds$rowAnn. c(Comparison, Subgroup)
#' @param colAnns A character vector of 1-2 column names in ds$colAnn. c(Parameter, Feature/Stain/Gene)
#' @param output_folder The main output folder for all custom analysis plots and boxplots for by.parameter and by.feature analysis
#' @param run A one row data frame or list object with logicals for what to run, names: make.boxplots, make.paired.boxplots, make.heatmaps, make.surv.curve
#' @param ds.imp A dataset object similar to ds with imputed or another transformation values.
#' @param customAn A list of 2 data frames for plotting specific rows and columns. @seealso customAn read in using \code{\link{import_custom_analysis_file}}, see description of file.
#' @param global_palette A named vector with colors as values and annotations/groups as names.
#' @param gradient_palette RColorBrewer palette name for gradients (e.g. heatmap, correlation plots). See RColorBrewer::display.brewer.all() for all options. 
#' @param corr_method Method for correlation (one of "pearson","spearman","kendall").
#' @param pval.test Which two-samples testing should be used? String corresponding to "method" parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "t.test" and "wilcox.test".
#' @param pval.label How to display p-values? String corresponding to "label" parameter in \code{\link[ggpubr]{stat_compare_means}}. Allowed values are "p.signif" (stars) and "p.format" (number).
#' @param FC.method Fold change computation method to use, either "divide" (for non-transformed values) or "subtract" (for log2-transformed values)
#' @param make.QC.param,make.QC.feature,make.indiv.boxplot,make.overview.boxplot,make.heatmap,make.corrplot,make.overview.corrscatt,make.indiv.corrscatt,make.barplot,make.FC.pval.plot Logicals (TRUE/FALSE) indicating whether to make these plots. Note: make.indiv.corrscatt = T takes a long time.
#' @param paired_analysis_column column name in ds$rowAnn to create paired analysis plots for, e.g. PatientID if ds is data for all cores
#' @param discrete_stacked_params parameter names to search for in colAnns[1] to make discrete stacked barplots, e.g. "Het.Score"
#' @export
run_comparison <- function(ds, rowAnns, colAnns = NA, output_folder = ".", ds.imp = NULL, customAn = NULL, global_palette = NULL, gradient_palette = "RdBu", 
                           corr_method = "pearson", pval.test = "t.test", pval.label = "p.signif", 
                           FC.method = "divide", paired_analysis_column = NA, make.QC.param = F, make.QC.feature = F, discrete_stacked_params = NULL, 
                           make.indiv.boxplot = F, make.overview.boxplot = F, make.heatmap = F, make.corrplot = F, 
                           make.overview.corrscatt = F, make.indiv.corrscatt = F, make.barplot = F, make.FC.pval.plot = F) {
  
  # Make all plots
  run_comparison_helper(ds, rowAnns, colAnns, out_dir = create_folder(paste(output_folder, ds$comparison, sep="/")),
                        customAn, global_palette, gradient_palette, corr_method, pval.test, pval.label, make.QC.param, make.QC.feature,
                        make.indiv.boxplot, make.overview.boxplot, make.heatmap, make.corrplot, 
                        make.overview.corrscatt, make.indiv.corrscatt, make.barplot, make.FC.pval.plot
  )
  
  # Make discrete barplots, e.g. Het.Score 
  run_discrete_barplot_analysis(ds, rowAnns[1], colAnns, parameters = discrete_stacked_params,
                                out_dir = create_folder(paste(output_folder, ds$comparison, sep = "/")),
                                gradient_palette = gradient_palette
  )
  
  # Paired analysis 
  if (is.character(paired_analysis_column)) { #TODO check if it works/optimize - paired_analysis_column has to be argument in function below NOT manually "Case ID"
    run_paired_analysis(ds, rowAnns, colAnns, out_dir = create_folder(paste(output_folder, ds$comparison, "Paired", sep="/")), global_palette)
  }
  
  # Imputed
  if (isFALSE(is.null(ds.imp))) {
    run_comparison_helper(ds.imp, rowAnns, colAnns, out_dir = create_folder(paste(ds.imp$name, ds.imp$comparison, sep="/")),
                          customAn, global_palette, gradient_palette, corr_method, pval.test, pval.label, make.QC.param, make.QC.feature,
                          make.indiv.boxplot, make.overview.boxplot, make.heatmap, make.corrplot,
                          make.overview.corrscatt, make.indiv.corrscatt, make.barplot, make.FC.pval.plot
    )

  }
}

#' Runs main analysis for all parameter combos specified in colAnns and creates plots.
#'
#' @inheritParams run_comparison ds,rowAnns,colAnns,out_dir,customAn,global_palette,gradient_palette,corr_method,pval.test,pval.label,make.QC.param,make.QC.feature,make.indiv.boxplot,make.overview.boxplot,make.heatmap,make.corrplot,make.overview.corrscatt,make.indiv.corrscatt,make.barplot,make.FC.pval.plot
#' @export
run_comparison_helper <- function(ds, rowAnns = 1, colAnns = NA, out_dir = ".", customAn = NULL, global_palette = NULL, gradient_palette = NULL,
                                  corr_method = "pearson", pval.test = "wilcox.test", pval.label = "p.signif",
                                  make.QC.param = F, make.QC.feature = F, 
                                  make.indiv.boxplot = F, make.overview.boxplot = F, make.heatmap = F, make.corrplot = F, 
                                  make.overview.corrscatt = F, make.indiv.corrscatt = F, make.barplot = F, make.FC.pval.plot = F) {
  
  
  # Analysis 1: Make QC plots stratified by parameter
  if (isTRUE(make.QC.param)) {
    # Create a directory in each comparison folder
    out_dir2 <- create_folder(sprintf("%s/QC %s", out_dir, colAnns[1]))
    
    # Parameters of column annotations to run
    params <- ds$colAnn[, colAnns[1]] %>% unique()
    
    # Considering all stains, make heatmaps
    for (param1 in params) { 
      # Get logical of whether or not to keep columns in data table
      cols_to_keep <- ds$colAnn[, colAnns[1]] %in% param1
      if (sum(cols_to_keep) < 3) next
      # Subset dataset object accordingly
      ds_sub <- subset_dataset(ds, cols_to_keep = cols_to_keep)
      colnames(ds_sub$vals) <- ds_sub$colAnn[, colAnns[2]]
      # Create plots # just boxplots
      create_plots(ds_sub, rowAnns, colAnns, out_dir2, labels = param1, global_palette, gradient_palette,
                   corr_method, pval.test, pval.label,
                   make.indiv.boxplot, make.overview.boxplot)
      # Turn off null devices
      turn_off_null_devices()
    }
  }
  
  # Analysis 2: Make QC plots stratified by stain/feature/gene
  if (isTRUE(make.QC.feature)) {
    # Create a directory in each comparison folder
    out_dir2 <- create_folder(sprintf("%s/QC %s", out_dir, colAnns[2]))
    
    # Parameters of column annotations to run
    features <- ds$colAnn[, colAnns[2]] %>% unique()
    
    # Considering all stains, make heatmaps
    for (feat1 in features) {
      # Get logical of whether or not to keep columns in data table
      cols_to_keep <- ds$colAnn[, colAnns[2]] %in% feat1
      if (sum(cols_to_keep) < 3) next
      # Subset dataset object accordingly
      ds_sub <- subset_dataset(ds, cols_to_keep = cols_to_keep)
      colnames(ds_sub$vals) <- ds_sub$colAnn[, colAnns[1]]
      # Create plots # just boxplots
      create_plots(ds_sub, rowAnns, colAnns, out_dir2, feat1, global_palette, gradient_palette,
                   corr_method, pval.test, pval.label,
                   make.indiv.boxplot, make.overview.boxplot)
      # Turn off null devices
      turn_off_null_devices()
    }
  }
  
  # Analysis 3: Make biologically relevant heatmaps by combining stains from handpicked parameters #defined in main.script
  if (!is.null(customAn)) {
    # Create a directory in "custom" folder if colAnns = NA
    out_dir2 <- ifelse(all(is.na(colAnns)), out_dir, create_folder(sprintf("%s/Custom", out_dir)))
    
    # ca = custom analysis
    for (i in 1:nrow(customAn$keys)) {
      tryCatch(
        {
          # Subset dataset
          res <- get_customAn_ds(ds, customAn, i) #TODO new_colAnns is no more, remake function :D 
          colnames(res$ds$vals) <- get_nth_part(colnames(res$ds$vals), "\\.", 1) #TODO do this part in get_customAn_ds() last line
          # Create plots # make all plots
          create_plots(res$ds, rowAnns, res$colAnns, create_folder(sprintf("%s/%s", out_dir2, res$customAn_name)), res$customAn_name, global_palette, gradient_palette,
                       corr_method, pval.test, pval.label,
                       make.indiv.boxplot, make.overview.boxplot, make.heatmap, make.corrplot, 
                       make.overview.corrscatt, make.indiv.corrscatt, make.barplot, make.FC.pval.plot, also.complete = T)
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

