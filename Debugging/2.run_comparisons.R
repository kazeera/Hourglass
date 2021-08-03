# Read in all comparisons 
comparisons_to_run <- openxlsx::read.xlsx(sheet = "Comparisons", xlsxFile = list.files()[grepl("Comparisons_to_run", list.files())]) 

# Read in custom analysis file
customAn <- list.files() %>%
  .[grepl("CustomAnalyses", .)] %>%
  import_customAn_file

# For each row in the comparisons excel file df, get run criteria from excel file 
# i=3
# core.or.patient="BY.PATIENT"

for(i in 1:nrow(comparisons_to_run)){ 
  # lapply(1:nrow(comparisons_to_run), function(i){
  # Current comparison
  run <- comparisons_to_run[i,]
  # Specify which columns in columns annotations we want to split analysis by
  colAnns <- c(run$param_column, run$feature_column)
  # Pick whether you want to look at all cores or cores averaged across patients
  for(core.or.patient in c("BY.CORE", "BY.PATIENT")){
    # If the value is FALSE, skip
    if(!run[,core.or.patient]) next
    
    # If it is TRUE, pick whether we are currently looking at all cores and/or cores averaged across patients
    if(core.or.patient == "BY.CORE"){
      ds <- df.cores # dataset
      ds.imp <- df.cores_imp
      rows_to_keep <- subset_PDAC_rowAnn(ds$rowAnn, run$EXC_HRD, run$EXC_NEO, run$LIMIT_TO_PANC, run$LIMIT_TO_PDAC)
      run$survival.curve <- F
    }else{
      ds <- df.patients
      ds.imp <- df.patients_imp
      rows_to_keep <- subset_PDAC_rowAnn(ds$rowAnn, run$EXC_HRD, run$EXC_NEO) # df doesn't have "Pancreas.tissue" column
      run$paired.analysis <- F
    }
    
    # What columns are we keeping in the analysis
    cols_to_keep <- ds$colAnn$Keep.In.Analysis & 
      !apply(ds$vals, 2, function(x) all(is.na(x))) # all values in columns are NA
    
    # Subset ds (vals, rowAnn, colAnn)
    ds <- subset_dataset(ds, rows_to_keep, cols_to_keep)
    ds.imp <- subset_dataset(ds.imp, rows_to_keep, cols_to_keep)
    
    # Print message if the param and feature columns aren't found in colAnn
    if(any(! colAnns %in% colnames(ds$colAnn))){
      print(sprintf("%s not found in %s colAnn table. Skip comparison.", rowAnn1, core.or.patient)); next
    }
    # Get some parameters for current analysis
    rowAnn1 <- run$MainComparison; rowAnn2 <- run$Subgroup
    
    # Print message if comparison column or colour code subgroup not found in rowAnn
    if(!rowAnn1 %in% colnames(ds$rowAnn) & !is.na(rowAnn1)){ #scaffold col could be NA if it's a custom analysis
      print(sprintf("%s not found in %s rowAnn table. Skip comparison.", rowAnn1, core.or.patient)); next
    }
    if(!is.na(rowAnn2) & !rowAnn2 %in% colnames(ds$rowAnn)){
      print(sprintf("%s not found in %s rowAnn table. Skip comparison.", rowAnn2)); next
    }
    
    # If it's a custom analysis, make new scaffold column of expression level (low, intermediate, high) of a specific stain/parameter (i.e. any column in the ds matrix)
    if(!is.na(run$CustomComparison)){ 
      # ds 1: Raw data
      new <- add_to_rowAnn(ds, run$CustomComparison)
      rowAnn1 <- new$rowAnn1; ds$rowAnn <- new$rowAnn
      # ds 2: Imputed 
      new <- add_to_rowAnn(ds.imp, run$CustomComparison)
      ds.imp$rowAnn <- new$rowAnn
    }
    
    # Remove NAs in MainComparison
    ds <- subset_dataset(ds, rows_to_keep = !is.na(ds$rowAnn[,rowAnn1]))
    ds.imp <- subset_dataset(ds.imp, rows_to_keep = !is.na(ds$rowAnn[,rowAnn1]))
    
    # save.image("2run_comparisons.RData")
    
    # Make comparison label which will be the main out directory
    if(is.null(ds$comparison)){
      current_comparison <- get_out_dir(current_dir = rowAnn1, rowAnn2 = rowAnn2, EXC_HRD = isTRUE(run$EXC_HRD), EXC_NEO = isTRUE(run$EXC_NEO))
      ds$comparison <- ds.imp$comparison <- current_comparison
    }
    
    # Run analysis
    run_comparison(
      ds,
      rowAnns = c(rowAnn1, rowAnn2),
      colAnns = colAnns,
      output_folder = ds$name,
      ds.imp = ds.imp,
      customAn = customAn,
      global_palette = global_palette,
      gradient_palette = "RdBu",
      corr_method = run$corr_method,
      pval.test = run$pval_test,
      pval.label = ifelse(grepl("star", run$pval_label), "p.signif", "p.format"),
      paired_analysis_column = run$paired_analysis_column,
      make.QC.param = run$QC_param_boxplots,
      make.QC.feature = run$QC_feature_boxplots,
      discrete_stacked_params =  run$discrete_stacked_plots_params,
      make.indiv.boxplot = run$boxplot_indiv,
      make.overview.boxplot = run$boxplot_overview,
      make.heatmap = run$heatmap,
      make.corrplot = run$corrplot,
      make.overview.corrscatt = run$corrscatt_overview,
      make.FC.pval.plot = run$pval_FC_heatmap,
      make.barplot = run$barplot,
    )
  }
} 
# 
# 
# rowAnns = c(rowAnn1, rowAnn2)
# output_folder = ds$name
# make.QC.param = run$QC_param_boxplots
# make.QC.feature = run$QC_feature_boxplots
# paired.analysis = run$paired.analysis
# corr_method = run$corr_method
# pval.test = run$pval_test
# pval.label = ifelse(grepl("star", run$pval_label), "p.signif", "p.format")
# make.indiv.boxplot = run$boxplot_indiv
# make.overview.boxplot = run$boxplot_overview
# make.heatmap = run$heatmap
# make.corrplot = run$corrplot
# make.overview.corrscatt = run$corrscatt_overview
# make.FC.pval.plot = run$pval_FC_heatmap
# make.barplot = run$barplot
# save.image("run.RData")

# df <- data.frame(rowAnn1 = ds$rowAnn[,rowAnn1], ds.imp$vals[,c(1:4,35:56)])
# df = df[!is.na(df[,1]),]
# df2 = make_FC.pval_df(df)
# make_FC.pval_plot(df2, save.to.file = F,apply_scale_colFC = F)
# plot_indiv_boxplot(df, rowAnns = c(rowAnn1, rowAnn2), save.to.file = F)