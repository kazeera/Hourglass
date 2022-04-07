xl_file <- "../../Example2.xlsx"

i=1
datasets = NULL
keep_column_colAnn = "Keep.In.Analysis"
sample.or.patient ="BySample"

rowAnns = c(rowAnn1, rowAnn2)
colAnns = c(run$param_column, run$feature_column)
output_folder = main_folder
ds.imp = ds.imp
feat_sets = feat_sets
var_colors = var_colors
gradient_palette = run$color_gradient
corr_method = run$corr_method
pval.test = run$pval_test
pval.label = ifelse(grepl("star", run$pval_label), "p.signif", "p.format")
paired_analysis_column = ifelse(sample.or.patient == "BySample", run$paired_id_column, NA)
make.QC.param = run$qc_param_boxplots
make.QC.feature = run$qc_feature_boxplots
discrete_stacked_params =  run$discrete_params
make.het.plot = run$barplot_het
make.indiv.boxplot = run$boxplot_indiv
make.overview.boxplot = run$boxplot_overview
make.heatmap = run$heatmap
make.corrplot = run$corrplot
make.overview.corrscatt = run$corrscatt_overview
make.FC.pval.plot = run$pval_FC_heatmap
make.barplot = run$barplot_profile
save_table=

out_dir = create_folder(paste(output_folder, ds$name, ds$comparison, "Paired", sep = "/"))
pair_id=paired_analysis_column
