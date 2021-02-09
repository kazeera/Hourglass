
# install.packages("styler")
styler::style_dir("R")
# TODO Fri 20210205
# > styler::style_dir("R")
# Styling  21  files:
#   boxplot.R              ???
# cluster.R              ???
# corrplot.R             ???
# corrscatt.R            ???
# create_heatmap_plots.R i
# discrete_barplot.R     i
# FC.pval.R              ???
# get_colors.R           ???
# heatmap.R              i
# impute_w_mean.R        ???
# make_dataset_ob.R      ???
# paired.R               ???
# profile_barplot.R      i
# remove_outliers.R      ???
# run_comparisons.R      ???
# sort.R                 ???
# stats.R                ???
# subset.R               ???
# survival_analysis.R    ???
# utils.R                ???
# utils_files.R          ???
# ----------------------------------------
#   Status	Count	Legend
# ??? 	14	File unchanged.
# i 	4	File changed.
# x 	3	Styling threw an error.
# ----------------------------------------
#   Please review the changes carefully!
#   Warning messages:
#   1: When processing boxplot.R: <text>:4:40: unexpected string constant
# 3: df <- cbind(ToothGrowth, var = rep(paste("Chicken", 1:5), 6))
# 4: plot_overview_boxplot(df[,c("var", supp", "
#                                ^
#    2: When processing stats.R: <text>:3:9: unexpected '['
#  2: pval_to_stars(c(0.0001, 0.00012, 0.002, 0.049, 0.05, 0.06))
#  3: Output: [
#    ^
#      3: When processing utils.R: <text>:7:0: unexpected end of input
#    5: # [3] "Heatmaps/BY.CORE/TMA.stromal.subtype_all excl HRD excl neo dots Moffitt"
#      6: # [4] "Heatmaps/BY.CORE/TMA.stromal.subtype_all excl HRD excl neo dots Moffitt/Surv"


#' @family plotting
#' @param df A data frame with the first column as discrete values to group by (i.e. rowAnn_col), and the rest of the columns are numeric variables to plot.
#' @param ... Additional parameters passed to \code{\link[psych]{pairs.panels}}. e.g. rug = T, scale = T (scale font by size of correlation), jiggle = T (jitter points).
#' @return Plot object if save.to.file is FALSE.
#'
#' # MAKE ALL
#' - rowAnn_col --> group_column
#' - trim_each_part (funct) --> abbrev
#' - get_file --> get_match
#'
#' Add @example, @description to all
#'
#'
#' Search for
#' - get_file (file utils) function in all files - can be renamed to make more general
#' - has_at.least_n.vals - reordered arguments within function call and change col.or.row to row.or.col
#' - has_less.than.eq.to_NA.thres - reordered arguments within function call and change col.or.row to row.or.col
#' - is this used in utils? reform_ann_df()
#' - "..." and make sure the  \code{\link{functioname}}, or another package \code{\link[packagename]{functioname}}.
#' - replace "logical indicating whether,,, with " If TRUE, " or " If FALSE, "
#' - Look for ::, library(), load_packages()
#' -Make sure all functions are present in files lol
#' - Ensure no many_NAs
#' Make more general
#' - sep_param_by_levels
#' - Make 2.Run_comparisons into a function?
#' - Function for preparing PDAC data
#'
#' Check
#' as_numeric_factor in utils.R
print("Hi")
