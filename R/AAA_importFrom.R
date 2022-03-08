#' @importFrom grDevices boxplot.stats colorRampPalette dev.list dev.off pdf
#' @importFrom graphics box title
#' @importFrom stats IQR aggregate as.hclust ave complete.cases cor df dist filter hclust median na.omit pairwise.t.test  pairwise.wilcox.test quantile reorder runif sd symnum
#' @importFrom utils install.packages installed.packages  read.csv read.delim
#' @importFrom ComplexHeatmap cluster_within_group
#' @importFrom corrplot corrplot cor.mtest
#' @importFrom ggplot2 aes element_blank element_line stat_summary element_text facet_grid geom_bar geom_boxplot geom_hline geom_jitter geom_line geom_point geom_text geom_tile ggplot ggsave ggtitle labs layer_scales scale_color_gradientn scale_color_manual scale_fill_gradientn scale_fill_manual scale_size scale_x_discrete scale_y_continuous scale_y_discrete position_dodge position_jitterdodge theme coord_equal unit
#' @importFrom ggpubr ggscatter stat_compare_means
#' @importFrom ggsignif geom_signif
#' @importFrom dplyr %>%
#' @importFrom gtools combinations permutations
#' @importFrom openxlsx read.xlsx
#' @importFrom pheatmap pheatmap
#' @importFrom psych pairs.panels
#' @importFrom plyr mapvalues rename
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom reshape2 melt
#' @importFrom scales percent_format show_col
#' @importFrom survival survfit
#' @importFrom survminer ggsurvplot theme_cleantable
#' @importFrom viridis viridis plasma
# importFrom("grDevices", "boxplot.stats", "colorRampPalette",
#            "dev.list", "dev.off", "pdf")
# importFrom("graphics", "box", "title")
# importFrom("stats", "IQR", "aggregate", "as.hclust", "ave",
#            "complete.cases", "cor", "df", "dist", "filter", "hclust",
#            "median", "na.omit", "pairwise.t.test",
#            "pairwise.wilcox.test", "quantile", "reorder", "runif",
#            "sd", "symnum")
# importFrom("utils", "install.packages", "installed.packages",
#            "read.csv", "read.delim")
# # Dependencies
# importFrom("ComplexHeatmap", "cluster_within_group")
# importFrom("corrplot", "corrplot", "cor.mtest")
# importFrom("ggplot2", "aes", "element_blank", "element_line", "stat_summary",
#            "element_text","facet_grid","geom_bar","geom_boxplot","geom_hline","geom_jitter","geom_line","geom_point",
#            "geom_text","geom_tile","ggplot","ggsave","ggtitle","labs","layer_scales","scale_color_gradientn","scale_color_manual",
#            "scale_fill_gradientn","scale_fill_manual","scale_size","scale_x_discrete","scale_y_continuous","scale_y_discrete",
#            "position_dodge","position_jitterdodge","theme","coord_equal","unit")
# importFrom("ggpubr", "ggscatter", "stat_compare_means")
# importFrom("ggsignif", "geom_signif") # not needed
# importFrom("dplyr", "%>%")
# importFrom("gtools", "combinations", "permutations")
# importFrom("openxlsx", "read.xlsx")
# importFrom("pheatmap", "pheatmap")
# importFrom("psych", "pairs.panels")
# importFrom("plyr", "mapvalues", "rename")
# importFrom("RColorBrewer", "brewer.pal", "brewer.pal.info")
# importFrom("reshape2", "melt")
# importFrom("scales", "percent_format", "show_col")
# importFrom("survival", "survfit)
# importFrom("survminer", "ggsurvplot", "theme_cleantable")
# importFrom("viridis", "viridis", "plasma")
NULL
