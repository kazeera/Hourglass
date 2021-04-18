# Load in data
load("20210401_debugging.RData")

# Import functions to run this script
library(dplyr)
# List directories containing functions
dir_funcs <- "../R"

# Read in source files in each directory
for(dir in dir_funcs) {
  for(fil in list.files(dir) ){
    source(paste(dir,fil, sep="/"))
  }
}

# Call required libraries
load_packages(c("ggplot2", "ggpubr", "ggsignif", "reshape2",# box plots and bar graph
                "pheatmap", "RColorBrewer", "scales", "corrplot", "viridis", "psych", # heatmaps
                "openxlsx", "plyr", #mapvalues
                "dplyr", "ComplexHeatmap", "gtools", #combinations
                "survival", "survminer"))

# Color palette for scaffold column
PAL_SCAFF = list(deserted="#CC3399", intermediary="#513573", reactive="#002060",
                 "basal-like" = "darkgoldenrod1", classic = "royalblue4", # unknown = "gray50",
                 HRD = "darkorchid4", NO.HRD = "darkgreen",
                 neo="darkred", "non-neo"="gray26", #neo - red, dark gray (can't discriminate black #000000 from NA dots on box plot)
                 adj_normal="gray37", PDAC="darkcyan", #pancreas tissue - gray, dark cyan
                 NA_ = "black", #NA black
                 "deserted&non-neo"="#CC3399", "int+rea&non-neo"="#002060" #added 20201202 for neo_vs_TMAsubtype
)
