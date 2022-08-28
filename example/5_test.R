
# Save start time to a variable
start_time <- Sys.time()
todays_date <- format(Sys.Date(), "%Y%m%d")

LEVELS <- Hourglass::LEVELS
PANC_TISS_ORDER <- Hourglass::PANC_TISS_ORDER

# Import functions to run this script
library(dplyr)

# Read in source files in each directory
lapply(list.files("../R"), function(fil){
  tryCatch(
    source(paste("../R",fil, sep="/")), finally = return()
  )
})

# Call required libraries 
load_packages(c("ggplot2", "ggpubr", "ggsignif", "reshape2",# box plots and bar graph
                "pheatmap", "RColorBrewer", "scales", "corrplot", "viridis", "psych", # heatmaps
                "openxlsx", "plyr", #mapvalues
                "dplyr", "ComplexHeatmap", "gtools", #combinations
                "survival", "survminer"))