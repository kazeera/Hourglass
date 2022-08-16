## Run once

# Import functions to run this script
library(dplyr)
# List directories containing functions
dir_funcs <- "R"
# Read in source files in each directory
invisible(lapply(dir_funcs, function(dir) {
  lapply(list.files(dir), function(fil){
    tryCatch(
      source(paste(dir,fil, sep="/")), finally = return()
    )
  })
}))

# Call required libraries 
load_packages(c("ggplot2", "ggpubr", "ggsignif", "reshape2",# box plots and bar graph
                "pheatmap", "RColorBrewer", "scales", "corrplot", "viridis", "psych", # heatmaps
                "openxlsx", "plyr", #mapvalues
                "dplyr", "ComplexHeatmap", "gtools", #combinations
                "survival", "survminer"))

# Read in  data tables - core and by patient
data_in <- "Data/20210714"
x <- make_dataset_ob(vals = read_file(sprintf("%s_BY.CORE_values.csv", data_in)) %>% df_to_numeric %>% remove_outliers_df,
                              rowAnn = read_file(sprintf("%s_BY.CORE_rowAnn.csv", data_in)),
                              colAnn = read_file(sprintf("%s_BY.CORE_colAnn.csv", data_in)),
                              name = "BY.SAMPLE")
# lapply(x, dim) # 648 696

# Rows to keep
x <- subset_dataset(x, # cols_to_keep = x$colAnn$Keep.In.Analysis, 
                    rows_to_keep = !is.na(x$rowAnn$Pancreas.tissue1) & (x$rowAnn$Pancreas.tissue1 == "PDAC") & (!x$rowAnn$Moffitt %in% c("unknown", "uncallable")))
x$rowAnn$Tissue.type %>% table

# Subset columns
x$rowAnn %>% colnames
x$rowAnn <- x$rowAnn[,c("Case_ID", "neo", "Tissue.type", "Sex", "TMA.stromal.subtype", "Moffitt", "main_stroma_type", "OS.from.Sx..days.", "event", "age_at_diagnosis", "Deceased")]

# Keep columns of values that have more than 300 values
cols_to_keep <- apply(x$vals, 2, function(x){
  sum(is.na(x))
}) %>% unname > 250
x <- subset_dataset(x, cols_to_keep = cols_to_keep)

# Remove rows with all NA
rows_to_keep <- !apply(x$vals, 1, function(x){
  all(is.na(x))
}) %>% unname 
x <- subset_dataset(x, rows_to_keep = rows_to_keep)

# Stains 
stains <- table(x$colAnn$Stain)
stains <- stains[stains >= 2]
stains <- stains[!names(stains) %in% c("AHR", "IR1")]
x <- subset_dataset(x, cols_to_keep = x$colAnn$Stain %in% names(stains))
x$colAnn[,c("Stain", "Parameter")]

saveRDS(x, file = "ds_core_example.rds") #todo read in this file and use as reference but simulate dataset with groups and use ranges from x$vals
# Make data frame and rename columns
df <- ds$rowAnn[,c("OS.from.Sx..days.", "event", rowAnn1)]
# Rename columns
colnames(df)[1:3] <- c("time", "status", "col")
