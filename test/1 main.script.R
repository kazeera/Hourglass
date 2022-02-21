# Save start time to a variable
start_time <- Sys.time()

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

# for(dir in dir_funcs) {
#   for(fil in+ list.files(dir) ){
#     source(paste(dir,fil, sep="/"))
#   }
# }

# Call required libraries
load_packages(c("ggplot2", "ggpubr", "ggsignif", "reshape2",# box plots and bar graph
                "pheatmap", "RColorBrewer", "scales", "corrplot", "viridis", "psych", # heatmaps
                "openxlsx", "plyr", #mapvalues
                "dplyr", "ComplexHeatmap", "gtools", #combinations
                "survival", "survminer"))

# Load constants
source("constants_vars.R")

# User options file
in_file <- "220118_UserOptions.xlsx"

# Read in color palette
global_palette <- get_colors(in_file, sheet = "Colors")
# scales::show_col(unlist(global_palette))

# Read in all comparisons (includes data file paths and advanced options)
comparisons <- get_comparisons(in_file, sheet = "Comparisons")

# Read in custom analysis
feat_sets <- get_feat_sets(in_file)

# todo
# global_palette --> variable_colors
# customAn --> feature_sets
# outdir in upload data instead of dataset_name
#

# Read in data (sample) and create dataset object
data_in <- "IHC_Data/20220113"
ds.samples <- make_dataset_ob(vals = read_file(sprintf("%s_BY.CORE_values.csv", data_in)),
                              rowAnn = read_file(sprintf("%s_BY.CORE_rowAnn.csv", data_in)),
                              colAnn = read_file(sprintf("%s_BY.CORE_colAnn.csv", data_in)),
                              name = "BySample")

# Read in data (patient)
ds.patients <- make_dataset_ob(vals = read_file(sprintf("%s_BY.PATIENT_values.csv", data_in)),
                               rowAnn = read_file(sprintf("%s_BY.PATIENT_rowAnn.csv", data_in)), #
                               colAnn = read_file(sprintf("%s_BY.PATIENT_colAnn.csv", data_in)),
                               name = "ByPatient")
# Make list of datasets
datasets <- list(samples = ds.samples, patients = ds.patients)

run$do_survival_analysis <- F #TODO account for it
# # Run hourglass
# run_hourglass(comparisons, global_palette, customAn, datasets)
source("1b run_hourglass.R")

# Print time difference to run log
end_time <- Sys.time()
print(end_time - start_time)
print(Sys.time() - start_time)
# Time difference of 1.503484 hours
# Stop sinking console output
sink()

# # Quit R
# quit(save = "no", status=0)
