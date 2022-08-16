# Continue from 3
library(openxlsx)

# # Equivalent
# load("ExampleData2/example_IHC_samples_dataset.rdata")

# Make a new list for other tables
example_IHC_samples_run <- list()

# Make a new list element for each sheet for the hourglass template
user_excel <- "Example_IHC_sample_UserOptions.xlsx" #from interface
for(sheet in getSheetNames(user_excel)){
  example_IHC_samples_run[[sheet]] <- read.xlsx(user_excel, sheet = sheet)
}

# Add info
info_excel <- "Example_IHC_sample_Information.xlsx" #from interface
for(sheet in getSheetNames(info_excel)){
  example_IHC_samples_run[[sheet]] <- read.xlsx(info_excel, sheet = sheet)
}

# Save R data files
save(example_IHC_samples_run, file="ExampleData2/example_IHC_samples_run.RData")

# Other inbuilt objects
PANC_TISS_ORDER <- c("reactive","intermediary","deserted")
save(PANC_TISS_ORDER, file="PANC_TISS_ORDER.RData")

# Custom column levels - quantiles
LEVELS <- list(l="low", i="intermed", h="high")
save(LEVELS, file="LEVELS.RData")
