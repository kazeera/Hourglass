
# 1. Data Preparation
# Subset to cases that were histopathologically assessed: positive selection for mature&intermediate&immature; note neoadjuvant cases were already removed from data_resected

data_in <- "Data/20210714"
# Read in merged data tables - core and by patient
df.cores <- make_dataset_ob(vals = read_file(sprintf("%s_BY.CORE_values.csv", data_in)) %>% df_to_numeric %>% remove_outliers_df,
                            rowAnn = read_file(sprintf("%s_BY.CORE_rowAnn.csv", data_in)),
                            colAnn = read_file(sprintf("%s_BY.CORE_colAnn.csv", data_in)),
                            name = "BY.CORE")

# Read in merged data tables - core and by patient
df.patients <- make_dataset_ob(vals = read_file(sprintf("%s_BY.PATIENT_values.csv", data_in)) %>% df_to_numeric %>% remove_outliers_df,
                               rowAnn = read_file(sprintf("%s_BY.PATIENT_rowAnn.csv", data_in)), #
                               colAnn = read_file(sprintf("%s_BY.PATIENT_colAnn.csv", data_in)),
                               name = "BY.PATIENT")

# Remove outliers and impute with random values around mean (default is 5%)
impute_ds <- function(ds){
  make_dataset_ob(vals = ds$vals %>% impute_w_mean_df,
                  rowAnn = ds$rowAnn, 
                  colAnn = ds$colAnn, 
                  name = paste(ds$name, "imputed"))
}

df.cores_imp <- impute_ds(df.cores)
df.patients_imp <- impute_ds(df.patients)
save.image(sprintf("1_%s.RData", format(Sys.Date(), "%Y%m%d")))

           