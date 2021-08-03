# Libraries
library(survival) # computing survival analyses
library(survminer) #  summarizing and visualizing the results of survival analysis
library(viridis) # color palette
library(dplyr)
library(plyr)

# Read in all comparisons and data
comparisons_to_run <- openxlsx::read.xlsx(xlsxFile = list.files()[grepl("Comparisons_to_run", list.files())], sheet = "Comparisons") 
load("1_20210415.RData")
source("surv.functions_plot.curve.R")
# Create main output
surv_folder <- create_folder("Survival")

# For each row in the comparisons excel file df, get run criteria from excel file 
for(i in 1:nrow(comparisons_to_run)){ 
  # Current comparison
  run <- comparisons_to_run[i,]
  
  if(isTRUE(run[, BY.PATIENT])){
    # Get some parameters for current analysis
    rowAnn1 <- run$Scaffold.Column
    
    # Subset ds (vals, rowAnn, colAnn)
    ds <- subset_dataset(df.patients, rows_to_keep = subset_PDAC_rowAnn(df.patients$rowAnn, run$EXC_HRD, run$EXC_NEO))

    if(!is.na(run$Scaffold.Column)){
      # Subset ds (vals, rowAnn, colAnn)
      ds <- subset_dataset(ds, rows_to_keep = ds$rowAnn[,rowAnn1] %in% names(PAL_SCAFF))
    }
    
    # Print message if scaffold column or colour code not found in scaffold
    if(!rowAnn1 %in% colnames(ds$rowAnn) & !is.na(rowAnn1)){ #scaffold col could be NA if it's a custom analysis
      print(sprintf("%s not found in %s table. Skip comparison.", rowAnn1, core.or.patient)); next
    }
    
    # If it's a custom analysis, make new scaffold column of expression level (low, intermediate, high) of a specific stain/parameter (i.e. any column in the ds matrix)
    if(!is.na(run$Custom.Column)){ 
      l <- add_to_rowAnn(ds, col_name = run$Custom.Column)
      rowAnn1 <- l$rowAnn1; ds$rowAnn <- l$rowAnn
      # rm(l)
    }
    
    # Make survival plots
    tryCatch({
      # Make data frame and rename columns
      df <- ds$rowAnn[,c("OS.months", "event", rowAnn1)]
      # Rename columns
      colnames(df)[1:3] <- c("time", "status", "col")
      # Back up variables from original comparisons for later basal vs, classical comparisons
      df_original <- df
      
      # Create output directory 
      out_dir <- create_folder(paste(surv_folder, ds$name, get_out_dir(rowAnn1, EXC_HRD = run$EXC_HRD, EXC_NEO = run$EXC_NEO), sep="/"))
      label <- rowAnn1
      # save.image("3.RData")
      
      plot_surv_curve(df, label, out_dir)
      
      # If it's a custom analysis (ie groups split into low, int, high), perform binning of 3 groups
      if ("LEVELS" %in% ls(.GlobalEnv)) {
        if (all(unique(df$col[!is.na(df$col)]) %in% unlist(LEVELS))) {
          # First bin first and second quartile
          col_lvls <- df$col
          df$col <- bin_vars(col_lvls, LEVELS$i, LEVELS$l) # "intermed" will become "low"
          plot_surv_curve(df, descr = paste(label, "(low+int vs high)"), out_dir)
          
          # Next bin second and third quartile
          df$col <- bin_vars(col_lvls, LEVELS$i, LEVELS$h) # "intermed" will become "high"
          plot_surv_curve(df, descr = paste(label, "(low vs int+high)"), out_dir)
          
          # Now remove int
          col_lvls[col_lvls == LEVELS$i] <- NA # "intermed" will become NA
          df$col <- col_lvls
          plot_surv_curve(df, descr = paste(label, "(no int)"), out_dir)
        }
      }
    })
    
    # Now for basal.vs.classical --------------------------------------------
    # Positively select basal and classical cores and run hourglass on those
    if(isTRUE(run$BASAL.vs.CLASSICAL)){
      out_dir_orig <- out_dir
      
      # For each Moffitt subset
      for (group in c("basal-like", "classic")){
        # all(rownames(df) == rownames(ds$rowAnn)) # T
        df <- subset_dataframe(df_original, rows_to_keep = ds$rowAnn$Moffitt == group)
       
        tryCatch({
          # Make survival plots
          out_dir <- create_folder(paste(out_dir_orig, gsub("-like", "", group)))
          label <- paste(rowAnn1, gsub("-like", "", group))
          
          plot_surv_curve(df, label, out_dir)
          
          # If it's a custom analysis (ie groups split into low, int, high), perform binning of 3 groups
          if ("LEVELS" %in% ls(.GlobalEnv)) {
            if (all(unique(df$col[!is.na(df$col)]) %in% unlist(LEVELS))) {

              # First bin first and second quartile
              col_lvls <- df$col
              df$col <- bin_vars(col_lvls, LEVELS$i, LEVELS$l) # "intermed" will become "low"
              plot_surv_curve(df, descr = paste(label, "(low+int vs high)"), out_dir)
              
              # Next bin second and third quartile
              df$col <- bin_vars(col_lvls, LEVELS$i, LEVELS$h) # "intermed" will become "high"
              plot_surv_curve(df, descr = paste(label, "(low vs int+high)"), out_dir)
              
              # Now remove int
              col_lvls[col_lvls == LEVELS$i] <- NA # "intermed" will become NA
              df$col <- col_lvls
              plot_surv_curve(df, descr = paste(label, "(no int)"), out_dir)
            }
          }
        })
      }
    }
  }
} 

