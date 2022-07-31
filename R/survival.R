#' Iteratively plot survival curves (Kaplan Meier)
#'
#' @param ds A dataset object (a list with vals, rowAnn, colAnn, comparison, name).
#' @param rowAnn1 A string description/label for file names and plot title; could be strata name
#' @param run A one row data frame or list object with logicals for what to run, names: surv_time_column, surv_status_column
#' @param output_folder The main output folder for all custom analysis plots and boxplots for by.parameter and by.feature analysis
#' @export
run_surv_analysis <- function(ds, rowAnn1, run, surv_folder = ".") {
  # Make survival plots
  tryCatch({
    # Make data frame and rename columns
    df <- ds$rowAnn[,c(run$surv_time_column, run$surv_status_column, rowAnn1)]
    # Rename columns
    colnames(df)[1:3] <- c("time", "status", "col")
    # Back up variables from original comparisons for later sex comparisons
    df_original <- df
    
    # Create output directory 
    out_dir <- create_folder(paste(surv_folder, ds$comparison, sep="/"))
    label <- rowAnn1
    
    # Save to file
    filename <- sprintf("%s/%s_survplot.pdf", out_dir, label)
    pdf(filename)
    
    # Plot curve with current strata
    plot_surv_curve(df, label, out_dir)
    
    # If it's a custom analysis (ie groups split into low, int, high), perform binning of 3 groups
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
  })
  dev.off()
  
  # Backup original output folder
  out_dir_orig <- out_dir
  
  # Check whether user wants to divide cohort
  sub_analyses <- strsplit(run$WithinGroup, ";") %>% unlist %>% trimws
  if(length(sub_analyses) == 0 | isTRUE(is.na(sub_analyses))) return()
  
  # For each within group analysis, divide cohort and run hourglass within groups
  for(rowAnn_col in sub_analyses){
    if(!rowAnn_col %in% colnames(ds$rowAnn)) next
    # Get unique groups 
    # e.g. If rowAnn_column is "Sex" with unique values NA, "F", "M", groups returns "F" and "M"
    groups <- ds$rowAnn[,rowAnn_col] %>% unique %>% na.omit %>% as.character
    
    # Run hourglass within cohorts
    for (group in groups){
      # Positively select group
      df <- subset_dataframe(df_original, rows_to_keep = ds$rowAnn[,rowAnn_col] == group)
      
      tryCatch({
        # Make survival plots
        # out_dir <- create_folder(paste(out_dir_orig, gsub("-like", "", group)))
        label <- paste(rowAnn1, group)
        
        # Save to file
        filename <- sprintf("%s/%s_survplot.pdf", out_dir_orig, label)
        pdf(filename)
        plot_surv_curve(df, label, out_dir)
        
        # If it's a custom analysis (ie groups split into low, int, high), perform binning of 3 groups
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
      })
      dev.off()
    }
  }
}

#' Plot individual survival curve (Kaplan Meier)
#'
#' @param df A data frame with 3 columns: time, status (censoring), col (variable to stratify by)
#' @param descr A string description/label for file names and plot title; could be strata name
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param save.to.file A logical indicating whether to save to out_dir (T) or print to panel (F)
#' @export
plot_surv_curve <- function(df, descr = "", out_dir = ".", save.to.file = F) {
  # library(survival) # computing survival analyses
  # library(survminer) #  summarizing and visualizing the results of survival analysis
  # library(viridis) # color palette
  # library(dplyr)
  
  # Rename columns
  colnames(df)[1:3] <- c("time", "status", "col")
  
  tryCatch(
    {
      # Compute KM survival estimate
      fit <- do.call(survfit, list(formula = Surv(time, status) ~ col, data = df))
      
      # Legend labels
      labs <- gsub("col=", "", names(fit$strata)) # "col=high" "col=intermed" "col=low" to "high"     "intermed" "low"
      
      # Make colors
      line_colors <- plasma(n = length(fit$n)+1) %>% .[-(length(.)+1)]
      
      # ggplot theme
      theme <- theme(
        # grid
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # font size
        plot.title = element_text(size = 8),
        legend.title	= element_text(size = 10),
        # Axes labels
        axis.text = element_text(colour = "black", size = 12),
        axis.text.x = element_text(margin = margin(t = 4, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
        axis.text.y = element_text(margin = margin(t = 0, r = 4, b = 0, l = 0)),
        # Ticks
        axis.ticks = element_line(colour = "black", size = 0.5), # increase the tick thickness)
        axis.ticks.length = unit(.15, "cm"),
        # axes tick labels
        axis.title = element_text(colour = "black", size = 12, face = "bold"), # axes title labels
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), # increase space between x axis title and labels
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))
      suppressWarnings({
        # P-value postiion on x axis
        pval_x <- (diff(range(unique(fit$time)))-300)/2
        
        # Plot  
        g <- ggsurvplot(fit,
                        # Stats
                        pval = T, # of the Log-Rank test comparing the groups
                        # pval.size = 4, # font size
                        pval.coord = c(pval_x, 1), # location on plot
                        pval.size = 4,
                        conf.int = F, # 95% CI
                        ## legends and labels
                        legend.title = descr,
                        legend.labs = labs,    # change legend labels
                        xlab = "Time", # customize X axis label
                        title = paste0("Kaplan-Meier, ", out_dir), # plot title
                        # linetype = "col", # Change line type by groups
                        surv.median.line = "hv", # Specify median survival
                        
                        ## Colors and themes 
                        palette = line_colors, # colors of lines
                        ggtheme =  theme, # theme_cleantable()Change ggplot2 theme
                        
                        ## tables: 
                        cumevents = T, # add cumulative num of events table
                        risk.table = "absolute", # "abs_pct", # show the absolute number and the percentage of subjects at risk by time
                        # ncensor.plot = TRUE,      # plot the number of censored subjects at time t
                        # fontsize = 2.4, # size of tabls text
                        # tables.col = T, # Change risk table color by groups instead of all black text
                        # tables.height = 0.2,
                        tables.y.text = F, # risk.table.y.text = FALSE, cumevents.y.text = FALSE,# show bars instead of names in text annotations
                        tables.theme = theme_cleantable())
      })
      
      # Save plot to file
      if (save.to.file) {
        # Print to file
        filename <- sprintf("%s/%s_survplot.pdf", out_dir, descr)
        ggsave(file = filename, print(g), width = 5, height = 7)
      } else {
        # Print to image panel
        print(g)
      }
    },
    error = function(err) {
      print(sprintf("%s", err))
    }
  )
}