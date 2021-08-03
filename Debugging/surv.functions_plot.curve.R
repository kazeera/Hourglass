# This function takes in a vector, v, and replaces all values in orig_vals, to new_val
bin_vars <- function(v, orig_vals, new_val){
  v[v %in% orig_vals] <- new_val
  return(v)
}

# Plot survival curve (Kaplan Meier)
# df = data frame with 3 columns: time, status (censoring), col (variable to stratify by)
# descr = string label for file names and plot title; could be strata name
plot_surv_curve <- function(df, descr, out_dir = "."){
  library(survival) # computing survival analyses
  library(survminer) #  summarizing and visualizing the results of survival analysis
  library(viridis) # color palette
  library(dplyr)
  
  # Compute KM survival estimate
  fit <- survfit(Surv(time, status) ~ col, data = df)
  
  # Legend labels
  labs <- fit$strata %>% names %>% gsub("col=", "", .) # "col=high" "col=intermed" "col=low" to "high"     "intermed" "low"  
 
  # Make colors
  line_colors <- plasma(n = length(fit$n)+1) %>% .[-(length(.)+1)]
  
  # Plot 
  suppressWarnings({
    g <- ggsurvplot(fit,
                    # Stats
                    pval = T, # of the Log-Rank test comparing the groups
                    # pval.size = 4, # font size
                    pval.coord = c(3000, 0.8), # location on plot
                    conf.int = F, # 95% CI
                    
                    ## legends and labels
                    legend.title = descr,
                    legend.labs = labs,    # change legend labels
                    xlab = "Time", # customize X axis label
                    title = paste0("Kaplan-Meier curve, ", descr, ", ", out_dir), # plot title
                    # linetype = "col", # Change line type by groups
                    surv.median.line = "hv", # Specify median survival
                    
                    ## Colors and themes 
                    palette = line_colors, # colors of lines
                    ggtheme = theme_bw(), # theme_cleantable()Change ggplot2 theme
                    
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
  # Save to file
  filename <- sprintf("%s/%s_survplot.png", out_dir, descr)
  ggsave(file = filename, print(g), width = 5, height = 7)
  
  # Return p-VALUE
  surv_pvalue(fit) %>% .[["pval"]] %>% round(4)
}


# Note  
# # # survfit() returns a list of variables, including the following components:
# # d <- data.frame(time = fit$time, # time points on the curve
# #                 n.risk = fit$n.risk, # number of subjects at risk at time t
# #                 n.event = fit$n.event, # number of events that occurred at time t
# #                 n.censor = fit$n.censor, # number of censored subjects, who exit the risk set, without an event, at time t
# #                 surv = fit$surv, # 
# #                 upper = fit$upper, #  lower and upper confidence limits for the curve
# #                 lower = fit$lower
# # )
# # head(d)
# # Summary of survival curves
# summary(fit)
# # Access to the sort summary table
# summary(fit)$table