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
                    xlab = "Days", # customize X axis label
                    title = paste0("KM curve, ", out_dir), # plot title
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
  # # Save to file
  # filename <- sprintf("%s/%s_survplot.pdf", out_dir, descr)
  # 
  # # ggsave(file = filename, print(g), width = 5, height = 7)
  # pdf(filename)
  # 
  print(g)
  # dev.off()
  # # Return p-VALUE
  # surv_pvalue(fit) %>% .[["pval"]] %>% round(4)
}

# Make plot
df <- data.frame(time = rowAnn$OS_time, status = rowAnn$Status, col = rowAnn$Sex)
plot_surv_curve(df, descr = "Sex", out_dir = ".")

# Make plot
df <- data.frame(time = rowAnn$OS_time, status = rowAnn$Status, col = rowAnn$Smoker)
plot_surv_curve(df, descr = "Smoker", out_dir = ".")

# Make plot 
df <- data.frame(time = rowAnn$OS_time, status = rowAnn$Status, col = rowAnn$Sample_Cancer_Subtype)
plot_surv_curve(df, descr = "Sample_Cancer_Subtype", out_dir = ".")

# Make plot
df <- data.frame(time = rowAnn$OS_time, status = rowAnn$Status, col = rowAnn$Patient_Cancer_Subtype)
plot_surv_curve(df, descr = "Patient_Cancer_Subtype", out_dir = ".")
