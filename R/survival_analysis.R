#' Plot survival curves (Kaplan Meier)
#'
#' @param df2 A data frame with 3 columns: time, status (censoring), col (variable to stratify by)
#' @param label A string label for file names and plot title; could be strata name
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @export
run_surv_analysis <- function(df2, label = "", out_dir = ".") {
  # Rename columns
  colnames(df2)[1:3] <- c("time", "status", "col")
  plot_surv_curve(df2, label, out_dir)

  # If it's a custom analysis (ie groups split into low, int, high), perform binning of 3 groups
  # Else plot label as is
  if ("LEVELS" %in% ls(.GlobalEnv)) {
    if (all(unique(df2$col[!is.na(df2$col)]) %in% unlist(LEVELS))) {
      # Plot
      # plot_surv_curve(df2, label = paste(label, "(all 3)"), out_dir)

      # First bin first and second quartile
      col_lvls <- df2$col
      df2$col <- bin_vars(col_lvls, LEVELS$i, LEVELS$l) # "intermed" will become "low"
      plot_surv_curve(df2, label = paste(label, "(low+int vs high)"), out_dir)

      # Next bin second and third quartile
      df2$col <- bin_vars(col_lvls, LEVELS$i, LEVELS$h) # "intermed" will become "high"
      plot_surv_curve(df2, label = paste(label, "(low vs int+high)"), out_dir)

      # Now remove int
      col_lvls[col_lvls == LEVELS$i] <- NA # "intermed" will become NA
      df2$col <- col_lvls
      plot_surv_curve(df2, label = paste(label, "(no int)"), out_dir)
    }
  }
}


#' Plot survival curve (Kaplan Meier)
#'
#' @param df1 A data frame with 3 columns: time, status (censoring), col (variable to stratify by)
#' @param label A string label for file names and plot title; could be strata name
#' @param out_dir The output directory where the plot will be saved, default is current working directory.
#' @param save.to.file A logical indicating whether to save to out_dir (T) or print to panel (F)
#' @export
plot_surv_curve <- function(df1, label = "", out_dir = ".", save.to.file = T) {
  # library(survival) # computing survival analyses
  # library(survminer) #  summarizing and visualizing the results of survival analysis
  # library(viridis) # color palette

  # Rename columns
  colnames(df1)[1:3] <- c("time", "status", "col")

  tryCatch(
    {
      # Compute KM survival estimate
      fit <- survfit(Surv(time, status) ~ col, data = df1)
      # Legend labels
      labs <- gsub("col=", "", names(fit$strata)) # "col=high" "col=intermed" "col=low" to "high"     "intermed" "low"

      # Plot
      g <- ggsurvplot(fit,
        # Stats
        pval = T, # of the Log-Rank test comparing the groups
        # pval.size = 4, # font size
        pval.coord = c(50, 0.8), # location on plot
        conf.int = F, # 95% CI
        ## legends and labels
        legend.title = label,
        legend.labs = labs, # change legend labels
        xlab = "Time in months", # customize X axis label
        title = paste0("Kaplan-Meier curve, ", label, ", ", out_dir), # plot title
        # linetype = "col", # Change line type by groups
        surv.median.line = "hv", # Specify median survival

        ## Colors and themes
        palette = viridis(n = length(fit$n)), # colors of lines
        ggtheme = theme_bw(), # theme_cleantable()Change ggplot2 theme

        ## tables:
        cumevents = T, # add cumulative num of events table
        risk.table = "absolute", # "abs_pct", # show the absolute number and the percentage of subjects at risk by time
        # ncensor.plot = TRUE,      # plot the number of censored subjects at time t
        # fontsize = 2.4, # size of tabls text
        # tables.col = T, # Change risk table color by groups instead of all black text
        # tables.height = 0.2,
        tables.y.text = F, # risk.table.y.text = FALSE, cumevents.y.text = FALSE,# show bars instead of names in text annotations
        tables.theme = theme_cleantable()
      )


      if (save.to.file) {
        # Print to file
        filename <- sprintf("%s/%s_survplot.pdf", out_dir, label)
        ggsave(file = filename, print(g), width = 5, height = 7)
      } else {
        # Print to image panel
        print(p)
      }
    },
    error = function(err) {
      print(sprintf("%s", err))
    }
  )
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
