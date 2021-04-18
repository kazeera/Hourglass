rowAnn1 <- TMA.STROMAL.SUBTYPE
# Get palette
d <- get_rowAnn_color_pal(ds, rowAnn1)
ds <- d$ds
color_pal <- d$pal

load("debug_20210413.RData")
# Call required libraries
load_packages(c("ggplot2", "ggpubr", "ggsignif", "reshape2", "scales", "RColorBrewer",
                "dplyr", "gtools")) #combinations)

# error1: trim_x on boxplot not showing
df <- data.frame(rowAnn1 = ds.imp$rowAnn[,rowAnn1], ds.imp$vals[,c(1:4,35:56)])
colnames(df) <- c(rowAnn1, sample(LETTERS, length(c(1:4,35:56)), replace = F))

# Remove NAs
df = df[!is.na(df[,1]) & df[,1] %in% PANC_TISS_ORDER,]

# error1: trim_x on boxplot not showing - fixed
plot_indiv_boxplot(df, rowAnns = c(rowAnn1, NA), save.to.file = F)

# error2: pval_heatmap -- ask Barbara
# TODO start here # open FC.pval.R
pval_df = make_FC.pval_df(df)
p <- make_FC.pval_plot(pval_df, save.to.file = F, scale_FC = "none")

# Stimulate outliers
pval_df2 <- pval_df

# Only outliers above scale
pval_df2$Fold.change[5:7] <- 2
make_FC.pval_plot(pval_df2, save.to.file = F, scale_FC = "none") # unscaled
make_FC.pval_plot(pval_df2, save.to.file = F, scale_FC = "threshold") # outliers have been given upper limit

# Add outliers below scale
pval_df2$Fold.change[8:10] <- 0.001
outliers <- get_outliers(pval_df2$Fold.change)

make_FC.pval_plot(pval_df2, save.to.file = F, scale_FC = "none") # unscaled
p <- make_FC.pval_plot(pval_df2, save.to.file = F, scale_FC = "cap_outliers") # outliers have been given upper limit

# rename "threshold" to "outliers"?
 p +
  theme(legend.position = "bottom", legend.box = "vertical")

# error3: paired
# Subset data frame to only column of interest
p_df <- data.frame(
  case = ds$rowAnn[, CASE_ID],
  box = ds$rowAnn[, rowAnn1],
  value = ds$vals[, 4], stringsAsFactors = F
)

p_df <- get_duplicated_cases(p_df, col = "case", rm.NA = "value")

# In case there are a couple of cases or zero left, do not continue
if (nrow(p_df) < 2) next
# Create plot
a <- plot_indiv_paired(p_df, color_pal = color_pal,
                  xlab = rowAnn1, rowAnns = rowAnn1, save.to.file = F
)

# Make list of unique elements
e <- unique(as.character(p_df$box))
# Make list of combinations (order doesn't matter) for p-values
comb <- combinations(n = length(e), r = 2, v = e, repeats.allowed = F) %>% # gtools package
  split(., seq(nrow(.)))
# Add stats to plot using ggpubr
## PAIRED - does not work
tryCatch({
 a + stat_compare_means(paired = T, method = "wilcox", comparisons = comb, na.rm = T, label = "p.format", size = 12 / 5, bracket.size = 1)
})
## UNPAIRED - works
a + stat_compare_means(paired = F, method = "wilcox",comparisons = comb, na.rm=T, label="p.format", size = 12/5, bracket.size = 1)

plot_indiv_paired(p_df, color_pal = color_pal,
                  xlab = rowAnn1, rowAnns = rowAnn1, save.to.file = F
)

# df = p_df
# pval.test = "wilcox.test"
# pval.label = "p.signif"
# font_size=14
# ** 20201124 and before - not paired result

