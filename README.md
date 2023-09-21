# Hourglass (R Package)
[![DOI](https://zenodo.org/badge/397323448.svg)](https://zenodo.org/badge/latestdoi/397323448)

<!-- badges: start -->
  <!-- badges: end -->
  Available as an R package, Hourglass is a computational toolkit that streamlines processing and visualization of massive multiparametric datasets.

<!-- badges: start -->
  <!-- badges: end -->
  
   We constructed and compiled the relevant individual data filtering functions (such as subset retrieval, imputation and outlier removal) and plotting functions (such as boxplot, heatmap, correlation matrix, survival plot visualizations). 
   
   Hourglass serves as both a screening and exploratory tool to query multiparametric datasets and create publication-ready plots from multiple parallel comparisons of biological groups under identical and fully reproducible analytical conditions.   

## Installation
You can install the latest version from Github:
``` r
devtools::install_github("kazeera/Hourglass")
```

## How to Use 
Load package.
``` r
library(Hourglass)
```

There are 4 components:
  1) comparisons and user options
2) color palette for subgroups
3) feature sets and respective parameters
4) optional - dataset object

### Option 1: Run from Hourglass parameters Excel file.
Download  link for template.
``` r
run_from_excel("path_to_file/file.xlsx")
```


### Option 2: Run from individual components. 
``` r
run_Hourglass(comparisons, var_colors, feat_sets, datasets)
```

### Example of features and functionality.
#### Data preparation
Define dataset object - list with 3 tables: 
1) "vals" (sample by feature/parameter matrix)
2) "rowAnn" (row annotations = sample/patient information)
3) "colAnn" (column annotations = feature/parameter information)

Load in example dataset object.
``` r
# Retrieve built in dataset
ds <- example_IHC_samples_dataset
# Name your dataset
ds$name <- "BySample"
```
Note: use make_dataset_ob() to create your own.


Remove outliers.
``` r
ds$vals <- remove_outliers_df(ds$vals)
```

Optional: Create new imputed dataset to run in parallel. 
``` r
ds.imp <- impute_ds(ds)
```

Add IL6 levels to rowAnn. Leverage quantified resultsto create stain-based annotation column.
``` r
# Get levels from "IL6_Num.Positive"
result <- add_to_rowAnn(ds, col_name = "IL6_Num.Positive")
# Add new rowAnn with new column to ds 
ds$rowAnn <- result$rowAnn
result$rowAnn1 # [1] "IL6_Num.Positive"
unique(ds$rowAnn[,"IL6_Num.Positive"]) # [1] "intermed" NA         "high"     "low"     
```

#### Create color palette list for variables/subgroups.
``` r
var_colors <-list('Sample_Cancer_Subtype-A'="#9a8aabff",'Sample_Cancer_Subtype-B'="#9446e8ff", 'Sample_Cancer_Subtype-C'="#3b3374ff",
                  'Sex-F'="#bff2A7ff",'Sex-M'="#f64d47ff",
                  'Smoker-Yes'="#d6c938ff",'Smoker-No'="#3e8a5bff",
                  'Custom-low'="#18528cff",'Custom-intermediate'="#ebb7b7ff",'Custom-high'="#911919ff")
var_colors <- clean_colors(color_palette)
# Show color palette using scales package
scales::show_col(unlist(var_colors))
```
Note: use show_col() from scales package to show palette (vector of color HEX codes/names)

#### Use individual plotting functions.

##### Boxplot - Comparing cancer subtypes.
Make a single boxplot. Notice how the differences are siginificant.
``` r
library(gtools)
library(ggplot2)
# Prepare 2 column data frame
df3 <- data.frame(box = ds$rowAnn$Sample_Cancer_Subtype,
                  value = ds$vals$CD20_Pos.Pixel.Percent)
# Boxplot 2
plot_indiv_boxplot(df3, labels = "CD20 in Cancer Subtypes", ylab =  "CD20_Pos.Pixel.Percent", x = "", lvl.colors = c(A="#9a8aabff",B="#9446e8ff", C="#3b3374ff"), save.to.file = F)
```


Make another boxplot for IL6 which shows no significance.
``` r
# Prepare 2 column data frame
df3 <- data.frame(box = ds$rowAnn$Sample_Cancer_Subtype,
                  value = ds$vals$IL6_Num.Positive)
# Boxplot 1
plot_indiv_boxplot(df3, labels = "IL6 across Cancer Subtypes", ylab = "IL6_Num.Positive", lvl.colors = c(A="#9a8aabff", B="#9446e8ff", C="#3b3374ff"), save.to.file = F)

```

Try again but only look at female patients. The trend is now visible and significant now (p < 0.05).
``` r
library(gtools)
library(ggplot2)

# Subset dataset object
ds_F <- subset_dataset(ds, rows_to_keep = ds$rowAnn$Sex == "F")
# Prepare 2 column data frame
df3 <- data.frame(box = ds_F$rowAnn$Sample_Cancer_Subtype,
                  value = ds_F$vals$IL6_Num.Positive)
# Boxplot 1
plot_indiv_boxplot(df3, labels = "IL6 in Cancer Subtypes in female", ylab = "IL6_Num.Positive", lvl.colors = c(A="#9a8aabff",B="#9446e8ff", C="#3b3374ff"), save.to.file = F)

```

Note: You may add a second subgroup annotation.
``` r
# Make a third column
df3$dots <- ds_F$rowAnn$Smoker
# Boxplot 3 - add more colors to color palette "lvl.colors"
plot_indiv_boxplot(df3, labels = "Comparing IL6 in Sample Cancer Subtypes", lvl.colors = c(A="#9a8aabff",B="#9446e8ff", C="#3b3374ff", 'Yes'="#d6c938ff",'No'="#3e8a5bff"), save.to.file = F)
```

##### Feature sets - correlation plots and heatmap

##### Survival plot
Make survival plot to see differences between sexes.
``` r
library(survival)
# Make table with 3 columns
df1 <- data.frame(time = ds$rowAnn$OS_time,
                  status = ds$rowAnn$Status, 
                  col = ds$rowAnn$Sex)
# Plot survival curve
plot_surv_curve(df1, label = "Sex", out_dir = ".")
```
