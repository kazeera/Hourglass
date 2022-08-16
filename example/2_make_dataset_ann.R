library(kazutils)
load_packages(c("dplyr"))

# Import reference dataset
dsEX <- readRDS("ds_core_example.rds")

#--------------------------- rowAnn
# 30 patients
# 1-6 samples per patient
pIDs <- sample(2139:4054, size = 30, replace = F)
any(duplicated(pIDs))

# Unique ids for all patients
uIDs <- lapply(pIDs, function(p){
  # generate_unique_ID <- function(n=10)
  n <- sample(1:6,1,T) # number of unique ids - 1-6 samples per patient
  grid <- paste0(sample(LETTERS[1:7],n,T),sample(0:9,n,F)) # LETTERNUMBER eg. C0
  
  # Generate random string
  a <- do.call(paste0, replicate(5, sample(LETTERS,n,T), F))
  a <- paste0(a, sprintf("%04d", sample(9999,n,T)), sample(LETTERS,n,T))
  
  # Return all values pasted together
  paste(grid, p, a, sep = "_") %>% sort
})

# Make info for each unique ID
u = uIDs[[1]]

## RowAnns
# dsEX$rowAnn %>% colnames()
# columns_rowAnn <- c("Patient_ID", "TissueType", "Cancer_Subtype", "Smoker", "Sex", "Sample_Cancer_Subtype", "Patient_Cancer_Subtype", "NeoAdjuvant", "OS_time", "Deceased", "Censoring_status", "Age_at_diagnosis")
# rowAnn1

# Subtypes
sts <- c("A", "B", "C")

rowAnn <- lapply(uIDs, function(u){
  # Make data frame with appropriate columns
  df <- data.frame(
    Unique_ID = u, 
    Patient_ID = get_nth_part(u,"_",2),
    TissueType ="Stroma",
    # Cancer_Subtype = sample(c("A","B","C"),1),
    Smoker = sample(c("Yes", "No", NA),1),
    Sex = sample(c("F", "M"),1),
    Patient_Cancer_Subtype =  sample(sts,1), 
    row.names = u)
  
  # To this?
  df$Sample_Cancer_Subtype <- df$Patient_Cancer_Subtype # more elegant way of doing this?
  
  # Depending on how many samples per patient
  if(length(u) > 2){
    n <- length(u)
    # If n=1, max 0 (ie. n-1); if n=2, max 0 (n-2); if n=3, max 1 (n-2); if n=4, max 1 (n-3);  if n=5, max 2 (n-3);  if n=6, max 2 (n-4);  if n=7, max 3 (n-4);  
    # Find max = n-[minus] depending on odd or even n
    minus <- ifelse(is.integer(n/2), ceiling(n/2)+1, ceiling(n/2))
    max <- n-minus
    # Find number of samples to sub with another subtype
    num_other <- ifelse(length(u) > 3, sample(1:max,1,T), sample(0:max,1,T))
    # Sub in to df
    # e.g. sts[!sts %in% df$Patient_Cancer_Subtype] returns "B","C" if df$Patient_Cancer_Subtype is "A"
    if(num_other != 0){
      df$Sample_Cancer_Subtype[1:num_other] <- sample(sts[!sts %in% df$Patient_Cancer_Subtype],num_other,T) 
    }
  }
  
  # Add other clinical info
  df <- cbind(df,
              NeoAdjuvant = sample(c("neo", rep("non_neo",6)),1),
              Deceased = sample(c("Dead", rep("Alive", 6)),1), 
              Age_at_diagnosis = sample(unique(dsEX$rowAnn$age_at_diagnosis),1))
  df$Status <- ifelse(df$Deceased == "Dead", 0, 1)
  
  # Order OS time
  OS <- unique(dsEX$rowAnn$OS.from.Sx..days.) %>% sort
  length(OS) # 103
  # If subtype A, take from lower OS values, C from higher
  df$OS_time <- ifelse(all(df$Patient_Cancer_Subtype == sts[1]), sample(OS[34:74],1), ifelse(all(df$Patient_Cancer_Subtype == sts[2]), sample(OS[53:80],1), sample(OS[84:103],1)))
  
  # Return this table
  return(df)
})
# Make a random one gender X
rowAnn[[5]]$Sex <- "X"
rowAnn[[5]]$OS_time <- NA

# List of dfs to dataframe
rowAnn <- do.call(rbind.data.frame, rowAnn)
# d <- dsEX$rowAnn[order(dsEX$rowAnn$OS.from.Sx..days.), c("OS.from.Sx..days.", "Deceased") ]
# c("Cancer_Subtype"=c("A","B","C"),  "TissueType" = c("Tumour","Stroma"), Smoker"=c("Yes","No",NA), "Sex" = c("M","F","X"))
rowAnn$Unique_ID ==unlist(uIDs)

#--------------------------- vals
## Parameters from reference dataset
dsEX$colAnn$Parameter %>% unique
parameters <- dsEX$colAnn$Parameter %>% unique %>% .[!grepl("Stroma|Tumor|Num.1|Num.2|Num.3", .)] %>% sort
parameters <- parameters [!parameters %in% c("Stained.Area", "Missing", "Neg.Pixel.Area", "Pos.Pixel.Area")]

# Ranges (min, max) for each parameter
vals_ranges <- lapply(parameters, function(p){
  # p = parameters[1]
  # Columns in ds$vals with this parameter
  if (p == "Area"){
    cols <- dsEX$colAnn[dsEX$colAnn$Parameter == p & dsEX$colAnn$PPC.or.PCD == "PPC",] %>% rownames
  } else {
    cols <- dsEX$colAnn[dsEX$colAnn$Parameter == p,] %>% rownames
  }
  dsEX$vals[,cols] %>% range(na.rm = T)  
})
names(vals_ranges) <- parameters


# #TODO make it patchy (with NAs)
# dsEX$vals[1:100,c("CD4.PCD.Num.Positive", "CD4.PCD.Num.Negative",  "CD4.PCD.Num.Pos.per.mm.2.Total", "CD4.PCD.Negative.Perc")]
# dsEX$vals[1:100,c("CD4.PCD.Area", "CD4.PCD.Num.Positive", "CD4.PPC.Pos.Pix.Perc.Total")]

# See original parameters
parameters
# [1] "Area"  "Num.Detections"   "Num.Neg.Perc"  "Num.Negative" "Num.Pos.per.mm.2.Total" "Num.Positive" "Pos.Pix.Perc.Total"     "Positive.Percent"  
# Num.Detections = Num.Negative + Num.Positive
# Negative.Percent = 1 - Positive.Percent

# Define parameters based on subset of original data
parameters <- c("Area","Num.Detections","Het.Score", "Num.Positive","Num.Negative","Positive.Percent","Negative.Percent","Num.Pos.per.mm.2","Pos.Pixel.Percent")

## Stains
stains <- list(TCell = c("CD3", "CD8"), 
               BCell = c("CD20", "CD27", "CD5", "PDL1"),
               other = c("IL6", "SMA"))

## Make columns
columns_vals <- apply(expand.grid(unlist(stains), parameters), 1, paste, collapse="_") %>% sort

# length
nrows <- nrow(rowAnn)

# Fill in non-dependent ones first
vals <- lapply(columns_vals, function(o){
  # parameter (p)
  p <- get_nth_part(o, "_", 2)
  
  if(p == "Area" | p == "Num.Negative"){
    # Range of values for reference
    r <- vals_ranges[[p]]
    # Return enough values for samples in rowAnn
    sample(r[1]:r[2], nrows, T) %>% 
      round
  } else { 
    rep(0, nrows)
    # NULL 
  }
})
names(vals) <- columns_vals

# Main difference is between TCell and BCell subtypes
# IL6 different between A,B,C subtypes only seen in females (Sex=="F")
# Smokers of all subtypes make it higher

# Fill in values Num.Positive, it be driving the trends as follows
for(o in names(vals)[grepl("Num.Positive", names(vals))]){
  # Stain (s) and parameter (p)
  s <- get_nth_part(o, "_", 1)
  p <- get_nth_part(o, "_", 2)
  
  # Length of vector to choose from
  l <- nrows*1.2
  # "Num.Pos.per.mm.2.Total", "Pos.Pix.Perc.Total"
  r <- vals_ranges[[p]] + 1
  sample(r[1]:r[2], nrows*1.2, T) %>% sort
  
  # Fill in values for TCell stains
  # Trend: subtype (sts) "A": low, "B": med, "C": high
  # Values are range low - mid
  # Smokers for all subtypes are unusually high
  if(s %in% stains[[1]]){ 
    x <- round(r[1]:r[2]/2); l <- length(x)
    # y is current subtype, sts is vector of subtype names, x is vector of values to choose from, l is length of x
    vals[[o]] <- as.numeric(unlist(lapply(rowAnn$Sample_Cancer_Subtype, function(y){ ifelse(y == sts[1], sample(x[1:(l/4)],1), ifelse(y == sts[2], sample(x[(l/3):(l/2)],1), sample(x[(l/1.5):(l/1.14)],1)))})))
    
    # Add in higher values for smokers
    smokers <- which(rowAnn$Patient_Cancer_Subtype %in% sts & rowAnn$Smoker == "Yes")
    if(length(smokers) > 0){
      vals[[o]][smokers] <- sample(x[(l/1.1):l], length(smokers))
    }
  }
  
  # Reverse trend in BCells
  # Trend: sts "A": med, "B": high, "C": low
  # Values are range mid - high
  if(s %in% stains[[2]]){
    x <- round(r[2]/1.8):r[2]; l <- length(x)
    vals[[o]] <- as.numeric(unlist(lapply(rowAnn$Sample_Cancer_Subtype, function(y){ ifelse(y == sts[3], sample(x[1:(l/4)],1), ifelse(y == sts[1], sample(x[(l/3):(l/2)],1), sample(x[(l/1.5):(l/1.1)],1)))})))
    
    # Add low values for neo cases
    vals[[o]][rowAnn$NeoAdjuvant == "neo"] <- sample(r[1]:(r[2]/3),1)
  }
  
  # No particular trend
  if(s %in% stains$other){
    x <- r[1]:r[2]
    vals[[o]] <- sample(x, nrows, T)
  }
  
  # Difference in IL6 across subtypes only seen in males
  if(s == "IL6"){
    x <- r[1]:r[2]
    vals[[o]] <- sample(x, nrows, T)
    # For male, high in subtype 3
    vals[[o]][which(rowAnn$Sex == "M")] <- as.numeric(unlist(lapply(rowAnn$Patient_Cancer_Subtype, function(y){ ifelse(y == sts[3], sample(x[1:(l/4)],1), ifelse(y == sts[2], sample(x[(l/3):(l/2)],1), sample(x[(l/1.5):(l/1.1)],1)))})))[which(rowAnn$Sex == "M")]
  }
  
  # Random outliers *high
  num_outliers <- 4
  vals[[o]][sample(1:nrows, num_outliers, F)] <- sample(r[2]:(r[2]*1.5),num_outliers)
  
  # Random NAs
  vals[[o]][sample(1:nrows, num_outliers*2, F)] <- NA
}

# Fill in values Het.Scores 
for(o in names(vals)[grepl("Het.Score", names(vals))]){
  # Stain (s) and parameter (p)
  s <- get_nth_part(o, "_", 1)
  p <- get_nth_part(o, "_", 2)
  
  # Fill in values for TCell stains
  # Trend: subtype (sts) "A": low, "B": med, "C": high
  if(s %in% stains[[1]]){ 
    x <- round(r[1]:r[2]/2); l <- length(x)
    # y is current subtype, sts is vector of subtype names, x is vector of values to choose from, l is length of x
    vals[[o]] <- lapply(rowAnn$Sample_Cancer_Subtype, function(y){ ifelse(y == sts[1], sample(1:5,1), ifelse(y == sts[2],  sample(4:8,1),  sample(6:10,1)))}) %>%
        unlist %>% 
        as.numeric
  } else {
    vals[[o]] <- sample(1:10, nrows, T)
  }
}

# Num.Detections = Num.Positive + Num.Negative
for(o in names(vals)[grepl("Num.Detections", names(vals))]){
  # Stain (s) and parameter (p)
  s <- get_nth_part(o, "_", 1)
  p <- get_nth_part(o, "_", 2)
  
  vals[[o]] <- vals[[paste0(s,"_Num.Negative")]] + vals[[paste0(s,"_Num.Positive")]]
}

# Fill in number of negative pixels and positive pixel percent
for(o in names(vals)[grepl("Positive.Percent|Num.Pos.per.mm.2|Pos.Pixel.Percent", names(vals))]){
  # Stain (s) and parameter (p)
  s <- get_nth_part(o, "_", 1)
  p <- get_nth_part(o, "_", 2)
  
  
  # Number of positive pixels / num detections (total)
  if (p == "Positive.Percent"){
    vals[[o]] <- vals[[paste0(s,"_Num.Positive")]] * 100 / vals[[paste0(s,"_Num.Detections")]] %>%
      round(2)
  }
  
  # "Num.Pos.per.mm.2.Total" = round("Num.Positive"*10^4/"Area" ,3)
  if (p == "Num.Pos.per.mm.2"){
    vals[[o]] <-  vals[[paste0(s,"_Num.Positive")]]*10^4/vals[[paste0(s,"_Area")]]%>%
      round(3)
  }
  
  # "Pos.Pixel.Percent" = "Num.Positive"/10^3
  if (p == "Pos.Pixel.Percent"){
    r <- vals_ranges$Pos.Pix.Perc.Total
    vals[[o]] <- vals[[paste0(s,"_Num.Positive")]] / 1000 * sample(seq(from=0.3, to=1.5, by=0.1), 1) %>%
      round(2)
  }
}

# Fill in negative percent 
for(o in names(vals)[grepl("Negative.Percent", names(vals))]){
  # Stain (s) and parameter (p)
  s <- get_nth_part(o, "_", 1)
  p <- get_nth_part(o, "_", 2)
  
  vals[[o]] <- round(100 - vals[[paste0(s,"_Positive.Percent")]], 2) %>%
    round(2) 
  # ALT: vals[[paste0(s,"Num.Negative")]] * 100 / vals[[paste0(s,"_Num.Detections")]]
}

# Finally, 
vals <- do.call(cbind.data.frame, vals)
rownames(vals) <- rownames(rowAnn)

#--------------------------- colAnn
colAnn <- data.frame(Feature=get_nth_part(colnames(vals),"_",1),
                     Parameter=get_nth_part(colnames(vals),"_",2),
                     isNumeric = T,
                     row.names = colnames(vals))
colAnn$Parameter_of_interest = ifelse(colAnn$Parameter %in% c("Num.Positive","Num.Negative","Area","Num.Detections"), F, T)


# Switch sexes because surv plot showing opposite trend
rowAnn$Sex <- plyr::mapvalues(rowAnn$Sex, from=c("F","M", "X"), to=c("M","F", "X"))
#--------------------------- Save
# Dataset object
ds <- list(rowAnn = rowAnn,
           vals = vals,
           colAnn = colAnn)
lapply(ds, dim)

# Create a folder
out <- kazutils::create_folder("ExampleData1")

example_IHC_samples_dataset <- ds
# Save for data objects for package
save(example_IHC_samples_dataset, file = sprintf("%s/example_IHC_samples_dataset.RData", out))

# Save to file
for(ft in names(ds)){
  # save appropriate filetype (ft)
  write.csv(ds[[ft]], file = sprintf("%s/%s_Example_IHC_sample.csv",out,ft))
}

