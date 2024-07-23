
rm(list = ls())

# load libraries ---------------

library(data.table)
library(stringr)

library(randomForestSRC)

# read input dataset --------------------

df = "input/Final_Data_clean.xlsx" |> readxl::read_xlsx() |> setDT()


# create survival time -------------

df$OS = ifelse(df$Progress_ == "Yes", 1, 0)
df$OS_time = ifelse(
    df$OS == 1, 
    df$DateProgress - df$AcceptedDateTime, 
    df$LastContact - df$AcceptedDateTime
)

# clean unnecessary columns -------------------------

df$PNUMBER          = NULL
df$AcceptedDateTime = NULL
df$LastContact      = NULL
df$Death_           = NULL
df$DateDeath        = NULL
df$Progress_        = NULL
df$DateProgress     = NULL

# create factors ----------------------


for( i in seq_len(ncol(df)) ) {
    
    flag = df[[i]] |> class() == "character"
    
    if(flag) df[[i]] = df[[i]] |> as.factor()
    
}

# define survival function --------------------------------



compute_rfsrc <- function(x, ntree = 500, nodesize = 5, nsplit = 50) {
    
    obj <- rfsrc(
        Surv(OS_time, OS) ~ ., x,
        ntree      = ntree, 
        nodesize   = nodesize, 
        nsplit     = nsplit, 
        importance = TRUE
    )
    
    imp = data.table(
        "Predictor"  = names(obj$importance),
        "Importance" = obj$importance
    )
    
    imp = imp[order(-Importance)]
    
    imp$`Relative Imp` = imp$Importance / max(imp$Importance)
    
    out = list(
        "obj"        = obj,
        "c_index"    = get.cindex(obj$yvar$OS_time, obj$yvar$OS, obj$predicted.oob),
        "importance" = imp
    )
    
    return(out)
    
}


# run experiments --------------------------------------

## experiment 1 ------------------------

# set.seed(10)

r1 = compute_rfsrc(df, ntree = 10000, nodesize = 3, nsplit = 0)

r1$obj |> print()
r1$obj |> plot(cex = .5)
## experiment 2 ----------------------------------

index = c(
    "GroupTreatm", "AgeAccepted", "PS", "EBERPROBE_neg_pos",
    "Smoking", "Sex", "T", "N", "CyclinD1", "ki67",
    "Mutation status", "Stability status", "OS", "OS_time"
)

r2 = compute_rfsrc(df[, index, with = FALSE], ntree = 10000, nodesize = 3, nsplit = 0)

r2$obj |> plot(cex = .85)

## experiment 3 --------------------------------------

index = c(
    "TAU_score",   "ERCC1_score", 
    "p63_score",   "ECADH_score", 
    "PADH_score",  "pakt473_score", 
    "MTOR_score",  "VEGFR2_score", 
    "VEGFA_score", "VEGFR3_score", 
    "VEGFC_score", "PTEN_score", 
    "COX2_score",  "FASCIN_score", 
    "p53_score",   "EGFR_score", 
    "GSK3B_score", "p4442_score",
    "OS", "OS_time"
)

r3 = compute_rfsrc(df[, index, with = FALSE], ntree = 500, nodesize = 10, nsplit = 0)

## experiment 4 --------------------------------------

index = c(
    "ABL1",   "ARID1A", "ATG13", "AXIN1",  "BAP1", 
    "BRCA1",  "BRCA2",  "BRD4",  "CD274",  "CD8A",             
    "CD8B",   "CHD6",   "CHEK2", "CTLA4",  "EGFR",             
    "EP300",  "ERBB2",  "ERBB3", "FGFR2",  "JAK1",             
    "JAK2",   "KIT",    "KMT2C", "KMT2D",  "KRAS",             
    "MAP2K2", "MLH1",   "MSH2",  "MSH6",   "NIN",              
    "NOTCH3", "PALB2",  "PDCD1", "PIK3CA", "POLE",             
    "PTCH1",  "RANBP2", "SETD2", "SRCAP",  "SYNE1",            
    "TET1",   "TET2",   "TP53",  "TSC1",   "TSHZ3",            
    "ZNF521", "OS",     "OS_time"
)

r4 = compute_rfsrc(df[, index, with = FALSE], ntree = 500, nodesize = 10, nsplit = 0)


## experiment 5 --------------------------------------

index = c(
    "BRCA1_pathogenic",  "BRCA2_pathogenic",  "EGFR_pathogenic",   "KRAS_pathogenic",  
    "PIK3CA_pathogenic", "POLE_pathogenic",   "SYNE1_pathogenic",  "TP53_pathogenic",
    "OS",     "OS_time"
)

r5 = compute_rfsrc(df[, index, with = FALSE], ntree = 500, nodesize = 10, nsplit = 0)
