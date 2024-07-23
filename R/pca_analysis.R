
rm(list = ls())

# ---------------

library(data.table)
library(stringr)

# --------------------

df = "input/Final_Data_clean.xlsx" |> readxl::read_xlsx() |> setDT()


# -------------

df$OS = ifelse(df$Death_ == "Yes", 1, 0)
df$OS_time = ifelse(
    df$OS == 1, 
    df$DateDeath - df$AcceptedDateTime, 
    df$LastContact - df$AcceptedDateTime
)

# -------------------------

# df$PNUMBER          = NULL
df$AcceptedDateTime = NULL
df$LastContact      = NULL
df$Death_           = NULL
df$DateDeath        = NULL
df$Progress_        = NULL
df$DateProgress     = NULL


# analysis 1 ------------------


index = c(
    "TAU_score",   "ERCC1_score", 
    "p63_score",   "ECADH_score", 
    "PADH_score",  "pakt473_score", 
    "MTOR_score",  "VEGFR2_score", 
    "VEGFA_score", "VEGFR3_score", 
    "VEGFC_score", "PTEN_score", 
    "COX2_score",  "FASCIN_score", 
    "p53_score",   "EGFR_score", 
    "GSK3B_score", "p4442_score"
)

mm = df[, index, with = FALSE] |> setDF(rownames = as.character(df$PNUMBER))

nas = mm |> is.na() |> rowSums()

index = which(nas == 0) |> names()



pr = prcomp(mm[index, ], scale. = TRUE, center = TRUE)

pr$x = pr$x |> as.data.frame() |> setDT(keep.rownames = "id")

library(ggplot2)

index = match(pr$x$id, df$PNUMBER)

pr$x$OS = df[index]$OS
pr$x$OS_time = df[index]$OS_time


y = pr$x |> melt(id.vars = c("id", "OS", "OS_time"), variable.factor = FALSE, value.factor = FALSE)

y$variable = y$variable |>
    factor(
        levels = y$variable |> unique() |> str_sort(numeric = TRUE)
    )

y |> 
    ggplot(aes(variable, value)) +
    
    geom_boxplot(aes(fill = as.character(OS)))

pr$x |>
    ggplot(aes(PC1, PC2)) +
    geom_point(aes(color = OS))

# -----------------------

index = c(
    "TAU_score",   "ERCC1_score", 
    "p63_score",   "ECADH_score", 
    "PADH_score",  "pakt473_score", 
    "MTOR_score",  "VEGFR2_score", 
    "VEGFA_score", "VEGFR3_score", 
    "VEGFC_score", "PTEN_score", 
    "COX2_score",  "FASCIN_score", 
    "p53_score",   "EGFR_score", 
    "GSK3B_score", "p4442_score"
)

mm = df[, index, with = FALSE] |> setDF(rownames = as.character(df$PNUMBER))

