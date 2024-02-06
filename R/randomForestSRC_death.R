
rm(list = ls())
gc()

# load libraries -----------------

library(data.table)
library(stringr)

library(randomForestSRC)
library(caret)

# load data ----------------

df_r = "input/Final_Data_Merged_processed.xlsx" |>
    readxl::read_xlsx() |>
    setDT()


# filtering ---------------------------------

index = df_r |>
    colnames() |>
    str_subset("pathogenic")

index = df_r[, index, with = FALSE] |>
    lapply(function(q) q == "PATH") |>
    setDT() |>
    rowSums(na.rm = TRUE)


df_r$NGS_Result = ifelse(index != 0, "PATH", df_r$NGS_Result) |>
    str_replace_all("MUT", "NON-PATH")


index = df_r |>
    colnames() |>
    str_subset("pathogenic")


df_r = df_r[, -index, with = FALSE]


# correct columns ---------------------------

colnames(df_r) = df_r |>
    colnames() |>
    janitor::make_clean_names()

df_r$scd8_f = df_r$scd8_f |> as.numeric()
df_r$icd8_f = df_r$icd8_f |> as.numeric()
df_r$total_cd8_f = df_r$total_cd8_f |> as.numeric()

df_r$p53_ihc_percent = df_r$p53_ihc_percent |> as.numeric()
df_r$ki67_percent = df_r$ki67_percent |> as.numeric()
df_r$tils3_class = df_r$tils3_class |> as.numeric()

df_r$pnumber = NULL
df_r$progress = NULL
df_r$progression_time = NULL


# remove not available records --------------------------------

df_r = df_r[which(!is.na(death) & !is.na(survival_time))]

# impute NA values with `Not available` ------------------------

df_rf = df_r |>
    lapply(function(q) {

        if(is.character(q)) {

            out = ifelse(q |> is.na() , "Not Available", q)
        } else {

            out = ifelse(q |> is.na() , 0, q)

        }

        return(out)

    }) |>
    lapply(function(q) {
        
        if(is.character(q)) {
            
            out = as.factor(q)
            
        } else {
            
            out = q
        }
        
        return(out)
    }) |>
    setDT()

# Zero- and Near Zero-Variance Predictors ------------------------

nzv = df_rf |> nearZeroVar(saveMetrics = TRUE)

index = row.names(nzv)[which(!nzv$nzv)]

df_filtered = df_rf[, index, with = FALSE]

# build model --------------------

obj <- rfsrc(
    Surv(survival_time, death) ~ ., df_filtered,
    ntree = 500, 
    nodesize = 5, 
    nsplit = 50, 
    importance = TRUE
)

# get summary -------------------------

obj

pdf(file = "output/Death predictors/Rplot.pdf", width = 12, height = 6)

plot(obj, cex = .75)

dev.off()

# get C-index --------------------------

get.cindex(obj$yvar$survival_time, obj$yvar$death, obj$predicted.oob)

# Variable Importance ------------------

out = data.table(
    "Variable" = names(obj$importance),
    "Importance" = obj$importance
)

out = out[order(-Importance)]

out$`Relative Imp` = out$Importance / max(out$Importance)

writexl::write_xlsx(out, "output/Death predictors/variable-importance.xlsx")

# jk.obj <- subsample(obj)
# 
# 
# print(jk.obj)

# # Find Interactions Between Pairs of Variables -----------------------
# 
# mm = find.interaction(obj)
# 
# library(ComplexHeatmap)
# 
# Heatmap(
#     mm, 
#     row_names_gp = gpar(fontsize = 4),
#     column_names_gp = gpar(fontsize = 4)
# )










