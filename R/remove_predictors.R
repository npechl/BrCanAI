

rm(list = ls())

# ------------------

library(data.table)
library(stringr)

library(caret)

# ---------------------

file_path = "input/Final_Data.xlsx"

# --------------------------

df = file_path |> readxl::read_xlsx(sheet = "Nikos") |> setDT()

tm = data.table(
    "predictor" = df |> colnames(),
    "class"     = df |> lapply(class) |> lapply(paste, collapse = "_") |> unlist(),
    "typeof"    = df |> lapply(typeof) |> lapply(paste, collapse = "_") |> unlist()
)

for(i in seq_along(tm$predictor)) {
    
    flag = df[[i]] |> unique() |> str_detect("[a-z]|[A-Z]|\\>|\\=|\\<|\\/") |> any(na.rm = TRUE)
    
    if(tm[i]$class == "character" & !flag) df[[i]] = df[[i]] |> as.numeric()
    
}


# -------------------

index = tm[which(str_detect(predictor, "_pathogenic"))]

for(i in index$predictor) {
    
    df[[i]] = df[[i]] |> 
        str_equal("na") |> ifelse(NA, df[[i]]) |>
        str_equal("0") |> ifelse("NON-PATH", df[[i]])
    
}

# ----------------------

rm(file_path, flag, i, tm, index)

# ----------------------------


nzv = nearZeroVar(df, saveMetrics = TRUE) |> setDT(keep.rownames = "predictor")

dir.create("output/neaZeroVar", showWarnings = FALSE)

writexl::write_xlsx(nzv, "output/neaZeroVar/neaZeroVar.xlsx")

# -------------------------------

index = nzv[which(!nzv)]$predictor

df_clean = df[, index, with = FALSE]

writexl::write_xlsx(df_clean, "input/Final_Data_clean.xlsx")












