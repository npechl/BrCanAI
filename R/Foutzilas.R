library(lubridate)

## PFS
df$Progress = ifelse(df$Progress == 'YES' | df$Death_ == 1, 1, 0)

df$ProgDate = apply(df[, c('ProgDate', 'DeathDate'], 1, min, na.rm = T)

df$ProgressionDate = time_length(difftime(
    
    df$LastContact,
    df$HistologyDate
    
), unit = 'month')

df$ProgressionDate[which(df$Progress == 1)] = time_length(difftime(
    
    df$ProgDate,
    df$HistologyDate
    
), unit = 'month')[which(df$Progress == 1)]

