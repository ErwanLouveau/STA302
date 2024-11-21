library(dplyr)
library(forcats)

data <- readRDS("C:/Users/erwan/Desktop/M2 Biostat/STA302/STA302/Data/database_ams_a_b_c.rds")

str(data)
data <- data %>% 
  mutate(across(c(anyDC,DIAG0,UMSARS4,SPT1CERE,SPT1DYS,SEXE,CPAP),as.factor)) %>% 
  mutate(SEXE = fct_recode(SEXE, "0" = "1", "1" = "2"))
str(data)

summary(data)

cor(data$SCHRAG_TOT[!is.na(data$SCHRAG_TOT)],data$delaiDecesCensor[!is.na(data$SCHRAG_TOT)], method = c("pearson"))
ams <- data
