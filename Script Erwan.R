library(dplyr)
library(forcats)
library(JMbayes2)
library(ggplot2)
library(ggalluvial)


data <- readRDS("C:/Users/erwan/Desktop/M2 Biostat/STA302/STA302/Data/database_ams_a_b_c.rds")

str(data)
data <- data %>% 
  mutate(across(c(anyDC,DIAG0,UMSARS4,SPT1CERE,SPT1DYS,SEXE,CPAP),as.factor)) %>% 
  mutate(SEXE = fct_recode(SEXE, "0" = "1", "1" = "2"))
str(data)

summary(data)

cor(data$SCHRAG_TOT[!is.na(data$SCHRAG_TOT)],data$delaiDecesCensor[!is.na(data$SCHRAG_TOT)], method = c("pearson"))
ams <- data

m1complet <- lme(fixed= SCHRAG_TOT ~ delai_consult_vis0 + CPAP + DUREE_SYMPT_VIS0 + AGE_VISITE0 + 
                   UMSARS1and2_TOT + UMSARS4 + delaiDecesCensor + anyDC + DIAG0 + SPT1CERE + 
                   SPT1DYS + SEXE, random = ~ delai_consult_vis0 | id, data= ams, method='ML', 
                 na.action=na.omit)
summary(m1complet)
p <- (ggplot(ams)
      + geom_line(aes(x = delai_consult_vis0, y = SCHRAG_TOT, group = id), color="grey30", alpha = 0.8)
      + stat_smooth(aes(x = delai_consult_vis0, y = SCHRAG_TOT), method = "loess", linewidth = 0.75)
      + theme_bw()
      + xlab("Temps depuis l'entrée dans l'étude (en dizaines d'années)")
      + ylab("qol")
)
p
m1complet2 <- lme(fixed= SCHRAG_TOT ~ delai_consult_vis0 + I(exp(delai_consult_vis0-2.8)*(delai_consult_vis0>2.8)) + 
                    CPAP + DUREE_SYMPT_VIS0 + AGE_VISITE0 + 
                   UMSARS1and2_TOT + UMSARS4 + delaiDecesCensor + anyDC + DIAG0 + SPT1CERE + 
                   SPT1DYS + SEXE, random = ~ delai_consult_vis0 + 
                    I(exp(delai_consult_vis0-2.8)*(delai_consult_vis0>2.8)) | id, data= ams, method='ML', 
                 na.action=na.omit)
summary(m1complet2)
c(summary(m1complet)$AIC,summary(m1complet2)$AIC)

ams %>% filter(delai_consult_vis0 > 0 & delai_consult_vis0 < 0.2) %>% 
  ggplot(.,
         aes(x = delai_consult_vis0, stratum = CPAP, alluvium = id, y = 1)) +
  scale_fill_brewer(type = "qual", palette = "Set2") + # couleurs
  geom_flow() + # les flux entre les temps
  geom_stratum() + # les "barres" d'effectifs ? chq tps
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 1) + # noms des catégories sur chq barre
  theme(legend.position = "bottom",
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) + # légende en bas
  labs(title = paste("Evolution of dependance status through years",sep=""), # titres du graphe et desy = "Number of patients",
       x = "Time (years)")

ams_sankey <- ams %>% 
  select(id, delai_consult_vis0, SCHRAG_TOT) %>% 
  mutate(tempsD = cut(delai_consult_vis0, 
                      breaks = seq(0, max(delai_consult_vis0, na.rm = TRUE) + 0.25, by = 0.25), 
                      right = TRUE, 
                      include.lowest = TRUE)) %>% 
  mutate(tempsD = if_else(delai_consult_vis0 == 0, 0, 
                          as.numeric(sapply(strsplit(as.character(tempsD), ","), function(x) gsub("[^0-9\\.]", "", x[2]))))) %>% 
  mutate(R = if_else(is.na(SCHRAG_TOT), 1, 0),
         tempsD = as.numeric(tempsD))


df <- data.frame(id = rep(unique(ams_sankey$id), each = length(unique(ams_sankey$tempsD))),
                 time = rep(c(sort(unique(ams_sankey$tempsD))), 
                            length(unique(ams_sankey$id))),
                 score = NA)
