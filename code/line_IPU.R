# traitement données kohio
library(tidyverse)
library(janitor)

donnees_eaux <- read_csv2("C:/Users/Serigne Mbacké/Downloads/donnees_eaux_kohio.csv")
donnees_sols <- read_csv2("C:/Users/Serigne Mbacké/Downloads/donnees_sols_kohio.csv") %>% 
  clean_names()
ds <- donnees_sols %>% 
  mutate(IPU = free_cyanide_mg_kg / 0.5,
         classes = ifelse(IPU < 1, "Non pollué",
                          ifelse(IPU >= 1 & IPU < 3, "Moderement",
                                 ifelse(IPU>= 3 & IPU < 6, "Considerablement", "Fortement"))))
df <- donnees_eaux %>% 
  mutate(IPU = `Free Cyanide (mg/L)` / 0.07,
         classes = ifelse(IPU < 1, "Non pollué",
                          ifelse(IPU >= 1 & IPU < 3, "Moderement",
                                 ifelse(IPU>= 3 & IPU < 6, "Considerablement", "Fortement")))) %>% 
  select(`ASGM site`, `Sampling point`, IPU, classes)

df$`Sampling point` <- str_replace(df$`Sampling point`, "Ech ", "Ga")
df$`Sampling point` <- str_replace(df$`Sampling point`, "GE", "Gu")
df$`Sampling point` <- str_replace(df$`Sampling point`, " = Puits", "")
df$`Sampling point` <- str_replace(df$`Sampling point`, "Barrage", "Zo9")
df$`Sampling point` <- str_replace(df$`Sampling point`, "Trou d'orpaillage", "Zo8")
df$`Sampling point` <- str_replace(df$`Sampling point`, "F", "Zo")

df_ii <- df %>% filter(!(IPU > 15))

attach(df)
# Créer le diagramme en barres
ggplot(df_ii, aes(y = IPU, color = `ASGM site`)) +
  geom_line( group = 1)+
  geom_point() +
  theme_bw() +
  facet_wrap(~`ASGM site`, nrow = 3,
             scales = "free")

  facet_wrap(`ASGM site`~ classes , ncol = 3, nrow = 3,
             scales = "free") +  # Séparer par classes
  labs(x = "Site ASGM", y = "IPU", title = "Diagramme en barres de l'IPU par site et classe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  df_ii %>% filter(`ASGM site`=="Guido") %>% 
  ggplot(aes(x = seq_along(IPU), y = IPU, color = `ASGM site`)) +
    geom_line(color = "blue") +
    geom_point(color= "black") +
    labs(x = " ", y = "IPU") +
    ggthemes::theme_hc()
