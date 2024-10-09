# Chargement des bibliothèques
library(tidyverse)
library(forcats)
library(echarts4r)
library(magrittr)
library(webshot2)
library(htmlwidgets)

# Données
regions <- c("Dakar", "Thiès", "Diourbel", "Kaolack",
             "Saint-Louis", "Louga", "Tambacounda",
             "Kolda", "Fatick", "Matam", "Kaffrine",
             "Ziguinchor","Sédhiou", "Kédougou")
pourcentages <- c(22.1, 13.6, 11.5, 7.4, 6.6, 6.2,
                  5.4, 5.0, 5.0, 4.6, 4.5, 3.4, 3.3, 1.4)

# Créer un dataframe
data <- data.frame(region = regions, pourcentage = pourcentages)

# Ordonner par ordre décroissant
data <- data %>%
  mutate(region = fct_reorder(region, pourcentage, .desc = TRUE))

# Créer le graphique
pie_sen <- data %>% 
  e_charts(region) %>% 
  e_pie(pourcentage, legend = FALSE,
        label = list(
          formatter = ("{b} ({d}%)"),
          fontSize = 32,
          fontStyle = 'bold'
        )) 
# Sauvegarder le graphique
saveWidget(widget = pie_sen, file = "figures/pie_chart.html")
Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
webshot("figures/pie_chart.html",
        file = "figures/pie_chart.png",
        delay = 0.5, vwidth = 2048, vheight = 1152)
