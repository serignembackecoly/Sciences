# Code for a pie chart

# Libraries
library(tidyverse)
library(shadowtext)

# Load the data

data <- read_csv2("data/Type_of_publication.csv") %>% 
  pivot_longer(-(`Type of publication`),
               names_to = "group",
               values_to = "value") %>% 
  select(-(`Type of publication`))

data$group <- gsub("Scientific article", "Scientific \n article", data$group)

# Compute the position of labels
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = c(1.5, 32, 88, 95.5, 98.4),
         xpos = c(1.2, 1.1, 1.1, 1.05, 1.25),
         angle = c(265, 333, 315, 290, 275),
         group = paste0(group, " [", value, "]"))


# Basic piechart
ggplot(data, aes(x = "", y = prop, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_shadowtext(aes(y = ypos, x = xpos, label = group),
                  color = "white", size=4,
                  angle = data$angle,
                  fontface = "bold") + 
  scale_fill_brewer(palette="Set1") +
  #scale_fill_manual(values = c("gold", "navy", "grey", "red", "violet")) +
  geom_point(aes(x = 0, y = 0),
             size = 55, color = "white")

ggsave("figures/pie_type_publication.png",
       height = 5, width = 5, dpi = 600)

##### Second plot ####

# Load the data

data <- read_csv2("data/Publication_period.csv") %>% 
  pivot_longer(-(`Publication period`),
               names_to = "group",
               values_to = "value") %>% 
  select(-(`Publication period`))

# Compute the position of labels
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = c(20, 55, 80, 92.5, 96.5, 99.2),
         angle = c(288, 335, 70, 25, 14,  0),
         group = paste0(group, " [", value, "]"))

# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=55) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_shadowtext(aes(y = ypos, x = 1.1, label = group), 
                  color = "white", size=4, 
                  angle = data$angle,
                  fontface = "bold") + 
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  #scale_fill_manual(values = c("gold", "navy", "grey", "red", "violet", "cyan")) +
  geom_point(aes(x = 0, y = 0),
             size = 55, color = "white")

ggsave("figures/pie_number_publication.png",
       height = 6, width = 6, dpi = 600)

#### SMC ####