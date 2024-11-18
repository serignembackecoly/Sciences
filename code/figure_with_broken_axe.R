# Chart with a broken x-axis

# Library
library(tidyverse)

set.seed(19760620)
# factis data
my_data <- tibble(condition = c(rep("technical", 108 * 7),
                                rep("random", 108 * 7),
                                rep("biological", 18 * 6)),
                  week = c(rep(c(0:4,8:9),each = 108),
                           rep(c(0:4,8:9),each = 108),
                           rep(c(1:4,8:9),each = 18)),
                  spearman = c(
                    #technical
                    runif(108, 0.50, 0.90), runif(108, 0.85, 0.95), runif(108, 0.90, 0.97),
                    runif(108, 0.50, 0.90), runif(108, 0.85, 0.95), runif(108, 0.90, 0.97),
                    runif(108, 0.98, 1.00),
                    
                    #random
                    runif(540,-0.4,0.4), runif(216, -0.6, 0.6),
                    
                    #biological
                    runif(18, 0.20, 0.65), runif(18, 0.55, 0.85), runif(18, 0.58, 0.88),
                    runif(18, 0.58, 0.88), runif(18, 0.50, 01.00), runif(18, 0.25, 0.75))
                  )
my_data %>% 
  bind_rows(tibble(week = 5, 
                   condition = c("biological", "technical", "random"),
                   spearman = -3)) %>% 
  mutate(condition = factor(condition,
                            levels = c("biological", "technical", "random")),
         week = as.character(week)) %>% 
  ggplot(aes(x = week, y =  spearman, color = condition, fill = condition)) +
  stat_summary(geom = "errorbar", fun.data = median_hilow, 
               position = position_dodge(width = 0.5),
               width = 0.4, show.legend = F) +
  stat_summary(geom = "point" ,fun.data = median_hilow,
               shape = 21, color = "black",
               position = position_dodge(width = 0.5)) +
  annotate("segment", x = c(5.75, 6.40), xend = c(5.60, 6.25),
           y = c(-0.55,-0.55), yend = c(-0.65, -0.65),
           color = "black", linewidth = 0.5) +
  annotate("segment", x = c(0.7,6.33), xend = c(5.67,9), y = -0.6, yend = -0.6,
           color = "black", linewidth = 0.5) +
  annotate("segment", x = c(1:5, 7:8), xend = c(1:5, 7,8),
           y = -0.6, yend = -0.65, color = "black") +
  scale_color_manual(breaks = c("technical", "random", "biological"),
                     values = c("darkgrey", "black", "blue"),
                     labels = c("technical variability", "random permutation",
                                "biological variability")) +
  scale_fill_manual(breaks = c("technical", "random", "biological"),
                     values = c("darkgrey", "darkred", "blue"),
                     labels = c("technical variability", "random permutation",
                                "biological variability")) +
  scale_x_discrete(breaks = c(0:5,8:9),
                   labels = c(0:4, "", 8:9)) +
  labs(color = NULL, fill = NULL, y = "Spearman R", x = "time (weeks)") +
  coord_cartesian(ylim = c(-0.6, 1), expand = F, clip = "off") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.key.size = unit(3, "pt"),
        legend.key.spacing.x = unit(12, "pt"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(7, "pt"),
        axis.text.x = element_text(margin = margin(t = 7)))
# Save the plot
ggsave(filename = "figures/plot_with_broken_x_axis.png",
       width = 6, height = 4)
