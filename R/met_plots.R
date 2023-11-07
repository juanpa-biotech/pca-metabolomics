
# Scatterplots  for metabolites with the biggest loadings ------------------

# Packages
library(tidyverse)

# Set global theme
source("analysis/global_theme.R")

# 1 Import Data -----------------------------------------------------------
main_data <- read.csv("data/main_data.csv")

# 2 Scatter plot for metabolites with the biggest loadings for PC1 --------
met_plot_pc1 <- main_data %>% 
  filter(MET == "lactate" | MET == "sucrose") %>% 
  group_by(MET, TIME) %>% 
  summarise(MEAN_QT = mean(QT)) %>% 
  ggplot(aes(x = TIME, y = MEAN_QT, color = MET)) +
  geom_line(size = 1) +
  geom_point() +
  xlab("Time (h)") +
  ylab("Relative quantity")


# 3 Scatter plot for metabolites with the biggest loadings for PC2 --------
met_plot_pc2 <- main_data %>% 
  filter(MET == "tryptophan" | MET == "phenylalanine" | MET == "glycerol" |
         MET == "tyrosine" | MET == "inositol" | MET == "lysine" |
         MET == "uric acid") %>% 
  group_by(MET, TIME) %>% 
  summarise(MEAN_QT = mean(QT)) %>% 
  ggplot(aes(x = TIME, y = MEAN_QT, color = MET)) +
  geom_line(size = 1) +
  geom_point() +
  xlab("Time (h)") +
  ylab("Relative quantity")

# 4 Save plots
ggsave(filename = "graphs/met_plot_pc1.jpg", plot = met_plot_pc1)
ggsave(filename = "graphs/met_plot_pc2.jpg", plot = met_plot_pc2)