# Plots ----------------------------------------------------------------------

# Packages 
if (!"tidyverse" %in% .packages()) library(tidyverse)

# Set global theme
source("analysis/global_theme.R")

# 1 Import data --------------------------------------------------------------
per_var_pca <- read.csv("data/per_var_pca.csv")
pca_data <- read.csv("data/pca_data.csv")
loadings_data <- read.csv("data/loadings_data.csv")

# 2 Scree Plot ---------------------------------------------------------------
bar_pca <- per_var_pca[1:6,] %>% 
  ggplot(aes(x = as.factor(PC), y = PER_VAR)) +
  geom_col() + 
  xlab("PC") +
  ylab("% of total variance") 
  

# 3 Score plot PC1 and PC2 ----------------------------------------------------
pca_plot <- pca_data %>% 
  mutate(TIME = as_factor(TIME)) %>% 
  ggplot(aes(x = PC1, y = PC2, color = TIME)) +
  geom_point(size = 3) +
  xlab(paste0("PC1-", per_var_pca$PER_VAR[1], "%")) +
  ylab(paste0("PC2-", per_var_pca$PER_VAR[2], "%")) +
  scale_color_brewer(palette = "Dark2")

# 4 Loadings Plot -------------------------------------------------------------

# 4.1 Function to assign compound name if absolute value of loading is bigger
#     than set threshold
load_tr <- function(loadings, threshold, compound_name) {
  compound_name[!abs(loadings) > threshold] <- " "
  return(compound_name)
}

# 4.2 Line plot for PC1 loadings
load_line_pc1 <- loadings_data %>% 
  select(MET, INDEX, PC1) %>% 
  mutate(
    MET = load_tr(loading = PC1, threshold = 0.5, compound_name = MET)
  ) %>% 
  ggplot(aes(x = INDEX, y = PC1, label = MET)) +
  geom_line() +
  geom_text(fontface = "bold", position=position_nudge(), size = 2.5) +
  xlab("Compound Index") +
  ylab("Loadings")


# 4.3 Line plot for PC2 loadings
load_line_pc2 <- loadings_data %>% 
  select(MET, INDEX, PC2) %>% 
  mutate(
    MET = load_tr(loading = PC2, threshold = 0.19, compound_name = MET)
  ) %>% 
  ggplot(aes(x = INDEX, y = PC2, label = MET)) +
  geom_line() +
  geom_text(
    fontface = "bold", position = position_nudge(), angle = -40, size = 2.5
    ) +
  xlab("Compound Index") +
  ylab("Loadings")


# 5 Save plots ---------------------------------------------------------------
ggsave("graphs/scree_plot.jpeg", plot = bar_pca)
ggsave("graphs/pca_plot.jpeg", plot = pca_plot)
ggsave("graphs/load_line_pc1.jpeg", plot = load_line_pc1)
ggsave("graphs/load_line_pc2.jpeg", plot = load_line_pc2)
