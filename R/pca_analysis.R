
# PCA Analysis on Main Data ------------------------------------------------

# Packages
if (!"tidyverse" %in% .packages()) library(tidyverse)

# 1 Import Data -----------------------------------------------------------
main_data <- read_csv("data/main_data.csv")

# 2 Formatting Data for PCA ---------------------------------------------------

# 2.1 Obtain wide data 
main_data_wd <- main_data %>% 
  mutate(SAMPLE_TIME = paste0(SAMPLE, "-", TIME)) %>% 
  pivot_wider(names_from = SAMPLE_TIME, values_from = QT, -TIME:-SAMPLE)

# 2.2 Transposing just quantity values
qt_data <- t(main_data_wd[, -1])

# 3 PCA analysis ----------------------------------------------------------
qt_pca <- prcomp(qt_data, scale. = FALSE)

# 3.1 PCA summary
pca_summary <- summary(qt_pca)

# 3.2 PCA data
pca_data <- as_tibble(qt_pca$x) %>% 
  mutate(TIME = as_factor(substr(rownames(qt_pca$x), start = 3, stop =5))) %>% 
  relocate(TIME)

# 3.3 Loadings Data

# Compound names
compound_names <- filter(main_data, SAMPLE == 1, TIME == 0.5) %>% 
  select(MET) %>% 
  unlist()          

loadigns_data <- as_tibble(qt_pca$rotation) %>% 
  signif(3) %>% 
  mutate(MET = compound_names, INDEX = 1:length(MET)) %>% 
  relocate(MET, INDEX) 

# 3.4 Variation percentage per PC
var_pca <- qt_pca$sdev^2
per_var_pca <- round((var_pca / sum(var_pca))*100, 2)
per_var_pca <- tibble(
  PC = 1:length(per_var_pca),
  PER_VAR = per_var_pca
)

# 4 Save results ----------------------------------------------------------
capture.output(pca_summary, file = "data/pca_summary.txt")
write_csv(pca_data, "data/pca_data.csv")
write_csv(loadigns_data, "data/loadings_data.csv")
write_csv(per_var_pca, "data/per_var_pca.csv")
