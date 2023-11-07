
# Paper Data Processing -----------------------------------------------------

# Packages
if (!"tidyverse" %in% .packages()) library(tidyverse)

# 1 Import data -----------------------------------------------------------
table_1  <- read.csv("data/table_1.csv")  # table in discussion section
table_s2 <- read.csv("data/table_s2.csv") # table in supplementary material

# 2 Data Processing ---------------------------------------------------------

# 2.1 Table for means 

# Table 1
t1_means <- map(table_1[,-1], substr, start = 1, stop = 4)
t1_means <- map(t1_means, as.numeric)
t1_means <- as.data.frame(t1_means)

# Table S2
ts2_means <- map(table_s2[,-1], substr, start = 1, stop = 4)
ts2_means <- map(ts2_means, as.numeric)
ts2_means <- as.data.frame(ts2_means)

# Join both tables
means_data <- rbind(t1_means, ts2_means)
colnames(means_data) <- c("0.5", "1", "2", "4", "12", "24", "48", "72")

# Add a column with compound names in both tables 
means_data <- means_data %>% 
  mutate(MET = c(table_1$metabolites, table_s2$metabolites)) %>% 
  relocate(MET)

# Save data frame with means 
write.csv(means_data, "data/means_data.csv", row.names = FALSE)

# 2.2 Table for standard deviations

# Table 1
t1_sds <- map(table_1[,-1], substr, start = 6, stop = 9)
t1_sds <- map(t1_sds, as.numeric) 
t1_sds <- as.data.frame(t1_sds)

# Table S2
ts2_sds <- map(table_s2[,-1], substr, star = 8, stop = 12)
ts2_sds <- map(ts2_sds, as.numeric)
ts2_sds <- as.data.frame(ts2_sds)

# Join both tables 
sds_data <- rbind(t1_sds, ts2_sds)
colnames(sds_data) <- c("0.5", "1", "2", "4", "12", "24", "48", "72")

# Add a column with compound names in both tables 
sds_data <- sds_data %>% 
  mutate(MET =  c(table_1$metabolites, table_s2$metabolites)) %>% 
  relocate(MET)

# Save data frame with sds
write.csv(sds_data, "data/sds_data.csv", row.names = FALSE)

# 3 Gather data for means and sds ----------------------------------------------

# 3.1 Means
mg_data <- pivot_longer(means_data, `0.5`:`72`, 
                        names_to = "TIME", values_to = "MEAN")

# 3.2 SDs
sdg_data <- pivot_longer(sds_data, `0.5`:`72`,
                         names_to = "TIME", values_to = "SD")

# 3.3 Join means ans sd 
complete_data <- full_join(mg_data, sdg_data, by = c("TIME", "MET"))

# 3.4 Save cp_data (to simulation)
write.csv(complete_data, "data/complete_data.csv", row.names = FALSE)
