library(tidyverse)
library(ggplot2)
library(readr)
library(paletteer)
library(ghibli)
library(dplyr)
library(ggrepel)
library(cowplot)

data <- read_delim("path/to/metadata.csv", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)

#run alias against read count
# Assuming 'study_accession' has factor levels 'PRJDB5592', 'PRJNA169169', 'PRJNA703366'
# Create a new column to encode the desired order within each group
data$order_within_group <- factor(data$run_alias, levels = unique(data$run_alias))

# Grouping data by study accession
data <- data[order(data$study_accession), ]

# Plotting run alias against read count
ggplot(data, aes(x = read_count, y = order_within_group, fill = study_accession)) + 
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("PRJDB5592" = "#89689d", "PRJNA169169" = "#e69b99", "PRJNA703366" = "#2c6184")) +
  labs(x = expression("Read count "  (10^5)), y = "Sample", fill = "Study Accession") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(labels = function(x) x / 10^5) 

#run alias against base count
# Assuming 'study_accession' has factor levels 'PRJDB5592', 'PRJNA169169', 'PRJNA703366'
# Create a new column to encode the desired order within each group
data$order_within_group <- factor(data$run_alias, levels = unique(data$run_alias))

# Grouping data by study accession
data <- data[order(data$study_accession), ]

# Plotting run alias against base count
ggplot(data, aes(x = base_count, y = order_within_group, fill = study_accession)) + 
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("PRJDB5592" = "#89689d", "PRJNA169169" = "#e69b99", "PRJNA703366" = "#2c6184")) +
  labs(x = expression("Base count " * (10^8)), y = "Sample", fill = "Study Accession") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(labels = function(x) x / 10^8)