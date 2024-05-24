library(dplyr)
library(readr)
library(formattable)
library(tidyverse)

get_cu <- function(.cc_df, .cu_df) {
  
  # This function calculates codon relative frequency and frequency 
  # per thousand.
  
  cc <- .cc_df %>% 
    group_by(codon) %>% 
    summarise(sum = sum(codon.count))
  
  cc <- cc %>%
    mutate(freq.thousand = (sum / sum(sum))*1000 ) # calculate frequency/thousand
  
  cc$codon <- gsub("T", "U", cc$codon)
  
  cu <- merge(.cu_df, cc, by = "codon") %>%
    select(., "aa", "codon", "heg.rf", "freq.thousand")
  
  return(cu)
  
}

make_codon_table <- function(.data) {
  
  # Function reformats codon counts dataframe into codon table used
  # as input to formattable function.
  # ARG: takes ".data" as input and outputs reformatted codon table.
  
  # format table with specific codon order
  split_codons_1 <- c("UUU", "UUC", "UUA", "UUG",
                      "CUU", "CUC", "CUA", "CUG",
                      "AUU", "AUC", "AUA", "AUG",
                      "GUU", "GUC", "GUA", "GUG")
  
  split_codons_2 <- c("UCU", "UCC", "UCA", "UCG",
                      "CCU", "CCC", "CCA", "CCG",
                      "ACU", "ACC", "ACA", "ACG",
                      "GCU", "GCC", "GCA", "GCG")
  
  split_codons_3 <- c("UAU", "UAC", "UAA", "UAG",
                      "CAU", "CAC", "CAA", "CAG",
                      "AAU", "AAC", "AAA", "AAG",
                      "GAU", "GAC", "GAA", "GAG")
  
  split_codons_4 <- c("UGU", "UGC", "UGA", "UGG",
                      "CGU", "CGC", "CGA", "CGG",
                      "AGU", "AGC", "AGA", "AGG",
                      "GGU", "GGC", "GGA", "GGG")
  
  cu_1 <- subset(.data, codon %in% split_codons_1)
  # reorder codon column values with correct order
  cu_1 <- cu_1[match(split_codons_1, cu_1$codon),]
  
  cu_2 <- subset(.data, codon %in% split_codons_2)
  cu_2 <- cu_2[match(split_codons_2, cu_2$codon),]
  
  cu_3 <- subset(.data, codon %in% split_codons_3)
  cu_3 <- cu_3[match(split_codons_3, cu_3$codon),]
  
  cu_4 <- subset(.data, codon %in% split_codons_4)
  cu_4 <- cu_4[match(split_codons_4, cu_4$codon),]
  
  # bind dataframes into one
  combined_df <- cbind(cu_1, cu_2, cu_3, cu_4)
  rownames(combined_df) <- NULL # remove row numbering (so it won't appear in codon table...)
  
  # make codon table
  colnames(combined_df) <- c("aa", "codon", "rf", "freq.thousand", 
                             "aa.1", "codon.1", "rf.1", "freq.thousand.1", 
                             "aa.2", "codon.2", "rf.2", "freq.thousand.2", 
                             "aa.3", "codon.3", "rf.3", "freq.thousand.3")
  
  combined_df <- combined_df %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
  
  return(combined_df)
  
}

plot_codon_table <- function(.ct) {
  
  formattable(.ct,
              align = c("l", "l", "l", "l",
                        "l", "l", "l", "l",
                        "l", "l", "l", "l",
                        "l", "l", "l", "l"),
              list(rf = color_tile("transparent", "lightpink"),
                   rf.1 = color_tile("transparent", "lightpink"),
                   rf.2 = color_tile("transparent", "lightpink"),
                   rf.3 = color_tile("transparent", "lightpink"),
                   freq.thousand = color_bar("lightblue"),
                   freq.thousand.1 = color_bar("lightblue"),
                   freq.thousand.2 = color_bar("lightblue"),
                   freq.thousand.3 = color_bar("lightblue")),
              col.names = c("Amino Acid", "Codon", "Relative\nFrequency", 
                            "Frequency/\nThousand", 
                            "Amino Acid", "Codon", "Relative\nFrequency", 
                            "Frequency/\nThousand",
                            "Amino Acid", "Codon", "Relative\nFrequency", 
                            "Frequency/\nThousand",
                            "Amino Acid", "Codon", "Relative\nFrequency", 
                            "Frequency/\nThousand")
  )
  
}



# load data
cu <- read_csv("path/to/codon-usage-frequencies.csv")
cc.heg <- read_csv("path/to/hegs-summary-counts.csv")
cc.global <- read_csv("path/to/all-protein-coding-genes-summary-counts.csv")

# get cu tables
cu.global <- get_cu(cc.global, cu)
cu.heg <- get_cu(cc.heg, cu)

# create codon table for global and HEG
# ct.global <- make_codon_table(cu.global)
ct.heg <- make_codon_table(cu.heg)
ct.global <- make_codon_table(cu.global)

# generate figuresprintp
formattable.global <- plot_codon_table(ct.global)
formattable.heg <- plot_codon_table(ct.heg)


# save as PDF
# TODO: still need to figure this out (tried various methods).
# issue is figure gets generated in the "Viewer port", not in "Plots".




