# load libraries
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(cowplot))
suppressMessages(library(paletteer))
suppressMessages(library(ggrepel))
suppressMessages(library(ggbump))
suppressMessages(library(scales)) # used for label_number() function

figure_aa_boxplot_hline <- function(data, x, y, fill) {
  
  boxplot_aa <- ggplot(data, aes(x=x, y=y, fill=fill)) +
    geom_boxplot(lwd = 0.3) +
    labs(fill="Amino acid property") +
    geom_hline(yintercept = 1, linetype = 'dotted', col = 'red') +
    geom_hline(yintercept = -1, linetype = 'dotted', col = 'red') +
    geom_hline(yintercept = 0, linetype = 'dotted', col = 'blue') +
    xlab("Amino Acids") +
    # ylab(expression("Change in relative codon frequency " (log[2]))) +
    ylab("Change in relative\ncodon frequency (log2)") +
    theme_cowplot(12) +
    scale_fill_paletteer_d("ggsci::nrc_npg") +
    # ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.direction = "horizontal",
          # need to manually center legend as legend.position="bottom" fails 
          legend.position  = c(0.15, 0.6),
          # legend.position = "bottom", 
          # legend.box = "horizontal",
          axis.line = element_blank(),
          axis.title.y = element_text(vjust = -1.0),
          panel.border = element_rect(colour = "#737373",
                                      fill = NA,
                                      linewidth = 0.3))
  
}

figure_aa_boxplot <- function(data, x, y, fill) {
  
  boxplot_aa <- ggplot(data, aes(x=x, y=y, fill=fill)) +
    geom_boxplot(lwd = 0.3) +
    labs(fill="Amino acid property") +
    geom_hline(yintercept = 0, linetype = 'dotted', col = 'blue') +
    xlab("Amino Acids") +
    # ylab(expression("Change in relative codon frequency " (log[2]))) +
    ylab("Change in relative\ncodon frequency (log2)") +
    theme_cowplot(12) +
    scale_fill_paletteer_d("ggsci::nrc_npg") +
    # ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.direction = "horizontal",
          # need to manually center legend as legend.position="bottom" fails 
          legend.position  = c(0.15, 0.6),
          # legend.position = "bottom", 
          # legend.box = "horizontal",
          axis.line = element_blank(),
          axis.title.y = element_text(vjust = -1.0),
          panel.border = element_rect(colour = "#737373",
                                      fill = NA,
                                      linewidth = 0.3))
  
}

figure_codon_barplot_flipped_hline <- function(data, x.codon, y.rf, fill) {
  
  barplot_codon <- ggplot(data, aes(x=reorder(y, x.codon), y=y.rf, group=factor(aa), fill=fill)) + # NB: "aa" column is hard-coded here
    geom_bar(aes(x=x.codon), stat = "identity") +
    labs(fill="Amino acid property") +
    geom_hline(yintercept = 1, linetype = 'dotted', col = 'red') +
    geom_hline(yintercept = -1, linetype = 'dotted', col = 'red') +
    geom_hline(yintercept = -2, linetype = 'dotted', col = 'blue') +
    xlab("Codon (grouped by amino acid)") +
    # ylab(expression("Change in relative codon frequency " (log[2]))) +
    ylab("Change in relative\ncodon frequency (log2)") +
    # scale_y_discrete(drop = TRUE, expand = c(0, 0)) +
    # facet_grid(aa~., scales = "free", space = "free_x", switch = "x") + # NB: "aa" column is hard-coded here
    facet_grid(~aa, scales = "free", space = "free_x", switch = "x") +
    theme(strip.placement = "outside",
          panel.spacing = unit(0, "in"),
          strip.background.x = element_rect(fill = "white", color = "gray75"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray95"), 
          legend.direction = "horizontal", 
          legend.position = "bottom", 
          legend.box = "horizontal",
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_fill_paletteer_d("ggsci::nrc_npg") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) # cannot use cowplot as it messes up x-label, so have to set title theme manually
  
}

figure_codon_barplot_flipped <- function(data, x.codon, y.rf, fill) {
  
  barplot_codon <- ggplot(data, aes(x=reorder(y, x.codon), y=y.rf, group=factor(aa), fill=fill)) + # NB: "aa" column is hard-coded here
    geom_bar(aes(x=x.codon), stat = "identity") +
    labs(fill="Amino acid property") +
    xlab("Codon (grouped by amino acid)") +
    ylab("Change in relative\ncodon frequency (log2)") +
    # scale_y_discrete(drop = TRUE, expand = c(0, 0)) +
    # facet_grid(aa~., scales = "free", space = "free_x", switch = "x") + # NB: "aa" column is hard-coded here
    facet_grid(~aa, scales = "free", space = "free_x", switch = "x") +
    theme(strip.placement = "outside",
          panel.spacing = unit(0, "in"),
          strip.background.x = element_rect(fill = "white", color = "gray75"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "gray95"), 
          legend.direction = "horizontal", 
          legend.position = "bottom", 
          legend.box = "horizontal",
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_fill_paletteer_d("ggsci::nrc_npg") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) # cannot use cowplot as it messes up x-label, so have to set title theme manually
  
}

figure_variance_barplot <- function(data, x.var, y.aa, fill.aa_property) {
  
  data %>%
    ggplot(aes(x = x.var, y = reorder(y.aa, x.var), fill = fill.aa_property)) +
    geom_bar(stat = "identity") +
    xlab("Variance of codon relative frequency\n(grouped by amino acid)") +
    ylab("Amino Acid") +
    labs(fill="Amino acid\nproperty") +
    theme_cowplot(12) +
    scale_fill_paletteer_d("ggsci::nrc_npg")
  
}

figure_odds_barplot <- function(.data, .y_odds, .aa_property) {
  
  cu.var <- .data %>% 
    group_by(aa) %>% 
    filter(!(aa %in% c("M", "W"))) %>%
    summarize(var.rp.rf = var(rp.rf), 
              var.heg.rf = var(heg.rf),
              or.rp.rf = max(rp.rf)/(1-max(rp.rf)), 
              or.heg.rf =  max(heg.rf)/(1-max(heg.rf)),  
              nc = n()) %>% # gives group size (total codons within amino acid group)
    merge(., .aa_property, by.x = "aa", by.y = "aa")
  
  plot <- cu.var %>%
    ggplot(aes(x = aa, y = cu.var[[.y_odds]], fill = aa.property)) +
    geom_bar(stat = "identity") +
    xlab("Amino Acid") +
    ylab("Odds of most\nfrequent codon") +
    labs(fill="Amino acid\nproperty") +
    geom_hline(yintercept = 2, linetype = 'dotted', col = 'red') +
    theme_cowplot(12) +
    scale_fill_paletteer_d("ggsci::nrc_npg")
  
  return(plot)
  
}

main <- function() {
  
  # load datasets
  cu <- read_csv("path/to/codon-usage-frequencies.csv")
  aa.property <- read_csv("path/to/aa-properties.csv") # to colour amino acids by property (user-specified)
  cu <- merge(cu, aa.property, by.x = "aa", by.y = "aa")
  cu <- cu %>% filter(!(aa %in% c("M", "W")))

    
  ### FIGURE MAIN 1
  # CUBseq transcriptome-wide gene CU versus CUBseq highly expressed genes panel
  
  # rank CUs for bump plots
  cu_rank <- cu %>%
    group_by(aa) %>%
    mutate(rank.cubseq = rank(heg.rf), rank.cocoputs = rank(cocoputs.rf), rank.cubseq.global = rank(protein.rf)) %>%
    ungroup() %>%
    select(aa, codon, rank.cubseq, rank.cocoputs, rank.cubseq.global) %>%
    pivot_longer(-c('aa','codon'), names_to = "method", values_to = "rank") %>%
    mutate(aa = reorder(aa, codon))
  
  # plot bump plot - CUBseq transcriptome-wide vs. HEG
  plot1A <-  cu_rank %>%
    filter(!(aa %in% c("M", "W")) & !(method %in% c("rank.cocoputs"))) %>%
    # N.B. convert y variable to factor (to avoid duplicate ranks appearing)
    ggplot(aes(x = codon, y = factor(rank), group = method, col = method)) +
    geom_bump() +
    geom_point(size = 3) +
    # use scale_y_discrete and auto label number (to avoid duplicate ranks appearing)
    scale_y_discrete(labels = scales::label_number_auto()) +
    labs(color = "Codon table") +
    xlab("") +
    ylab("Rank") +
    facet_wrap(~aa,
               scales = "free",
               ncol = 5
    ) +
    scale_color_paletteer_d("ggsci::nrc_npg", labels = c("CUBseq (transcriptome-wide genes) rank", "CUBseq (highly expressed genes) rank")) +
    theme_cowplot(12) +
    theme(legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.justification = "center",
          strip.background =element_rect(fill="white"),
          strip.text = element_text(face="bold"),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "#737373",
                                      fill=NA,
                                      linewidth=0.3))
  
  
  # plot log odds barplot - CUBseq transcriptome-wide genes
  plot1B <- figure_odds_barplot(cu, "or.protein.rf", aa.property)
  
  # plot log odds ratio barplot - CUBseq highly expressed genes
  plot1C <- figure_odds_barplot(cu, "or.heg.rf", aa.property)
  
  plot1D <- figure_aa_boxplot_hline(cu, cu$aa, cu$log2.heg.protein, cu$aa.property)
  plot1E <- figure_codon_barplot_flipped_hline(cu, cu$codon, cu$log2.heg.protein, cu$aa.property)
  
  ### FIGURE MAIN 2
  # CUBseq transcriptome-wide gene CU versus CoCoPUTs panel
  
  # plot bump plot - CUBseq transcriptome-wide vs. CoCoPUTs
  plot2A <-  cu_rank %>%
    filter(!(aa %in% c("M", "W")) & !(method %in% c("rank.cubseq"))) %>%
    # N.B. convert y variable to factor (to avoid duplicate ranks appearing)
    ggplot(aes(x = codon, y = factor(rank), group = method, col = method)) +
    geom_bump() +
    geom_point(size = 3) +
    # use scale_y_discrete and auto label number (to avoid duplicate ranks appearing)
    scale_y_discrete(labels = scales::label_number_auto()) +
    labs(color = "Codon table") +
    xlab("") +
    ylab("Rank") +
    facet_wrap(~aa,
               scales = "free"
    ) +
    scale_color_paletteer_d("ggsci::nrc_npg", labels = c("CoCoPUTs rank", "CUBseq (all protein-coding genes) rank")) +
    theme_cowplot(12) +
    theme(legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.justification = "center",
          strip.background =element_rect(fill="white"),
          strip.text = element_text(face="bold"),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "#737373",
                                      fill=NA,
                                      linewidth=0.3))
  
  # plot log2 AA CU bar plot
  plot2B <- figure_aa_boxplot(cu, cu$aa, cu$log2.protein.cocoputs, cu$aa.property)
  
  # plot log2 codon CU bar plot
  plot2C <- figure_codon_barplot_flipped(cu, cu$codon, cu$log2.protein.cocoputs, cu$aa.property)
  
  
  
  
  ### FIGURE MAIN 3
  # CUBseq transcriptome-wide gene CU versus CoCoPUTs panel
  
  # plot bump plot - CUBseq transcriptome-wide vs. CoCoPUTs
  plot3A <-  cu_rank %>%
    filter(!(aa %in% c("M", "W")) & !(method %in% c("rank.cubseq.global"))) %>%
    # N.B. convert y variable to factor (to avoid duplicate ranks appearing)
    ggplot(aes(x = codon, y = factor(rank), group = method, col = method)) +
    geom_bump() +
    geom_point(size = 3) +
    # use scale_y_discrete and auto label number (to avoid duplicate ranks appearing)
    scale_y_discrete(labels = scales::label_number_auto()) +
    labs(color = "Codon table") +
    xlab("") +
    ylab("Rank") +
    facet_wrap(~aa,
               scales = "free"
    ) +
    scale_color_paletteer_d("ggsci::nrc_npg", labels = c("CoCoPUTs rank", "CUBseq HEGs rank")) +
    theme_cowplot(12) +
    theme(legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.justification = "center",
          strip.background =element_rect(fill="white"),
          strip.text = element_text(face="bold"),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "#737373",
                                      fill=NA,
                                      linewidth=0.3))
  
  # plot log2 AA CU bar plot
  plot3B <- figure_aa_boxplot(cu, cu$aa, cu$log2.heg.cocoputs, cu$aa.property)
  
  # plot log2 codon CU bar plot
  plot3C <- figure_codon_barplot_flipped(cu, cu$codon, cu$log2.heg.cocoputs, cu$aa.property)
  
  
  
  
  ### FIGURE MAIN 4
  # CUBseq rp gene CU versus CUBseq highly expressed genes panel
  
  # rank CUs for bump plots
  cu_rank <- cu %>%
    group_by(aa) %>%
    mutate(rank.cubseq = rank(heg.rf), rank.cocoputs = rank(cocoputs.rf), rank.cubseq.rp = rank(rp.rf)) %>%
    ungroup() %>%
    select(aa, codon, rank.cubseq, rank.cocoputs, rank.cubseq.rp) %>%
    pivot_longer(-c('aa','codon'), names_to = "method", values_to = "rank") %>%
    mutate(aa = reorder(aa, codon))
  
  # plot bump plot - CUBseq rp vs. HEG
  plot4A <-  cu_rank %>%
    filter(!(aa %in% c("M", "W")) & !(method %in% c("rank.cocoputs"))) %>%
    # N.B. convert y variable to factor (to avoid duplicate ranks appearing)
    ggplot(aes(x = codon, y = factor(rank), group = method, col = method)) +
    geom_bump() +
    geom_point(size = 3) +
    # use scale_y_discrete and auto label number (to avoid duplicate ranks appearing)
    scale_y_discrete(labels = scales::label_number_auto()) +
    labs(color = "Codon table") +
    xlab("") +
    ylab("Rank") +
    facet_wrap(~aa,
               scales = "free",
               ncol = 5
    ) +
    scale_color_paletteer_d("ggsci::nrc_npg", labels = c("CUBseq (ribosomal protein-coding genes) rank", "CUBseq (highly expressed genes) rank")) +
    theme_cowplot(12) +
    theme(legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.justification = "center",
          strip.background =element_rect(fill="white"),
          strip.text = element_text(face="bold"),
          axis.line = element_blank(),
          panel.border = element_rect(colour = "#737373",
                                      fill=NA,
                                      linewidth=0.3))
  
  
  # plot log odds barplot - CUBseq rp genes
  plot4B <- figure_odds_barplot(cu, "or.rp.rf", aa.property)
  
  # plot log odds ratio barplot - CUBseq highly expressed genes
  plot4C <- figure_odds_barplot(cu, "or.heg.rf", aa.property)
  
  plot4D <- figure_aa_boxplot_hline(cu, cu$aa, cu$log2.heg.rp, cu$aa.property)
  plo4E <- figure_codon_barplot_flipped_hline(cu, cu$codon, cu$log2.heg.rp, cu$aa.property)
  
  
  # plot CUBseq transcriptome-wide (global) gene CU versus CUBseq HEGs panel
  plot_global_heg_top <- plot_grid(plot1B + theme(legend.position="none"),
                                   plot1C + theme(legend.position="none"),
                                   align = 'vh',
                                   labels = c('A', 'B'),
                                   hjust = -1,
                                   ncol = 2,
                                   nrow = 1,
                                   label_size = 12,
                                   rel_widths = c(1, 1))
  
  plot_global_heg_middle <- plot_grid(plot1A,
                                      align = 'vh',
                                      labels = c('C'),
                                      hjust = -1,
                                      nrow = 1,
                                      label_size = 12)
  
  plot_global_heg_bottom <- plot_grid(plot1D + theme(legend.position="none"),
                                      plot1E + theme(legend.position="none"),
                                      align = 'h',
                                      labels = c('D', 'E'),
                                      hjust = -1,
                                      ncol = 2,
                                      nrow = 1,
                                      label_size = 12,
                                      rel_widths = c(1, 2))
  
  legend <- get_legend(plot1E + theme(legend.box.margin = margin(0, 0, 0, 12)))
  
  plot1 <- plot_grid(plot_global_heg_top,
                     plot_global_heg_middle,
                     plot_global_heg_bottom,
                     label_size = 12, 
                     ncol = 1, 
                     rel_heights = c(2, 6, 2.5))
  
  plot1 <- plot_grid(plot1, 
                     legend, 
                     nrow = 2, 
                     rel_heights = c(5, .4))
  
  
  # plot CUBseq transcriptome-wide (global) gene CU versus CoCoPUTs panel
  plot_global_cocoputs_top <- plot_grid(plot2A,
                                        align = 'vh',
                                        labels = c('A'),
                                        hjust = -1,
                                        nrow = 1,
                                        label_size = 12)
  
  plot_global_cocoputs_bottom <- plot_grid(plot2B + theme(legend.position="none"),
                                           plot2C + theme(legend.position="none"),
                                           align = 'vh',
                                           labels = c('B', 'C'),
                                           hjust = -1,
                                           ncol = 2,
                                           nrow = 1,
                                           label_size = 12,
                                           rel_widths = c(1, 2))
  
  
  
  plot2 <- plot_grid(plot_global_cocoputs_top,
                     plot_global_cocoputs_bottom,
                     label_size = 12, 
                     ncol = 1, 
                     rel_heights = c(4, 2))
  
  plot2 <- plot_grid(plot2, 
                     legend, 
                     nrow = 2, 
                     rel_heights = c(5, .4))
  
  
  
  
  # plot CUBseq rp gene CU versus CUBseq HEGs panel
  plot_rp_heg_top <- plot_grid(plot4B + theme(legend.position="none"),
                               plot4C + theme(legend.position="none"),
                               align = 'vh',
                               labels = c('A', 'B'),
                               hjust = -1,
                               ncol = 2,
                               nrow = 1,
                               label_size = 12,
                               rel_widths = c(1, 1))
  
  plot_rp_heg_middle <- plot_grid(plot4A,
                                  align = 'vh',
                                  labels = c('C'),
                                  hjust = -1,
                                  nrow = 1,
                                  label_size = 12)
  
  plot_rp_heg_bottom <- plot_grid(plot4D + theme(legend.position="none"),
                                  plot4E + theme(legend.position="none"),
                                  align = 'h',
                                  labels = c('D', 'E'),
                                  hjust = -1,
                                  ncol = 2,
                                  nrow = 1,
                                  label_size = 12,
                                  rel_widths = c(1, 2))
  
  legend <- get_legend(plot4E + theme(legend.box.margin = margin(0, 0, 0, 12)))
  
  plot4 <- plot_grid(plot_rp_heg_top,
                     plot_rp_heg_middle,
                     plot_rp_heg_bottom,
                     label_size = 12, 
                     ncol = 1, 
                     rel_heights = c(2, 6, 2.5))
  
  plot4 <- plot_grid(plot1, 
                     legend, 
                     nrow = 2, 
                     rel_heights = c(5, .4))
  
  
  # plot HEGs CU versus CoCoPUTs panel
  plot_global_heg_cocoputs_top <- plot_grid(plot3A,
                                            align = 'vh',
                                            labels = c('A'),
                                            hjust = -1,
                                            nrow = 1,
                                            label_size = 12)
  
  plot_global_heg_cocoputs_bottom <- plot_grid(plot3B + theme(legend.position="none"),
                                               plot3C + theme(legend.position="none"),
                                               align = 'vh',
                                               labels = c('B', 'C'),
                                               hjust = -1,
                                               ncol = 2,
                                               nrow = 1,
                                               label_size = 12,
                                               rel_widths = c(1, 2))
  
  # plot_global_cocoputs_bottom <- plot_grid(plot1D,
  #                                          align = 'h',
  #                                          labels = c('D'),
  #                                          hjust = -1,
  #                                          ncol = 1,
  #                                          nrow = 1,
  #                                          label_size = 12)
  
  plot3 <- plot_grid(plot_global_heg_cocoputs_top,
                     plot_global_heg_cocoputs_bottom,
                     label_size = 12, 
                     ncol = 1, 
                     rel_heights = c(4, 2))
  
  plot3 <- plot_grid(plot3, 
                     legend, 
                     nrow = 2, 
                     rel_heights = c(5, .4))
  
  
  
  save_plot(filename = "figures/global-cu-analysis/main-global-heg-cu-panel.pdf", plot = plot1, base_height = 13, base_width = 13)
  message("Figure main - CUBseq transcriptome-wide (global) vs HEGs CU panel done.")
  
  save_plot(filename = "figures/global-cu-analysis/main-global-cocoputs-cu-panel.pdf", plot = plot2, base_height = 10, base_width = 13)
  message("Figure main - CUBseq transcriptome-wide (global) vs CoCoPUTs CU panel done.")
  
  
  save_plot(filename = "figures/global-cu-analysis/main-rp-heg-cu-panel.pdf", plot = plot4, base_height = 13, base_width = 13)
  message("Figure main - CUBseq transcriptome-wide (global) vs HEGs CU panel done.")
  
  save_plot(filename = "figures/global-cu-analysis/main-heg-cocoputs-cu-panel-2.pdf", plot = plot3, base_height = 10, base_width = 13)
  message("Figure main - CUBseq HEGs vs CoCoPUTs CU panel done.")
  
  message("Finished!")
  
}

main()
