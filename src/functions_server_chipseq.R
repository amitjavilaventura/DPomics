### ==============================================================================================================================
### DPomics - FUNCTIONS CHIPSEQ
### ==============================================================================================================================

# ----- PEAK NUM ----- #
peakNum <- function(data, legend_position = "right", pannels = T, plotly = F){
  
  library(ggplot2)
  library(ggpubr)
  
  
  if(pannels){ g <- ggplot(data, aes(condition, fill = condition)) + geom_bar(position = "dodge") + facet_wrap(~protein) }
  else{ g <- ggplot(data, aes(sample, fill = sample)) + geom_bar(position = "dodge") }
  
  g <- g + ggtitle("Number of TF peaks in each condition") + ylab("Number of peaks") + xlab("") +
    
    theme_pubr() +
    
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
          legend.title = element_blank(),
          legend.position = legend_position)
  
  if(plotly){
    require(plotly)
    g <- ggplotly(g)
  }
  
  return(g)
}

# ----- bar anno ----- #
barAnno  <- function(data, names,
                     main = "Distribution of peaks in the genome",
                     ylab = "Proportion", xlab = "Condition", pannels = T,
                     palette = "Set1", anno_num = 3, peak_type = "Proportion",
                     plotly = F){
  
  # Load packages
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(ggpubr)
  library(magrittr)
  
  
  # Format promoter annnotation names to take parentheses out for each annotatePeak object in the list
  data$annotation <- sub(" \\(.*\\)", "", data$annotation)
  
  # Rewrite annotation as distal or gene body depending on the feature
  if(anno_num == 2){
    data <- data %>%
      dplyr::mutate(annotation = dplyr::recode(annotation,
                                               "Distal Intergenic" = "Distal",
                                               "Downstream" = "Distal",
                                               "Intron" = "Distal",
                                               "Exon" = "Distal",
                                               "5' UTR" = "Distal",
                                               "3' UTR" = "Distal"))
    
    subtitle = "Promoter and distal regions"
  }
  else if(anno_num == 3){
    data <- data %>%
      dplyr::mutate(annotation = dplyr::recode(annotation,
                                               "Distal Intergenic" = "Distal",
                                               "Downstream" = "Distal",
                                               "Intron" = "Gene body",
                                               "Exon" = "Gene body",
                                               "5' UTR" = "Gene body",
                                               "3' UTR" = "Gene body"))
    
    subtitle = "Promoter, gene body and distal regions"
  }
  
  # Create gglpot2-based barplot
  if(peak_type == "Proportion"){
    ggposition <- "fill"
  }
  else{
    ggposition <- "stack"
  }
  
  # ggplot function
  if(pannels){ g <- ggplot(data, aes(condition, fill = factor(annotation))) + geom_bar(position = ggposition) + facet_wrap(~protein) }
  else{ g <- ggplot(data, aes(sample, fill = factor(annotation))) + geom_bar(position = ggposition) }
  
  # Write plot title, subtitle and axis labels
  g <- g + ggtitle(main, subtitle = subtitle) + ylab(ylab) + xlab(xlab) +
    
    # Format colors with ggplot2 function scale_fill_brewer
    scale_fill_brewer(palette = palette) +
    
    # Basic formatting
    theme_pubr() +
    
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
          legend.title = element_blank(),
          legend.position = "right")
  
  if(plotly){
    require(plotly)
    g <- ggplotly(g)
  }
  
  # Draw plot
  return(g)
}


# ----- intersectPeaks ----- #
intersectPeaks <- function(peaks, conditions){
  
  list <- list()
  for(i in 1:length(conditions)){
    cond <- peaks %>% dplyr::filter(sample == conditions[i]) %>% plyranges::as_granges()
    list[[i]] <- cond
  }
  
  venn <- makeVennDiagram(Peaks = list, 
                          NameOfPeaks = conditions, 
                          main = "Overlap of peaks", fill = rainbow(n = length(conditions)), alpha = 0.6)
  
  return(venn)
}
