#################
### FUNCTIONS ###
#################

### RNA-SEQ FUNCTIONS
# ==================================================================================================================== #

## ----- Volcano Plot ----- ##
volcanoPlot2 <- function(df, xlim = c(-10,10), ylim = c(0,30),
                         pval = 0.05, log2FC = 1.5,
                         main = NULL, mainSize = 9, sub = NULL, subSize = 8,
                         labelSize = 7, labelColor = c("darkgreen", "red"), labelPos = 0,
                         xlab = bquote(~Log[2]~ "FC"), ylab = (bquote(~-Log[10]~italic(P))) , axisLabelSize = 7, axisTextSize = 7, 
                         pointColor = c("darkgreen", "gray", "red"), legendTitle = FALSE, legendPos = "bottom",
                         degsLabel = F , degsLabelNum=5, degsLabelSize = 3,
                         ggrastr = F) {
  
  #load packages
  library(ggplot2)
  library(dplyr)
  
  # Mutate the dataframe to include the shape of the points.
  df <- df %>%
    
    # Shape of al the points
    dplyr::mutate(shape = "circle") %>%
    
    # Change the shape of the point to triangle if the -log10(padj) is greater than the ylim variable
    dplyr::mutate(shape = ifelse(test = -log10(padj) > ylim[2], yes = "triangle", no = shape)) %>%
    
    # Change the shape of the points to triangle if the log2FoldChange is greater or lower than the xlim
    dplyr::mutate(shape = ifelse(test = log2FoldChange > xlim[2], yes = "triangle", no = shape)) %>%
    dplyr::mutate(shape = ifelse(test = log2FoldChange < xlim[1], yes = "triangle", no = shape)) %>%
    
    # Change the padj to the value that plots the points at the top of the graph.
    dplyr::mutate(padj  = ifelse(test = -log10(padj) > ylim[2], yes = 10^-ylim[2], no = padj)) %>%
    
    # Change the log2FoldChange of the points with log2FoldChange than the limits, so they will be plotted in the limits of the graph
    dplyr::mutate(log2FoldChange = ifelse(test = log2FoldChange > xlim[2], yes = xlim[2], no = log2FoldChange)) %>%
    dplyr::mutate(log2FoldChange = ifelse(test = log2FoldChange < xlim[1], yes = xlim[1], no = log2FoldChange))
  
  
  # Start plot with ggrastr points. This is useful when using workflowr.
  if(ggrastr){
    
    # Load ggrastr
    library(ggrastr)
    
    p <- ggplot(data = na.omit(df), aes(x=log2FoldChange, y=-log10(padj), colour=DEG, shape=shape)) +
      
      # Draw points with geom_point_rast from ggrastr, because it will draw points as images, making the graph lighter.
      geom_point_rast(alpha=0.7, size=1.7, raster.height = 5.15, raster.width = 6, raster.dpi = 400)
  }
  
  # Start plot normally
  else{
    p <- ggplot(data = na.omit(df), aes(x=log2FoldChange, y=-log10(padj), colour=DEG, shape=shape)) + 
      
      geom_point(alpha=0.7, size=1.7)
  }
  
  
  # Annotate the number of up and downregulated DEGs
  p <- p +
    annotate("text", label = sum(df$DEG == "Upregulated"), color = labelColor[2], y = labelPos, x = xlim[2], 
             vjust=0.5,hjust="inward", size = labelSize) +
    annotate("text", label = sum(df$DEG == "Downregulated"), color = labelColor[1], y = labelPos, x = xlim[1],
             vjust=0.5,hjust="inward", size = labelSize)
  
  # Basic formatting
  p <- p +
    
    # Stablish a predefined theme
    theme_classic() +
    
    # Write and format the graph title, can be nothing. 
    ggtitle(label = main, subtitle = sub) +
    theme(plot.title = element_text(face="bold", hjust = .5, size = mainSize),
          plot.subtitle = element_text(hjust = .5, size = subSize)) +
    
    # Stablish the x and y axes ranges. 
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    # Put an horizontal line in the -log10(pval) value and two vertival lines in the -logFC and logFC values. 
    geom_hline(yintercept = -log10(pval), linetype = 2) +
    geom_vline(xintercept = c(-log2FC, log2FC), linetype = 2) +
    
    # Format the axis names and sizes
    xlab(xlab) + ylab(ylab) + theme(axis.title = element_text(size = axisLabelSize)) +
    
    # Format the color of the points
    scale_colour_manual(values=c("Downregulated" = pointColor[1], "NS" = pointColor[2], "Upregulated" = pointColor[3]),
                        labels = c("Downregulated" = "Downregulated", "NS" = "NS", "Upregulated" = "Upregulated"),
                        drop = FALSE) +
    
    # Remove the legend for shape
    guides(shape=FALSE) +
    
    # Format the axis values
    theme(axis.text = element_text(size = axisTextSize)) +
    
    # Decide the position of the legend (default: "bottom")
    theme(legend.position = legendPos) 
  
  # Decide if legend title is writen or not. Default: not writen.  
  if(!legendTitle){
    p <- p + theme(legend.title = element_blank())
  }
  
  # Write names of the most DE genes in terms of lowest adjusted p-value
  if(degsLabel) {
    
    # Load ggrepel
    library(ggrepel)
    
    # Organaize and retrieve lowest p-value genes
    degs <- df %>%
      
      # Filter non significant genes
      dplyr::filter(DEG!="NS") %>%
      
      # Arrange by ascendent order of padjusted
      dplyr::arrange(padj)
    
    # Create a dataframe with the labels of the DEGs with highest abs(log2FC).
    degs <- head(na.omit(degs),degsLabelNum) %>% as.data.frame()
    
    # Put labels in the plot
    p <- p + geom_text_repel(data = degs, mapping = aes(x = log2FoldChange, y = -log10(padj), label = Geneid), size = degsLabelSize, color = "Black")
  }

    
  # Draw the graph.
  return(p)
  
}

## ----- annotateDEGs for a lof2FC and pvalue ----- ##
annotate_de <- function(degs, log2FC = 1.5, padj = 0.05){
  
  degs$DEG <- "NS"
  degs$DEG[which(degs$log2FoldChange >= log2FC & degs$padj <= padj)] <- "Upregulated"
  degs$DEG[which(degs$log2FoldChange <= -(log2FC) & degs$padj <= padj)] <- "Downregulated"
  
  return(degs)
  
}

### CHIP-SEQ FUNCTIONS
# ==================================================================================================================== #

# ----- PEAK NUM ----- #
peakNum <- function(data){
  
  library(ggplot2)
  library(ggpubr)
  
  d <- data.frame(Condition = data$condition) %>% 
    ggplot(aes(Condition, fill = Condition)) + geom_bar(position = "dodge") +
    
    ggtitle("Number of TF peaks in each condition") + ylab("Number of peaks") + xlab("") +
    
    theme_pubr() +
    
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
          legend.title = element_blank(),
          legend.position = "right")
  
  return(d)
}

# ----- bar anno ----- #
barAnno  <- function(data, names,
                     main = "Distribution of peaks in the genome",
                     ylab = "Proportion", xlab = "Condition",
                     palette = "Set1", anno_num = 3, peak_type = "Proportion"){
  
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
  
  g <- ggplot(data, aes(condition, fill = factor(annotation))) + geom_bar(position = ggposition)
  
  
  # Write plot title, subtitle and axis labels
  g <- g + ggtitle(main, subtitle = subtitle) + ylab(ylab) + xlab(xlab) +
    
    # Format colors with ggplot2 function scale_fill_brewer
    scale_fill_brewer(palette = palette) +
    
    # Basic formatting
    theme_pubr() +
    
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
          legend.title = element_blank(),
          legend.position = "right")
  
  # Draw plot
  return(g)
}


# ----- intersectPeaks ----- #
intersectPeaks <- function(peaks, conditions){
  
  list <- list()
  for(i in 1:length(conditions)){
    cond <- peaks %>% dplyr::filter(condition == conditions[i]) %>% plyranges::as_granges()
    list[[i]] <- cond
  }
  
  venn <- makeVennDiagram(Peaks = list, 
                          NameOfPeaks = conditions, 
                          main = "Overlap of peaks", fill = rainbow(n = length(conditions)), alpha = 0.6)
  
  return(venn)
}
