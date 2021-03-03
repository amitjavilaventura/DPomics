
### DPomics SERVER FUNCTION - RNAseq                                         
### =========================================================================================== ###

### DPomics SERVER RNASEQ explore -----------------------------------------------------------------
server_rnaseq_explore <- function(input, output, session){
  
  # table with the data of the table
  output$rna_dataTable <- DT::renderDataTable({
    degs <- annotate_de(degs = rna_data(), log2FC =  input$rna_log2fc, padj =  input$rna_padj)
    DT::datatable(data = degs, options = list(lengthMenu = c(5, 10, 30, 50), pageLength = length(unique(degs$Contrast))))
  })
  
}

### DPomics SERVER RNASEQ volcano -----------------------------------------------------------------
server_rnaseq_volcano <- function(input, output, session, data){
  
  # volcano select input -----
  output$volcano_contrasts <- renderUI({
    data <- rna_data()
    contrasts <- data$Contrast %>% unique
    selectizeInput(inputId = "rna_contrasts", label = "Select a contrasts for the volcanoPlot",
                   choices = contrasts, selected = NULL, multiple = T, 
                   options = list(placeholder = "Select the contrasts..."))
  })
  
  # plot volcanos -----
  observeEvent(eventExpr = input$plot_volcano, {
    output$ui_rna_volcanos <- renderUI({ plotOutput(outputId = "rna_volcanos", width = 1200, height = isolate(input$volcanosHeight)) })
    output$rna_volcanos <- renderPlot({
      input$plot_volcano
      data <- rna_data()
      degs <- annotate_de(degs = data, log2FC = isolate(input$rna_log2fc), padj = isolate(input$rna_padj))
      plotlist <- list()
      for(i in 1:length(isolate(input$rna_contrasts))){
        p <- volcanoPlot2(df = degs[which(degs$Contrast == isolate(input$rna_contrasts[i])),], 
                          log2FC = isolate(input$rna_log2fc), pval = isolate(input$rna_padj), 
                          main = isolate(input$rna_contrasts[i]), mainSize = isolate(input$rna_mainSize), labelSize = isolate(input$rna_sizeDEGs), 
                          axisLabelSize = isolate(input$rna_axisText), axisTextSize = isolate(input$rna_axisText),
                          degsLabel = isolate(input$rna_checkLabels), degsLabelNum = isolate(input$rna_numLabels), degsLabelSize = isolate(input$rna_sizeLabels))
        plotlist[[i]] <- p
      }
      
      ggarrange(plotlist = plotlist, ncol = 2, nrow = ceiling(length(isolate(input$rna_contrasts))/2), legend = "bottom", common.legend = T)
    
    })
  })
}

### DPomics SERVER RNASEQ overlap -----------------------------------------------------------------
server_rnaseq_overlap <- function(input, output, session){
  
  # renuder UIs to select contrasts -----
  output$rna_overlap1_contrasts <- renderUI({
    data <- rna_data()
    contrasts <- data$Contrast %>% unique
    selectizeInput(inputId = "contrastDegs1", choices = contrasts, multiple = T, label = paste("Select the contrasts to overlap", sep = " "), 
                   options = list(maxItems = 3, placeholder = "Select 2 or 3 contrasts..."))
  })
  output$rna_overlap2_contrasts <- renderUI({
    data <- rna_data()
    contrasts <- data$Contrast %>% unique
    selectizeInput(inputId = "contrastDegs2", choices = contrasts, multiple = T, label = paste("Select the contrasts to overlap", sep = " "), 
                   options = list(maxItems = 3, placeholder = "Select 2 or 3 contrasts..."))
  })
  
  # plot venn diagrams 1 ------------------------
  observeEvent(input$submit_rnaOverlaps1, {
    
    ## upregulated genes
    output$updegs_overlaps1 <- renderPlot({
      input$submit_rnaOverlaps1
      data <- rna_data()
      data <- data %>% dplyr::filter(DEG == "Upregulated")
      list <- list()
      for (i in 1:length(isolate(input$contrastDegs1))){
        x <- data %>% dplyr::filter(Contrast == isolate(input$contrastDegs1[i])) %>% dplyr::select(Geneid)
        list[[i]] <- x$Geneid
      }
      colors <- rainbow(n = length(isolate(input$contrastDegs1)), alpha = 0.6)
      cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = isolate(input$contrastDegs1), fill = colors))
    })
    
    ## downregulated genes
    output$downdegs_overlaps1 <- renderPlot({
      input$submit_rnaOverlaps1
      data <- rna_data()
      data <- data %>% dplyr::filter(DEG == "Downregulated")
      list <- list()
      for (i in 1:length(isolate(input$contrastDegs1))){
        x <- data %>% dplyr::filter(Contrast == isolate(input$contrastDegs1[i])) %>% dplyr::select(Geneid)
        list[[i]] <- x$Geneid
      }
      colors <- rainbow(n = length(isolate(input$contrastDegs1)), alpha = 0.6)
      cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = isolate(input$contrastDegs1), fill = colors))
    })
    
  }) # observeEvent end (actionButton generate overlaps1)
  
  # plot venn diagrams 2 ------------------------
  observeEvent(input$submit_rnaOverlaps2, {
    
    ## upregulated genes
    output$updegs_overlaps2 <- renderPlot({
      input$submit_rnaOverlaps2
      data <- rna_data()
      data <- data %>% dplyr::filter(DEG == "Upregulated")
      list <- list()
      for (i in 1:length(isolate(input$contrastDegs2))){
        x <- data %>% dplyr::filter(Contrast == isolate(input$contrastDegs2[i])) %>% dplyr::select(Geneid)
        list[[i]] <- x$Geneid
      }
      colors <- rainbow(n = length(isolate(input$contrastDegs2)), alpha = 0.6)
      cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = isolate(input$contrastDegs2), fill = colors))
    })
    
    ## downregulated genes
    output$downdegs_overlaps2 <- renderPlot({
      input$submit_rnaOverlaps2
      data <- rna_data()
      data <- data %>% dplyr::filter(DEG == "Downregulated")
      list <- list()
      for (i in 1:length(isolate(input$contrastDegs2))){
        x <- data %>% dplyr::filter(Contrast == isolate(input$contrastDegs2[i])) %>% dplyr::select(Geneid)
        list[[i]] <- x$Geneid
      }
      colors <- rainbow(n = length(isolate(input$contrastDegs2)), alpha = 0.6)
      cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = isolate(input$contrastDegs2), fill = colors))
    })
  }) # observeEvent end (actionButton generate overlaps2)
  
  
  # Log2FC heatmap ------
  # get gene lists
  output$gene_list_heatmap <- renderUI({
    data <- rna_data()
    genes <- as.character(data$Geneid)
    textInput(inputId = "select_genes_heatmap", label = "Write a list of genes separated by blank spaces")
  })
  
  # format matrix of data
  log2fc <- reactive({
    data  <- rna_data()
    x <- data %>% select(Contrast, Geneid, log2FoldChange, padj, DEG)
    l <- list()
    for(i in x$Contrast %>% unique){
      z <- x %>% dplyr::filter(Contrast == i)  %>% set_colnames(c("Contrast", "Geneid", i, paste(i,"DEG", sep = "_"), paste(i,"padj", sep = "_"))) %>% 
        dplyr::select(everything(), -Contrast)
      l[[i]] <- z
    }
    l %>% plyr::join_all()
  })
  
  # plot heatmap -----
  observeEvent(eventExpr = input$heatmap_button, {
    output$rna_heatmap <- renderPlot({
      input$heatmap_button
      genes <- isolate(input$select_genes_heatmap)
      genes <- genes %>% stringr::str_split(pattern = " ") %>% unlist()
      l <- log2fc() %>% dplyr::filter(Geneid %in% genes)
      log2FC <- l %>% dplyr::select(-matches("_DEG"), -matches("_padj")) %>% column_to_rownames("Geneid")
      ComplexHeatmap::Heatmap(matrix = as.matrix(log2FC), name = "Log2FC", show_row_names = T,
                              column_title = "Log2FC of selected genes", column_title_side = "top",
                              col = colorRamp2(c(-3, 0, 3), c("blue", "white", "red")),
                              cell_fun = function(j, i, x, y, width, height, fill) {
                                grid.text(sprintf("%.1f", as.matrix(log2FC)[i, j]), x, y, gp = gpar(fontsize = 10))
                              })
    })
  }) # observe event actionButton heatmap
}

### DPomics SERVER RNASEQ heatmap -----------------------------------------------------------------
server_rnaseq_heatmap <- function(input, output, session){}


### DPomics SERVER rnaseq -------------------------------------------------------------------------
dpomics_server_rnaseq <- function(input, output, session){
  
  # text with the name of the input file
  output$rnaseq_inputFile <- renderPrint({ input$rnaseq_input$name })
  
  # read reactive input
  rna_data <- eventReactive(eventExpr = input$submit_rna, { read.delim(input$rnaseq_input$datapath) })
  
  #observeEvent(eventExpr = input$submit_rna, {
  
  server_rnaseq_explore(input, output, session)
  server_rnaseq_volcano(input, output, session)
  server_rnaseq_overlap(input, output, session)
  server_rnaseq_heatmap(input, output, session)
  
  #}) # observeEvent end (submit_rna file)
}