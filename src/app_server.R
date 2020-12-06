### ======================================================================
### DPomics SERVER FUNCTION
### ======================================================================

# SERVER FUNCTION
server <- function(input, output) {

  ### ----- RNA-SEQ STUFF ----- ###
  # text with the name of the file
  output$rnaseq_inputFile <- renderPrint({ input$rnaseq_input$name })
  
  observeEvent(eventExpr = input$submit_rna, {
    # INPUTS ----------------------------------------------------------------------------------------------------------------------
    # reactive input
    rna_data <- reactive({ read.delim(input$rnaseq_input$datapath) })

    # EXPLORATION TAB ------------------------------------------------------------------------------------------------------------ 
    # table with the data of the table
    output$rna_dataTable <- DT::renderDataTable({
      degs <- annotate_de(degs = rna_data(), log2FC =  input$rna_log2fc, padj =  input$rna_padj)
      DT::datatable(data = degs, options = list(lengthMenu = c(5, 10, 30, 50), pageLength = length(unique(degs$Contrast))))
    })
    
    # VOLCANO TAB ----------------------------------------------------------------------------------------------------------------
    # define contrasts to select
    output$volcano_contrasts <- renderUI({
      data <- rna_data()
      contrasts <- data$Contrast %>% unique
      selectizeInput(inputId = "rna_contrasts", label = "Select a contrasts for the volcanoPlot",
                     choices = contrasts, selected = NULL, multiple = T, 
                     options = list(placeholder = "Select the contrasts..."))
    })
    output$volcano_contrasts_list <- renderText({ paste(input$rna_contrasts, sep = ";") })
    
    # plot volcaons
    observeEvent(eventExpr = input$plot_volcano, {
      output$ui_rna_volcanos <- renderUI({ plotOutput(outputId = "rna_volcanos", width = 1200, height = input$volcanosHeight) })
      output$rna_volcanos <- renderPlot({
        data <- rna_data()
        degs <- annotate_de(degs = data, log2FC = input$rna_log2fc, padj = input$rna_padj)
        plotlist <- list()
        for(i in 1:length(input$rna_contrasts)){
          p <- volcanoPlot2(df = degs[which(degs$Contrast == input$rna_contrasts[i]),], 
                            log2FC = input$rna_log2fc, pval = input$rna_padj, 
                            main = input$rna_contrasts[i], mainSize = input$rna_mainSize, labelSize = input$rna_sizeDEGs, 
                            axisLabelSize = input$rna_axisText, axisTextSize = input$rna_axisText,
                            degsLabel = input$rna_checkLabels, degsLabelNum = input$rna_numLabels, degsLabelSize = input$rna_sizeLabels)
          plotlist[[i]] <- p
        }
        ggarrange(plotlist = plotlist, ncol = 2, nrow = ceiling(length(input$rna_contrasts)/2),legend = "bottom", common.legend = T)
      })
      
    }) # observeEvent submit_rnaVolcano end
    
    # OVERLAP DEGS TAB -----------------------------------------------------------------------------------------------------------
    # renuder UIs to select contrasts
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
    
    # render verbatimTextOutputs rna_contrast1_list rna_contrast2_list
    output$rna_contrast1_list <- renderPrint({ print(input$rna_overlap1_contrasts) })
    
    output$rna_contrast2_list <- renderPrint({ print(input$rna_overlap2_contrasts) })
    
    # plot venn diagrams 1
    observeEvent(input$submit_rnaOverlaps1, {
      ## all degs
      output$degs_overlaps1 <- renderPlot({
        data <- rna_data()
        data <- data %>% dplyr::filter(DEG != "NS")
        list <- list()
        for (i in 1:length(input$contrastDegs1)){
          x <- data %>% dplyr::filter(Contrast == input$contrastDegs1[i]) %>% dplyr::select(Geneid)
          list[[i]] <- x$Geneid
        }
        colors <- rainbow(n = length(input$contrastDegs1), alpha = 0.6)
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs1, fill = colors))
      })
      
      ## upregulated genes
      output$updegs_overlaps1 <- renderPlot({
        data <- rna_data()
        data <- data %>% dplyr::filter(DEG == "Upregulated")
        list <- list()
        for (i in 1:length(input$contrastDegs1)){
          x <- data %>% dplyr::filter(Contrast == input$contrastDegs1[i]) %>% dplyr::select(Geneid)
          list[[i]] <- x$Geneid
        }
        colors <- rainbow(n = length(input$contrastDegs1), alpha = 0.6)
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs1, fill = colors))
      })
      
      ## downregulated genes
      output$downdegs_overlaps1 <- renderPlot({
        data <- rna_data()
        data <- data %>% dplyr::filter(DEG == "Downregulated")
        list <- list()
        for (i in 1:length(input$contrastDegs1)){
          x <- data %>% dplyr::filter(Contrast == input$contrastDegs1[i]) %>% dplyr::select(Geneid)
          list[[i]] <- x$Geneid
        }
        colors <- rainbow(n = length(input$contrastDegs1), alpha = 0.6)
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs1, fill = colors))
      })
      
    }) # observeEvent end (actionButton generate overlaps1)
    
    # plot venn diagrams 2
    observeEvent(input$submit_rnaOverlaps2, {
      ## all degs
      output$degs_overlaps2 <- renderPlot({
        data <- rna_data()
        data <- data %>% dplyr::filter(DEG != "NS")
        list <- list()
        for (i in 1:length(input$contrastDegs2)){
          x <- data %>% dplyr::filter(Contrast == input$contrastDegs2[i]) %>% dplyr::select(Geneid)
          list[[i]] <- x$Geneid
        }
        colors <- rainbow(n = length(input$contrastDegs2), alpha = 0.6)
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs2, fill = colors))
      })
      
      ## upregulated genes
      output$updegs_overlaps2 <- renderPlot({
        data <- rna_data()
        data <- data %>% dplyr::filter(DEG == "Upregulated")
        list <- list()
        for (i in 1:length(input$contrastDegs2)){
          x <- data %>% dplyr::filter(Contrast == input$contrastDegs2[i]) %>% dplyr::select(Geneid)
          list[[i]] <- x$Geneid
        }
        colors <- rainbow(n = length(input$contrastDegs2), alpha = 0.6)
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs2, fill = colors))
      })
      
      ## downregulated genes
      output$downdegs_overlaps2 <- renderPlot({
        data <- rna_data()
        data <- data %>% dplyr::filter(DEG == "Downregulated")
        list <- list()
        for (i in 1:length(input$contrastDegs2)){
          x <- data %>% dplyr::filter(Contrast == input$contrastDegs2[i]) %>% dplyr::select(Geneid)
          list[[i]] <- x$Geneid
        }
        colors <- rainbow(n = length(input$contrastDegs2), alpha = 0.6)
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs2, fill = colors))
      })
    }) # observeEvent end (actionButton generate overlaps2)
    
    # HEATMAPS TAB -----------------------------------------------------------------------------------------------------------
    # get gene lists
    output$gene_list_heatmap <- renderUI({
      data <- rna_data()
      genes <- as.character(data$Geneid)
      textInput(inputId = "select_genes_heatmap", label = "Write a list of genes separated by blank spaces")
    })
    
    # format matrix of data
    log2fc <- reactive({
      data  <- rna_data()
      x <- data %>% select(Contrast, Geneid, log2FoldChange, DEG)
      l <- list()
      for(i in x$Contrast %>% unique){
        z <- x %>% dplyr::filter(Contrast == i)  %>% set_colnames(c("Contrast", "Geneid", i, paste(i,"DEG", sep = "_"))) %>% 
          dplyr::select(everything(), -Contrast)
        l[[i]] <- z
      }
      l %>% plyr::join_all()
    })
    
    # plot heatmap
    observeEvent(eventExpr = input$heatmap_button, {
      output$rna_heatmap <- renderPlot({
        genes <- input$select_genes_heatmap
        genes <- genes %>% stringr::str_split(pattern = " ") %>% unlist()
        l <- log2fc() %>% dplyr::filter(Geneid %in% genes)
        log2FC <- l %>% dplyr::select(-matches("_DEG")) %>% column_to_rownames("Geneid") 
        ComplexHeatmap::Heatmap(matrix = as.matrix(log2FC), name = "Log2FC", show_row_names = T,
                                column_title = "Log2FC of selected genes", column_title_side = "top",
                                col = colorRamp2(c(-3, 0, 3), c("blue", "white", "red")),
                                cell_fun = function(j, i, x, y, width, height, fill) {
                                  grid.text(sprintf("%.1f", as.matrix(log2FC)[i, j]), x, y, gp = gpar(fontsize = 10))
                                })
      })
    }) # observe event actionButton heatmap
        
  }) # observeEvent end (submit_rna file)
  
} ## SERVER FUNCTION END