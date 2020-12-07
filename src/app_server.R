### ======================================================================
### DPomics SERVER FUNCTION
### ======================================================================

# SERVER FUNCTION
server <- function(input, output) {

  ### ----- RNA-SEQ STUFF ----- ###
  # text with the name of the file
  output$rnaseq_inputFile <- renderPrint({ input$rnaseq_input$name })
  
  # INPUTS ----------------------------------------------------------------------------------------------------------------------
  # reactive input
  rna_data <- eventReactive(eventExpr = input$submit_rna, { read.delim(input$rnaseq_input$datapath) })
  
  observeEvent(eventExpr = input$submit_rna, {

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
    
    output$volcano_contrasts_list <- renderPrint({ input$rna_contrasts })
    
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
                    options = list(maxItems = 3, placeholder = "Select 1 contrasts..."))
    })
    output$rna_overlap2_contrasts <- renderUI({
      data <- rna_data()
      contrasts <- data$Contrast %>% unique
      selectizeInput(inputId = "contrastDegs2", choices = contrasts, multiple = T, label = paste("Select the contrasts to overlap", sep = " "), 
                     options = list(maxItems = 3, placeholder = "Select 2 or 3 contrasts..."))
    })
    
    # render verbatimTextOutputs rna_contrast1_list rna_contrast2_list
    output$rna_contrast1_list <- renderPrint({ print(input$contrastDegs1) })
    output$rna_contrast2_list <- renderPrint({ print(input$contrastDegs2) })
    
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
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = isolate(input$contrastDegs1), fill = colors))
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
  
  
  ### ----- CHIP-SEQ STUFF ----- ###  
  output$chipseq_inputFile <- renderPrint({ input$chip_input$name })
  
  # read peaks file
  chip_data <- eventReactive(eventExpr = input$submit_chip, { read.delim(input$chip_input$datapath) })

  observeEvent(eventExpr = input$submit_chip, {
    
    ### EXPLORATION TAB ----------------------------------------------------------------------------------------------------
    # dataTable for exploration
    output$chip_dataTable <- DT::renderDataTable({
      peaks <- chip_data()
      DT::datatable(data = peaks,  options = list(lengthMenu = c(5, 10, 20, 30, 50), pageLength = 10))
    })
    
    ### PEAKS TAB ----------------------------------------------------------------------------------------------------------
    observeEvent(eventExpr = input$generate_chipPeaks, {
      # plot number of peaks
      output$chip_peaks <- renderPlot({
        peaks <- chip_data()
        peakNum(data = peaks, legend_position = input$chip_peaks_legend, pannel = input$chip_peaks_pannel)
      })
    }) # ovserveEvent end (actionButton generate_peaks)
    
    ### ANNOTATION TAB -----------------------------------------------------------------------------------------------------
    observeEvent(eventExpr = input$generate_chipAnno, {
      # plot distribution of peaks
      output$chip_anno <- renderPlot({
        peaks <- chip_data()
        barAnno(data = peaks, names = peaks$sample %>% unique(), pannel = input$chip_anno_pannel,
                anno_num = input$chip_numRegions, peak_type = input$chip_anno_type, palette = "Set2", ylab = paste(input$chip_anno_type, "of peaks"))
      })
    }) # ovserveEvent end (actionButton generate_barAnno)
    
    ### INTERSECTION TAB ---------------------------------------------------------------------------------------------------
    # get selectInputs for conditions to intersect
    output$ui_chip_intersect1 <- renderUI({
      peaks <- chip_data()
      samples <- peaks$sample %>% unique
      selectizeInput(inputId = "chip_conditions1", label = "Select the conditions to overlap the peaks",
                     choices = samples, selected = NULL, multiple = T, 
                     options = list(maxItems = 3, placeholder = "Select between 2 and 3 conditions"))
    })
    output$chip_cond_list1 <- renderPrint({ paste(input$chip_conditions1, sep = "; ") })
    
    output$ui_chip_intersect2 <- renderUI({
      peaks <- chip_data()
      samples <- peaks$sample %>% unique
      selectizeInput(inputId = "chip_conditions2", label = "Select the conditions to overlap the peaks",
                     choices = samples, selected = NULL, multiple = T, 
                     options = list(maxItems = 3, placeholder = "Select between 2 and 3 conditions"))
    })
    
    output$chip_cond_list2 <- renderPrint({ paste(input$chip_conditions2, sep = "; ") })
    
    # plot intersections
    observeEvent(eventExpr = input$generate_chip_intersection1, {
      output$chip_intersect1 <- renderPlot({
        peaks <- chip_data()
        intersectPeaks(peaks = peaks, conditions = input$chip_conditions1)
      })
    })
    
    observeEvent(eventExpr = input$generate_chip_intersection2, {
      output$chip_intersect2 <- renderPlot({
        peaks <- chip_data()
        intersectPeaks(peaks = peaks, conditions = input$chip_conditions2)
      })
    })
  }) #observeEvent submit chip end
  
  ###############################
  ### ----- INTEGRATION ----- ###  
  ### ===== CHIP + RNA ===== ###
  # EXPLORE TAB ----------------
  # get ui to select contrasts etc
  output$ui_int_rna_contrast1 <- renderUI({
    data <- rna_data()
    contrasts <- data$Contrast %>% unique
    selectizeInput(inputId = "int_rna_contrast1", choices = contrasts, multiple = T,
                   options = list(placeholder = "Select a contrast..."), label = "Contrast")
  })
  output$chip_proteins_select <- renderUI({
    data <- chip_data()
    protein <- data$protein %>% unique
    selectizeInput(inputId = "chip_proteins_select", choices = protein, multiple = F,
                   options = list(placeholder = "Select a protein..."), label = "Chipped protein")
  })
  
  observeEvent(eventExpr = input$intersect_rna_chip, {
    
    output$int_chip_cont1_cond1 <- renderPrint({
      data <- rna_data()
      cond1 <- (data %>% filter(Contrast %in% input$int_rna_contrast1) %>% select(Contrast, cond1) %>% unique() %>% select(cond1))$cond1 %>% as.character()
      print(cond1)
    })  
    output$int_chip_cont1_cond2 <- renderPrint({
      data <- rna_data()
      cond2 <- (data %>% filter(Contrast %in% input$int_rna_contrast1) %>% select(Contrast, cond2) %>% unique() %>% select(cond2))$cond2 %>% as.character()
      print(cond2)
    })
    
    # peak subtraction
    peak_subtraction <- reactive({
      rna_data <- rna_data()
      chip_data <- chip_data()
      peak_subtracted_list <- list
      for(i in 1:length(input$int_rna_contrast1)){
      cond1 <- (rna_data %>% filter(Contrast == input$int_rna_contrast1[i]) %>% select(Contrast, cond1) %>% unique() %>% select(cond1))$cond1 %>% as.character()
      cond2 <- (rna_data %>% filter(Contrast == input$int_rna_contrast1[i]) %>% select(Contrast, cond2) %>% unique() %>% select(cond2))$cond2 %>% as.character()
      chip1 <- chip_data %>% filter(condition == cond1, protein == input$chip_proteins_select) %>% as_granges()
      chip2 <- chip_data %>% filter(condition == cond2, protein == input$chip_proteins_select) %>% as_granges()
      peak_subtraction <- plyranges::filter_by_overlaps(chip1, chip2)
      peak_subtracted_list[[i]] <- peak_subtraction %>% as_data_frame()
      }
      
      peak_subtracted_list
    })
    
    # do interesections
    degs_with_peaks <- reactive({
      rna_data <- rna_data()
      chip_peaks <- peak_subtraction()
      degs_with_peaks <- list()
      for(i in 1:length(input$int_rna_contrast1)){
        degs_with_peaks[[i]] <- rna_data %>% filter(Contrast == input$int_rna_contrast1[i], Geneid %in% chip_peaks[[i]]$SYMBOL)
      }
      degs_with_peaks
    })
    
    peaks_in_degs <- reactive({
      rna_data <- rna_data() 
      chip_peaks <- peak_subtraction()
      peaks_in_degs <- list()
      for(i in 1:length(input$int_rna_contrast1)){
        peaks_in_degs[[i]] <- chip_peaks[[i]] %>% filter(SYMBOL %in% (rna_data %>% filter(Contrast == input$int_rna_contrast1[i]))$Geneid)
      }
      peaks_in_degs
    })
    
    # do data tables
    output$degs_with_peaks_dt <- DT::renderDT({ DT::datatable(data = degs_with_peaks() %>% bind_rows(), options = list(lengthMenu = c(5, 10, 20), pageLength = 10)) })
    output$peaks_in_degs_dt   <- DT::renderDT({ DT::datatable(data = peaks_in_degs() %>% bind_rows(), options = list(lengthMenu = c(5, 10, 20), pageLength = 10)) })
    
    # plot volcano plot 
    observeEvent(eventExpr = input$int_rnachip_volcano, {
      output$int_rna_chip_volcanos <- renderPlot(height = input$int_volcanosHeight, {
        degs_with_peaks <- degs_with_peaks()
        volcanoList <- list()
        for(i in 1:length(input$int_rna_contrast1)){
          p <- volcanoPlot2(df = degs_with_peaks[[i]], log2FC = input$rna_log2fc, pval = input$rna_padj,  main = input$int_rna_contrast1[i], 
                            sub = input$input$chip_proteins_select, mainSize = input$int_rna_mainSize, subSize = input$int_rna_subSize, 
                            labelSize = input$int_rna_sizeDEGs, legendPos = "bottom", degsLabel = input$int_rna_checkLabels, 
                            degsLabelNum = input$int_rna_numLabels, degsLabelSize = input$int_rna_sizeLabels,
                            axisTextSize = input$int_rna_axisText, axisLabelSize = input$int_rna_axisText)
          volcanoList[[i]] <- p
        }
        ggarrange(plotlist = volcanoList, ncol = 2, nrow = ceiling(length(volcanoList)/2), common.legend = T, legend = "bottom")
      })
    }) # observeEvent end actionButton make volcano rna_chip intersection 
  }) # observeEvent intersect_rna_chip end
} ## SERVER FUNCTION END