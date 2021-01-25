### ======================================================================
### DPomics SERVER FUNCTION
### ======================================================================

# Load source files -------------------------------------------------------------------------------
source("src/app_server_rnaseq.R")

# SERVER FUNCTION ---------------------------------------------------------------------------------
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
    
    # plot volcaons
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

    
    # plot venn diagrams 1
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
        for (i in 1:length(isolate(input$contrastDegs2))){
          x <- data %>% dplyr::filter(Contrast == isolate(input$contrastDegs1[i])) %>% dplyr::select(Geneid)
          list[[i]] <- x$Geneid
        }
        colors <- rainbow(n = length(isolate(input$contrastDegs2)), alpha = 0.6)
        cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = isolate(input$contrastDegs2), fill = colors))
      })
      
    }) # observeEvent end (actionButton generate overlaps1)
    
    # plot venn diagrams 2
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
        colors <- rainbow(n = length(isolate(input$contrastDegs2), alpha = 0.6))
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
      x <- data %>% select(Contrast, Geneid, log2FoldChange, padj, DEG)
      l <- list()
      for(i in x$Contrast %>% unique){
        z <- x %>% dplyr::filter(Contrast == i)  %>% set_colnames(c("Contrast", "Geneid", i, paste(i,"DEG", sep = "_"), paste(i,"padj", sep = "_"))) %>% 
          dplyr::select(everything(), -Contrast)
        l[[i]] <- z
      }
      l %>% plyr::join_all()
    })
    
    # plot heatmap
    observeEvent(eventExpr = input$heatmap_button, {
      output$rna_heatmap <- renderPlot({
        #input$heatmap_button
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
        peakNum(data = peaks, legend_position = isolate(input$chip_peaks_legend), pannel = isolate(input$chip_peaks_pannel))
      })
    }) # ovserveEvent end (actionButton generate_peaks)
    
    ### ANNOTATION TAB -----------------------------------------------------------------------------------------------------
    observeEvent(eventExpr = input$generate_chipAnno, {
      # plot distribution of peaks
      output$chip_anno <- renderPlot({
        peaks <- chip_data()
        barAnno(data = peaks, names = peaks$sample %>% unique(), pannel = isolate(input$chip_anno_pannel),
                anno_num = isolate(input$chip_numRegions), peak_type = isolate(input$chip_anno_type), palette = "Set2", ylab = paste(isolate(input$chip_anno_type), "of peaks"))
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
        intersectPeaks(peaks = peaks, conditions = isolate(input$chip_conditions1))
      })
    })
    
    observeEvent(eventExpr = input$generate_chip_intersection2, {
      output$chip_intersect2 <- renderPlot({
        peaks <- chip_data()
        intersectPeaks(peaks = peaks, conditions = isolate(input$chip_conditions2))
      })
    })
    
    ### CUSTOM COORDINATES TAB ---------------------------------------------------------------------------------------------------
    output$uploaded_coords_name <- renderPrint({ input$upload_coords$name })
    
    custom_coords <- eventReactive(eventExpr = input$upload_coords_button, {
      if(input$upload_coords_header == T){read.delim(file = input$upload_coords$datapath, header = input$upload_coords_header, sep = input$upload_coords_sep)}
      else{read.delim(file = input$upload_coords$datapath, header = input$upload_coords_header, sep = input$upload_coords_sep) %>%
          set_colnames(input$custom_header %>% stringr::str_split(pattern = " ") %>% unlist())}
      
    })
    
    observeEvent(eventExpr = input$upload_coords_button, {
      output$coords_chip <- DT::renderDataTable({
        custom_coords <- custom_coords() %>% set_colnames(c("seqnames", "start", "end")) %>% as_granges()
        chip_data <- chip_data() %>% as_granges()
        coords_chip <- find_overlaps(x = custom_coords, y = chip_data) %>% as_data_frame
        DT::datatable(data = coords_chip)
      })
      
    })
    
    
  }) #observeEvent submit chip end
  
  ###############################
  ### ----- INTEGRATION ----- ###  
  ### ===== CHIP + RNA ===== ###
  # peak subtraction
  peak_subtract <- eventReactive(eventExpr = input$peak_subtraction_button, {
    rna_data <- rna_data()
    chip_data <- chip_data()
    
    contrasts <- rna_data$Contrast %>% unique %>% as.character()
    
    peak_contrast_list <- list()
    for(i in 1:length(contrasts)){
      
      rna <- rna_data %>% filter(Contrast == contrasts[i])
      rna <- rna %>% mutate(contrast2 = paste(cond1, "_", cond2, sep = ""))
      contrast <- rna$contrast2 %>% unique()
      
      if( rna$celltype %>% unique() %in% chip_data$celltype %>% unique()) {
        
        # take chip peaks
        chip <- chip_data %>% filter(celltype %in% rna$celltype, protein == input$int_chip_rna_protein)
        chip1 <- chip %>% filter(condition %in% rna$cond1) %>% as_granges()
        chip2 <- chip %>% filter(condition %in% rna$cond2) %>% as_granges()
        
        # conditions names
        cond1 <- chip1$condition %>% unique() %>% as.character()
        cond2 <- chip2$condition %>% unique() %>% as.character()
        
        # peak subtraction
        chip1_chip2 <- filter_by_non_overlaps(chip1, chip2) %>% as_tibble() %>% 
          select(everything(), -condition, -sample) %>% 
          mutate(contrast = contrast) %>% mutate(cond1 = cond1) %>% mutate(cond2 = cond2)
        
        peak_contrast_list[[contrast]] <- chip1_chip2
       
      }
      else{ paste("Celltype", rna$celltype %>% unique(), "in RNAseq is not in ChIPseq.") }
    }
    
    peak_contrast_list
    
  })
  
  observeEvent(eventExpr = input$peak_subtraction_button, {
  # data tables
  output$degs_near_peaks <- DT::renderDataTable({
    d <- list()
    peak_contrast_list <- peak_subtract()
    rna_data <- rna_data()
    for(i in 1:length(peak_contrast_list)){
      
      rna <- rna_data %>% 
        filter(celltype == peak_contrast_list[[i]]$celltype %>% as.character %>% unique) %>% 
        filter(cond1 == peak_contrast_list[[i]]$cond1 %>% as.character %>% unique) %>% 
        filter(cond2 == peak_contrast_list[[i]]$cond2 %>% as.character %>% unique) %>% 
        filter(DEG != "NS")
      
      
      degs_with_peaks <- rna %>% filter(Geneid  %in% peak_contrast_list[[i]]$SYMBOL)
      
      d[[i]] <- degs_with_peaks
    }
    
    d %>% bind_rows
  })
  
  
  # VOLCANO PLOTS
  observeEvent(input$plot_int_volcanos, {
  output$ui_int_volcanos <- renderUI({ plotOutput(outputId = "int_chiprna_volcano", width = 1200, height = isolate(input$int_volcanosHeight)) })
  output$int_chiprna_volcano <- renderPlot({
    d <- list()
    peak_contrast_list <- peak_subtract()
    rna_data <- rna_data()
    for(i in 1:length(peak_contrast_list)){
      
      rna <- rna_data %>% 
        filter(celltype == peak_contrast_list[[i]]$celltype %>% as.character %>% unique) %>% 
        filter(cond1 == peak_contrast_list[[i]]$cond1 %>% as.character %>% unique) %>% 
        filter(cond2 == peak_contrast_list[[i]]$cond2 %>% as.character %>% unique) %>% 
        filter(DEG != "NS")
      
      
      degs_with_peaks <- rna %>% filter(Geneid  %in% peak_contrast_list[[i]]$SYMBOL)
      
      d[[i]] <- degs_with_peaks
    }
    
    volcanos <- list()
    for(i in 1:length(d)){
      v <- volcanoPlot2(df = d[[i]], main = "DEGs targeted by/close to peaks", sub = d[[i]]$Contrast %>% unique())
      volcanos[[i]] <- v
    }
    ggarrange(plotlist = volcanos, ncol = 2, nrow = ceiling(length(volcanos)/2), common.legend = T)
    
  })
     
  })
  
  })
  
} ## SERVER FUNCTION END