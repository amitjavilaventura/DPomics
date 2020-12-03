###################
## SHINY APP PRO ##
###################

# Shiny app for the visualization of omics data for DPlab

# Load Shiny and required packages.
library(shiny)
source("www/dependencies.R")
source("www/functions.R")

 # increasing capacity of file uploading to 30kb
options(shiny.maxRequestSize = 70*1024^2)

#
ui <- navbarPage(title = "shiny DPlab",
         # Home tab in the navbar --> explain in what the app consists.
         tabPanel(title = "Home",
            titlePanel("What does this app do?")),
         
         #### ----- RNA-SEQ STUFF ----- ####
         tabPanel(title = "RNAseq", 
            titlePanel("RNAseq stuff"),
            sidebarLayout(
                # charge inputs
                sidebarPanel(
                  conditionalPanel(condition = "input.rna_tabset=='rna_exploration'",
                    h3("Select the inputs for the RNAseq"),
                    helpText("Select the input for the RNAseq data visualization. It should be a file named xxxx"),
                    fileInput(inputId = "rnaseq_input", 
                              label = "Select the input file (degs)", placeholder = "Select a file..."),
                    verbatimTextOutput(outputId = "rnaseq_inputFile", placeholder = T),
                    
                    helpText("Once you have selected the file, click to 'Submit'"),
                    actionButton(inputId = "submit_rna", label = "Submit")),
                  
                    conditionalPanel(condition = "input.submit_rna > 0",
                      h3("Select the parameters"),
                      h4("Select the thresholds"),
                      helpText("Select the thresholds for the log2FC and the adjusted-pvalue to define what are DEGs."),
                      sliderInput(inputId = "volcano_log2fc", label = "Select a log2FC threshold", min = 0, max = 3, value = 1.5, step = 0.5, ticks = T),
                      sliderInput(inputId = "volcano_padj", label = "Select an adjusted p-value threshold", min = 0, max = 0.05, value = 0.1, ticks = T),
                    
                      conditionalPanel(condition = "input.rna_tabset=='rna_pca'",
                        h4("Select parameters for the PCA")), # conditionalPanel end
                      
                      conditionalPanel(condition = "input.rna_tabset=='rna_volcanos'",
                        h4("Select parameters for the volcano plots"),
                        helpText("Here you can select the contrast to plot the volcano, as well as the size of the plot title, 
                                 the axis title, the axis text and the number of DEGs."),
                        uiOutput(outputId = "contrasts"), # to create a list with the contrasts in the input file
                        verbatimTextOutput(outputId = "rna_contrastList2", placeholder = T),
                        sliderInput(inputId = "rna_mainSize", label = "Size of the plot title", min = 5, max = 20, value = 10),
                        sliderInput(inputId = "rna_axisText", label = "Size of the axis numbers", min = 5, max = 20, value = 9),
                        sliderInput(inputId = "rna_sizeDEGs", label = "Size of the DEG numbers", min = 5, max = 20, value = 7),
                        br(), helpText("If you want to show the names of the most significant DEGs, 
                                       check the following box and chose the number of labels to show and their size."),
                        checkboxInput("rna_checkLabels", label = "DEGs labels?", value = FALSE),
                        conditionalPanel(condition = "input.rna_checkLabels==true",
                        sliderInput("rna_numLabels", label = "Number of DEG labels to show", min = 0, max = 10, step = 1, value = 3),
                        sliderInput("rna_sizeLabels", label = "Size of the DEG labels", min = 3, max = 9, value = 4)
                        ), # conditionalPanel end
                        helpText("Once you have all the parameters set, clic to 'Generate volcano'."),
                        actionButton("submit_rnaVolcano", "Generate volcano")), # conditionalPanel end
                      

                      conditionalPanel(condition = "input.rna_tabset=='rna_overlaps'",
                        h4("Select parameters for the overlaps"),
                        helpText("Select the contrasts to overlap their DEGs. You can 2 to 4 contrast to overlap."), 
                        uiOutput(outputId = "rna_contrasts2"),
                        verbatimTextOutput(outputId = "rna_contrastList", placeholder = T),
                        
                        helpText("Once you have selected the contrasts to overlap, click to 'Generate overlaps'."),
                        actionButton("submit_rnaOverlaps", "Generate overlaps")),  # conditionalPanel end
                      
                      conditionalPanel(condition = "input.rna_tabset=='rna_heatmaps'",
                        h4("Select the parameters for the log2FC heatmaps"),
                        helpText("Select the genes for which you want to do the heatmap."),
                        uiOutput(outputId = "gene_list_heatmap"),
                        actionButton(inputId = "heatmap_button", label = "Plot heatmap"))
                      
                      
                    ) # conditionalPanel end (submit_rna > 0)
                    
                  ), # rnaseq inputs end
                
                mainPanel(
                  tabsetPanel(id = "rna_tabset",
                    # table of the data (need Degs in the input)
                    tabPanel(title = "Exploration", value = "rna_exploration",
                             h3("Data exploration"),
                             helpText("This is a data table with all the DE information. 
                                      Write a gene inside the search box to see its DE data in all contrasts."),
                             DT::dataTableOutput("rna_dataTable")),
                    
                    # do PCA (need counts in the inputs)
                    tabPanel(title = "PCA", value = "rna_pca",
                             h3("not available yet")), # rnaseq pca end
                    
                    # do DEGs/volcanos, etc (needs DEGs in the inputs)
                    tabPanel(title = "Volcanos", value = "rna_volcanos",
                             h3("Volcano plots"),
                             plotOutput(outputId = "rna_volcano1"),
                             plotOutput(outputId = "rna_volcano2"),
                             plotOutput(outputId = "rna_volcano3"), # this could be modified to be able to see points as genes and their value
                    ), # rnaseq degs end
                          
                    # do DEGs overlaps (need degs in the input)
                    tabPanel(title = "Overlaps", value = "rna_overlaps",
                             h3("Overlaps of DEGs"),
                             h4("All DEGs"),
                             plotOutput(outputId = "degs_overlaps", width = 500, height = 500),
                             h4("Upregulated genes"),
                             plotOutput(outputId = "updegs_overlaps", width = 500, height = 500),
                             h4("Downregulated genes"),
                             plotOutput(outputId = "downdegs_overlaps", width = 500, height = 500)), # DEGs overlaps end
                    
                    tabPanel(title = "Heatmaps", value = "rna_heatmaps",
                             h3("Not available yet"),
                             plotOutput(outputId = "rna_heatmap"))
                   
                  ) #tabsetPanel end
                ) #mainPanel end
                
                
              ) # sidebarLayout end
          ), # RNAseq tabPanel end
         
         #### ----- CHIP-SEQ STUFF ----- ####
         tabPanel(title = "ChIPseq",
            titlePanel(title = "ChIPseq stuff"),
            sidebarLayout(
              sidebarPanel(
                h3("Select the inputs for ChIPseq"),
                fileInput(inputId = "chip_input", label = "Select the input file",
                          buttonLabel = "Browse", placeholder = "Select a file..."),
                verbatimTextOutput(outputId = "chipseq_inputFile", placeholder = T),
                helpText("Once you have selected the file, click to 'Submit'."),
                actionButton(inputId = "submit_chip", label = "Submit"),
                br(),br(),
                
                conditionalPanel(condition = "input.chip_tabset=='chip_peaks_tab'",
                  h4("Select the parameters for the peak distribution plot"),
                  helpText("Once you have selectet the parameters, click to 'Plot peaks'."),
                  actionButton(inputId = "generate_chipPeaks", label = "Plot peaks")),
                
                conditionalPanel(condition = "input.chip_tabset=='chip_annotation'",
                  h4("Select the parameters for the peak distribution plot"),
                  helpText("Select if you want to annotate the peaks as distal and promoter 
                           regions (2) or distal, gene body and promoter regions (3)."),
                  sliderInput(inputId = "chip_numRegions", label = "Select the number of regions to annotate", 
                              min = 2, max = 3, value = 3, step = 1),
                  helpText("Specify whether you want the distribution as a proportion or as the total number of peaks."),
                  selectInput(inputId = "chip_anno_type", label = "Select a type of peak distribution", 
                              choices = c("Proportion", "Total"), selected = "Proportion", multiple = F),
                  helpText("Once you hace selected the parameters, click to 'Plot distribution'."),
                  actionButton("generate_chipAnno", "Plot distribution")
                ),
                
                conditionalPanel(condition = "input.chip_tabset=='chip_overlaps'",
                  h4("Select the parameters for the peak intersections between conditions"),
                  helpText("Select 2, 3 or 4 conditions to plot the peak overlaps."),
                  uiOutput(outputId = "chip_select"),
                  verbatimTextOutput(outputId = "chip_conditions", placeholder = T),
                  helpText("Once you have selected the parameters, click to 'Plot overlaps'."),
                  actionButton(inputId = "generate_chip_vens", label = "Plot overlaps")
                  )
                
              ), # sidebarPanel end
              
              mainPanel(
                tabsetPanel(id = "chip_tabset", selected = "chip_exploration",
                            
                    # TAB FOR DATA EXPLORATION
                    tabPanel(title = "Exploration", value = "chip_exploration",
                        h3("Data exploration"),
                        p("A data table to explore all the peaks, their annotation and their nearest gene."),
                        helpText("Write the name of a gene in the search box to see the peaks that have that gene as the nearest gene. 
                                 You can change the number of entries in the table clicking in the top-left dropdown list."),
                        DT::dataTableOutput(outputId = "chip_datatable")), # tabPanel end (Exploration)
                    
                    tabPanel(title = "Peaks", value = "chip_peaks_tab",
                        h3("ChIPseq peaks in all conditions"),
                        p("How many peaks there are?"),
                        helpText("Select the parameters in the left panel and click to 'Plot peaks'."),
                        plotOutput(outputId = "chip_peaks", width = 800, height = 500)), # tabPanel end (Peaks)
                    
                    tabPanel(title = "Annotation", value = "chip_annotation",
                        h3("Annotation of peaks"),
                        p("Where the peaks are found in the genome?"),
                        helpText("Select the desired parameters in the left panel and click to 'Plot distribution'."),
                        plotOutput(outputId = "chip_anno", width = 800, height = 500)), # tabPanel end (Annotation)
                    
                    tabPanel(title = "Intersections", value = "chip_overlaps",
                        h3("Peak overlaps between conditions"),
                        p("How many peaks overlap between the selected conditions?"),
                        helpText("Select the desired parameters in the left panel and click to 'Plot overlaps'."),
                        plotOutput(outputId = "chip_intersections", width = 600, height = 600)) # tabPanel end (Intersection)
                    
                ) # tabsetPanel end     
              ) # mainPanel end
            ), # sidebarlayout end
            
         ), # Chipseq tabpanel end
         
         tabPanel(title = "Integration") # tabPanel end, integration.
         
    ) # navbarPage end


# Define server logic
server <- function(input, output) {
  
  
  ### ----- RNASEQ STUFF ----- ###
  # text with the name of the file
  output$rnaseq_inputFile <- renderPrint({ input$rnaseq_input$name })
  
  observeEvent(eventExpr = input$submit_rna, {
  # INPUTS ----------
  # reactive input
  rna_data <- reactive({ read.delim(input$rnaseq_input$datapath) })
  
  # EXPLORATION TAB ----- 
  # table with the data of the table
  output$rna_dataTable <- DT::renderDataTable({
    degs <- annotate_de(degs = rna_data(), log2FC =  input$volcano_log2fc, padj =  input$volcano_padj)
    DT::datatable(data = degs, options = list(lengthMenu = c(5, 10, 30, 50), pageLength = length(unique(degs$Contrast))))
  })
  
  # PCA TAB ---------
  
  # VOLCANO TAB ---------
  # define contrasts to select
  output$contrasts <- renderUI({
    data <- rna_data()
    contrasts <- data$Contrast %>% unique
    selectizeInput(inputId = "rna_contrasts", label = "Select a contrasts for the volcanoPlot",
                choices = contrasts, selected = NULL, multiple = T, 
                options = list(maxItems = 3, placeholder = "Select 1, 2 or 3 contrasts..."))
  })
  output$rna_contrastList2 <- renderPrint({ paste(input$rna_contrasts, sep = ";") })
  
  observeEvent(eventExpr = input$submit_rnaVolcano, {
  # volcanoPlot1
  output$rna_volcano1 <- renderPlot({
    data <- rna_data()
    degs <- annotate_de(degs = data, log2FC = input$volcano_log2fc, padj = input$volcano_padj)
    p <- volcanoPlot2(df = degs[which(degs$Contrast == input$rna_contrasts[1]),], 
                      log2FC = input$volcano_log2fc, pval = input$volcano_padj, 
                      main = input$rna_contrasts[1], mainSize = input$rna_mainSize, labelSize = input$rna_sizeDEGs, 
                      axisLabelSize = input$rna_axisText, axisTextSize = input$rna_axisText,
                      degsLabel = input$rna_checkLabels, degsLabelNum = input$rna_numLabels, degsLabelSize = input$rna_sizeLabels)
    p
  }) 
  # volcanoPlot2
  output$rna_volcano2 <- renderPlot({
    data <- rna_data()
    degs <- annotate_de(degs = data, log2FC = input$volcano_log2fc, padj = input$volcano_padj)
    p <- volcanoPlot2(df = degs[which(degs$Contrast == input$rna_contrasts[2]),], 
                      log2FC = input$volcano_log2fc, pval = input$volcano_padj, 
                      main = input$rna_contrasts[2], mainSize = input$rna_mainSize, labelSize = input$rna_sizeDEGs, 
                      axisLabelSize = input$rna_axisText, axisTextSize = input$rna_axisText,
                      degsLabel = input$rna_checkLabels, degsLabelNum = input$rna_numLabels, degsLabelSize = input$rna_sizeLabels)
    p
  })
  # volcanoPlot3
  output$rna_volcano3 <- renderPlot({
    data <- rna_data()
    degs <- annotate_de(degs = data, log2FC = input$volcano_log2fc, padj = input$volcano_padj)
    p <- volcanoPlot2(df = degs[which(degs$Contrast == input$rna_contrasts[3]),], 
                      log2FC = input$volcano_log2fc, pval = input$volcano_padj, 
                      main = input$rna_contrasts[3], mainSize = input$rna_mainSize, labelSize = input$rna_sizeDEGs, 
                      axisLabelSize = input$rna_axisText, axisTextSize = input$rna_axisText,
                      degsLabel = input$rna_checkLabels, degsLabelNum = input$rna_numLabels, degsLabelSize = input$rna_sizeLabels)
    p
  }) 
  }) # observeEvent submit_rnaVolcano end
  
  # OVERLAP DEGs ---------- 
  # get contrasts to select
  output$rna_contrasts2 <- renderUI({
    data <- rna_data()
    contrasts <- data$Contrast %>% unique
    selectizeInput(inputId = "contrastDegs", choices = contrasts, multiple = T, 
                   label = paste("Choose between 2 and 4 contrasts", sep = " "), 
                   options = list(maxItems = 4, placeholder = "Select the contrasts..."))
  })
  observeEvent(eventExpr = input$submit_rnaOverlaps, {
  # get contrast list output
  output$rna_contrastList <- renderPrint({ paste(input$contrastDegs, sep = ";") })
  
  # do venn diagrams
  ## all degs
  output$degs_overlaps <- renderPlot({
    data <- rna_data()
    data <- data %>% dplyr::filter(DEG != "NS")
    list <- list()
    for (i in 1:length(input$contrastDegs)){
      x <- data %>% dplyr::filter(Contrast == input$contrastDegs[i]) %>% dplyr::select(Geneid)
      list[[i]] <- x$Geneid
    }
    colors <- rainbow(n = length(input$contrastDegs), alpha = 0.6)
    cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs, fill = colors))
  })
  
  ## upregulated genes
  output$updegs_overlaps <- renderPlot({
    data <- rna_data()
    data <- data %>% dplyr::filter(DEG == "Upregulated")
    list <- list()
    for (i in 1:length(input$contrastDegs)){
      x <- data %>% dplyr::filter(Contrast == input$contrastDegs[i]) %>% dplyr::select(Geneid)
      list[[i]] <- x$Geneid
    }
    colors <- rainbow(n = length(input$contrastDegs), alpha = 0.6)
    cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs, fill = colors))
  })
  
  ## downregulated genes
  output$downdegs_overlaps <- renderPlot({
    data <- rna_data()
    data <- data %>% dplyr::filter(DEG == "Downregulated")
    list <- list()
    for (i in 1:length(input$contrastDegs)){
      x <- data %>% dplyr::filter(Contrast == input$contrastDegs[i]) %>% dplyr::select(Geneid)
      list[[i]] <- x$Geneid
    }
    colors <- rainbow(n = length(input$contrastDegs), alpha = 0.6)
    cowplot::plot_grid(venn.diagram(filename = NULL, x = list, category.names = input$contrastDegs, fill = colors))
  })
  }) # observeEvent end (actionButton submit_rnaOverlaps)
  
  # HEATMAPS TAB -----------
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
  }) # observeEvent end (action_Button heatmap_button)
  
  }) # observeEvent end (actionButton rna_submit)
  
  ### ---- CHIPseq STUFF ----- ### 
  output$chipseq_inputFile <- renderPrint({ input$chip_input$name })
  
  observeEvent(eventExpr = input$submit_chip, {
    
    # read peaks file
    chip_data <- reactive({ read.delim(input$chip_input$datapath) })
    
    ### EXPLORATION TAB ----------
    # dataTable for exploration
    output$chip_datatable <- DT::renderDataTable({
      peaks <- chip_data()
      DT::datatable(data = peaks,  options = list(lengthMenu = c(5, 10, 20, 30, 50), pageLength = 10))
    })
    
    ### PEAKS TAB ----------
    observeEvent(eventExpr = input$generate_chipPeaks, {
    # plot number of peaks
    output$chip_peaks <- renderPlot({
      peaks <- chip_data()
      peakNum(peaks)
    })
    }) # ovserveEvent end (actionButton generate_peaks)
    
    ### ANNOTATION TAB ----------
    observeEvent(eventExpr = input$generate_chipAnno, {
    # plot distribution of peaks
    output$chip_anno <- renderPlot({
      peaks <- chip_data()
      barAnno(data = peaks, names = peaks$condition %>% unique(), 
              anno_num = input$chip_numRegions, peak_type = input$chip_anno_type)
    })
    }) # ovserveEvent end (actionButton generate_barAnno)
    
    ### INTERSECTIONS TAB -----------
    # UI for condition selection
    output$chip_select <- renderUI({
      peaks <- chip_data()
      conditions <- peaks$condition %>% unique
      selectizeInput(inputId = "chip_conditions", label = "Select the conditions to overlap the peaks",
                     choices = conditions, selected = NULL, multiple = T, 
                     options = list(maxItems = 4, placeholder = "Select between 2 and 4 conditions"))
    })
    
    # output conditions
    output$chip_conditions <- renderPrint({ paste(input$chip_conditions, sep = "; ") })
    
    observeEvent(eventExpr = input$generate_chip_vens, {
    # plot intersections
    output$chip_intersections <- renderPlot({
      peaks <- chip_data()
      intersectPeaks(peaks = peaks, conditions = input$chip_conditions)
    })
    }) # ovserveEvent end (actionButton generate_venns)
    
  }) # ovserveEvent end (actionButton chip_submit)
  
  
} # SERVER FUNCTION END

# Run the application 
shinyApp(ui = ui, server = server)