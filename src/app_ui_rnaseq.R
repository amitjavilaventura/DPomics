
### MYomics USER INTERFACE                                                 
### =========================================================================================== ###

### MYomics UI RNA-seq Principal Component -------------------------------------------------------------------
dpomics_ui_rnaseq_pca <- 
  tabItem(tabName = "rnaseq_pca")

### MYomics UI RNA-seq Gene Exp -------------------------------------------------------------------
dpomics_ui_rnaseq_geneexp <- 
tabItem(tabName = "rnaseq_geneexp")


### MYomics UI RNA-seq Diff Exp -------------------------------------------------------------------
dpomics_ui_rnaseq_de <- 
tabItem(tabName = "rnaseq_diffexp",
        h3("RNA-seq - Differential expression analysis"),
        tabsetPanel(id = "rnaseq_de_tabset", type = "pills",
                    # EXPLORE THE DATA
                    tabPanel(title = "Upload & explore", value = "explore", br(),
                             splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                         box(title = "RNA-seq inputs", id = "rna_inputs_box", height = "300px", width = 12, collapsible = T, collapsed = F,
                                             helpText("Select the input for the RNAseq data visualization and click to 'Upload'."),
                                             fileInput(inputId = "rnaseq_input", label = "Browse", placeholder = "Select a file..."),
                                             verbatimTextOutput(outputId = "rnaseq_inputFile", placeholder = T),
                                             actionButton(inputId = "submit_rna", label = "Upload")),
                                         box(title = "DE thresholds", id = "rna_thresholds", height = "300px", width = 12, collapsible = T, collapsed = F,
                                             helpText("Select the thresholds for log2FC and adjusted p-value"),
                                             sliderInput(inputId = "rna_log2fc", label = "Select a log2FC threshold", min = 0, max = 3, value = 1.5, step = 0.5, ticks = T),
                                             sliderInput(inputId = "rna_padj", label = "Select an adjusted p-value threshold", min = 0, max = 0.1, value = 0.05, ticks = T))),
                             
                             br(),
                             conditionalPanel(condition = "input.submit_rna > 0",
                                              h3("Explore the data!"),
                                              helpText("This is a data table with all the DE information. Write a gene inside the search box to see its DE data in all contrasts."),
                                              DT::dataTableOutput("rna_dataTable"))),
                    
                    # VOLCANO PLOTS
                    tabPanel(title = "Volcano plots", value = "volcanos", br(),
                             ## PARAMS VOLCANOS
                             splitLayout(cellWidths = c("30%", "70%"),  cellArgs = list(style='white-space: normal;'),
                                         box(title = "Volcano contrasts", id = "volcano_contrasts_box", height = "450px", width = 12, collapsible = T, collapsed = F,
                                             helpText("Select the contrasts to draw the volcano plots and click to 'Plot volcanos'."),
                                             uiOutput(outputId = "volcano_contrasts"),
                                             actionButton(inputId = "plot_volcano", label = "Plot volcanos")),
                                         box(title = "Volcano params", id = "volcano_params_box", height = "450px", width = 12, collapsible = T, collapsed = F,
                                             helpText("Select the extra parameters for the volcano plots."),
                                             splitLayout(cellWidths = c("33%", "33%", "33%"),  cellArgs = list(style='white-space: normal;'),
                                                         sliderInput(inputId = "rna_mainSize", label = "Plot title size", min = 5, max = 20, value = 10),
                                                         sliderInput(inputId = "rna_axisText", label = "Axis text size", min = 5, max = 20, value = 9),
                                                         sliderInput(inputId = "rna_sizeDEGs", label = "DEG number size", min = 5, max = 20, value = 7)),
                                             sliderInput(inputId = "volcanosHeight", label = "Plot height", min = 500, max = 1500, value = 1000),
                                             checkboxInput("rna_checkLabels", label = "Show DEGs names?", value = TRUE),
                                             conditionalPanel(condition = "input.rna_checkLabels==true",
                                                              splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                                                          sliderInput("rna_numLabels", label = "Number of DEG names to show", min = 0, max = 10, step = 1, value = 3),
                                                                          sliderInput("rna_sizeLabels", label = "Size of the DEG names", min = 3, max = 9, value = 4))))),
                             ## PLOTS VOLCANOS
                             HTML('<center><h3>Volcano plots</h3></center>'),
                             uiOutput("ui_rna_volcanos")),
                    
                    # OVERLAP DEGS
                    tabPanel(title = "Overlap DEGs", value = "overlap_degs",
                             br(),
                             splitLayout(cellWidths = c("40%", "60%"),  cellArgs = list(style='white-space: normal;'),
                                 box(title = "Overlap params", id = "rna_overlap_params",  height = "200px", width = 12, collapsible = T, collapsed = F,
                                     checkboxInput(inputId = "rna_overlap_percent", label = "Show percentage", value = T)),
                                 box(title = "Contrasts to overlap", id = "overlap_contrasts", height = "200px", width = 12, collapsible = T, collapsed = F,
                                     helpText("Select the contrasts whose DEGs to overlap."), 
                                     uiOutput(outputId = "rna_overlap1_contrasts"),
                                     actionButton("submit_rnaOverlaps1", "Plot overlaps"))),
                             
                             splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                         HTML('<center><h3>Upregulated genes</h3></center>'), 
                                         HTML('<center><h3>Downregulated genes</h3></center>')),
                            
                             splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                         plotOutput(outputId = "updegs_overlaps1", width = "100%", height = 600), 
                                         plotOutput(outputId = "downdegs_overlaps1", width = "100%", height = 600))),
                    # HEATMAPS
                    tabPanel(title = "Heatmaps",
                      splitLayout(cellWidths = c("25%", "75%"),  cellArgs = list(style='white-space: normal;'),
                         column(width = 12,
                                br(),
                             box(title = tagList(icon("bars"), "List of DEGs"), width = 12, collapsible = T, collapsed = F,
                                 helpText("Select the genes to do the log2FC heatmap and click to 'Plot heatmap'."),
                                 uiOutput(outputId = "gene_list_heatmap"),
                                 actionButton(inputId = "heatmap_button", label = "Plot heatmap")),
                             box(title = tagList(icon("gear"), "Parameters"), width = 12, collapsible = T, collapsed = F,
                                 helpText("Select the parameters for the heatmaps"),
                                 checkboxInput(inputId = "cluster_rows_heatmaps_rna", label = "Cluster rows?", value = T),
                                 conditionalPanel(condition = "input.cluster_rows_heatmaps_rna==true",
                                                  checkboxInput(inputId = "show_rowdend_heatmaps_rna", label = "Show row dendogram?")),
                                 checkboxInput(inputId = "cluster_cols_heatmaps_rna", label = "Cluster columns?", value = T),
                                 conditionalPanel(condition = "input.cluster_cols_heatmaps_rna==true",
                                                  checkboxInput(inputId = "show_coldend_heatmaps_rna", label = "Show column dendogram?")),)),
                         column(width = 12,   
                             HTML('<center><h3>Heatmap of Log2FCs</h3></center>'),
                             helpText("Note that only Log2FCs are used here, hence a gene may have a good Log2FC without being statistically significant!"),
                             plotOutput(outputId = "rna_heatmap"))))
                    
        ) # tabsetPanel rnaseq_tabset end
) # tabItem rnaseq end

