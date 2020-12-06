### ======================================================================
### DPomics USER INTERFACE
### ======================================================================


ui <- dashboardPage(header  = dashboardHeader(title = "DPomics"), title   = "DPomics", skin = "blue",
                    sidebar = dashboardSidebar(
                      sidebarMenu(id = "menu",
                        br(),
                        menuItem(text = "Home", tabName = "home", icon = icon(name = "home", lib = "font-awesome")),
                        #menuItem(text = "Load data", tabName = "inputs", icon = icon(name = "file", lib = "font-awesome")),
                        menuItem(text = "RNA-seq", tabName = "rnaseq", icon = icon(name = "dna", lib = "font-awesome")),
                        menuItem(text = "ChIP-seq", tabName = "chipseq", icon = icon(name = "dna", lib = "font-awesome")),
                        #menuItem(text = "Enhancers", tabName = "enhancers", icon = icon(name = "dna", lib = "font-awesome")), #not available yet
                        #menuItem(text = "ATAC-seq", tabName = "atacseq", icon = icon(name = "dna", lib = "font-awesome")), #not available yet
                        menuItem(text = "Integration", tabName = "integration", icon = icon(name = "dna", lib = "font-awesome")),
                        hr(),
                        menuItem(text = "Code", icon = icon(name = "code", lib = "font-awesome")))),
                    body = dashboardBody(
                      tabItems(
                        tabItem(tabName = "home",
                          tabsetPanel(id = "home_tabset",
                                  tabPanel(title = "Welcome",
                                    HTML('<center><p style="font-size:60px;font-family:Helvetica;color:black;"><img src="DPomics_logo.png" width="300px">Welcome to DPomics!</p></center>'),
                                    HTML("<hr style='border-color:black'>"),
                                    h1("General information"),
                                    br(),
                                    h2("Goal"),
                                    p("Shiny app to visualize RNAseq and ChIPseq data and integrate them."),
                                    br(),
                                    h2("Contributors"),
                                    tags$p("DPomics is an app deveolped by:",
                                           tags$ul(tags$li(tags$a(href = "https://amitjavilaventura.github.io","Adria Mitjavila Ventura", target="_blank")),
                                                   tags$li(tags$a(href = "https://github.com/dfernandezperez", "Daniel Fernandez Perez", target="_blank")),
                                                   tags$li(tags$a(href = "https://github.com/Ferossitiziano", "Federico Rossi", target="_blank"))))
                                  ), # welcome tab end
                                  tabPanel(title = "Instructions") # instructions tab end
                                )), # home tab end
                        
                        #tabItem(tabName = "inputs",
                        #    br(),
                        #    splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                        #      box(title = "RNA-seq inputs", id = "rna_inputs_box", height = "300px", width = 12,
                        #          helpText("Select the input for the RNAseq data visualization and click to 'Upload'."),
                        #          fileInput(inputId = "rnaseq_input", label = "Select the input file (degs)", placeholder = "Select a file..."),
                        #          verbatimTextOutput(outputId = "rnaseq_inputFile", placeholder = T),
                        #          actionButton(inputId = "submit_rna", label = "Upload")),
                        #      box(title = "RNAseq thresholds", id = "rna_params_box", height = "300px", width = 12,
                        #      helpText("Select the thresholds for log2FC and adjusted p-value"),
                        #      sliderInput(inputId = "rna_log2fc", label = "Select a log2FC threshold", min = 0, max = 3, value = 1.5, step = 0.5, ticks = T),
                        #      sliderInput(inputId = "rna_padj", label = "Select an adjusted p-value threshold", min = 0, max = 0.05, value = 0.1, ticks = T))),
                                
                        #     br(),
                        #     box(title = "ChIP-seq inputs", id = "chip_inputs_box", height = "300px", width = 12,
                        #     helpText("Select the input for the ChIP-seq data visualization and click to 'Upload'."),
                        #     fileInput(inputId = "rnaseq_input", label = "Select the input file (degs)", placeholder = "Select a file..."),
                        #     verbatimTextOutput(outputId = "rnaseq_inputFile", placeholder = T),
                        #     actionButton(inputId = "submit_rna", label = "Upload"))),
                        
                        ### RNASEQ TAB ------------------------------------------------------------------------------------------------------------------------
                        tabItem(tabName = "rnaseq",
                            h4("RNA-seq"),
                            tabsetPanel(id = "rnaseq_tabset",
                                tabPanel(title = "Upload & explore", value = "explore", br(),
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                                     box(title = "RNA-seq inputs", id = "rna_inputs_box", height = "300px", width = 12, collapsible = T, collapsed = F,
                                                         helpText("Select the input for the RNAseq data visualization and click to 'Upload'."),
                                                         fileInput(inputId = "rnaseq_input", label = "Select the input file (degs)", placeholder = "Select a file..."),
                                                         verbatimTextOutput(outputId = "rnaseq_inputFile", placeholder = T),
                                                         actionButton(inputId = "submit_rna", label = "Upload")),
                                                     box(title = "DE thresholds", id = "rna_thresholds", height = "300px", width = 12, collapsible = T, collapsed = F,
                                                         helpText("Select the thresholds for log2FC and adjusted p-value"),
                                                         sliderInput(inputId = "rna_log2fc", label = "Select a log2FC threshold", min = 0, max = 3, value = 1.5, step = 0.5, ticks = T),
                                                         sliderInput(inputId = "rna_padj", label = "Select an adjusted p-value threshold", min = 0, max = 0.05, value = 0.1, ticks = T))),
                                         
                                         br(),
                                         conditionalPanel(condition = "input.submit_rna > 0",
                                          h3("Explore the data!"),
                                          helpText("This is a data table with all the DE information. Write a gene inside the search box to see its DE data in all contrasts."),
                                          DT::dataTableOutput("rna_dataTable"))),
                                
                                tabPanel(title = "Volcano plots", value = "volcanos", br(),
                                         ## PARAMS VOLCANOS
                                         splitLayout(cellWidths = c("30%", "70%"),  cellArgs = list(style='white-space: normal;'),
                                                     box(title = "Volcano contrasts", id = "volcano_contrasts_box", height = "400px", width = 12, collapsible = T, collapsed = F,
                                                         helpText("Select the contrasts to draw the volcano plots and click to 'Plot volcanos'."),
                                                         uiOutput(outputId = "volcano_contrasts"),
                                                         verbatimTextOutput(outputId = "volcano_contrasts_list", placeholder = T),
                                                         actionButton(inputId = "plot_volcano", label = "Plot volcanos")),
                                                     box(title = "Volcano params", id = "volcano_params_box", height = "400px", width = 12, collapsible = T, collapsed = F,
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
                                                     
                                
                                ### OVERLAP DEGS --------------------------------------------------------------------------------------------------------------
                                tabPanel(title = "Overlap DEGs", value = "overlap_degs",
                                         br(),
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                          box(title = "Overlap 1", id = "overlap_contrasts", height = "300px", width = 12, collapsible = T, collapsed = F,
                                              helpText("Select between 2 and 4 contrasts to overlap their DEGs and click to 'Plot overlaps'."), 
                                              uiOutput(outputId = "rna_overlap1_contrasts"),
                                              verbatimTextOutput(outputId = "rna_contrast1_list", placeholder = T),
                                              actionButton("submit_rnaOverlaps1", "Plot overlaps")),
                                          
                                          box(title = "Overlap 2", id = "overlap_contrasts", height = "300px", width = 12,
                                              helpText("Select between 2 and 4 contrasts to overlap their DEGs and click to 'Plot overlaps'."), 
                                              uiOutput(outputId = "rna_overlap2_contrasts"),
                                              verbatimTextOutput(outputId = "rna_contrast2_list", placeholder = T),
                                              actionButton("submit_rnaOverlaps2", "Plot overlaps"))),
                                         
                                         HTML('<center><h3>All DEGs</h3></center>'),
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                                     plotOutput(outputId = "degs_overlaps1", width = 500, height = 500), 
                                                     plotOutput(outputId = "degs_overlaps2", width = 500, height = 500)),
                                         HTML('<center><h3>Upregulated genes</h3></center>'),
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                                     plotOutput(outputId = "updegs_overlaps1", width = 500, height = 500), 
                                                     plotOutput(outputId = "updegs_overlaps2", width = 500, height = 500)),
                                         HTML('<center><h3>Downregulated genes</h3></center>'),
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                                     plotOutput(outputId = "downdegs_overlaps1", width = 500, height = 500), 
                                                     plotOutput(outputId = "downdegs_overlaps2", width = 500, height = 500))),
                                
                                ### HEATMAPS --------------------------------------------------------------------------------------------------------------
                                tabPanel(title = "Heatmaps", 
                                    box(title = "Parameters", id = "rna_heatmaps_box", width = 12, collapsible = T, collapsed = F,
                                        helpText("Select the genes to do the log2FC heatmap and click to 'Plot heatmap'."),
                                        uiOutput(outputId = "gene_list_heatmap"),
                                        actionButton(inputId = "heatmap_button", label = "Plot heatmap")),
                                    
                                    HTML('<center><h3>Heatmap of Log2FCs</h3></center>'),
                                    plotOutput(outputId = "rna_heatmap"))
                                
                              ) # tabsetPanel rnaseq_tabset end
                            ), # tabItem rnaseq end
                        
                        tabItem(tabName = "chipseq",
                           tabsetPanel(id = "chipseq_tabset",
                               tabPanel(title = "Upload & explore",
                                   splitLayout(cellArgs = list(style='white-space: normal;'),
                                      box(title = "ChIPseq inputs", width = 12, collapsible = T, collapsed = F,
                                          helpText("Select the input file and click to upload."),
                                          fileInput(inputId = "chip_input", label = "Select the input file", buttonLabel = "Browse", placeholder = "Select a file..."),
                                          verbatimTextOutput(outputId = "chipseq_inputFile", placeholder = T),
                                          actionButton(inputId = "submit_chip", label = "Upload")))       
                                ) # tabsetPanel chipseq_tabset end
                        ) # tabItem chipseq end
                        )))




