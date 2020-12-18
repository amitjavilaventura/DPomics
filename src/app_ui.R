### ======================================================================
### DPomics USER INTERFACE
### ======================================================================

ui <- dashboardPage(header  = dashboardHeader(title = "DPomics"), title   = "DPomics", skin = "blue",
                    sidebar = dashboardSidebar(
                      sidebarMenu(id = "menu",
                        br(),
                        menuItem(text = "Home", tabName = "home", icon = icon(name = "home", lib = "font-awesome"),
                                 menuSubItem(text = "Welcome", tabName = "welcome"),
                                 menuSubItem(text = "Instructions", tabName = "instructions")),
                        menuItem(text = "RNA-seq", tabName = "rnaseq", icon = icon(name = "dna", lib = "font-awesome")),
                        menuItem(text = "ChIP-seq", tabName = "chipseq", icon = icon(name = "dna", lib = "font-awesome")),
                        #menuItem(text = "Enhancers", tabName = "enhancers", icon = icon(name = "dna", lib = "font-awesome")), #not available yet
                        #menuItem(text = "ATAC-seq", tabName = "atacseq", icon = icon(name = "dna", lib = "font-awesome")), #not available yet
                        menuItem(text = "Integration", tabName = "integration", icon = icon(name = "dna", lib = "font-awesome"),
                          menuSubItem(text = "RNA-seq + ChIP-seq", tabName = "rna_chip")),
                        hr(),
                        menuItem(text = "Code", href = "https://github.com/amitjavilaventura/DPomics", icon = icon(name = "code", lib = "font-awesome")))),
                    body = dashboardBody(
                      tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                      tabItems(
                      ### welcome tab ------------------------------------------------------------------------------------------------------------------------
                      tabItem(tabName = "welcome",
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
                          tabItem(tabName = "instructions",
                            HTML('<center><p style="font-size:60px;font-family:Helvetica;color:black;"><img src="DPomics_logo.png" width="300px">Welcome to DPomics!</p></center>'),
                            HTML("<hr style='border-color:black'>"),
                            tabsetPanel(id = "instructions_tab_end", type = "pills",
                                tabPanel(title = "RNAseq"),
                                tabPanel(title = "ChIPseq"),
                                tabPanel(title = "Integration | RNAseq + ChIPseq"))), # instructions tab end
                                
                        
                        ### RNASEQ TAB ------------------------------------------------------------------------------------------------------------------------
                        tabItem(tabName = "rnaseq",
                            h3("RNA-seq"),
                            tabsetPanel(id = "rnaseq_tabset", type = "pills",
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
                                                         sliderInput(inputId = "rna_padj", label = "Select an adjusted p-value threshold", min = 0, max = 0.05, value = 0.1, ticks = T))),
                                         
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
                                                         verbatimTextOutput(outputId = "volcano_contrasts_list", placeholder = T),
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
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                          box(title = "Overlap 1", id = "overlap_contrasts", height = "300px", width = 12, collapsible = T, collapsed = F,
                                              helpText("Select 2 or 3 contrasts to overlap their DEGs and click to 'Plot overlaps'."), 
                                              uiOutput(outputId = "rna_overlap1_contrasts"),
                                              verbatimTextOutput(outputId = "rna_contrast1_list", placeholder = T),
                                              actionButton("submit_rnaOverlaps1", "Plot overlaps")),
                                          
                                          box(title = "Overlap 2", id = "overlap_contrasts", height = "300px", width = 12,
                                              helpText("Select 2 or 3 contrasts to overlap their DEGs and click to 'Plot overlaps'."), 
                                              uiOutput(outputId = "rna_overlap2_contrasts"),
                                              verbatimTextOutput(outputId = "rna_contrast2_list", placeholder = T),
                                              actionButton("submit_rnaOverlaps2", "Plot overlaps"))),
                                         
                                         HTML('<center><h3>Upregulated genes</h3></center>'),
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                                     plotOutput(outputId = "updegs_overlaps1", width = 600, height = 600), 
                                                     plotOutput(outputId = "updegs_overlaps2", width = 600, height = 600)),
                                         HTML('<center><h3>Downregulated genes</h3></center>'),
                                         splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                                     plotOutput(outputId = "downdegs_overlaps1", width = 600, height = 600), 
                                                     plotOutput(outputId = "downdegs_overlaps2", width = 600, height = 600))),
                                # HEATMAPS
                                tabPanel(title = "Heatmaps", 
                                    box(title = "Parameters", id = "rna_heatmaps_box", width = 12, collapsible = T, collapsed = F,
                                        helpText("Select the genes to do the log2FC heatmap and click to 'Plot heatmap'."),
                                        uiOutput(outputId = "gene_list_heatmap"),
                                        actionButton(inputId = "heatmap_button", label = "Plot heatmap")),
                                    
                                    HTML('<center><h3>Heatmap of Log2FCs</h3></center>'),
                                    plotOutput(outputId = "rna_heatmap"))
                                
                              ) # tabsetPanel rnaseq_tabset end
                            ), # tabItem rnaseq end
                        
                        # CHIP-SEQ TAB --------------------------------------------------------------------------------------------------------------------
                        tabItem(tabName = "chipseq",
                           h3("ChIP-seq"),
                           tabsetPanel(id = "chipseq_tabset", type = "pills",
                               # UPLOAD and EXPLORE
                               tabPanel(title = "Upload & explore", br(),
                                   splitLayout(cellArgs = list(style='white-space: normal;'),
                                      box(title = "ChIPseq inputs", width = 12, collapsible = T, collapsed = F,
                                          helpText("Select the input file and click to upload."),
                                          fileInput(inputId = "chip_input", label = "Select the input file", buttonLabel = "Browse", placeholder = "Select a file..."),
                                          verbatimTextOutput(outputId = "chipseq_inputFile", placeholder = T),
                                          actionButton(inputId = "submit_chip", label = "Upload"))),
                                   
                                   br(),
                                   conditionalPanel(condition = "input.submit_chip > 0",
                                       h3("Explore the data!"),
                                       helpText("This is a data table with all the peak information. Write a gene inside the search box to see the peaks that are near that gene in each condition."),
                                       DT::dataTableOutput(outputId = "chip_dataTable"))), # tabPanel chip explore end.
                               
                               # PEAKS AND DISTRIBUTION
                               tabPanel(title = "Peaks",  br(), 
                                    splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                        box(title = "Number of peaks", width = 12, collapsible = T, collapsed = F,
                                            helpText("Select the desired parameters and click to 'Plot peaks'."),
                                            selectInput(inputId = "chip_peaks_legend", label = "Select the legend postion", selected = "No legend", 
                                                        choices = list("No legend" = "none", "Right" = "right", "Left" = "left", "Bottom" = "bottom", "Top" = "top")),
                                            checkboxInput(inputId = "chip_peaks_pannel", label = "Separate proteins in pannels?", value = T),
                                            actionButton(inputId = "generate_chipPeaks", label = "Plot peaks")),
                                        box(title = "Peak distribution", width = 12, collapsible = T, collapsed = F,
                                            helpText("Select the desired parameters and click to 'Plot distribution'."),
                                            radioButtons(inputId = "chip_numRegions", label = "Select the number of regions to annotate", choices = c(2,3), inline = T),
                                            selectInput(inputId = "chip_anno_type", label = "Select a type of peak distribution", choices = c("Proportion", "Total number"), selected = "Proportion", multiple = F),
                                            checkboxInput(inputId = "chip_anno_pannel", label = "Separate proteins in pannels?", value = T),
                                            actionButton("generate_chipAnno", "Plot distribution"))),
                                    
                                    splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                        conditionalPanel(condition = "input.generate_chipPeaks > 0", HTML('<center><h3>Total number of peaks</h3></center>')),
                                        conditionalPanel(condition = "input.generate_chipAnno > 0", HTML('<center><h3>Distribution of peaks in the genome</h3></center>'))),
                                    
                                    splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                        conditionalPanel(condition = "input.generate_chipPeaks > 0", plotOutput("chip_peaks", width = 600, height = 500)),
                                        conditionalPanel(condition = "input.generate_chipAnno > 0", plotOutput("chip_anno", width = 600, height = 500)))),
                               
                               # PEAKS INTERSECTION
                               tabPanel(title = "Intersection", br(),
                                  splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                        box(title = "Intersection 1", width = 12, collapsible = T, collapsed = F,
                                            helpText("Select the desired parameteres and click to 'Plot intersection'."),
                                            uiOutput(outputId = "ui_chip_intersect1"),
                                            verbatimTextOutput(outputId = "chip_cond_list1", placeholder = T),
                                            actionButton(inputId = "generate_chip_intersection1", label = "Plot intersection")),
                                        box(title = "Intersection 1", width = 12, collapsible = T, collapsed = F,
                                            helpText("Select the desired parameteres and click to 'Plot intersection'."),
                                            uiOutput(outputId = "ui_chip_intersect2"),
                                            verbatimTextOutput(outputId = "chip_cond_list2", placeholder = T),
                                            actionButton("generate_chip_intersection2", "Plot intersection"))),
                                  
                                  splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                        conditionalPanel(condition = "input.generate_chip_intersection1 > 0", HTML('<center><h3>Intersection 1</h3></center>')),
                                        conditionalPanel(condition = "input.generate_chip_intersection2 > 0", HTML('<center><h3>Intersection 2</h3></center>'))),
                                  
                                  splitLayout(cellWidths = c("50%", "50%"),  cellArgs = list(style='white-space: normal;'),
                                        conditionalPanel(condition = "input.generate_chip_intersection1 > 0", plotOutput("chip_intersect1", width = 600, height = 500)),
                                        conditionalPanel(condition = "input.generate_chip_intersection2 > 0", plotOutput("chip_intersect2", width = 600, height = 500)))),
                               
                               tabPanel(title = "Peaks in custom coordinates", br(),
                                      box(title = "Upload coordinates", width = 12, collapsible = T, collapsed = F,
                                          helpText("Upload a file with a list of custom coordinates, select the parameters and click to 'Upload'."),
                                          fileInput(inputId = "upload_coords", label = "Browse", multiple = F, buttonLabel = "Browse", placeholder = "Select a file..."),
                                          verbatimTextOutput("uploaded_coords_name", placeholder = T),
                                          checkboxInput("upload_coords_header", label = "Field names or header?", value = F),
                                          conditionalPanel(condition = "input.upload_coords_header==true",
                                            selectInput("upload_coords_sep", label = "Field separator", selected = "Tabulation", multiple = F,
                                                        choices = list("Tabulation" = "\t", "Space" = " ", "Comma" = ",", "Semicolon" = ";", "Colon" = ":"))),
                                          conditionalPanel(condition = "input.upload_coords_header==false",
                                            helpText("Write the field/column names for the uploaded list of custom coordinates."),
                                            helpText("The fields corresponding to chromosome, start and end must be 'seqnames', 'start' and 'end'."),
                                            helpText("Use blank spaces to separate each written field name."),
                                            textInput(inputId = "custom_header", label = "Write the field names:", placeholder = "seqnames start end ..."))),
                                          actionButton(inputId = "upload_coords_button", label = "Upload"),
                                      
                                      DT::dataTableOutput(outputId = "coords_chip"))
                               
                               
                               
                               
                                ) # tabsetPanel chipseq_tabset end
                        ), # tabItem chipseq end
                        
                        # INTEGRATION ---------------------------------------------------------------------------------
                        # INTEGRATION RNASEQ + CHIPSEQ
                        tabItem(tabName = "rna_chip",
                          h3("Integration | RNA-seq + ChIP-seq"),
                          tabsetPanel(id = "rna_chip_tabset", type = "pills",
                              tabPanel(title = "Explore", value = "int_chiprna_explore_tab",
                                box(title = "Peak subtraction parameters", width = 12,
                                    uiOutput(outputId = "int_chip_rna_protein_ui"),
                                    selectInput(inputId = "int_chip_rna_protein", label = "Protein", choices = c("TCF4", "bCAT")),
                                    actionButton(inputId = "peak_subtraction_button", label = "Peak subtraction")),
                                DT::dataTableOutput("degs_near_peaks"),
                                DT::dataTableOutput("peaks_targetting_degs")),
                              tabPanel(title = "Volcano plots", value = "int_chiprna_volcano_tab",
                                box(title = "", width = 12,
                                    sliderInput(inputId = "int_volcanosHeight", label = "Plot height", min = 500, max = 1500, value = 1000),
                                    actionButton(inputId = "plot_int_volcanos", label = "Plot volcano")),
                                uiOutput(outputId = "ui_int_volcanos")))) # tabItem rna_chip integration end
                        )))




