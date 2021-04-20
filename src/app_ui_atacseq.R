### MYomics USER INTERFACE                                                 
### =========================================================================================== ###

### MYomics UI atacseq ----------------------------------------------------------------------------
dpomics_ui_atacseq <-
  tabItem(tabName = "atacseq",
          h3("ATACseq-seq"),
          tabsetPanel(id = "atac_tabset", type = "pills",
                      # UPLOAD and EXPLORE
                      tabPanel(title = "Upload & explore", br(),
                               splitLayout(cellArgs = list(style='white-space: normal;'),
                                           box(title = "ATACseq inputs", width = 12, collapsible = T, collapsed = F,
                                               helpText("Select the input file and click to upload."),
                                               fileInput(inputId = "atac_input", label = "Select the input file", buttonLabel = "Browse", placeholder = "Select a file..."),
                                               verbatimTextOutput(outputId = "atacseq_inputFile", placeholder = T),
                                               actionButton(inputId = "submit_atac", label = "Upload"))),
                               
                               br(),
                               conditionalPanel(condition = "input.submit_atac > 0",
                                                h3("Explore the data!"),
                                                helpText("This is a data table with all the peak information. Write a gene inside the search box to see the peaks that are near that gene in each condition."),
                                                DT::dataTableOutput(outputId = "atac_dataTable"))), # tabPanel atac explore end.
                      
                      # PEAKS AND DISTRIBUTION
                      tabPanel(title = "Peaks",  br(), 
                               box(title = "Peak distribution", width = 12, collapsible = T, collapsed = F,
                                   helpText("Select the desired parameters and click to 'Plot distribution'."),
                                   radioButtons(inputId = "atac_numRegions", label = "Select the number of regions to annotate", choices = c(2,3), inline = T),
                                   selectInput(inputId = "atac_anno_type", label = "Select a type of peak distribution", choices = c("Proportion", "Total number"), selected = "Total number", multiple = F),
                                   checkboxInput(inputId = "atac_anno_pannel", label = "Separate proteins in pannels?", value = T),             
                                   actionButton("generate_atacAnno", "Plot distribution")),
                               
                               conditionalPanel(condition = "input.generate_atacAnno > 0", HTML('<center><h3>Distribution of peaks in the genome</h3></center>')),
                               
                               conditionalPanel(condition = "input.generate_atacAnno > 0", plotOutput("atac_anno", width = 600, height = 500))),
                      
                      # PEAKS INTERSECTION
                      tabPanel(title = "Intersection", br(),
                              box(title = "Intersection", width = 12, collapsible = T, collapsed = F,
                                  helpText("Select the desired parameteres and click to 'Plot intersection'."),
                                  uiOutput(outputId = "ui_atac_intersect1"),
                                  verbatimTextOutput(outputId = "atac_cond_list1", placeholder = T),
                                  actionButton(inputId = "generate_atac_intersection1", label = "Plot intersection")),
                               
                               conditionalPanel(condition = "input.generate_atac_intersection1 > 0", HTML('<center><h3>Intersection 1</h3></center>')),

                               conditionalPanel(condition = "input.generate_atac_intersection1 > 0", plotOutput("atac_intersect1", width = 600, height = 500)),
                
          ) # tabsetPanel atacseq_tabset end
  )) # tabItem atacseq end


