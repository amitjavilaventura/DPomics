### DPomics USER INTERFACE                                                 
### =========================================================================================== ###

### DPomics UI chipseq ----------------------------------------------------------------------------
dpomics_ui_chipseq <-
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
                               box(title = "Peak distribution", width = 12, collapsible = T, collapsed = F,
                                               helpText("Select the desired parameters and click to 'Plot distribution'."),
                                               radioButtons(inputId = "chip_numRegions", label = "Select the number of regions to annotate", choices = c(2,3), inline = T),
                                               selectInput(inputId = "chip_anno_type", label = "Select a type of peak distribution", choices = c("Proportion", "Total number"), selected = "Proportion", multiple = F),
                                               checkboxInput(inputId = "chip_anno_pannel", label = "Separate proteins in pannels?", value = T),
                                               actionButton("generate_chipAnno", "Plot distribution")),
                               
                               conditionalPanel(condition = "input.generate_chipAnno > 0", HTML('<center><h3>Distribution of peaks in the genome</h3></center>')),
                               conditionalPanel(condition = "input.generate_chipAnno > 0", plotOutput("chip_anno", width = 700, height = 600))),
                      
                      # PEAKS INTERSECTION
                      tabPanel(title = "Intersection", br(),
                               box(title = "Intersection", width = 12, collapsible = T, collapsed = F,
                                   helpText("Select the desired parameteres and click to 'Plot intersection'."),
                                   uiOutput(outputId = "ui_chip_intersect1"),
                                   verbatimTextOutput(outputId = "chip_cond_list1", placeholder = T),
                                   actionButton(inputId = "generate_chip_intersection1", label = "Plot intersection")),
                               
                              
                               conditionalPanel(condition = "input.generate_chip_intersection1 > 0", HTML('<center><h3>Intersection 1</h3></center>')),

                               conditionalPanel(condition = "input.generate_chip_intersection1 > 0", plotOutput("chip_intersect1", width = 600, height = 500)))#,
                      
                      # PEAKS in custom coordinates
                      #tabPanel(title = "Peaks in custom coordinates", br(),
                      #         box(title = "Upload coordinates", width = 12, collapsible = T, collapsed = F,
                      #             helpText("Upload a file with a list of custom coordinates, select the parameters and click to 'Upload'."),
                      #             fileInput(inputId = "upload_coords", label = "Browse", multiple = F, buttonLabel = "Browse", placeholder = "Select a file..."),
                      #             verbatimTextOutput("uploaded_coords_name", placeholder = T),
                      #             checkboxInput("upload_coords_header", label = "Field names or header?", value = F),
                      #             conditionalPanel(condition = "input.upload_coords_header==true",
                      #                              selectInput("upload_coords_sep", label = "Field separator", selected = "Tabulation", multiple = F,
                      #                                          choices = list("Tabulation" = "\t", "Space" = " ", "Comma" = ",", "Semicolon" = ";", "Colon" = ":"))),
                      #             conditionalPanel(condition = "input.upload_coords_header==false",
                      #                              helpText("Write the field/column names for the uploaded list of custom coordinates."),
                      #                              helpText("The fields corresponding to chromosome, start and end must be 'seqnames', 'start' and 'end'."),
                      #                              helpText("Use blank spaces to separate each written field name."),
                      #                              textInput(inputId = "custom_header", label = "Write the field names:", placeholder = "seqnames start end ..."))),
                      #         actionButton(inputId = "upload_coords_button", label = "Upload"),
                      #         
                      #         DT::dataTableOutput(outputId = "coords_chip"))
                      
                      
                      
                      
          ) # tabsetPanel chipseq_tabset end
  ) # tabItem chipseq end


