### ======================================================================
### DPomics SERVER FUNCTION
### ======================================================================

# Load source files -------------------------------------------------------------------------------
source("src/app_server_rnaseq.R")
# Load source files -------------------------------------------------------------------------------
source("src/app_server_chipseq.R")

# SERVER FUNCTION ---------------------------------------------------------------------------------
server <- function(input, output, session) {

  ### ----- RNA-SEQ STUFF -----
  # text with the name of the file
  output$rnaseq_inputFile <- renderPrint({ input$rnaseq_input$name })
  
  # INPUTS
  # reactive input
  rna_data <- eventReactive(eventExpr = input$submit_rna, { read.delim(input$rnaseq_input$datapath) })
  
  observeEvent(eventExpr = input$submit_rna, {

    # EXPLORATION TAB 
    server_rnaseq_explore(input, output, session, data = rna_data())
    # VOLCANO TAB
    server_rnaseq_volcano(input, output, session, data = rna_data())
    # OVERLAP DEGS TAB 
    server_rnaseq_overlap(input, output, session, data = rna_data())
    # HEATMAPS TAB
    server_rnaseq_heatmap(input, output, session, data = rna_data())
        
  }) # observeEvent end (submit_rna file)
  
  
  ### ----- CHIP-SEQ STUFF ----- ###  
  # text with the name of the input file
  output$chipseq_inputFile <- renderPrint({ input$chip_input$name })
  
  # read peaks file
  chip_data <- eventReactive(eventExpr = input$submit_chip, { read.delim(input$chip_input$datapath) })
  
  observeEvent(eventExpr = input$submit_chip, {
    
    ### EXPLORATION TAB
    server_chipseq_explore(input, output, session, data = chip_data())
    ### PEAKS TAB
    server_chipseq_peaks(input, output, session, data = chip_data())
    ### INTERSECTIONS TAB
    server_chipseq_intersections(input, output, session, data = chip_data())
    ### CUSTOM COORDINATES TAB
    #server_chipseq_custom_coords(input, output, session, data = chip_data())
    
  })
  
  ## INTEGRATION RNASEQ+CHIPSEQ --> LOOK AT THE APP_SERVER2.R IN THE ZZ_OLD_SRC --> MUST BE UPDATED AND IMPROVED
  
} ## SERVER FUNCTION END