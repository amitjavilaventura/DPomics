
### DPomics SERVER FUNCTION - ChIPseq                                         
### =========================================================================================== ###

### EXPLORATION TAB -----
server_chipseq_explore <- function(input, output, session, data){
  
  # dataTable for exploration
  output$chip_dataTable <- DT::renderDataTable({
    DT::datatable(data = data,  options = list(lengthMenu = c(5, 10, 20, 30, 50), pageLength = 10))
  })
}

### PEAKS TAB -----
server_chipseq_peaks <- function(input, output, session, data){
  ### PEAKS TAB ----------------------------------------------------------------------------------------------------------
  # number of peaks
  observeEvent(eventExpr = input$generate_chipPeaks, {
    # plot number of peaks
    output$chip_peaks <- renderPlot({
      peakNum(data = data, legend_position = isolate(input$chip_peaks_legend), pannel = isolate(input$chip_peaks_pannel))
    })
  }) # ovserveEvent end (actionButton generate_peaks)
  
  # annotation (distal/promo) 
  observeEvent(eventExpr = input$generate_chipAnno, {
    # plot distribution of peaks
    output$chip_anno <- renderPlot({
      barAnno(data = data, names = peaks$sample %>% unique(), pannel = isolate(input$chip_anno_pannel),
              anno_num = isolate(input$chip_numRegions), peak_type = isolate(input$chip_anno_type), palette = "Set2", ylab = paste(isolate(input$chip_anno_type), "of peaks"))
    })
  }) # ovserveEvent end (actionButton generate_barAnno)
}

### INTERSECTIONS TAB -----
server_chipseq_intersections <- function(input, output, session, data){
  ### INTERSECTION TAB ---------------------------------------------------------------------------------------------------
  # get selectInputs for conditions to intersect
  output$ui_chip_intersect1 <- renderUI({
    samples <- data$sample %>% unique
    selectizeInput(inputId = "chip_conditions1", label = "Select the conditions to overlap the peaks",
                   choices = samples, selected = NULL, multiple = T, 
                   options = list(maxItems = 3, placeholder = "Select between 2 and 3 conditions"))
  })
  output$chip_cond_list1 <- renderPrint({ paste(input$chip_conditions1, sep = "; ") })
  
  output$ui_chip_intersect2 <- renderUI({
    samples <- data$sample %>% unique
    selectizeInput(inputId = "chip_conditions2", label = "Select the conditions to overlap the peaks",
                   choices = samples, selected = NULL, multiple = T, 
                   options = list(maxItems = 3, placeholder = "Select between 2 and 3 conditions"))
  })
  
  output$chip_cond_list2 <- renderPrint({ paste(input$chip_conditions2, sep = "; ") })
  
  # plot intersections
  observeEvent(eventExpr = input$generate_chip_intersection1, {
    output$chip_intersect1 <- renderPlot({
      intersectPeaks(peaks = data, conditions = isolate(input$chip_conditions1))
    })
  })
  
  observeEvent(eventExpr = input$generate_chip_intersection2, {
    output$chip_intersect2 <- renderPlot({
      intersectPeaks(peaks = data, conditions = isolate(input$chip_conditions2))
    })
  })
}

### CUSTOM COORDINATES TAB -----
server_chipseq_custom_coords <- function(input, output, session, data){
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
}



### DPomics server function =====
dpomics_server_chipseq <- function(input, output, session){
  
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
    server_chipseq_custom_coords(input, output, session, data = chip_data())
    
  })
  
}