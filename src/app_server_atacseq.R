
### MYomics SERVER FUNCTION - ATAC-seq                                         
### =========================================================================================== ###

### EXPLORATION TAB -----
server_atacseq_explore <- function(input, output, session, data){
  
  # dataTable for exploration
  output$atac_dataTable <- DT::renderDataTable({
    DT::datatable(data = data,  options = list(lengthMenu = c(5, 10, 20, 30, 50), pageLength = 10))
  })
}

### PEAKS TAB -----
server_atacseq_peaks <- function(input, output, session, data){
  ### PEAKS TAB ----------------------------------------------------------------------------------------------------------
  # annotation (distal/promo) 
  observeEvent(eventExpr = input$generate_atacAnno, {
    # plot distribution of peaks
    output$atac_anno <- renderPlot({
      barAnno(data = data, names = peaks$sample %>% unique(), pannel = isolate(input$atac_anno_pannel),
              anno_num = isolate(input$atac_numRegions), peak_type = isolate(input$atac_anno_type), palette = "Set2", ylab = paste(isolate(input$atac_anno_type), "of peaks"))
    })
  }) # ovserveEvent end (actionButton generate_barAnno)
}

### INTERSECTIONS TAB -----
server_atacseq_intersections <- function(input, output, session, data){
  ### INTERSECTION TAB ---------------------------------------------------------------------------------------------------
  # get selectInputs for conditions to intersect
  output$ui_atac_intersect1 <- renderUI({
    samples <- data$sample %>% unique
    selectizeInput(inputId = "atac_conditions1", label = "Select the conditions to overlap the peaks",
                   choices = samples, selected = NULL, multiple = T, 
                   options = list(maxItems = 3, placeholder = "Select the conditions to overlap"))
  })
  output$atac_cond_list1 <- renderPrint({ paste(input$atac_conditions1, sep = "; ") })
  
  # plot intersections
  observeEvent(eventExpr = input$generate_atac_intersection1, {
    output$atac_intersect1 <- renderPlot({
      intersectPeaks(peaks = data, conditions = isolate(input$atac_conditions1))
    })
  })
}

### CUSTOM COORDINATES TAB -----
server_atacseq_custom_coords <- function(input, output, session, data){
  output$uploaded_coords_name <- renderPrint({ input$upload_coords$name })
  
  custom_coords <- eventReactive(eventExpr = input$upload_coords_button, {
    if(input$upload_coords_header == T){read.delim(file = input$upload_coords$datapath, header = input$upload_coords_header, sep = input$upload_coords_sep)}
    else{read.delim(file = input$upload_coords$datapath, header = input$upload_coords_header, sep = input$upload_coords_sep) %>%
        set_colnames(input$custom_header %>% stringr::str_split(pattern = " ") %>% unlist())}
    
  })
  
  observeEvent(eventExpr = input$upload_coords_button, {
    output$coords_atac <- DT::renderDataTable({
      custom_coords <- custom_coords() %>% set_colnames(c("seqnames", "start", "end")) %>% as_granges()
      atac_data <- atac_data() %>% as_granges()
      coords_atac <- find_overlaps(x = custom_coords, y = atac_data) %>% as_data_frame
      DT::datatable(data = coords_atac)
    })
    
  })
}



### MYomics server function =====
dpomics_server_atacseq <- function(input, output, session){
  
  # text with the name of the input file
  output$atacseq_inputFile <- renderPrint({ input$atac_input$name })
  
  # read peaks file
  atac_data <- eventReactive(eventExpr = input$submit_atac, { read.delim(input$atac_input$datapath) })
  
  observeEvent(eventExpr = input$submit_atac, {
    
    ### EXPLORATION TAB
    server_atacseq_explore(input, output, session, data = atac_data())
    ### PEAKS TAB
    server_atacseq_peaks(input, output, session, data = atac_data())
    ### INTERSECTIONS TAB
    server_atacseq_intersections(input, output, session, data = atac_data())
    ### CUSTOM COORDINATES TAB
    server_atacseq_custom_coords(input, output, session, data = atac_data())
    
  })
  
}