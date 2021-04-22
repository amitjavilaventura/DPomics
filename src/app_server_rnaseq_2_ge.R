
### MYomics SERVER FUNCTION - RNAseq - Gene expression                                        
### =========================================================================================== ###

### MYomics SERVER RNASEQ - Gene expression - Explore ---------------------------------------------
server_geneexp_explore <- function(input, output, session, data){
  
  # table with the data of the table
  output$rna_tpm_dataTable <- DT::renderDataTable({
    exp <- data %>% tibble::column_to_rownames("Geneid") %>% round(digits = 2) %>% rownames_to_column("Geneid")
    DT::datatable(data = exp, options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 10))
  })
  
}
