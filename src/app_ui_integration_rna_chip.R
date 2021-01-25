### DPomics USER INTERFACE                                                 
### =========================================================================================== ###

### DPomics INTEGRATION - RNA-seq + ChIP-seq ------------------------------------------------------
dpomics_ui_integration_rna_chip <- 
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
                             uiOutput(outputId = "ui_int_volcanos"))))