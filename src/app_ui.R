### ====================================================================== ###
### DPomics USER INTERFACE                                                 ###
### ====================================================================== ###

source("src/app_ui_sidebar.R")
source("src/app_ui_about.R")
source("src/app_ui_rnaseq.R")
source("src/app_ui_chipseq.R")
source("src/app_ui_integration_rna_chip.R")

ui <- dashboardPage(header  = dashboardHeader(title = "DPomics"), title   = "DPomics", skin = "blue",
                    sidebar = dpomics_ui_sidebar,
                    body = dashboardBody(
                      tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                      tabItems(dpomics_ui_about,
                               dpomics_ui_instructions,
                               # Individual OMICS ----
                               dpomics_ui_rnaseq,
                               dpomics_ui_chipseq,
                               # INTEGRATION ----
                               #dpomics_ui_integration_rna_chip
                               )
                      )
                    )




