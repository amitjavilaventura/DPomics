### ====================================================================== ###
### MTomics USER INTERFACE                                                 ###
### ====================================================================== ###

source("src/app_ui_sidebar.R")
source("src/app_ui_about.R")
source("src/app_ui_rnaseq.R")
source("src/app_ui_chipseq.R")
source("src/app_ui_atacseq.R")

ui <- dashboardPage(header  = dashboardHeader(title = "MYomics"), title   = "MYomics", skin = "blue",
                    sidebar = dpomics_ui_sidebar,
                    body = dashboardBody(
                      tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                      tabItems(dpomics_ui_about,
                               dpomics_ui_instructions,
                               # Individual OMICS ----
                               ## RNA-seq
                               dpomics_ui_rnaseq_de, ## differential expression
                               
                               ## ChIP-seq
                               dpomics_ui_chipseq,
                               
                               ## ATAC-seq
                               dpomics_ui_atacseq
                               )
                      )
                    )




