### ======================================================================
### DPomics 
### ======================================================================

# Shiny app for the visualization of omics data for DPlab

# Load Shiny and required packages.
library(shiny)
source("src/dependencies.R")

# Load custom function
source("src/functions.R")
source("src/functions_server_rnaseq.R")
source("src/functions_server_chipseq.R")
source("src/functions_server_integrate_rnachip.R")
source("src/functions_ui.R")

# Load USER INTERFACE
source("src/app_ui.R")

# Load SERVER FUNCTION

source("src/app_server.R")


# RUN APP
shinyApp(ui = ui, server = server)