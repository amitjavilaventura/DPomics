
### DPomics - DEPENDENCIES
### ==============================================================================================================================

options(repos = BiocManager::repositories())


# shiny -------------------------------------------------------------------------------------------
#library(shiny)
library(shinydashboard)
library(shinyWidgets)
#library(shinythemes)
library(DT)

# tidy code ---------------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(purrr)
library(magrittr)
library(tibble)

# list handling ----------------------------------------------------------------------------------- 
library(plyranges)

# plotting ---------------------------------------------------------------------------------------- 
library(ggplot2)
library(VennDiagram)
library(cowplot)
library(ComplexHeatmap)
library(ggpubr)
library(ChIPpeakAnno)
library(circlize)
library(plotly)

# misc --------------------------------------------------------------------------------------------