
### MYomics USER INTERFACE                                                 
### =========================================================================================== ###

### MYomics UI sidebar ----------------------------------------------------------------------------
dpomics_ui_sidebar <-
dashboardSidebar(
  sidebarMenu(id = "menu",
              hr(),
              menuItem(text = "About", tabName = "about", icon = icon(name = "at", lib = "font-awesome")),
              menuItem(text = "Instructions", tabName = "instructions", icon = icon(name = "info-circle", lib = "font-awesome")),
              #hr(),
              #menuItem(text = "Input data", tabName = "inputs_menu", icon = icon(name = "upload", lib = "font-awesome")),
              hr(),
              menuItem(text = "RNA-seq", tabName = "rnaseq", icon = icon(name = "dna", lib = "font-awesome")),
              menuItem(text = "ChIP-seq", tabName = "chipseq", icon = icon(name = "dna", lib = "font-awesome")),
              #menuItem(text = "Enhancers", tabName = "enhancers", icon = icon(name = "dna", lib = "font-awesome")), #not available yet
              menuItem(text = "ATAC-seq", tabName = "atacseq", icon = icon(name = "dna", lib = "font-awesome")), #not available yet,
              hr(),
              menuItem(text = "Code", href = "https://github.com/amitjavilaventura/DPomics", icon = icon(name = "code", lib = "font-awesome")),
              hr()))