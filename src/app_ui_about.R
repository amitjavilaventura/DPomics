
### DPomics USER INTERFACE                                                 
### =========================================================================================== ###

### DPomics UI about ------------------------------------------------------------------------------
dpomics_ui_about <- 
  tabItem(tabName = "about",
          HTML('<center><p style="font-size:60px;font-family:Helvetica;color:black;"><img src="DPomics_logo.png" width="300px">Welcome to DPomics!</p></center>'),
          HTML("<hr style='border-color:black'>"),
          h1("General information"), br(),
          h2("Goal"),
          p("Shiny app to visualize RNAseq and ChIPseq data and integrate them."),
          br(),
          h2("Contributors"),
          tags$p("DPomics is an app deveolped by:",
                 tags$ul(tags$li(tags$a(href = "https://amitjavilaventura.github.io","Adria Mitjavila Ventura", target="_blank")),
                         tags$li(tags$a(href = "https://github.com/dfernandezperez", "Daniel Fernandez Perez", target="_blank")),
                         tags$li(tags$a(href = "https://github.com/Ferossitiziano", "Federico Rossi", target="_blank")))),
          br(), 
          h1("Instructions"),
          tabsetPanel(id = "instructions_tab_end", type = "pills",
                    tabPanel(title = "RNAseq"),
                    tabPanel(title = "ChIPseq"),
                    tabPanel(title = "Integration | RNAseq + ChIPseq")))