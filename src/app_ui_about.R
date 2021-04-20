
### DPomics USER INTERFACE                                                 
### =========================================================================================== ###

### DPomics UI about ------------------------------------------------------------------------------
dpomics_ui_about <- 
  tabItem(tabName = "about",
          HTML('<center><p style="font-size:60px;font-family:Helvetica;color:black;"><img src="MYomics.png" width="300px">Welcome to MYomics!</p></center>'),
          HTML("<hr style='border-color:black'>"),
          h1("General information"), br(),
          h2("Goal"),
          p("Shiny app to visualize RNAseq and ChIPseq data and integrate them."),
          br(),
          h2("Contributors"),
          tags$p("DPomics is an app deveolped by:",
                 tags$ul(tags$li(tags$a(href = "https://amitjavilaventura.github.io","Adria Mitjavila Ventura", target="_blank")))),
         
          br(),br(),br(),
          h3("License"),
          HTML('<center><a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">
               <img alt="Creative Commons Licence" style="border-width:0" 
               src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" />
               </a><br />This work is licensed under a <a rel="license" 
               href="http://creativecommons.org/licenses/by-nc-nd/4.0/">
               Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License
               </a>.</center>'))


### DPomics UI instruction ----------------------------------------------------------------------------
dpomics_ui_instructions <- 
  tabItem(tabName = "instructions",
          h1("Instructions"),
          tags$p("For detailed instructions on how to obtain the files 
                 used in these visualizations, look at the ", 
                 tags$a(href = "https://github.com/amitjavilaventura/DPomics", "DPomics Github repository", target="_blank"), "."),
          HTML('<p style="font-size:20px;font-family:Helvetica;color:red;">Some features may not be able yet!!!</p>'),
          tabsetPanel(id = "instructions_tab_end", type = "pills",
                      tabPanel(title = "RNAseq",
                               br(),
                               h3("RNAseq data visualization instructions"),
                               tags$p("To visualize the RNAseq data you must go through the following steps:",
                                      tags$ol(tags$li("Go to the RNAseq tab in the left sidebar."),
                                              tags$li("Go to 'Upload & explore'."),
                                              tags$ul(tags$li("Inside the 'RNAseq inputs' box, click to 'Browse' to browse for the DEG file you want to visualize."),
                                                      tags$li("Check that the file is in TSV format and that has, at least, the following columns with the exact column name (without quotes):"),
                                                      tags$ul(tags$li("Contrast: the names of the contrasts in the analysis."), 
                                                              tags$li("Geneid: the Gene IDs (i.e. Lef1)"), 
                                                              tags$li("log2FoldChange: the log2 value of the fold-change"), 
                                                              tags$li("padj: the adjusted p-value."), 
                                                              tags$li("DEG: a column with the DE status of that gene. Possible values are 'Upregulated', 'Downregulated' and 'NS'.")),
                                                      tags$li("Once you decided which file do you want to visualize, select it, click to open and then click to upload."),
                                                      tags$li("If you wish, you can change the log2FC and the p-value thresholds. Then reclick 'Upload' to refresh the data."),
                                                      tags$li("A table with the data in the file will appear. There you can sort the data or search for any term (i.e. Geneid) using the search box."),
                                                      tags$li("IMPORTANT: you must upload the correct data to go through the following steps and the integration.")),
                                              tags$li("Go to 'Volcano plots'."),
                                              tags$ul(tags$li("If the data has been loaded correctly in the 'Upload & explore' tab, in the 'Volcano contrasts' box you will see a list with the contrasts in your data."),
                                                      tags$li("Select the contrasts you want to plot."),
                                                      tags$li("Select the parameters you want (i.e. sizes of text and plot, show DEG labels...)"),
                                                      tags$li("Click to 'Plot volcanos'")),
                                              tags$li("Go to 'Overlap DEGs'."),
                                              tags$ul(tags$li("If the data has been loaded correctly in the 'Upload & explore' tab, in the 'Overlaps 1' and 'Overlaps 2' boxes you will see a list with the contrasts in your data."),
                                                      tags$li("Select up to 3 contrasts in each box to plot the venn diagrams of the overlapping DEGs in those contrasts"),
                                                      tags$li("Click to 'Plot overlaps' to plot the Venn diagrams."),
                                                      tags$li("You can to do it twice so you will be able to do comparisons.")),
                                              tags$li("Go to 'Heatmaps'."),
                                              tags$ul(tags$li("If the data has been loaded correctly in the 'Upload & explore' tab, you should be able to see a text box."),
                                                      tags$li("Write the Gene IDs of the genes you want to plot. The gene names must be exactly the same as the ones appearing in the 'Upload & explore' table "),
                                                      tags$li("Click to 'Plot heatmap'."),
                                                      tags$li("Note: the values used to plot the heatmap are only log2(FC), the p-values are not used. ç
                                                              Hence, a gene may have a log2(FC) that surpasses the DE threshold, but it may be not statistically significant."))))),
                      
                      ## CHIP SEQ INSTRUCTIONSS
                      tabPanel(title = "ChIPseq",
                               br(),
                               h3("ChIP-seq data visualization instructions"),
                               tags$p("To visualize the ChIP-seq data you must go through the following steps:",
                                      tags$ol(tags$li("Go to the ChIP-seq tab in the left sidebar."),
                                              tags$li("Go to 'Upload & explore'."),
                                              tags$ul(tags$li("Inside the 'ChIP-seq inputs' box, click to 'Browse' to browse for the peak file you want to visualize."),
                                                      tags$li("Check that the file is in TSV format and that has, at least, the following columns with the exact column name (without quotes):"),
                                                      tags$ul(tags$li("seqnames: the names of the chromosomes (i.e. chr1, chr2, chr19, chrY, chrM...)."), 
                                                              tags$li("start: starting position in the chromosome."), 
                                                              tags$li("end: ending position in the chromosome."), 
                                                              tags$li("log10pval: base-10 logarithm of the p-value (or q-value)."), 
                                                              tags$li("annotation: region where the peak falls (i.e. Promoter, Distal Intergenic, 5' UTR...)."),
                                                              tags$li("SYMBOL: genesymbol of the nearest gene (i.e. Nanog, NANOG...)."),
                                                              tags$li("sample: sample where the peaks come from, ideally, should be 'condition_protein'."),
                                                              tags$li("condition: condition where the peaks come from."),
                                                              tags$li("protein: protein chipped.")),
                                                      tags$li("Once you decided which file do you want to visualize, select it, click to open and then click to upload."),
                                                      tags$li("A table with the data in the file will appear. There you can sort the data or search for any term (i.e. Geneid) using the search box."),
                                                      tags$li("IMPORTANT: you must upload the correct data to go through the following steps and the integration.")),
                                              tags$li("Go to 'Peaks'."),
                                              tags$ul(tags$li("In the 'Number of peaks' box, select where you want the legend."),
                                                      tags$li("Check if you want to separate different proteins in different panels (this only works if the 'protein' and 'condition' columns are present in the data)"),
                                                      tags$li("Click to 'Plot peaks' to draw a bar plot with the number of peaks in each sample"),
                                                      tags$li("In the 'Peak distribution' box, select wether to annotate peaks in 2 (distal/promoter) or 3 (distal/promoter/gene body) genomic features."),
                                                      tags$li("Decide wether to plot the total number of peaks or the proportion to 1."),
                                                      tags$li("Check if you want to separate different proteins in different panels (this only works if the 'protein' and 'condition' columns are present in the data)"),
                                                      tags$li("Click to 'Plot distribution' to draw a bar plot with the distribution of the peaks.")),
                                              tags$li("Go to 'Intersection'."),
                                              tags$ul(tags$li("If the data has been loaded correctly in the 'Upload & explore' tab, in the 'Intersection 1' and 'Intersection 2' boxes you will see a list with the contrasts in your data."),
                                                      tags$li("Select up to 3 conditions in each box to plot the venn diagrams of the overlapping peaks in those conditions."),
                                                      tags$li("Click to 'Plot intersection' to plot the Venn diagrams."),
                                                      tags$li("You can to do it twice so you will be able to do comparisons.")),
                                              tags$li("Go to 'Custom coordinates'."),
                                              tags$ul(tags$li(HTML("<h2 style='color:red;'>NOT AVAILABLE YET</h2>")))))),
                      
                      ## ATAC-seq
                      tabPanel(title = "ATAC-seq",
                               HTML("<h1 style='color:red;'>NOT AVAILABLE YET</h1>"))))
          
          
          