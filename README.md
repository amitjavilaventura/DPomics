# Shiny DP <img src="www/DPomics_logo.png" align="right" alt="" width="250" />

_**DPomics**_: **DP** lab **omics** visualization and integration

## Goal

Shiny app to visualize RNAseq and ChIPseq data and integrate them. The data loaded here ouptus from the Snakemake RNAseq and ChIPseq pipelines from [dfernandezperez](https://github.com/dfernandezperez/), but it could be anything like:

* RNAseq input data is a data frame made from several DESeq2-like table with the following columns (several come directly from DESeq2):

Contrast | Geneid | log2FoldChange | padj | DEG | 
:------: | :----: | :------------: | :--: | :-: |
Extra column with the name of the contrasts where the gene in the corresponding row comes from | ID of the gene (i.e. Gene symbol) | log^2^(FC) | Adjusted p-value | Extra column with the differential expression of the corresponding genes, values are "Upregulated", "Downregulated", "NS".

* ChIPseq input data comes from `annotatePeak()` with all the conditions and has the following columns (several come directly from `annotatePeak()`):

seqnames | startnd | end            | annotation | SYMBOL | distanceToTSS | sample   | condition | protein   |
:------: | :----:  | :------------: | :--:       | :-:    | :-----------: | :------: | :------:  | :------:  |
Chromosome number (i.e. chr1) | Starting position of the peak | Ending position of the peak | Region where the peak is annotated (i.e. Distal intergenic) | Gene symbol or any identificator of the nearest gene (i.e. Lef1) | Distance to TSS of the nearest gene | Extra column with the sample of the corresponding peak, ideally should be condition_protein. | Column with the condition where the peak comes from .| Chipped protein. |



## Instructions

## General information

### Contributors

**Developers**: [amitjavilaventura](https://github.com/amitjavilaventura/), [dfernandezperez](https://github.com/dfernandezperez/) and [ferossitiziano](https://github.com/ferossitiziano/)

### Dependencies

*DPomics* depends on several R packages:

* Shiny-realted packages

   + `shiny`
   + `DT` <br><br>

* To have tidy code

   + `tidyverse`
   + `magrittr` <br><br>

* To work with lists and granges

   + `plyranges`<br><br>

* To plot things

   + `VennDiagram`: to plot the Venn diagrams from the intersecting degs
   + `ComplexHeatmap`: to plot the log2FC heatmaps
   + `ChIPpeakAnno`: to plot the Venn diagrams of the overlapping ChIP peaks
   + `cowplot`
   + `ggpubr`
   + `circlize` <br><br>
   

## To do

* Modify the Snakemake pipelines to create the desired input (one rule) --> easy.
* **Include download options for lists and plots**: `write.table()` and `ggsave()`/`png()` + `actionButton()`?
* Include more parameters in the RNA-seq Heatmaps.
* Put gene ontology in DEGs --> will be very slow using `clusterProfiler`.
* Integrate RNAseq and ChIPseq data. --> at the moment,  the integration does not work.
* Upload app to my shinyapps.io server.
* Make a Desktop app with `electron`/`photon` (make 2 different apps: 1 for MAC and 1 for Windows).

## CRAZY ideas

* Allow more omics data, like ChIPs from histone modifications (i.e. H3K4me1, H3K4me3, H3K27me3, H3K27ac), so we will able to predict some enhancers.
* Integrate all the data with the preexisting one. It can be done in R, so it should not be difficult
  