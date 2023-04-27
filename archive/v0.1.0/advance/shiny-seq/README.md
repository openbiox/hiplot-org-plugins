# Shiny-Seq

Complex data analysis can be quite a daunting task for biologists with limited or no programming and statistical background. To alleviate this problem interactive web based applications such as [shinyngs](https://github.com/pinin4fjords/shinyngs), [START](https://github.com/jminnier/STARTapp), [Degust](http://victorian-bioinformatics-consortium.github.io/degust/), [Explore DEG](http://fgcz-shiny.uzh.ch/fgcz_exploreDEG_app/), [DEBrowser](https://bioconductor.org/packages/release/bioc/html/debrowser.html) were designed to assist biologists to explore, visualize, and interpret RNA-Seq data (with particular focus on differential gene expression analysis) to understand the biological phenomena involved in genomic studies. However, these applications still lacked several crucial and useful features in one or more of the steps involved in RNA-Seq analysis. To address these inadequacies, we developed the tool Shiny-Seq that has many new features. As discussed below, these features help to do more comprehensive data analysis and obtain accurate results through suitable statistical analysis and correction techniques. 

Shiny-Seq is a comprehensive application tool that comprises of carefully chosen statistical techniques and R-packages to carry out the various steps required to do down-stream differential expression analysis , and, integrating them to function as a coherent workflow with interactive, visualization, analysis and report generation capabilities. It is designed to act as an interface between the user and the pipeline by taking inputs from the user at each step of the pipeline and calling the appropriate modules in the pipeline for the purpose of exploratory and differential expression analysis of RNA-Seq data. 

## Features unique to Shiny-Seq
- Data Input and pre-processing:
  - Shiny-Seq takes a read count table as input or files generated by transcript quantification software Kallisto.
  - Detection, visualization and adjustment of batch effects after normalization.
- Quality control for hypothesis testing: P-value correction.
- Enrichment Analysis:
  - Transcription factor binding site prediction.
  - Perform enrichment analysis using KEGG pathways, Gene ontology and Molecular signatures.
  - Identify hidden patterns using techniques such as weighted gene co-expression network analysis (WGCNA).
- Visualization:
  - Interactive 3D PCA plot
  - Venn diagram to visualize the overlap of the lists of DE genes obtained from two comparisons.
  - Fold change - fold change plot helps to visualize the fold change of the DE genes common to the two comparisons.
- Sumarize results and generate powerpoint presentation.

## Getting Started

1.
The app is hosted on the website: https://schultzelab.shinyapps.io/Shiny-Seq/.
Example data can be downloaded via "Download annotation" and "Download count data" button.

2.
Download the [App folder](https://github.com/UlasThomas/Shiny-Seq/tree/master). Open [App.r](https://github.com/schultzelab/Shiny-Seq/master/app.R) file and execute it as a shiny app. Make sure to allow loading at least 200 libraries. 

3.
The complete Shiny-Seq app is also available as fully working docker image, which can be downloaded here: (https://hub.docker.com/r/makaho/shiny-seq)

An example dataset has been provided for use in addition to a protocol to explain the various steps involved in doing the RNA-Seq data
analysis and demonstrate how the application helps to do an end to end analysis to derive useful insights.

 

## License
See the [LICENSE](https://github.com/szenitha/Shiny-Seq/blob/master/LICENSE.txt) file for license rights and limitations.