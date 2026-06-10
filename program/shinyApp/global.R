## cOmicsArt Global Configuration
## Loaded once at app startup

# Load required packages
library(DT)
library(plotly)
library(waiter)
library(shiny, lib.loc = .libPaths()[1])
library(shinyWidgets)
library(shinymanager)
library(shinyjs)
library(DESeq2)
library(grid)
library(ggplot2)
library(pheatmap)
library(pathview)
library(clusterProfiler)
library(BiocManager)
library(shinyhelper)
library(dplyr)
library(shinycssloaders)
library(ggpubr)
library(org.Mm.eg.db)
library(org.Hs.eg.db)
library(jsonlite)
library(rmarkdown)
library(tinytex)
library(testthat)
library(shinytest)
library(biomaRt)
library(zip)
library(cicerone)
library(shinyalert)
library(msigdbr)
library(tidyr)
library(kableExtra)
library(readxl)
library(ggvenn)
library(ComplexUpset)
library(gridExtra)
library(sva)
library(pcaPP) # requires gfortran. Not sure how to install on server
library(reshape2)
library(cowplot)  # already imported but now we use it explicitly
library("ComplexHeatmap")
library(svglite)
library(formattable)
library(codetools)
library(vsn)
library(hexbin)
library(htmlwidgets)

# Load global constants and utilities
source("R/C.R")
source("R/C_strings.R")

# Note: SourceAll.R CANNOT be moved to global.R without major refactoring
# because many functions rely on lexical scoping to access 'session'
# See PHASE1_ISSUES.md for details and solutions

# Source UI components for each panel
source("R/help_tab/ui.R")
source("R/data_selection/ui.R")
source("R/pre_processing/ui.R")
source("R/pca/ui.R")
source("R/heatmap/ui.R")
source("R/single_gene_visualisation/ui.R")
source("R/enrichment_analysis/ui.R")
source("R/sample_correlation/ui.R")
source("R/significance_analysis/ui.R")

# Set global options
options(repos = BiocManager::repositories())
options(spinner.color = "#1c8a3b", spinner.color.background = "#ffffff", spinner.size = 2)
