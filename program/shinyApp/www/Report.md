# ShinyOmics Report (18/12/2023)
 **AppVersion: 0.1.3 (2022-12-02)** 

## DataInput {.tabset .tabset-fade}

### Info

**DataInput** - Uploaded Omic Type: Transcriptomics

**DataInput** - Test Data set used

**DataInput** - All constant annotation entries for entities and samples are removed from the thin out the selection options!

**DataInput** - The raw data dimensions are:33469, 8

### Publication Snippet

The Transcriptomics data was read into R (v. 4.2.1) (R Core Team (2022). _R: A Language and Environment for Statistical Computing_. R Foundationfor Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.). The raw's data dimensions were: 33469, 8. All annotation that is constant over all samples is hidden within the Shiny-Application, as they do not provide any additional knowledge.This was done with the purrr package (v. 1.0.2)(Wickham H, Henry L (2023). _purrr: Functional Programming Tools_. R package version 1.0.2,<https://CRAN.R-project.org/package=purrr>.)

<br>

## Data Selection

**DataSelection** - The following selection was conducted:

**DataSelection** - Samples:
	 DataSelection - based on: cell: all

**DataSelection** - Entities:
	 DataSelection - based on: geneName: all

## Pre Processing

**PreProcessing** - As general remove all entities which are constant over all samples (automatically)

**PreProcessing** - Preprocessing procedure -standard (depending only on omics-type): Remove anything which row Count <= 10

**PreProcessing** - Preprocessing procedure -specific (user-chosen): filterOnly

**PreProcessing** - The resulting dimensions are: 22008, 8

