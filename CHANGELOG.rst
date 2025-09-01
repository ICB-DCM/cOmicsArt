Release notes
=============


1.2 series
..........


1.2.2 (2025-09-01)
------------------
**General**
- Improved Documentation (#443)
- Improved Column detection in upload (#539)
- Various Code improvements (#547, #559)
- Allow to copy of every plot to the Clipboard (#555)
- Increased file size limit to 70MB (#558)
**Preprocessing**
- Fixed bugs that led to failure of DESeq2 normalization (#544, #545, #548)
- Report generation (#546)
- Report batch correction output to user (#550)
**Differential Analysis**
- Volcano Plot improvements (#552)


1.2.1 (2025-03-25)
------------------
- Fixed bug in R code Download of Heatmap (#528)
- Fixed bug in Notes of Preprocessing reports (#527)
- Adjusted welcome page (#533)
- Semiautomatic default for PCA loadings matrix (#534)
- ".svg" option for downloads (#531)
- Documentation


1.2.0 (2025-02-21)
------------------
(Automated Changelog generation)
* Adjustments for review.
* Code creation simplified
* Plots updated (e.g. ellipses in PCA)
* Clearer navigation
* Improved visual inspection now automatically able to fix common mistakes


1.1.0 (2024-12-19)
------------------
(Automated Changelog generation)
* Changed the hyperlink from comicsart.org to iaas.uni-bonn.de site by @anikde07 in https://github.com/ICB-DCM/cOmicsArt/pull/345
* Remove Test data options till further development by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/348
* Added a toast in case of test data being used by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/347
* PCA scaling by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/346
* Log2 pre processing by @anikde07 in https://github.com/ICB-DCM/cOmicsArt/pull/315
* Fixed scaling and positioning of loading comic by @anikde07 in https://github.com/ICB-DCM/cOmicsArt/pull/337
* add waiters show/hide to all R Code Download by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/351
* First time user help by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/366
* Clearer disconnect by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/368
* Hide main panel options by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/371
* DESeq2 preprocessing by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/372
* Data Selection is "Optional" by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/373
* Fixed two small bugs by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/376
* Updated Documentation by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/374
* added info about test data, as well as citing resources by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/379
* Fix Matrix Not Full Rank by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/394
* Adjust helper padding by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/395
* Cookie first help by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/378
* Changed link and renamed image file by @anikde07 in https://github.com/ICB-DCM/cOmicsArt/pull/381
* Conditionally hide Result tab in SigAna by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/397
* Tutorial Adjustment by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/398
* Add questionnaire by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/403
* Update question marks by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/405
* Add excel workbook by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/407
* Visual inspection update by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/408
* "New Mini Release" by @PaulJonasJost in https://github.com/ICB-DCM/cOmicsArt/pull/406
* Updates based on thorough first user assessment by @LeaSeep in https://github.com/ICB-DCM/cOmicsArt/pull/426


1.0.0 (2022-12-02)
-------------------
Public Release of cOmicsArt alongside publication


0.1 series
..........


0.1.3 (2022-12-02)
-------------------
* General
   * Fix various bugs (#78, #76, #84, #80, #85, #86, #91)
* Significance Panel (#81)
    * allows for significance analysis of two conditions, multiple times
    * choice of test, adjustment
    * summary of each significance testing
    * visualisations of summaries via VennDiagramm and UpSetR


0.1.2 (2022-11-23)
-------------------
* General:
    * Fixes for server (#71)


0.1.1 (2022-11-23)
-------------------
* General:
    * Aesthetic Tab colors (#52)
    * Fixed birthday joke (#63)
    * Easy testing for user (#64)
* PC:
    * New visualisation method (#51)
* EA and OA:
    * Absolute LFC as default value in EA (#57)
    * Complete EA sets choice (#55)
    *  Data Table visualization (#53)
    * OA translation fixed (#59)


0.1.0 (2022-10-25)
-------------------
* General:
  * Renamed `Project to lower Dimension` to `PCA` (#10)
  * Modularization (#11, #13, #24, #25, #27, #28, #26, #38)
  * bug fixes(#14, #16, #33, #34, #36, #39)
  * Coding Style (#20, #21)
  * Added changelog (#49)
* Sample Correlation:
  * New Module (#35)
* Enrichment Analysis:
  * Additional Gene set allowed + selection (#41 , #22 )
  * fixed ORA (#48)
