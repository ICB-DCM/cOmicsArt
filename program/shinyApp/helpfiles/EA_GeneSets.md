## Gene Sets for Enrichment Analysis

You can choose a variety of gene sets for enrichment analysis. Here you find a 
description of the gene sets. The information was taken from the
[MSigDB database](https://www.gsea-msigdb.org/gsea/msigdb/collections.jsp), where you 
can also find more detailed information as well as lists of genes contained in the sets.

### Hallmark Gene Sets (H)
- **Description:** Summarize specific well-defined biological states or processes with coherent expression.
- **Number of Gene Sets:** 50
<details>
<summary>More Info</summary>
<br>
Hallmark gene sets summarize and represent specific well-defined biological states or processes and display coherent expression. These gene sets were generated by a computational methodology based on identifying overlaps between gene sets in other MSigDB collections and retaining genes that display coordinate expression.
</details>

### Positional Gene Sets (C1)
- **Description:** Correspond to human chromosome cytogenetic bands.
- **Number of Gene Sets:** 301
<details>
<summary>More Info</summary>
<br>
Gene sets corresponding to human chromosome cytogenetic bands.
</details>

### Curated Gene Sets (C2)
- **Description:** Curated from various sources, including pathway databases and biomedical literature.
- **Number of Gene Sets:** 7233
<details>
<summary>More Info</summary>
<br>
Gene sets in this collection are curated from various sources, including online pathway databases and the biomedical literature. Many sets are also contributed by individual domain experts. The gene set page for each gene set lists its source. The C2 collection is divided into the following two subcollections: Chemical and genetic perturbations (CGP) and Canonical pathways (CP).

  - **CGP (Chemical and Genetic Perturbations):** 3438 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    Gene sets represent expression signatures of genetic and chemical perturbations. A number of these gene sets come in pairs: xxx_UP (and xxx_DN) gene set representing genes induced (and repressed) by the perturbation.
    </details>
  - **CP (Canonical Pathways):** 3795 gene sets
    - **BioCarta:** 292 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Canonical Pathways gene sets derived from the BioCarta pathway database.
      </details>
    - **KEGG_MEDICUS:** 619 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Canonical Pathways gene sets derived from the KEGG MEDICUS pathway database.
      </details>
    - **PID:** 196 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Canonical Pathways gene sets derived from the PID pathway database.
      </details>
    - **Reactome:** 1692 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Canonical Pathways gene sets derived from the Reactome pathway database.
      </details>
    - **WikiPathways:** 791 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Canonical Pathways gene sets derived from the WikiPathways pathway database.
      </details>
    - **KEGG_LEGACY:** 186 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Canonical Pathways gene sets derived from the KEGG pathway database, considered Legacy gene sets since the introduction of the gene sets based on the more recent KEGG MEDICUS data.
      </details>
    </details>

### Regulatory Target Gene Sets (C3)
- **Description:** Represent potential targets of regulation by transcription factors or microRNAs.
- **Number of Gene Sets:** 3713
<details>
<summary>More Info</summary>
<br>
Gene sets representing potential targets of regulation by transcription factors or microRNAs. The sets consist of genes grouped by elements they share in their non-protein coding regions. The elements represent known or likely cis-regulatory elements in promoters and 3'-UTRs. The C3 collection is divided into two subcollections: microRNA targets (MIR) and transcription factor targets (TFT). 

- **MIR (MicroRNA Targets):** 2598 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    All miRNA target prediction gene sets. Combined superset of both miRDB prediction methods and legacy sets.
  
    - **miRDB:** 2377 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Gene sets containing high-confidence gene-level predictions of human miRNA targets as cataloged by miRDB v6.0 algorithm (Chen and Wang, 2020).
      </details>
    - **MIR_LEGACY:** 221 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Older gene sets that contain genes sharing putative target sites (seed matches) of human mature miRNA in their 3'-UTRs.
      </details>
</details>
      
  - **TFT (Transcription Factor Targets):** 1115 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    All transcription factor target prediction gene sets. Combined superset of both GTRD prediction methods and legacy sets.
    
    - **GTRD:** 505 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Genes that share GTRD (Kolmykov et al. 2021) predicted transcription factor binding sites in the region -1000,+100 bp around the TSS for the indicated transcription factor.
      </details>
    - **TFT_LEGACY:** 610 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Older gene sets that share upstream cis-regulatory motifs, which can function as potential transcription factor binding sites. Based on work by Xie et al. 2005.
      </details>
    </details>
</details>


### Computational Gene Sets (C4)
- **Description:** Defined by mining large collections of cancer-oriented expression data.
- **Number of Gene Sets:** 1007
<details>
<summary>More Info</summary>
<br>
Computational gene sets defined by mining large collections of cancer-oriented  
expression data. The C4 collection is divided into three subcollections: 3CA, CGN, and CM.

  - **3CA (Curated Cancer Cell Atlas):** 149 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    Gene sets mined from the Curated Cancer Cell Atlas (3CA) metaprograms. These sets consist of genes that are coordinately upregulated in subpopulations of cells within 24 tumor types, covering both generic and lineage-specific cellular processes. The resource underlying this collection is described in Gavish et al. 2023.
    </details>
  - **CGN (Cancer Gene Neighborhoods):** 427 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    Gene sets defined by expression neighborhoods centered on 380 cancer-associated genes. This collection is described in Subramanian, Tamayo et al. 2005.
    </details>
  - **CM (Cancer Modules):** 431 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    Gene sets defined by Segal et al. 2004. Briefly, the authors compiled gene sets ('modules') from a variety of resources such as KEGG, GO, and others. By mining a large compendium of cancer-related microarray data, they identified 456 such modules as significantly changed in a variety of cancer conditions.
    </details>
</details>

### Ontology Gene Sets (C5)
- **Description:** Contain genes annotated by the same ontology term.
- **Number of Gene Sets:** 16008
<details>
<summary>More Info</summary>
<br>
Gene sets that contain genes annotated by the same ontology term. The C5 collection is divided into two subcollections, the first derived from the Gene Ontology resource (GO) which contains BP, CC, and MF components and a second derived from the Human Phenotype Ontology (HPO).
    
  - **GO (Gene Ontology):** 10461 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    All gene sets derived from Gene Ontology.
    
    - **BP (Biological Process):** 7647 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Gene sets derived from the GO Biological Process ontology.
      </details>
    - **CC (Cellular Component):** 1015 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Gene sets derived from the GO Cellular Component ontology.
      </details>
    - **MF (Molecular Function):** 1799 gene sets
      <details>
      <summary>More Info</summary>
      <br>
      Gene sets derived from the GO Molecular Function ontology.
      </details>

  - **HPO (Human Phenotype Ontology):** 5547 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    Gene sets derived from the Human Phenotype Ontology.
    </details>
</details>

### Oncogenic Signature Gene Sets (C6)
- **Description:** Represent signatures of cellular pathways often dysregulated in cancer.
- **Number of Gene Sets:** 189
<details>
<summary>More Info</summary>
<br>
Gene sets that represent signatures of cellular pathways which are often dis-regulated in cancer. The majority of signatures were generated directly from microarray data from NCBI GEO or from internal unpublished profiling experiments involving perturbation of known cancer genes.
</details>

### Immunologic Signature Gene Sets (C7)
- **Description:** Represent cell states and perturbations within the immune system.
- **Number of Gene Sets:** 5219
<details>
<summary>More Info</summary>
<br>
Gene sets that represent cell states and perturbations within the immune system.

  - **ImmuneSigDB:** 4872 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    Gene sets representing chemical and genetic perturbations of the immune system generated by manual curation of published studies in human and mouse immunology.
    </details>
  - **VAX (Vaccine Response):** 347 gene sets
    <details>
    <summary>More Info</summary>
    <br>
    Gene sets curated by the Human Immunology Project Consortium (HIPC), describing human transcriptomic immune responses to vaccinations.
    </details>
</details>

### Cell Type Signature Gene Sets (C8)
- **Description:** Contain curated cluster markers for cell types identified in single-cell sequencing studies of human tissue.
- **Number of Gene Sets:** 830
<details>
<summary>More Info</summary>
<br>
Gene sets that contain curated cluster markers for cell types identified in single-cell sequencing studies of human tissue.
</details>