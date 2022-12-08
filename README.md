# Data Input Notes:

![example workflow](https://github.com/LeaSeep/OmicShiny/actions/workflows/run-tests.yaml/badge.svg)

![develop status](https://github.com/LeaSeep/OmicShiny/actions/workflows/run-tests.yaml/badge.svg?branch=develop)

![main status](https://github.com/LeaSeep/OmicShiny/actions/workflows/run-tests.yaml/badge.svg?branch=main)

3 tables are needed:
1. data-table (e.g. counts or intensitites)
  - row names must be entities (e.g. genes or metabolites)
  - column names must be sample names
2. sample annotation (e.g. Treatment, tissue type) 
  - rownames must match the colnames from (1.), hence each sample get a row
  - MUST have a column called global_ID (same as rownames) with unique entries
3. entitie annotation (e.g. type of gene or assoc pathway of metab)
  - rownames must match the rownames from (1.)
