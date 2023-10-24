# MetaAnalysis in R
This is the repository for the implementation of a meta-analysis on true effect of minimum wage on wage inequality.
the conducted meta-analysis focuses on ten papers published in the period between 2004 and 2021 that target different regions worldwide, including both developed and developing countries.
The meta-analysis is conducted using the package metafor for R.

The organisation is as follow:
- meta_code.R runs the meta-analysis logic.
- md.csv is the main data file.
- discarded.csv contains the papers discarded from the analysis.
- project.R groups the source code.

The file meta_code.R implements the meta-analisis following this logic:
- Basic meta regression
- Robustness check
- Pubblication bias
- Subgroup analysis
- Meta regression
- Multi model interference


