# MetaAnalysis in R
This is the repository for the implementation of a meta-analysis on true effect of minimum wage on wage inequality.

The organisation is as follow:
- meta_code.R runs the meta-analysis logic.
- md.csv: the data file tha
- discarded.csv runs the front-end logic.
- project.R groups the source code.

The meta-analysis is conducted using the package citation(package='metafor'):
- config.R file for storing global variables used in the source code (n.b. external ip is dynamic and needs to be changed manually before running the code).
- run_shiny.R is the main script for running the unser interface and the server logic in Shiny.