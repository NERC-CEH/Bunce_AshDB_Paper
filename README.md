# Bunce AshDB Paper
Code to support paper on ash dieback and deer effects on woodland flora, titled "The increasing role of tree disease and decreasing influence of anthropogenic management over fifty years of woodland dynamics", accepted for publication in Proceedings of the Royal Society B

Authors: Fiona M Seaton, Claire M Wood, Karen Hornigold, Keith J Kirby, Chris Nichols, David Jam, Emma Dear, Adam Kimberley, Simon M Smart

Correspondence: fseaton@ceh.ac.uk

This includes:

* [01_Summary_Calculations](01_Summary_Calculations.R) An R script for turning the raw data (as found on the EIDC) into summary datafiles for the models
* [02_ChangeOverTime](02_ChangeOverTime.Rmd) An R markdown script for running all models of change over time in the summary metrics (includes all checks, summaries and plots)
* [03_AshDieback](03_AshDieback.Rmd) An R markdown script for running all models of ash dieback effects (includes all checks, summaries and plots)
* [04_Deer](04_Deer.Rmd) An R markdown script for running all models of deer risk effects (includes all checks, summaries and plots)
* [05_AshDiebackandDeer](05_AshDiebackandDeer.Rmd) An R markdown script for running all models of interacting ash dieback and deer effects (includes all checks, summaries and plots)
* [06_Multivariate_Models](06_Multivariate_Models.Rmd) An R markdown script for running all multivariate models of interacting ground flora responses to ash dieback and deer (includes all checks, summaries and plots)
* [Helper_functions](Helper_functions.R) An R script that creates some functions used in the other files


The Metadata folder includes files used in the analysis - the sources for these are elsewhere (detailed in paper), but they are collected here for ease of analysis.

* AMALG_SPCODES.csv - contains the BRC codes for species amalgamations and their growth forms
* AWI_INDICATOR_LISTS.csv - contains regional Ancient Woodland Indicator (AWI) lists
* DEER_RISK.csv - contains deer risk ratings for all 103 sites
* SITES_AWI_REGIONS.csv - contains the AWI region for each of the 103 sites
* TREE_AMALGAMS.csv - contains the BRC codes for all tree species amalgamations

 
The analysis was carried out within R version 4.4.0, brms version 2.21.0, rstan version 2.36.6, emmeans version 1.10, and tidybayes version 3.0.6.
