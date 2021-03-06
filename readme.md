# bart_rep_design_OSF

This repository contains the data, analysis scripts and figures for the preregistration and later the studies 1 and 2 of the project "Representative Design in Psychological Assessment: A Case Study Using the Balloon Analogue Risk Task (BART)."

*data* contains four subfolders, *experiment*, *reanalyses*, *s1*, and *s2*. *experiment*, contains a file with the sequences of explosion points used in study 1. *reanalyses* contains the datafiles used for the reanalyses of the studies by Frey et al. (2017), Schürmann, Frey, and Pleskac (2018), and Steiner and Frey (2019). The data files of the former two can also be accessed with all other data of these studies via https://osf.io/rce7g/, and https://osf.io/bq3ym/, respectively. Please also see these repositories for descriptions of the files. *s1* and *s2* contain the datafiles of studies 1 and 2, respectively. Please see the Codebook.pdf for a detailed description of these datafiles and the variables therein.

*r* contains the R scripts used to run the analyses and for generating the plots. *bart_distributions_plot.R* creates figure *plots/bart_architecture.pdf* (that is, Figure 1 in the Preregistration). *create_bart_distributions.R* creates the sequences of explosion points stored at *data/experiment/bart_distributions.RDS*. *helper.R* contains the functions for the fitting procedure used in the reanalyses reported in the Preregistration. Finally, *reanalyses.R* runs the reanalyses and creates Figure 2 of the Preregistration (*plots/reanalyses.pdf*). The two subfolders *s1*, and *s2*, contain the data preparation and -analysis code.

*plots* contains the figures generated by the R scripts.
