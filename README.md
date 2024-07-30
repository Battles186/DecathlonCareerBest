# Predicting High-Performance Decathlon Career Best

Perry S. Battles, Tyler J. Noble, and Robert F. Chapman

This is the official GitHub repository for the paper _Predicting High-Performance Decathlon Career Best_.

A brief tour of the repo:

+ `dec_article_first_best_v14.pdf` is a copy of the final paper.
+ `DecData.csv` contains all of the raw data used in performing the analysis.
+ `preprocessing_first_best_dist_speed.py` is a Python script that filters the data to contain only marks from
athletes that meet the specified criteria and creates athlete profiles.
+ `decathlete_first_best_analysis_GLM_dist_speed.r` is an R script that performs data transformation,
model fitting, and model evaluation.
+ `coef_bootstrap_CIs.r` is an R script that generates the bootstrapped coefficient difference confidence intervals.
+ `CI_matrix_graph_maker.r` is a Python script that generates graphs for the paper using Matplotlib and Seaborn.
+ `Makefile` is a Makefile that allows for the automated execution of each of the steps of the analysis. See "Replication" below.

## Replicating

To replicate the analysis performed in the article, one need only to utilize the provided Makefile. On most Linux terminals,
this can be done as follows:

```bash
make build
```

Commands for `make` are as follows:

+ `build` recreates the entire analysis end to end.
+ `preprocess` performs data preprocessing.
+ `r` performs data analysis in R.
+ `boot` generates bootstrapped confidence intervals for the difference in coefficient esimates.
+ `graphs` generates the figures shown in the paper.

