# Predicting High-Performance Decathlon Career Best

Perry S. Battles, Tyler J. Noble, and Robert F. Chapman

This is the official GitHub repository for the paper _Predicting High-Performance Decathlon Career Best_.

A brief tour of the repo:

+ `DecData.csv` contains all of the raw data used in performing the analysis.
+ `preprocessing_first_best_dist_speed.py` is a Python script that filters the data to contain only marks from
athletes that meet the specified criteria and creates athlete profiles.
+ `decathlete_first_best_analysis_GLM_dist_speed.r` is an R script that performs data transformation,
model fitting, and model evaluation.
+ `coef_bootstrap_CIs.r` is an R script that generates the bootstrapped coefficient difference confidence intervals.
+ `CI_matrix_graph_maker.r` is a Python script that generates graphs for the paper using Matplotlib and Seaborn.
+ `Makefile` is a Makefile that allows for the automated execution of each of the steps of the analysis. See "Replication" below.

## Replication

Note: this analysis was performed using software with the following versions:

+ [Python](https://www.python.org/) 3.12.4
    - [Numpy](https://numpy.org/) 2.0.0
    - [Pandas](https://pandas.pydata.org/) 2.2.2
    - [Matplotlib](https://matplotlib.org/) 3.8.4
+ [R](https://www.r-project.org/) 4.4.1 (2024-06-14) -- "Race for Your Life"

As long as all the directory structure of the project is left intact, all scripts should execute without any errors.
Importantly, the proper versions of Python and R and the attendant packages should be installed to ensure complete
replication of results.

### On \*nix Systems

To replicate the analysis performed in the article, one need only to utilize the provided Makefile. In Bash/Zsh,
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
    - This script produces many graphs that display coefficient comparisons. Those that reflect the bootstrapped
    confidence intervals are found at the following paths:
        * `Images/gamma_analysis_r/first_delta_minus_outliers/log/boot_CI_matrix_first.png`
        * `Images/gamma_analysis_r/first_delta_minus_outliers/log/boot_CI_matrix_delta.png`
        * `Images/gamma_analysis_r/first_delta_minus_outliers/inverse/boot_CI_matrix_first.png`
        * `Images/gamma_analysis_r/first_delta_minus_outliers/inverse/boot_CI_matrix_delta.png`
        * `Images/gamma_analysis_r/first_delta_minus_outliers/identity/boot_CI_matrix_first.png`
        * `Images/gamma_analysis_r/first_delta_minus_outliers/identity/boot_CI_matrix_delta.png`

### Windows

On Windows, the commands can be executed individually to perform the analysis. In Powershell, run:

```bash
python preprocessing_first_best_dist_speed.py
```

Then, to evaluate the data, fit model, evaluate models, and produce datasets without high-leverage outliers, run:

```bash
Rscript decathlete_first_best_analysis_GLM_dist_speed.r
```

To generate bootstrap coefficient estimates, run the following [__WARNING__: this takes about 45 minutes to run]:

```bash
Rscript coef_bootstrap_CIs.r
```

Finally, to generate plots, run:

```
python CI_matrix_graph_maker.py
```

As mentioned above, the directory structure of the project should be left intact to ensure that all scripts execute without any errors.
Importantly, the proper versions of Python and R and the attendant packages should be installed to ensure complete
replication of results.

