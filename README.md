# healthcare-fraud
Data analysis and machine learning detection of healthcare fraud for a Kaggle
[dataset](https://www.kaggle.com/rohitrox/healthcare-provider-fraud-detection-analysis)

The analysis and machine-learning results are reported in the R markdown file
*healthcare_fraud.Rmd*.  The rendered html output is available at

https://markcbutler.github.io/healthcare-fraud/healthcare_fraud.html

## Rendering the markdown file

The R package `renv` was used to create a reproducible environment for
rendering the markdown file.  To install the environment, execute the R
command

`renv:init()`

with the root directory of the cloned repo as the working directory.  An
`renv` environment will be created, and the packages specified by the file
*renv.lock* will be installed into the new environment.

The csv files that provide data for the project can be downloaded from
[Kaggle](https://www.kaggle.com/rohitrox/healthcare-provider-fraud-detection-analysis).
These files should be placed in a `data` directory created in the repo root
directory.
