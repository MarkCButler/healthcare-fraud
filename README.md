# healthcare-fraud
Analysis of a healthcare-fraud
[dataset](https://www.kaggle.com/rohitrox/healthcare-provider-fraud-detection-analysis)
from Kaggle

The analysis has two goals:

  - Provide general insight into the healthcare market represented by the
    dataset
  - Reveal associations involving the "potential fraud" label for healthcare
    providers in the training dataset

The analysis is presented using R markdown in the file *healthcare_fraud.Rmd*.
The rendered html output is available at

https://markcbutler.github.io/healthcare-fraud/healthcare_fraud.html

## Rendering the markdown file

The R package `renv` was used to create a reproducible environment for
rendering the markdown file.  To install the environment, execute the R
command

`renv::init()`

with the root directory of the cloned repo as the working directory.  An
`renv` environment will be created, and the packages specified by the file
*renv.lock* will be installed into the new environment.

For the `renv` environment to be active, the *.Rprofile* file created by
`renv::init()` must be executed during R startup.  This is accomplished by
starting R with the repo root directory as the working directory.  In an IDE
such as RStudio or PyCharm, a project based in the repo root directory can be
created, and when this project is opened, the `renv` environment will be
active.

The csv files that provide data for the project can be downloaded from
[Kaggle](https://www.kaggle.com/rohitrox/healthcare-provider-fraud-detection-analysis).
These files should be placed in a `data` directory created in the repo root
directory.

The R markdown file includes JavaScript commands to load an image
`images/fraud-photo.jpg` and place it at the top of the displayed html page.
This image is not stored in the repo's master branch.  Instead, it is
available in the gh-pages branch, which also contains the rendered output file
*healthcare_fraud.html*.  (Note that the link
https://markcbutler.github.io/healthcare-fraud/healthcare_fraud.html given
above points to this html file in the gh-pages branch.)

After the R markdown file *healthcare_fraud.Rmd* in the master branch has been
rendered, copy the `images` directory from gh-pages into the directory where
the output file *healthcare_fraud.html* is stored.  This will allow the image to be
displayed in the html page.
