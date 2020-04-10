cytofkitlab: a fork of cytofkit, an integrated mass cytometry data analysis pipeline
============

**NOTE**: If you are looking for **cytofkit**, go to http://jinmiaochenlab.github.io/cytofkit/

### cytofkitlab

cytofkit**lab** is a fork of cytofkit, the famous package designed to facilitate the analysis workflow of mass cytometry data with automatic subset identification and mapping of cellular progression.

cytofkit**lab** will be forever in development. Please use the Issues section to improve it.

cytofkit**lab** does not include the Phenograph clustering anymore. Phenograph is available in as a **required** external package [Rphenograph](http://github.com/i-cyto/Rphenograph). You will have to install Rphenograph only once. Because cytofkit**lab** does not require a compiler anymore, it is easier and quicker to update and deploy it.

### Installation

#### 1. Install R and Rstudio

If you have never used R, please install R and Rstudio following the steps below:

- Download the proper R version for your operation system from [R download page](http://cran.stat.nus.edu.sg).

- Double-click the downloaded R installation file, install with all the defaults.

- Download the proper Rstudio version for your operation system from [here](https://www.rstudio.com/products/rstudio/download/).

- Double-click the downloaded Rstudio installation file, install with all the defaults.


<u>**Special Notes for Mac Users**</u>

For Mac OS X 10.8 or later, you need to install XQuartz to support the GUI:

* Download the disk image (dmg) file for [XQuartz](http://xquartz.macosforge.org).

* Open the dmg file by double-clicking on it, you'll find XQuartz.pkg, double-click on it to run the installer, clicking through all the defaults.

* After the installer runs, you'll have to **restart your mac computer**.


#### 2. Install cytofkitlab package

Install this development version:

``` r
# install devtools if not already done
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# install cytofkitlab
devtools::install_github("i-cyto/cytofkitlab")
```

The Phenograph algorithm must be installed as a separated package Rphenograph:

``` r
# install devtools if not already done
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# install Rphenograph if not already installed
if (!requireNamespace("Rphenograph", quietly = TRUE))
  devtools::install_github("i-cyto/Rphenograph")
```

### Usage

Same as cytofkit. After successfully installing the package, run the following codes to open the GUI:

``` r
library(cytofkitlab)
cytofkit_GUI()
```

Using the cytofkit shinyAPP to explore your analysis results:

``` r
cytofkitShinyAPP()
```

<u>Check the following vignettes for more details:</u>

- [Analysis Pipeline](http://bioconductor.org/packages/3.6/bioc/vignettes/cytofkit/inst/doc/cytofkit_workflow.html)    
- [Quick Start](http://bioconductor.org/packages/3.6/bioc/vignettes/cytofkit/inst/doc/cytofkit_example.html)   
- [ShinyAPP Tutorial](http://bioconductor.org/packages/3.6/bioc/vignettes/cytofkit/inst/doc/cytofkit_shinyAPP.html)    
