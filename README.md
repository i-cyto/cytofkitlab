cytofkitlab: a fork of cytofkit, an integrated mass cytometry data analysis pipeline
============

**NOTE**: If you are looking for the **original cytofkit**, go to http://jinmiaochenlab.github.io/cytofkit/.

**NOTE**: If you are looking for the NEWS about cytofkit**lab**, it's [here](NEWS.md).


### cytofkitlab

cytofkit**lab** is a fork of cytofkit, the famous package designed to facilitate the analysis workflow of mass cytometry data with automatic subset identification and mapping of cellular progression.

cytofkit**lab** will be forever in development. Please use the Issues section to improve it.

cytofkit**lab** does not include the Phenograph clustering anymore. Phenograph is available in as a **required** external package [Rphenograph](http://github.com/i-cyto/Rphenograph). You will have to install Rphenograph only once. Because cytofkit**lab** does not require a compiler anymore, it is easier and quicker to update and deploy it.

### Installation

#### 1. Install R and Rstudio

If you have never used R, please install R and Rstudio following the steps below:

- Download the proper R version for your operation system from [R download page](https://cran.r-project.org).

- Double-click the downloaded R installation file. For Windows, install with the 64 bits version with core files, but not the 32 bits version and neither the translations, as it is easier to find help in english using web search engines. For other OS, use defaults.

- Download the proper Rstudio version for your operation system from [here](https://www.rstudio.com/products/rstudio/download/).

- Double-click the downloaded Rstudio installation file, install with all the defaults.


<u>**Special Notes for Mac Users**</u>

For Mac OS X 10.8 or later, you need to install XQuartz to support the GUI:

* Download the disk image (dmg) file for [XQuartz](http://xquartz.macosforge.org).

* Open the dmg file by double-clicking on it, you'll find XQuartz.pkg, double-click on it to run the installer, clicking through all the defaults.

* After the installer runs, you'll have to **restart your mac computer**.


#### 2. Install cytofkitlab package

The Phenograph algorithm must be installed as a separated package Rphenograph.

For Windows, if you installed R 64 bits (without 32 bits) you can quickly install Rphenograph binary
version for R 64 bits with the following commands:

``` r
# if not already installed, install Rphenograph from binary release for R 64 bits on Windows
if (!requireNamespace("Rphenograph", quietly = TRUE))
  install.packages("https://github.com/i-cyto/Rphenograph/releases/download/0.99.1.9002/Rphenograph_0.99.1.9002.zip", repos = NULL, type = "win.binary")
```

For all other configurations, please use a classical installation approach:

``` r
# install devtools if not already done
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# install Rphenograph if not already installed
if (!requireNamespace("Rphenograph", quietly = TRUE))
  devtools::install_github("i-cyto/Rphenograph")
```

Install cytofkitlab development version:

``` r
# install devtools if not already done
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# install cytofkitlab
devtools::install_github("i-cyto/cytofkitlab")
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
