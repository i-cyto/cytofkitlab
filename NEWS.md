# News for Package **cytofkitlab**

## Changes in version cytofkitlab 1.11.6 (2019-11-08)

### MODIFICATIONS
* the cytof_relocateResults allows to interactively specify the directory of the input FCS files and of the output results. Files and RData object can be moved from one computer to another one easily.
* a log has been initiated in the analysis_results object and will be filled.
* when exporting FCS all flowCore keywords are now removed.

### BUG FIXES
* .


## Changes in version cytofkitlab 1.11.5 (2019-04-15)

### MODIFICATIONS
* Official start of cytofkitlab; a new name to avoid package conflict when debuging.
* UMAP implementation from uwot package is available; no Python use.
* Rphenograph is no more included, see installation notes; this avoids the need of compiler for 
cytofkitlab; Phenograph is still one of the best clustering algorithm.
* the GUI for computation is still using tcl/tk, but has more options and is clearly organized in
sections
* a standard asinh(x/cofactor) transformation is available; the cofactor is set by the user.
* post-analysis with cytofast is possible; readCytofkitFCS allows to import clustering into
cytofast; there is a vignette at http://i-cyto.github.io.
* storeMarkers_GUI creates a cytofkit_markers.txt file (if none defined) with user selected markers
of a FCS file (or the first if not specified) and format them for the analysis.
* getParameters_GUI is exported.

### BUG FIXES
* exporting FCS is still available.
* PDF plots stop when more than 26 samples. Partially fixed.
