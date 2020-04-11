# cytofkitlab 1.11.4

* Official start of cytofkitlab; a new name to avoid package conflict when debuging.
* UMAP implementation from uwot package is available.
* Rphenograph is no more included, see installation notes; this avoids the need of compiler for 
cytofkitlab; Phenograph is still one of the best clustering algorithm.
* readCytofkitFCS allows to import clustering into cytofast; there is a vignette at 
http://i-cyto.github.io.
* storeMarkers_GUI creates a cytofkit_markers.txt file (if none defined) with user selected markers
of a FCS file (or the first if not specified) and format them for the analysis.
* getParameters_GUI is exported.
