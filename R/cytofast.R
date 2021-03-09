#' Read .fcs files created by Cytofkit and return a cytofast object
#'
#' @description This function reads and combines .FCS files created specifically
#'   by Cytofokit.
#'
#' @param dir directory containing the .FCS files created by Cytofkit
#' @param colNames character string that specifies which label should be used as
#'   the column names. This could be the name of the markers 'names' or the
#'   description for the markers 'description'.
#' @param clusterID character string that specifies which label should be used
#'   to retrieve the cluster identifiers. Typically, this should be
#'   "FlowSOM_clusterIDs", "Rphenograph_clusterIDs" or "ClusterX_clusterIDs"
#' @param cleanUp logical that specifies to remove extra channels added by
#'   Cytofkit
#'
#' @return The function returns an object of class
#'   \code{\link[cytofast]{cfList-class}}.
#'
#' @note To successfully read the .FCS files created by cytofkit make sure that
#'   there are no double channels. It is possible to remove the double tag in
#'   software like Flowjo.
#'
#'   As for the `colNames` parameter, both 'name' and 'description' will always
#'   work, it is up to the user to decide which one is most preferable. "" or
#'   "<NA>" descriptions are replaced by the corresponding name.
#'
#'   This function is a wrapper around \code{\link[flowCore]{read.FCS}}. For
#'   more flexibility see their help page. Parameters currently set are:
#'   transformation=FALSE, truncate_max_range=FALSE, min.limit = NULL
#'
#'   NB: No compensation nor transformation is applied to the data.
#'
#' @importFrom methods new
#' 
#' @keywords read, data, FCS, cytofast
#'
#' @examples
#' 
#' # Directory of input FCS
#' dirFCS <- system.file('extdata',package='cytofkitlab')
#' # For demo, simulate a dir of results and copy the demo result .RData file
#' dirRes <- tempdir()
#' file.copy(file.path(dirFCS, "demo_fcs.RData"), file.path(dirRes, "demo_fcs.RData"))
#' # Load an annotated result .RData
#' load(file.path(dirRes, "demo_fcs.RData"))
#' # Write clustered data with Rphenograph to FCS files in a temporary directory
#' cytof_writeResults(
#'     analysis_results,  # from load function
#'     projectName = "demo",
#'     saveToRData = FALSE,
#'     saveToFCS = TRUE,
#'     saveToFiles = FALSE,
#'     resultDir = dirRes,
#'     rawFCSdir = dirFCS,
#'     inverseLgclTrans = FALSE)
#' # Verify the FCS files
#' #   fn <- dir(file.path(tempdir(), paste0("demo","_analyzedFCS")), full.names = TRUE)
#' #   flowCore::read.FCS(fn)
#' # Read FCS files into a cytofast cfList object
#' cfData <- readCytofkitFCS(
#'     dir = file.path(dirRes, paste0("demo","_analyzedFCS")),
#'     colNames = "description",
#'     clusterID = "Rphenograph_clusterIDs"
#' )
#' cfData
#'
#' @export
readCytofkitFCS <- function(
    dir = NULL, 
    colNames = c("description", "names"), 
    clusterID = "FlowSOM_clusterIDs",
    cleanUp = TRUE
) {
    if (!requireNamespace("cytofast", quietly = TRUE)) {
        stop("Package \"cytofast\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    
    # Check FCS dir
    if(is.character(dir) == FALSE){
        stop("directory is not a character string")
    } else if(dir.exists(dir) == FALSE) {
        stop("directory does not exist")
    }
    
    FCSFilePaths <- list.files(path=dir, pattern=".fcs", full.names=TRUE)
    FCSFileNames <- list.files(path=dir, pattern=".fcs", full.names=FALSE)
    if(length(FCSFileNames) == 0){
        stop("there are no .fcs files in this directory")
    }
    
    # Check arguments
    colNames <- match.arg(colNames)
    if(!is.logical(cleanUp))
        stop("cleanUp parameter should be TRUE or FALSE")
    
    # Check clusterID column name
    colnamesFCS <- flowCore::colnames(flowCore::read.FCS(
        FCSFilePaths[1], which.lines = 1:100, truncate_max_range = FALSE))
    if(!clusterID %in% colnamesFCS){
        stop("clusterID \"", clusterID, "\" column not found in channels of the first FSC file.")
    }
    
    # Loop through files and read into R environment
    x <- data.frame()
    for(i in seq_along(FCSFilePaths)){
        message(paste("Reading .FCS sample:",i))
        
        FCSfile <- flowCore::read.FCS(FCSFilePaths[i],
                                      transformation=FALSE,
                                      truncate_max_range=FALSE,
                                      min.limit = NULL)
        
        # Only keep expression
        exprs <- FCSfile@exprs
        
        # Change column names to description
        if(colNames == "description"){
            colnames(exprs) <- FCSfile@parameters@data$desc
            is_na <- is.na(colnames(exprs))
            is_na <- is_na | colnames(exprs) == "<NA>"
            colnames(exprs)[is_na] <- FCSfile@parameters@data$name[is_na]
        }
        
        # Simplify sampleID
        sampleID <- gsub("^cytofkit_", "", FCSFileNames[i], ignore.case = TRUE)
        sampleID <- gsub("\\.fcs", "", sampleID, ignore.case = TRUE)
        x <- rbind(x, data.frame(sampleID, exprs, check.names = FALSE))
    }
    
    # Make sampleID a factor
    x$sampleID <- as.factor(x$sampleID)
    # Copy clusterID
    x$clusterID <- as.factor(x[,clusterID])
    # Change order and return cfList
    x <- x[,c("clusterID", "sampleID",
              setdiff(colnames(x), c("clusterID", "sampleID")))]
    # Clean up
    if (cleanUp) {
        keep <- colnames(x)  # all
        # keep <- keep[!grepl("^(pca|tsne|umap)", keep)]  # no reduced dimensions
        # keep <- keep[!grepl("^(Rphenograph|ClusterX|FlowSOM)", keep)]  # no clustering
        keep <- keep[!grepl("_cor_(1|2)$", keep)]  # no cor dimensions
        keep <- keep[!grepl("_(1|2)_linear$", keep)]  # no linear reduced dimensions
        keep <- keep[!grepl("_(1|2)$", keep)]  # no reduced dimensions
        keep <- keep[!grepl("_clusterIDs$", keep)]  # no clusterings
        x <- x[, keep]
    }
    # Return
    methods::new("cfList", expr = x,
        samples = data.frame(sampleID = levels(x$sampleID),
                             row.names = levels(x$sampleID),
                             FCSfilename = FCSFileNames))
}
