#' Subset detection by clustering
#'
#' Apply clustering algorithms to detect cell subsets. \code{DensVM} clustering
#' is based on the transformed ydata and uses xdata to train the model.
#' \code{ClusterX} clustering works on the transformed ydata (data obtained from
#' a tSNE reduction). \code{Rphenograph} and \code{FlowSOM} directly work on high
#' dimensional xdata. \code{FlowSOM} is integrated from FlowSOM package
#' (https://bioconductor.org/packages/release/bioc/html/FlowSOM.html).
#'
#' @param ydata A matrix of the dimension reduced data.
#' @param xdata A matrix of the expression data.
#' @param method Cluster method including \code{DensVM}, \code{densityClustX},
#'   \code{Rphenograph} and \code{FlowSOM}.
#' @param Rphenograph_k Integer number of nearest neighbours to pass to
#'   Rphenograph.
#' @param FlowSOM_k Number of clusters for meta clustering in FlowSOM.
#' @param flowSeed Integer to set a seed for FlowSOM for reproducible results.
#' @param ... Parameters passed to clustering methods.
#'
#' @return a vector of the clusters assigned for each row of the ydata
#' @importFrom Rphenograph Rphenograph
#' @importFrom igraph membership
#' @export
#' @examples
#' d<-system.file('extdata',package='cytofkitlab')
#' fcsFile <- list.files(d, pattern='.fcs$', full=TRUE)
#' parameters <- list.files(d, pattern='.txt$', full=TRUE)
#' markers <- as.character(read.table(parameters, header = FALSE)[, 1])
#' xdata <- cytof_exprsMerge(fcsFile, mergeMethod = 'fixed', fixedNum = 100)
#' ydata <- cytof_dimReduction(xdata, markers = markers, method = "tsne")
#' clusters <- cytof_cluster(ydata, xdata, method = "ClusterX")
cytof_cluster <- function(ydata = NULL, 
                          xdata = NULL, 
                          method = c("Rphenograph", "ClusterX", "DensVM", "FlowSOM", "NULL", "FlowSOMDR"),
                          Rphenograph_k = 30,
                          FlowSOM_k = 40,
                          flowSeed = NULL,
                          ...){
    
    method = match.arg(method)
    if(method == "NULL"){
        return(NULL)
    }
    
    # Function to merge default options with dots arguments
    merge_options <- function(prefix, defaults, dots) {
        prefix.patt <- paste0("^", prefix, "\\.")
        prefix.dots <- grep(prefix.patt, names(dots))
        if (length(prefix.dots)) {
            # extract specific options and remove prefix
            new.names <- gsub(prefix.patt, "", names(dots[prefix.dots]))
            prefix.dots <- dots[prefix.dots]
            names(prefix.dots) <- new.names
            # merge arguments and defaults
            prefix.opts <- c(prefix.dots, defaults)
            prefix.opts <- prefix.opts[unique(names(prefix.opts))]
        } else
            prefix.opts <- defaults
        list(options = prefix.opts)
    }
    
    start_time <- Sys.time()
    if(is.numeric(flowSeed))
        set.seed(flowSeed) # Set a seed if you want reproducible results
    switch(method, 
           Rphenograph = {
               cat("  Running PhenoGraph...")
               RphenographOut <- Rphenograph(xdata, k=Rphenograph_k, verbose = TRUE)
               clusters <- as.numeric(membership(RphenographOut[[2]]))
           },
           ClusterX = {
               cat("  Running ClusterX...")
               clusters <- ClusterX(ydata, gaussian=TRUE, alpha = 0.001, detectHalos = FALSE)$cluster
           },
           DensVM = {
               cat("  Running DensVM...")
               clusters <- DensVM(ydata, xdata)$cluster$cluster
           },
           FlowSOM = {
               cat("  Running FlowSOM...")
               # set.seed(flowSeed)
               # default flowsom arguments
               flowsom.opts <- list(
                   xdata = xdata,
                   k = FlowSOM_k,
                   flowSeed = flowSeed
               )
               # merge options and execute
               flowsom.opts <- merge_options("flowsom", flowsom.opts, list(...))
               clusters <- do.call(FlowSOM_integrate2cytofkit, flowsom.opts[["options"]])
           },
           FlowSOMDR = {
               cat("  Running FlowSOM on DimRed...")
               # default flowsom arguments
               flowsomdr.opts <- list(
                   xdata = ydata,
                   k = FlowSOM_k,
                   flowSeed = flowSeed
               )
               # merge options and execute
               flowsomdr.opts <- merge_options("flowsomdr", flowsomdr.opts, list(...))
               clusters <- do.call(FlowSOM_integrate2cytofkit, flowsomdr.opts[["options"]])
           })
    cat("\n")
    
    if( length(clusters) != ifelse(is.null(ydata), nrow(xdata), nrow(ydata)) ){
        message("Cluster is not complete, cluster failed, try other cluster method(s)!")
        return(NULL)
    }else{
        if(!is.null(xdata) && !is.null(row.names(xdata))){
            names(clusters) <- row.names(xdata)
        }else if(!is.null(ydata) && !is.null(row.names(ydata))){
            names(clusters) <- row.names(ydata)
        }
        end_time <- Sys.time()
        cat("  DONE in", round(difftime(end_time, start_time, units = "mins"), 2), "mins\n")
        return(clusters)
    }
}


#' FlowSOM algorithm
#' 
#' @param xdata Input data matrix.
#' @param k Number of clusters.
#' @param flowSeed Seed for reproducibility to pass to metaClustering_consensus.
#' @param ... Other parameters passed to SOM.
#' 
#' @noRd
#' @importFrom FlowSOM SOM metaClustering_consensus
FlowSOM_integrate2cytofkit <- function(xdata, k, flowSeed = NULL, ...){
    cat("\n    Building SOM...")
    xdata <- as.matrix(xdata)
    
    ord <- tryCatch({
        map <- SOM(xdata, silent = TRUE, ...)
        cat("\n    Meta clustering to", k, "clusters...")
        metaClusters <- suppressMessages(metaClustering_consensus(map$codes, k = k, seed = flowSeed))
        cluster <- metaClusters[map$mapping[,1]]
    }, error=function(cond) {
        message("Run FlowSOM failed")
        message("Here's the error message:")
        message(cond)
        return(NULL)
    }) 
    
    if(is.null(ord)){
        cluster <- NULL
    }else{
        if(length(ord) != nrow(xdata)){
            message("Run FlowSOM failed!")
            return(NULL)
        }
        cluster <- ord
    }
    
    return(cluster)
}
