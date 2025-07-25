#' cytofkit: an integrated mass cytometry data analysis pipeline
#' 
#' The main function to drive the cytofkit workflow.
#' 
#' \code{cytofkit} works as the main funciton to perform the analysis of one or multiple FCS files. 
#' The workflow contains data merging from multiple FCS file, expression data transformation, 
#' dimensionality reduction with \code{PCA}, \code{isomap} or \code{tsne} (default), clustering 
#' analysis with methods includes \code{DensVM}, \code{ClusterX}, \code{Rphenograph)} and \code{FlowSOM} for 
#' subpopulation detection, and estimation of cellular progression using \code{isomap}. The analysis 
#' results can be visualized using scatter plot, heatmap plot or progression plot. Dimension reduced 
#' data and cluster labels will be saved back to new copies of FCS files. By default the analysis 
#' results will be automatically saved under \code{resultDir} for further annotation. Moreover An 
#' interactive web application is provided for interactive exploration of the analysis results, 
#' see \code{cytofkitShinyAPP}.
#' 
#' 
#' @param fcsFiles It can be either the path where your FCS files are stored or a vector of FCS file names. 
#' @param markers It can be either a text file that containing markers to be used for analysis or a vector of the marker names.
#' @param projectName A prefix that will be added to the names of all result files.
#' @param ifCompensation Boolean value, to apply compensation contained in FCS, or a compensation matrix.
#' @param transformMethod Data Transformation method, including \code{autoLgcl}, \code{cytofAsinh}, \code{logicle} and \code{arcsinh}, or \code{none} to avoid transformation.
#' @param mergeMethod When multiple fcs files are selected, cells can be combined using 
#' one of the four different methods including \code{ceil}, \code{all}, \code{min}, \code{fixed}. 
#' The default option is \code{ceil}, up to a fixed number (specified by \code{fixedNum}) of cells are sampled 
#' without replacement from each fcs file and combined for analysis.
#' \code{all}: all cells from each fcs file are combined for analysis. 
#' \code{min}: The minimum number of cells among all the selected fcs files are sampled from each fcs file and combined for analysis. 
#' \code{fixed}: a fixed num (specified by fixedNum) of cells are sampled (with replacement when the total number of cell is less than 
#' fixedNum) from each fcs file and combined for analysis.
#' @param fixedNum The fixed number of cells to be extracted from each FCS file.
#' @param normalizeMethod The normalization nmethod prior to clustering and dimension reduction. Any choice "default", "tsne", "umap", "quantile_100_evts". Default = as defined in original cytofkit package.
#' @param dimReductionMethod The method used for dimensionality reduction, including \code{tsne}, \code{pca} and \code{isomap}.
#' @param clusterMethods The clustering method(s) used for subpopulation detection, including \code{DensVM}, \code{ClusterX}, \code{Rphenograph} and \code{FlowSOM}. Multiple selections are accepted.
#' @param visualizationMethods The method(s) used for visualize the cluster data, including \code{tsne}, \code{pca} and \code{isomap}. Multiple selections are accepted.
#' @param progressionMethod Use the first ordination score of \code{isomap} to estimated the progression order of cells, choose \code{NULL} to ignore.
#' @param Rphenograph_k Integer number of nearest neighbours to pass to Rphenograph.
#' @param FlowSOM_k Number of clusters for meta clustering in FlowSOM.
#' @param seed Integer to set a seed for reproducible results.
#' @param clusterSampleSize The uniform size of each cluster.
#' @param resultDir The directory where result files will be generated.
#' @param saveResults Save the results, and the post-processing results including scatter plot, heatmap, and statistical results.
#' @param saveObject Save the results into RData objects for loading back to R for further analysis
#' @param openShinyAPP Opens the shinyAPP automatically when the analysis was done, default FALSE.
#' @param ... Other arguments passed to \code{cytof_exprsExtract}
#' 
#' @return a list containing \code{expressionData}, \code{dimReductionMethod}, \code{visualizationMethods}, \code{dimReducedRes}, \code{clusterRes}, \code{progressionRes}, \code{projectName}, \code{rawFCSdir} and \code{resultDir}. If choose 'saveResults = TRUE', results will be saved into files under \code{resultDir}.
#' @author Hao Chen, Jinmiao Chen
#' @references \url{https://github.com/JinmiaoChenLab/cytofkit}
#' @seealso \code{\link{cytofkit}}, \code{\link{cytofkit_GUI}}, \code{\link{cytofkitShinyAPP}}
#' @export
#' @examples
#' dir <- system.file('extdata',package='cytofkitlab')
#' file <- list.files(dir, pattern='.fcs$', full=TRUE)
#' parameters <- list.files(dir, pattern='.txt$', full=TRUE)
#' ## remove the hash symbol to run the following command
#' #cytofkit(fcsFile = file, markers = parameters) 
cytofkit <- function(fcsFiles = getwd(), 
                     markers = "parameter.txt", 
                     projectName = "cytofkit", 
                     ifCompensation = FALSE, 
                     transformMethod = c("autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"), 
                     mergeMethod = c("ceil", "all", "min", "fixed"), 
                     fixedNum = 10000, 
                     normalizeMethod = c("default", "tsne", "umap", "quantile_100_evts"),
                     dimReductionMethod = c("tsne", "pca", "isomap", "umap"), 
                     clusterMethods = c("Rphenograph", "ClusterX", "DensVM", "FlowSOM", "NULL", "FlowSOMDR"), 
                     visualizationMethods = c("tsne", "pca", "isomap", "umap", "NULL"), 
                     progressionMethod = c("NULL", "diffusionmap", "isomap"),
                     Rphenograph_k = 30,
                     FlowSOM_k = 40,
                     seed = NULL,
                     clusterSampleSize = 500,
                     resultDir = getwd(), 
                     saveResults = TRUE, 
                     saveObject = TRUE, 
                     openShinyAPP = FALSE, ...) {
    
    ## arguments checking
    if (is.null(fcsFiles) || any(is.na(fcsFiles)) || any(is.nan(fcsFiles))){
        stop("Wrong input fcsFiles!")
    }else if (length(fcsFiles) == 1 && file.info(fcsFiles)$isdir) {
        rawFCSdir <- fcsFiles  # argument is a directory in fact
        fcsFiles <- list.files(path = fcsFiles, pattern = ".fcs$", 
            full.names = TRUE)
        # rawFCSdir <- dirname(fcsFiles)
    }else{
        if(dirname(fcsFiles[1]) == "."){
            rawFCSdir <- getwd()
        }else{
            rawFCSdir <- dirname(fcsFiles[1])  
        }
    }
    setwd(rawFCSdir)
    if(length(fcsFiles) < 1)
        stop("No FCS file found, please select your fcsFiles!")
    if(!all(file.exists(fcsFiles)))
        stop("Can not find file(s):", fcsFiles[which(!file.exists(fcsFiles))])
    
    if (length(markers) == 1 && file.exists(markers)) {
        markers <- as.character(read.table(markers, sep = "\t", 
                                           header = TRUE)[, 1])
    }
    if (is.null(markers) || length(markers) < 1) 
        stop("no marker selected!")
  
    mergeMethod <- match.arg(mergeMethod)
    
    if (!is.null(fixedNum) && !(is.numeric(fixedNum))) 
        stop("fixedNum must be a numeric number!")
    
    transformMethod <- match.arg(transformMethod)

    normalizeMethod <- match.arg(normalizeMethod)

    dimReductionMethod <- match.arg(dimReductionMethod) 
    
    if(missing(clusterMethods)){
        clusterMethods <- "Rphenograph"
    }else{
        clusterMethods <- match.arg(clusterMethods, several.ok = TRUE)
    }
    
    if(missing(visualizationMethods)){
        visualizationMethods <- "tsne"
    }else{
        visualizationMethods <- match.arg(visualizationMethods, several.ok = TRUE)
    }
    
    progressionMethod <- match.arg(progressionMethod)
    
    if (!(is.numeric(clusterSampleSize))) 
        stop("clusterSampleSize must be a numeric number!")
    
    
    ## print arguments for user info
    message("Input arguments:")
    cat("* Project Name: ")
    cat(projectName, "\n")
    cat(sprintf("* Input FCS files for analysis (%d):\n", length(fcsFiles)))
    cat(paste0("    ", basename(fcsFiles), "\n", collapse = ""))
    cat(sprintf("* Markers (%d):\n", length(markers)))
    cat(paste0("    ", markers, "\n", collapse = ""))
    cat("* Data merging method: ")
    cat(mergeMethod, "\n")
    cat("* Data transformation method: ")
    cat(transformMethod, "\n")
    cat("* Data normalization method: ")
    cat(normalizeMethod, "\n")
    cat("* Dimensionality reduction method: ")
    cat(dimReductionMethod, "\n")
    cat("* Data clustering method(s): ")
    cat(clusterMethods, "\n")
    cat("* Data visualization method(s): ")
    cat(visualizationMethods, "\n")
    cat("* Subset progression analysis method: ")
    cat(progressionMethod, "\n\n")

    ## split ... arguments
    dots <- list(...)
    if (cytof_verbose())
        message("Given arguments: ",
                paste(names(dots), dots, sep = "=", collapse = ", "))
    # dimension reduction args
    dots.dr.idx <- sapply(
        c("tsne", "pca", "isomap", "umap"),
        function(x) {
            grep(paste0("^", x, "\\."), names(dots))
        })
    dots.dimReduction <- dots[unlist(dots.dr.idx)]
    # clustering args
    dots.cl.idx <- sapply(
        c("rphenograph", "clusterx", "densvm", "flowsom", "flowsomdr"),
        function(x) {
            grep(paste0("^", x, "\\."), names(dots))
        })
    dots.clustering <- dots[unlist(dots.cl.idx)]
    # remaining args for standard processing
    dots.remove.idx <- c(unlist(dots.dr.idx), unlist(dots.cl.idx))
    dots <- dots[setdiff(seq_along(dots), dots.remove.idx)]
    if (cytof_verbose())
        message("Remaining arguments: ",
                paste(names(dots), dots, sep = "=", collapse = ", "))
    
    set.seed(seed)
    ## get transformed, combined exprs data
    message("Extract expression data...")
    exprs_data <- do.call(
        cytof_exprsMerge,
        c(list(fcsFiles, comp = ifCompensation, verbose = FALSE, 
               transformMethod = transformMethod, 
               mergeMethod = mergeMethod, fixedNum = fixedNum), 
          dots))
    cat("  ", nrow(exprs_data), " x ", ncol(exprs_data), " data was extracted!\n")
    
    #browser()
    options.dimReduction <- list()
    if(normalizeMethod == "default") {
        # each function does its own
        # this is the original cytofkit setup
    } else if (normalizeMethod == "tsne") {
        # default for Rtsne
        # mean center each column
        # scale the whole matrix so the absolute maximum is 1
        # https://github.com/cran/Rtsne/blob/21f96fe7633c8b775496f3c9a85fd4aba8a6f4ea/src/normalize_input.cpp#L1
        exprs_data <- Rtsne::normalize_input(exprs_data)
        # same as UMAP "maxabs" scaling
    } else if (normalizeMethod == "umap") {
        # default for published UMAP, ie "colrange"
        # set column min to zero, column max to 1
        mmin <- apply(exprs_data, 2, min, na.rm = TRUE)
        exprs_data <- sweep(exprs_data, 2, mmin, "-")
        mmax <- apply(exprs_data, 2, max, na.rm = TRUE)
        mmax <- pmax(mmax, 1)  # avoid zero and small values
        exprs_data <- sweep(exprs_data, 2, mmax, "/")
        # exprs_data <- exprs_data - 0.5
        # reset scaling options
        options.dimReduction <- list(umap.scale = FALSE,
                                     tsne.pca = FALSE,
                                     tsne.normalize = FALSE)
    } else if (normalizeMethod == "quantile_100_evts") {
        mmin <- apply(exprs_data, 2, quantile, 
                      probs = 100/nrow(exprs_data), na.rm = TRUE)
        exprs_data <- sweep(exprs_data, 2, mmin, "-")
        mmax <- apply(exprs_data, 2, quantile, 
                      probs = 1-100/nrow(exprs_data), na.rm = TRUE)
        mmax <- pmax(mmax, 0.1)
        exprs_data <- sweep(exprs_data, 2, mmax, "/")
        # exprs_data <- exprs_data - 0.5
        # reset scaling options
        options.dimReduction <- list(umap.scale = FALSE,
                                     tsne.pca = FALSE,
                                     tsne.normalize = FALSE)
    }
    # Update dots/options
    for (k in names(options.dimReduction))
        dots.dimReduction[[k]] <- options.dimReduction[[k]]
    
    ## dimension reduced data, a list
    message("Dimension reduction...")
    alldimReductionMethods <- unique(c(visualizationMethods, dimReductionMethod))
    allDimReducedList <- lapply(
        alldimReductionMethods, function(method)
        do.call(
            cytof_dimReduction, 
            c(list(data = exprs_data, markers = markers, method = method, tsneSeed = seed), 
              dots.dimReduction)))
    names(allDimReducedList) <- alldimReductionMethods
    
    
    ## cluster results, a list
    message("Run clustering...")
    set.seed(seed)
    cluster_res <- lapply(
        clusterMethods, function(method)
        do.call(
            cytof_cluster, 
            c(list(ydata = allDimReducedList[[dimReductionMethod]], 
                   xdata = exprs_data[, markers],
                   method = method,
                   Rphenograph_k = Rphenograph_k,
                   FlowSOM_k = FlowSOM_k,
                   flowSeed = seed),
              dots.clustering)))
    names(cluster_res) <- clusterMethods
    
    
    ## progression analysis results, a list  
    ## NOTE, currently only the first cluster method results 
    ## are used for progression visualization(by default: cluster_res[[1]])
    message("Progression analysis...")   
    progression_res <- cytof_progression(data = exprs_data, 
                                         cluster = cluster_res[[1]], 
                                         method = progressionMethod,
                                         out_dim = 4,
                                         clusterSampleSize = clusterSampleSize)
    
    ## overall list of markers
    ## using cytof_exprsMerge to generate list instead of
    ## read.FCS, for consistent marker naming
    message("Listing markers used for dimension reduction...")
    markerlist <- markers
    
    ## original fcs sample names
    message("Stashing sample names...")
    names <- sub("^.+/", "", unique(sub(".fcs$", "", fcsFiles)))
    samples <- as.list(NULL)
    for(i in seq_along(names)){
      samples[[i]] <- names[i]
    }
    
    ## wrap the results
    message("Wrapping results...")
    analysis_results <- list(expressionData = exprs_data,
                             dimReductionMethod = dimReductionMethod,
                             visualizationMethods = alldimReductionMethods, #visualizationMethods,
                             dimReducedRes = allDimReducedList,
                             clusterRes = cluster_res, 
                             progressionRes = progression_res,
                             projectName = projectName,
                             rawFCSdir = rawFCSdir,
                             resultDir = resultDir,
                             dimRedMarkers = markers,
                             sampleNames = samples)
     
    
    ## save the results
    message("Analysis DONE, saving the results...") 
    cytof_writeResults(analysis_results = analysis_results,
                       saveToRData = saveObject,
                       saveToFCS = saveResults,
                       saveToFiles = saveResults)
    
    if(openShinyAPP){
        cytofkitShinyAPP(RData = analysis_results)
    }
    
    invisible(analysis_results)
}


#' check the package update news
#' 
#' @return Opens .Rd file of package update news
#' 
#' @export
cytofkitNews <- function() 
{
    newsfile <- file.path(system.file(package = "cytofkitlab"),
                          "NEWS.Rd")
    file.show(newsfile)
}


