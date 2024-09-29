#' Merge the expression matrix from multiple FCS files with preprocessing
#' 
#' Apply preprocessing on each FCS file including compensation (for FCM data only) and transformation
#' with selected markers, then expression matrix are extracted and merged using one of the methods, 
#' \code{all}, \code{min}, \code{fixed} or \code{ceil}
#' 
#' @param fcsFiles A vector of FCS file names.
#' @param comp If \verb{TRUE}, does compensation  by compensation matrix contained in FCS. Agrument also accepts a compensation matrix to be applied. Otherwise \verb{FALSE}.
#' @param transformMethod Data Transformation method, including \code{autoLgcl}, \code{cytofAsinh}, \code{logicle} and \code{arcsinh}, or \code{none} to avoid transformation.
#' @param scaleTo Scale the expression to a specified range c(a, b), default is NULL.
#' @param mergeMethod Merge method for mutiple FCS expression data. cells can be combined using one of the four different methods including \code{ceil}, \code{all}, \code{min}, \code{fixed}. The default option is 
#' \code{ceil}, up to a fixed number (specified by \code{fixedNum}) of cells are sampled without replacement from each fcs file and combined for analysis.
#' \code{all}: all cells from each fcs file are combined for analysis. 
#' \code{min}: The minimum number of cells among all the selected fcs files are sampled from each fcs file and combined for analysis. 
#' \code{fixed}: a fixed num (specified by fixedNum) of cells are sampled (with replacement when the total number of cell is less than fixedNum) from each fcs file and combined for analysis.
#' @param fixedNum The fixed number of cells to be extracted from each FCS file.
#' @param sampleSeed A sampling seed for reproducible expression matrix merging.
#' @param ... Other arguments passed to \code{cytof_exprsExtract}
#' 
#' @return A matrix containing the merged expression data, with selected markers, row names added as \code{filename_cellID}, column names added as \code{name<desc>}.
#' @seealso \code{\link{cytof_exprsExtract}}
#' @export
#' @examples
#' d<-system.file('extdata',package='cytofkitlab')
#' fcsFiles <- list.files(d,pattern='.fcs$',full=TRUE)
#' merged <- cytof_exprsMerge(fcsFiles)
cytof_exprsMerge <- function(fcsFiles, 
                             comp = FALSE, 
                             transformMethod = c("autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"), 
                             scaleTo = NULL, 
                             mergeMethod = c("ceil", "all", "fixed", "min"), 
                             fixedNum = 10000, 
                             sampleSeed = 123, ...) {
    
    transformMethod <- match.arg(transformMethod)
    mergeMethod <- match.arg(mergeMethod)
    
    # Quick and dummy check that all FCS have matched/aligned channels
    fs <- tryCatch(flowCore::read.flowSet(fcsFiles, which.lines = 1:9),
                   error = function(err) NULL )
    if (is.null(fs)) stop("Check that all FCS have exactly the channels.")
    
    dots = list(...)
    if (cytof_verbose()) message(
        "Compensation = ", comp, " Transformation: ", transformMethod, "\n",
        "scaleTo = ", scaleTo, " mergeMethod = ", mergeMethod, "\n",
        "fixedNum = ", fixedNum, " sampleSeed = ", sampleSeed, "\n",
        "extra args: ", paste(names(dots), dots, sep = "=", collapse = ", ")
    )
    
    # TODO: sample cells file by file to lower memory consumption
    # Read all data
    exprsL <- mapply(cytof_exprsExtract, fcsFiles, 
                     MoreArgs = c(list(comp = comp, 
                                     transformMethod = transformMethod, 
                                     scaleTo = scaleTo), dots),
                     SIMPLIFY = FALSE)

    if(is.numeric(sampleSeed))
        set.seed(sampleSeed)
    ## test if number of events in any fcs less than fixedNum
    if(mergeMethod == "fixed"){
      if(!is.null(fixedNum)){
        eventCountTest <- suppressWarnings(any(lapply(exprsL, function(x) if (nrow(x) < fixedNum) {1} else {0})))
        if(eventCountTest == TRUE){
          warning("One or more FCS files have less events than specified fixedNum")
          warning("using replacement and uniform randomization")
          # warning("using lowest as fixedNum")
          # fixedNum <- min(rapply(exprsL, nrow))
        }
      }
    }
    # TODO: order sample id to keep time ordering
    switch(mergeMethod,
           ceil = {
               mergeFunc <- function(x) {
                   if (nrow(x) < fixedNum) {
                       x
                   } else {
                       x[sample(nrow(x), size = fixedNum, replace = FALSE), , drop = FALSE]
                   }
               }
               merged <- do.call(rbind, lapply(exprsL, mergeFunc))
           },
           all = {
               merged <- do.call(rbind, exprsL)
           },
           fixed = {
               mergeFunc <- function(x) {
                   replace <- nrow(x) <= fixedNum
                   if (replace) {
                       rbind(x, x[sample(nrow(x), size = fixedNum - nrow(x), replace = TRUE), , drop=FALSE] + runif(fixedNum - nrow(x), -0.1, 0.1))
                   } else {
                       x[sample(nrow(x), size = fixedNum, replace = FALSE), , drop = FALSE]
                   }
                   # x[sample(nrow(x), size = fixedNum, replace = nrow(x) < fixedNum), , drop=FALSE]
               }
               merged <- do.call(rbind, lapply(exprsL, mergeFunc))
           },
           min = {
               minSize <- min(sapply(exprsL, nrow))
               mergeFunc <- function(x) {
                   x[sample(nrow(x), size = minSize, replace = FALSE), , drop=FALSE]
               }
               merged <- do.call(rbind, lapply(exprsL, mergeFunc))
           })
    
    return(merged)
}


#' Extract the expression data from a FCS file with preprocessing
#' 
#' Extract the FCS expresssion data with preprocessing of compensation (for FCM data only)
#' and transformation. Transformtion methods includes \code{autoLgcl}, \code{cytofAsinh}, 
#' \code{logicle} (customizable) and \code{arcsinh} (customizable).
#' 
#' @param fcsFile The name of the FCS file.
#' @param verbose If \verb{TRUE}, print the message details of FCS loading.
#' @param comp If \verb{TRUE}, does compensation  by compensation matrix contained in FCS. Agrument also accepts a compensation matrix to be applied. Otherwise \verb{FALSE}.
#' @param transformMethod Data Transformation method, including \code{autoLgcl}, \code{cytofAsinh}, \code{logicle} and \code{arcsinh}, or \code{none} to avoid transformation.
#' @param scaleTo Scale the expression to a specified range c(a, b), default is NULL.
#' @param ... parameters passed to transformation.
#' @param rename_rc boolean to rename or not rows and columns of the expresion matrix.
#' 
#' @return A transformed expression data matrix, row names added as \code{filename_cellID}, column names added as \code{name<desc>}.
#' @importClassesFrom flowCore transformList
#' @export
#' @examples
#' d <- system.file('extdata',package='cytofkitlab')
#' fcsFile <- list.files(d,pattern='.fcs$',full=TRUE)
#' transformed <- cytof_exprsExtract(fcsFile)
cytof_exprsExtract <- function(
        fcsFile, 
        verbose = FALSE, 
        comp = FALSE, 
        transformMethod = c("autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"), 
        scaleTo = NULL, 
        ...,
        rename_rc = TRUE
) {
    
    ## load FCS file
    fcs <- cytof_readFCS(
        fcsFile, 
        comp = comp, 
        verbose = verbose
    )
    ## transform exprs
    fcs <- cytof_transfFCS(
        fcs, 
        transformMethod = transformMethod, 
        scaleTo = scaleTo, 
        ...,
        rename_rc = rename_rc
    )

    ## done
    return(exprs(fcs))
}


#' Read a FCS file with compensation and return a flowFrame
#'
#' Read a FCS file with compensation and no transformation.
#'
#' @param fcsFile The name of the FCS file.
#' @param comp If \verb{TRUE}, does compensation  by compensation matrix
#'   contained in FCS. Argument also accepts a compensation matrix to be
#'   applied. Otherwise \verb{FALSE}.
#' @param verbose If \verb{TRUE}, print the message details of FCS loading.
#'
#' @return A flowFrame, read with no transformation at all, except compensation
#'   if asked. See \code{cytof_transfFCS}, to transform expression.
#' @importFrom flowCore read.FCS compensate
#' @export
#' @examples
#' d <- system.file('extdata', package = 'cytofkitlab')
#' fcsFiles <- list.files(d, pattern = '\\.fcs$', full=TRUE)
#' fcs_flowFrame <- cytof_readFCS(fcsFiles[1])
cytof_readFCS <- function(
        fcsFile, 
        comp = FALSE, 
        verbose = FALSE
) {
    
    if (cytof_verbose()) message(
        "Extract from file: ", fcsFile, "\n",
        "Compensation = ", comp
    )
    ## load FCS file
    if (verbose) {
        fcs <- read.FCS(
            fcsFile, transformation = FALSE, 
            truncate_max_range = FALSE, min.limit = NULL)
    } else {
        fcs <- suppressWarnings(read.FCS(
            fcsFile, transformation = FALSE, 
            truncate_max_range = FALSE, min.limit = NULL))
    }
    
    ## compensation
    comp_matrix <- NULL
    if (is.matrix(comp) || is.data.frame(comp)) {
        comp_matrix <- comp
    } else if (isTRUE(comp)) {
        # See discussion keywords at
        # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2892967/#S2title
        comp_matrix <- fcs@description[["$SPILLOVER"]]  # FCS 3.1
        if (is.null(comp_matrix)) {
            comp_matrix <- fcs@description[["SPILL"]]
            if (is.null(comp_matrix)) {
                comp_matrix <- fcs@description[["$COMP"]]  # FCS 3.0
            }
        }
        if (is.null(comp_matrix)) {
            warning(
                "Cannot find compensation matrix in FCS file '", basename(fcsFile),
                "'. Please CHECK the keyword of 'SPILL', '$SPILLOVER', or '$COMP' ",
                "in the FCS file and make sure it stores the compensation matrix.")
        }
    }
    if (!is.null(comp_matrix)) {
        fcs <- applyComp(fcs, comp_matrix)
        if (verbose)
            message("Compensation is applied on ", basename(fcsFile))
    }
        
    ## done
    return(fcs)
}

#' Transform the expression/intensity values of a FCS file (flowFrame)
#'
#' Transform the FCS expression data with transformation. Transformation methods
#' include \code{autoLgcl}, \code{cytofAsinh}, \code{logicle} (customizable) and
#' \code{arcsinh} (customizable).
#'
#' @param fcs The flowFrame object after reading a FCS file.
#' @param transformMethod Data Transformation method, including \code{autoLgcl},
#'   \code{cytofAsinh}, \code{logicle} and \code{arcsinh}, or \code{none} to
#'   avoid transformation.
#' @param scaleTo Scale the expression to a specified range c(a, b), default is
#'   NULL.
#' @param q Quantile of negative values removed for auto w estimation, default
#'   is 0.05, parameter for autoLgcl transformation.
#' @param l_w Linearization width in asymptotic decades, parameter for logicle
#'   transformation.
#' @param l_t Top of the scale data value, parameter for logicle transformation.
#' @param l_m Full width of the transformed display in asymptotic decades,
#'   parameter for logicle transformation.
#' @param l_a Additional negative range to be included in the display in
#'   asymptotic decades, parameter for logicle transformation.
#' @param a_a Positive double that corresponds to the base of the arcsinh
#'   transformation, \code{arcsinh} = asinh(a + b * x) + c.
#' @param a_b Positive double that corresponds to a scale factor of the arcsinh
#'   transformation, \code{arcsinh} = asinh(a + b * x) + c.
#' @param a_c Positive double that corresponds to another scale factor of the
#'   arcsinh transformation, \code{arcsinh} = asinh(a + b * x) + c.
#' @param ... currently unused as all used parameters are listed above.
#' @param rename_rc boolean to rename or not rows and columns of the expresion matrix.
#'
#' @return A transformed flowFrame, row names added as \code{filename_cellID},
#'   column names added as \code{name<desc>}.
#' @importFrom flowCore estimateLogicle logicleTransform parameters
#'   transformList arcsinhTransform biexponentialTransform
#' @importMethodsFrom flowCore transform
#' @importClassesFrom flowCore flowFrame transformList
#' @export
#' @examples
#' d <- system.file('extdata', package = 'cytofkitlab')
#' fcsFiles <- list.files(d, pattern = '\\.fcs$', full=TRUE)
#' ff_raw <- cytof_readFCS(fcsFiles[1])
#' ff_transformed <- cytof_transfFCS(
#'     ff_raw, transformMethod = "arcsinh", a_a = 0, a_b = 1/5, a_c =0, rename_rc = FALSE)
#' t(rbind(range(ff_raw, "data"), range(ff_transformed), range(ff_transformed, "data")))
cytof_transfFCS <- function(
        fcs, 
        transformMethod = c("autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"), 
        scaleTo = NULL, 
        q = 0.05,
        l_w = 0.1, l_t = 4000, l_m = 4.5, l_a = 0,
        a_a = 1, a_b = 1, a_c =0,
        ...,
        rename_rc = TRUE
) {
    
    dots <- list(...)
    pars <- as.list(match.call(expand.dots = FALSE))
    if (cytof_verbose()) message(
        paste(names(pars), pars, sep = "=", collapse = ", "), "\n",
        paste(names(dots), dots, sep = "=", collapse = ", ")
    )
    transformMethod <- match.arg(transformMethod)
    
    ## Exclude "Time", "Event" channel
    # TODO: there are more channels to ignore
    exclude_channels <- grep("Time|Event", colnames(fcs@exprs), ignore.case = TRUE)
    marker_id <- setdiff(seq_along(colnames(fcs@exprs)), exclude_channels)
    # size parameters
    size_channels <- grep("FSC|SSC", colnames(fcs@exprs), ignore.case = TRUE)
    transMarker_id <- setdiff(marker_id, size_channels)
    
    ## exprs transformation
    exprs <- exprs(fcs)
    switch(
        transformMethod,
        cytofAsinh = {
            exprs[ ,transMarker_id] <- apply(exprs[ ,transMarker_id, drop=FALSE], 2, cytofAsinh)
        },
        autoLgcl = {
            trans <- autoLgcl(fcs, channels = colnames(fcs@exprs)[transMarker_id], q = q)
            transformed <- flowCore::transform(fcs, trans)
            exprs[ ,transMarker_id] <- transformed@exprs[, transMarker_id, drop=FALSE]
        },
        logicle = {
            trans <- flowCore::logicleTransform(w = l_w, t = l_t, m = l_m, a = l_a)
            exprs[ ,transMarker_id] <- apply(exprs[ ,transMarker_id, drop=FALSE], 2, trans)
        },
        arcsinh = {
            trans <- flowCore::arcsinhTransform(a = a_a, b = a_b, c = a_c)
            exprs[ ,transMarker_id] <- apply(exprs[ ,transMarker_id, drop=FALSE], 2, trans)
        },
        none = {
        })
    
    ## apply linear transformation for the "FSC-x", "SSC-x" channel if exists
    if(length(size_channels) > 0){
        if(any(size_channels %in% marker_id)){
            used_size_channel <- size_channels[size_channels %in% marker_id]
            used_size_channel_id <- match(used_size_channel, marker_id)
            exprs[ ,used_size_channel_id] <- apply(exprs[ , used_size_channel_id, drop=FALSE], 2, 
                                                   function(x) scaleData(x, range=c(0, 4.5)))
        }
    }
    
    ## scale all data to same range
    if (!is.null(scaleTo)) {
        exprs <- apply(exprs, 2, function(x) scaleData(x, scaleTo))
    }

    ## transformations done
    if (!rename_rc) {
        exprs(fcs) <- exprs
    } else {
        ## add rownames
        fcsFile <- keyword(fcs, "FILENAME")[[1]]
        name <- sub("\\.fcs$", "", basename(fcsFile), ignore.case = TRUE)
        rownames(exprs) <- paste(name, 1:nrow(exprs), sep = "_")
        exprs(fcs) <- exprs
        ## rename channel names
        pd <- fcs@parameters@data
        col_names <- paste0(pd$name, "<", pd$desc,">")
        flowCore::colnames(fcs)[marker_id] <- col_names[marker_id]
    }
    
    ## done
    return(fcs)
}


#' Get the parameters aka channel names formatted for cytofkit, i.e. name and
#' description
#'
#' @param fcsFile FCS file name to read.
#' @param cat boolean to cat the result on the R console.
#'
#' @return a vector of parameters for cytofkit analysis
#' @export
#'
#' @examples
#' # None yet
cytof_getParamFCS <- function(
        fcsFile,
        cat = FALSE
) {
    
    ff = cytof_readFCS(fcsFile)
    pd = ff@parameters@data
    res = paste0(pd$name, "<", pd$desc,">")
    if (cat) {
        cat('"', paste(res, collapse = '",\n"'),'"\n', sep = "")
    } else
        return(res)
}


#' apply compensation on the FCS expression data
#' 
#' @param fcs FlowFrame.
#' @param compMatrix Compensation matrix.
#' @noRd
#' @return Compensated expression value
applyComp <- function(fcs, compMatrix) {
    comp_fcs <- compensate(fcs, compMatrix)
}

#' rescale the data
#' 
#' @param x data.
#' @param range The range of the data.
#' @noRd
#' @return scaled data
scaleData <- function(x, range = c(0, 4.5)) {
    (x - min(x))/(max(x) - min(x)) * (range[2] - range[1]) + range[1]
}


#' Noise reduced asinh transformation
#'
#' Inverse hyperbolic sine transformation (asinh) with a cofactor of 5, reduce
#' noise from negative values Adopted from Plos Comp reviewer
#'
#' @param value A vector of numeric values.
#' @param cofactor Cofactor for asinh transformation, default 5 for mass
#'   cytometry data.
#' @noRd
#' @return transformed value
cytofAsinh <- function(value, cofactor = 5) {
    value <- value-1
    loID <- which(value < 0)
    if(length(loID) > 0)
        value[loID] <- rnorm(length(loID), mean = 0, sd = 0.01)
    value <- value / cofactor
    value <- asinh(value) # value <- log(value + sqrt(value^2 + 1))
    return(value)
}


#' a modified version of "estimateLogicle" from flowCore
#' 
#' Used boxplot outlier detection to filter outliers in negative values 
#' before calculating the r using the fifth percentile of the negative values.
#' 
#' @param x A flowFrame object.
#' @param channels Channel names to be transformed.
#' @param m The full width of the transformed display in asymptotic decades. \code{m} should be greater than zero.
#' @param q The percentile of negative values used as reference poiont of negative range.
#' @importFrom methods is
#' @importFrom flowCore logicleTransform
#' @noRd
#' @return a list of autoLgcl transformations
autoLgcl <- function(x, channels, m = 4.5, q = 0.05) {
    if (!is(x, "flowFrame")) 
        stop("x has to be an object of class \"flowFrame\"")
    if (missing(channels)) 
        stop("Please specify the channels to be logicle transformed")
    indx <- channels %in% colnames(x@exprs)
    if (!all(indx)) 
        stop(paste("Channels", channels[!indx], "were not found in the FCS file.\n ", 
            sep = " "))

    trans <- lapply(channels, function(p) {
        data <- x@exprs[, p]
        w <- 0
        t <- max(data)
        ndata <- data[data < 0]
        ## use 1.5 * IQR to filter outliers in negative values
        nThres <- quantile(ndata, 0.25) - 1.5 * IQR(ndata)
        ndata <- ndata[ndata >= nThres]
        transId <- paste(p, "autolgclTransform", sep = "_")
        
        if (length(ndata)) {
            r <- .Machine$double.eps + quantile(ndata, q)
            ## Check to avoid failure of negative w
            if (10^m * abs(r) <= t) {
                w <- 0  
            } else {
                w <- (m - log10(t/abs(r)))/2
                if(is.nan(w) || w>2) {
                    warning(paste0("autoLgcl failed for channel: ", p, "; using default logicle transformation!"))
                    w <- 0.1
                    t <- 4000 
                    m <- 4.5 
                }
            }
        }
        logicleTransform(transformationId = transId, 
                         w = w, t = t, m = m, a = 0)
    })
    transformList(channels, trans)
}
