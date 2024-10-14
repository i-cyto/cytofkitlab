#' Log information to the cytofkit analysis results
#'
#' Append messages to the log of the  analysis results from cytofkit (RData
#' object). Print the log information if message is missing.
#'
#' @param messages A vector of strings.
#' @param time_prefix If TURE (default), the first strings is prefixed with
#'   Sys.time() function.
#' @param env The environment where to look for the analysis results objects
#'   (analysis_results). If missing the current environment is used.
#' @return None.
#' @export
#' @seealso \code{\link{cytof_writeResults}}
#' @examples
#'\dontrun{
#' cytof_logResults()  # Print log if any
#' cytof_logResults("UMAP at 50%", prefix_time = FALSE)
#' }
#' 
cytof_logResults <- function(messages, time_prefix = TRUE, env = environment()) {
    if (!exists("analysis_results", where = env))
        stop("No analysis_results found.")
    if (missing(messages)) {
        if (is.null(env$analysis_results$log)) {
            message("No log.")
        } else {
            cat(env$analysis_results$log, sep = "\n")
        }
    } else {
        if (is.null(env$analysis_results$log)) time_prefix <- TRUE
        if (time_prefix) messages[1] <- paste(Sys.time(), messages[1])
        for (m in messages) {
            env$analysis_results$log <- c(env$analysis_results$log, m)
        }
    }
}


#' Open a dialog to select a single file
#'
#' Use the standard file.choose function of the base package .
#'
#' @details Unlike file.choose, choose.files will always attempt to return a
#'   character vector giving a list of files. If the user cancels the dialog,
#'   then zero files are returned, whereas file.choose would signal an error.
#' @return A character vector giving zero or more file paths.
#' @export
#' @seealso \code{\link{file.choose}}
#' @examples
#'\dontrun{
#' filename <- cytof_chooseFile()
#' }
#' 
cytof_chooseFile <- function() {
    file_chosen <- tryCatch(file.choose(), error = function(e) e)
    if (inherits(file_chosen, "error")) file_chosen <- character(0)
    file_chosen
}


#' Relocate the directory of the input FCS and of the output results
#'
#' Interactively select the RData file, the directory of the input FCS and of
#' the output results, modify RData object and store it to disk.
#'
#' @param overwrite If FALSE (default) the modified RData object is stored in
#'   the same directory as the input RData file with a unique prefix.
#' @return The written file name invisibly.
#' @export
#' @seealso \code{\link{cytof_writeResults}}
#' @examples
#' \dontrun{
#' cytofkit_RData_file <- cytof_relocateResults()
#' load(cytofkit_RData_file)
#' # The RAW FCS files should be at the following location
#' dir(analysis_results$rawFCSdir)
#' # If it's OK, save the FCS again
#' cytof_writeResults(
#'     analysis_results = analysis_results,
#'     saveToRData = FALSE,
#'     saveToFCS = TRUE,
#'     saveToFiles = FALSE)
#' dir(analysis_results$resultDir)
#' }
#' 
cytof_relocateResults <- function(overwrite = FALSE) {
    
    # select file or quit
    message("Select the cytofkit RData whose directories will be modified.")
    ck_res <- cytof_chooseFile()
    if (length(ck_res)==0) {
        stop("No RData file selected. Cancel process.", call. = FALSE)
    }
    # load the RData in an isolated environment
    ckl_RData_env <- new.env()
    load(ck_res, envir = ckl_RData_env)
    # basic checks
    if (!exists("analysis_results", ckl_RData_env)) {
        rm(ckl_RData_env)
        stop("The loaded RData is not a Cytofkit RData file.", call. = FALSE)
    }
    # locate RAW FCS
    message("Select a FCS file to locate the directory of FCS submitted to Cytofkit.")
    ck_fcs_dir <- dirname(cytof_chooseFile())
    if (length(ck_res)==0) {
        rm(ckl_RData_env)
        stop("No FCS file selected. Cancel process.", call. = FALSE)
    }
    # Check file list
    ck_res_fcs <- unique(gsub("_\\d+$", "", rownames(ckl_RData_env$analysis_results$expressionData)))
    ck_fcs_files <- gsub("\\.fcs", "", dir(ck_fcs_dir, pattern = "*.FCS", ignore.case = TRUE), ignore.case = TRUE)
    fcs_diff <- setdiff(ck_res_fcs, ck_fcs_files)
    if (length(fcs_diff)) {
        rm(ckl_RData_env)
        stop("Some FCS files of the analysis are not found in the directory\n", fcs_diff, ck_fcs_dir, call. = FALSE)
    }
    # locate RESULT DIR
    # message("Select a file whitin the directory where the results will be stored.")
    # ck_res_dir <- dirname(cytof_chooseFile())
    # if (length(ck_res_dir)==0) {
    #   message("No result directory selected. Cancel process.")
    #   rm(ckl_RData_env)
    #   return()
    # }
    ck_res_dir <- dirname(ck_res)
    # Update directories and save to disk
    cytof_logResults(env = ckl_RData_env, time_prefix = TRUE, messages = c(
        "Changing directories",
        paste("previous rawFCSdir = ", ckl_RData_env$analysis_results$rawFCSdir),
        paste("previous resultDir = ", ckl_RData_env$analysis_results$resultDir)
    ))
    ckl_RData_env$analysis_results$rawFCSdir <- ck_fcs_dir
    ckl_RData_env$analysis_results$resultDir <- ck_res_dir
    if (!overwrite) {
        ck_res <- gsub("(\\.[^.]+)$", sprintf("_%08X\\1", as.numeric(format(Sys.time(), "%s"))), ck_res)
    }
    # sprintf("%s_%08X", out_dir, as.numeric(format(Sys.time(), "%s")))
    save("analysis_results", file = ck_res, envir = ckl_RData_env)
    message("The modified RData file is ", ck_res)
    invisible(ck_res)
}


#' Time Difference Calculation
#'
#' This function calculates the difference between two time points (`start_time` and `end_time`) 
#' and returns the result formatted as "HH:MM:SS.SS". The difference is measured in seconds.
#'
#' @param start_time Starting time (POSIXct object).
#' @param end_time Ending time (POSIXct object).
#'
#' @return A character string representing the time difference formatted as "HH:MM:SS.SS".
#' @export
#'
#' @author S. Jully
#' 
#' @examples
#' start_time <- Sys.time()
#' Sys.sleep(5)
#' end_time <- Sys.time()
#' time_difference(start_time, end_time)  # "00:00:05.00"
time_difference <- function(start_time, end_time) {
    diff <- as.numeric(difftime(end_time, start_time, units = "secs"))
    hrs <- floor(diff / 3600)
    mins <- floor((diff %% 3600) / 60)
    secs <- round(diff %% 60, 2)
    return(sprintf("%02d:%02d:%05.2f", hrs, mins, secs))
}
