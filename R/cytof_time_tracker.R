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

#' Generate Execution Time Information File
#'
#' This function records various time information from different stages of the analysis pipeline 
#' (such as dimensionality reduction, clustering, and mapping) into a text file.
#'
#' @param experience_folder Directory where the results are stored.
#' @param numero_exe Execution number or identifier for this run.
#' @param parameters_FDR Parameters used for the full-dimensional reduction (FDR).
#' @param parameters_SDR Parameters used for the sub-dimensional reduction (SDR).
#' @param method_dim_reduction_FDR Method used for full-dimensional reduction (e.g., t-SNE, UMAP).
#' @param fixedNum_FDR Number of cells used for the FDR step.
#' @param time_dimred_fdr Execution time for the full-dimensional reduction step (in seconds).
#' @param time_cluster_fdr Execution time for the FDR clustering step (in seconds).
#' @param max_nb_knn Maximum number of nearest neighbors to search during the mapping phase.
#' @param time_map Execution time for the mapping phase (in seconds).
#' @param method_dim_reduction_SDR Method used for sub-dimensional reduction (e.g., t-SNE, UMAP).
#' @param fixedNum_SDR Number of cells used for the SDR step.
#' @param time_dimred_sdr Execution time for the sub-dimensional reduction step (in seconds).
#' @param time_cluster_sdr Execution time for the SDR clustering step (in seconds).
#'
#' @return No return value. Writes the information to a text file.
#' @export
#'
#' @examples
#' time_info("results_folder", 1, c("param1", "param2"), c("paramA", "paramB"), 
#'           "UMAP", 1000, 120.5, 60.3, 50, 180, "t-SNE", 800, 100.1, 45.2)
time_info <- function(
    experience_folder,
    numero_exe,
    parameters_FDR,
    parameters_SDR,
    method_dim_reduction_FDR,
    fixedNum_FDR,
    time_dimred_fdr,
    time_cluster_fdr,
    max_nb_knn,
    time_map,
    method_dim_reduction_SDR,
    fixedNum_SDR,
    time_dimred_sdr,
    time_cluster_sdr
)
{
  txt_filename <- file.path(experience_folder, paste0("R_result_info_", numero_exe, ".txt"))
  writeLines(c(
    "1. FDR",
    paste(parameters_FDR, collapse = ", "),
    paste("Method:", method_dim_reduction_FDR),
    paste("Number of cells:", fixedNum_FDR),
    paste("Dimensionality reduction execution time:", time_dimred_fdr, "seconds"),
    paste("Cluster X execution time:", time_cluster_fdr, "seconds"),
    "",
    "2. MAPPING",
    paste("Maximum number of nearest neighbors to search:", max_nb_knn),
    paste("Total mapping phase execution time:", time_map, "seconds"),
    "",
    "3. SDR",
    paste(parameters_SDR, collapse = ", "),
    paste("Method:", method_dim_reduction_SDR),
    paste("Number of cells:", fixedNum_SDR),
    paste("Dimensionality reduction execution time:", time_dimred_sdr, "seconds"),
    paste("Cluster X execution time:", time_cluster_sdr, "seconds")
  ), txt_filename)
}
