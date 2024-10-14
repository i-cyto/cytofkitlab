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
#' @author S. Jully
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


#' Create and List Folders for Project Execution
#'
#' This function creates a set of folders for organizing output data related to the project, 
#' including folders for first dimensional reduction (FDR), second dimensional reduction (SDR), and mapping (MAP).
#' If the folders already exist, it notifies the user. The function also lists the created folders
#' and returns their paths along with the corresponding project names.
#'
#' @param destinationfile The base directory where the folders will be created.
#' @param numero_exe The number of the execution or experiment (used to create unique folder names).
#' @param PROJET The name of the project (used as part of the folder names).
#'
#' @return A list containing project names (FDR, SDR, MAP) and the paths of the created folders.
#' @export
#'
#' @author S. Jully
#'
#' @examples
#' create_and_list_folders("base_directory", 1, "MyProject")
create_and_list_folders <- function(destinationfile, numero_exe, PROJET) {
    # Define the experience folder name with the experience number
    experience_folder <- file.path(destinationfile, paste("Experience", numero_exe, sep = "_"))
    
    # Check and create the 'Experience' + experience number folder if it does not exist
    if (!dir.exists(experience_folder)) {
        dir.create(experience_folder, recursive = TRUE)
        message("The 'Experience' folder has been created: ", experience_folder)
    } else {
        message("The 'Experience' folder already exists: ", experience_folder)
    }
    
    # Update the folder paths to be inside the experience folder
    destinationfile_fdr <- file.path(experience_folder, paste(PROJET, "FDR", numero_exe, sep = "_"))
    destinationfile_sdr <- file.path(experience_folder, paste(PROJET, "SDR", numero_exe, sep = "_"))
    destinationfile_map <- file.path(experience_folder, paste(PROJET, "MAP", numero_exe, sep = "_"))
    
    # Define the path for the 'IMG' folder inside 'SDR'
    img_folder <- file.path(destinationfile_sdr, "IMG")
    
    # Check and create the folders as needed
    if (!dir.exists(destinationfile_fdr)) {
        dir.create(destinationfile_fdr, recursive = TRUE)
        message("The 'FDR' folder has been created: ", destinationfile_fdr)
    } else {
        message("The 'FDR' folder already exists: ", destinationfile_fdr)
    }
    
    if (!dir.exists(destinationfile_sdr)) {
        dir.create(destinationfile_sdr, recursive = TRUE)
        message("The 'SDR' folder has been created: ", destinationfile_sdr)
    } else {
        message("The 'SDR' folder already exists: ", destinationfile_sdr)
    }
    
    if (!dir.exists(img_folder)) {
        dir.create(img_folder, recursive = TRUE)
        message("The 'IMG' folder has been created in the 'SDR' directory: ", img_folder)
    } else {
        message("The 'IMG' folder already exists in the 'SDR' directory: ", img_folder)
    }
    
    if (!dir.exists(destinationfile_map)) {
        dir.create(destinationfile_map, recursive = TRUE)
        message("The 'MAP' folder has been created: ", destinationfile_map)
    } else {
        message("The 'MAP' folder already exists: ", destinationfile_map)
    }
    
    # Display the folder paths
    message("Path of the 'Experience' folder: ", experience_folder)
    message("Path of the 'FDR' folder: ", destinationfile_fdr)
    message("Path of the 'SDR' folder: ", destinationfile_sdr)
    message("Path of the 'IMG' folder: ", img_folder)
    message("Path of the 'MAP' folder: ", destinationfile_map)
    
    # Create project names by appending "FDR", "SDR", and "MAP"
    projet_fdr <- paste(PROJET, "FDR", sep = "_")
    projet_sdr <- paste(PROJET, "SDR", sep = "_")
    projet_map <- paste(PROJET, "MAP", sep = "_")
    
    # Display the project names
    message("Name of the FDR project: ", projet_fdr)
    message("Name of the SDR project: ", projet_sdr)
    message("Name of the MAP project: ", projet_map)
    
    # Return the project names and folder paths
    return(list(
        projet_fdr = projet_fdr,
        projet_sdr = projet_sdr,
        projet_map = projet_map,
        experience_folder = experience_folder,
        destinationfile_fdr = destinationfile_fdr,
        destinationfile_sdr = destinationfile_sdr,
        destinationfile_map = destinationfile_map,
        img_folder = img_folder
    ))
}

#' Map RAW FCS files to cytofkit result and transfer reduced dimensions and
#' clusterings.
#'
#' @param ck_res file name of a result
#' @param res_map_fcs_dir directory to write the mapped FCS files
#' @param knn number of nearest neighbors
#' @param average_dr method to average reduced dimension coordinates. Either
#'   "arithmetic", either "softmax" weighted, the latter being slower.
#' @param prefix prefix to the resulting FCS file name
#' @param suffix suffix to the resulting FCS file name
#' @param comp apply compensation
#' @param transformMethod transformation method
#' @param ... parameters passed for the transformation
#'
#' @return none
#' @importFrom FNN knnx.index
#' @importFrom Biobase AnnotatedDataFrame
#' @importFrom flowCore read.FCS
#' @importFrom flowCore pData
#' @importFrom flowCore parameters
#' @importFrom flowCore write.FCS
#' @export
#'
#' @author S. Jully
#'
#' @examples
#' # None
cytof_map_knn <- function(
        ck_res,
        res_map_fcs_dir = "%s_map_k%d",
        knn = 1,
        average_dr = "arithmetic",
        prefix = "",
        suffix = "_k%d",
        comp = TRUE,
        transformMethod,
        ...
) {
    # output directory
    prj_name = basename(dirname(ck_res))
    prj_dir = dirname(ck_res)
    if (grepl("%d", res_map_fcs_dir))
        res_map_fcs_dir = sub("%d", knn, res_map_fcs_dir)
    if (grepl("%s", res_map_fcs_dir))
        res_map_fcs_dir = sub("%s", prj_name, res_map_fcs_dir)
    res_map_fcs_dir = file.path(prj_dir, res_map_fcs_dir)
    # setup output directory
    if (!dir.exists(res_map_fcs_dir))
        dir.create(res_map_fcs_dir, recursive = TRUE)
    # suffix
    if (grepl("%d", suffix))
        suffix = sprintf(suffix, knn)
    # load result for cytofkit and extract parameters
    load(ck_res)
    res_raw_fcs_dir = analysis_results$rawFCSdir
    res_markers = analysis_results$dimRedMarkers
    # reference data = landmarks
    refrnc <- analysis_results$expressionData[,res_markers]
    # DR and C to add as channels
    to_add <- do.call(cbind, analysis_results$dimReducedRes)
    tcols <- colnames(to_add)
    to_add <- cbind(to_add, do.call(cbind, analysis_results$clusterRes))
    ccols <- setdiff(colnames(to_add), tcols)
    # parse RAW FCS files
    fcs_files = unlist(analysis_results$sampleNames)
    for (fcs in fcs_files) {
        # debug: fcs = fcs_files[1]
        # file path to RAW FCS
        fn <- file.path(res_raw_fcs_dir, paste0(fcs, ".fcs"))
        # read and transform the FCS file
        ff <- cytof_readFCS(fn, comp = comp)
        ft <- cytof_transfFCS(ff, transformMethod = transformMethod, ...)
        # set query data to be mapped to landmarks
        unkown <- exprs(ft)[,res_markers]
        # identify NN
        nn <- FNN::get.knnx(refrnc, unkown, k = knn, "kd_tree")
        # update the expression value with NN
        # values from 1 NN or as initilization
        to_add_i <- to_add[nn$nn.index[,1], ]
        if (knn > 1) {
            # arithmetic mean of coordinates
            if (average_dr == "arithmetic") {
                for (col in tcols) {
                    coords <- apply(nn$nn.index, 2, function(j)
                        to_add[j, col])
                    to_add_i[, col] <- rowMeans(coords)
                }
            }
            # softmax weighted mean of coordinates
            if (average_dr == "softmax") {
                for (col in tcols) {
                    coords <- apply(nn$nn.index, 2, function(j)
                        to_add[j, col])
                    weights <- exp(-nn$nn.dist)
                    weights <- weights / rowSums(weights)
                    to_add_i[, col] <- rowSums(coords * weights)
                }
            }
            # relative majority for clustering
            for (col in ccols) {
                tbl <- table(rep(1:nrow(to_add_i), knn),
                             to_add[nn$nn.index, col])
                cl_ids <- as.numeric(colnames(tbl))
                to_add_i[,col] <- cl_ids[apply(tbl, 1, which.max)]
            }
        }
        # rename cols to append
        colnames(to_add_i) = paste0("m_", colnames(to_add_i))
        # append information
        out_frame <- flowCore::fr_append_cols(ff, to_add_i)
        # write to disk
        suppressWarnings(write.FCS(out_frame, file.path(
            res_map_fcs_dir, sprintf("%s%s%s.fcs", prefix, fcs, suffix))))
    }
}
