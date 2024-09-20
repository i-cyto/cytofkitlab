#' CyTOF Data Mapping Function
#'
#' This function maps raw CyTOF data to a set of pseudo-coordinates derived from dimensionality reduction methods (e.g., UMAP or t-SNE).
#' It takes raw and processed FCS files as inputs, performs a nearest-neighbor search based on specified markers,
#' and adds the pseudo-coordinates to the raw data. The function saves the updated raw data with the new coordinates 
#' as both FCS and RData files and modifies the FCS file descriptions.
#'
#' @param rawFCSdir The directory containing the raw FCS files.
#' @param fdrFCRdir The directory containing the FDR (full-dimensional reduction) analyzed FCS files.
#' @param fdrprojectname The project name for the FDR data.
#' @param resultDir The directory where the resulting files (mapped FCS and RData) will be saved.
#' @param markers A character vector of markers to use for matching between raw and FDR data.
#' @param dims The number of dimensions for the pseudo-coordinates (default is 2).
#' @param transformMethod The transformation method to apply (options: "autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none").
#' @param method The dimensionality reduction method used (options: "tsne", "pca", "isomap", "diffusionmap", "umap", "NULL").
#' @param k The number of nearest neighbors to search for in the KNN algorithm.
#' @param algorithm The KNN search algorithm (options: "kd_tree", "cover_tree", "CR", "brute").
#'
#' @return A data frame containing the raw data with added pseudo-coordinates.
#' @importFrom FNN knnx.index
#' @importFrom Biobase AnnotatedDataFrame
#' @importFrom flowCore read.FCS
#' @importFrom flowCore pData
#' @importFrom flowCore parameters
#' @importFrom flowCore write.FCS
#' @export
#'
#' @examples
#' cytof_mapping("path/to/rawFCS", "path/to/fdrFCS", "ProjectName", "path/to/results", markers = c("CD3", "CD19"), dims = 2)
cytof_mapping <- function( 
    rawFCSdir,
    fdrFCRdir, 
    fdrprojectname,
    resultDir, 
    markers=NULL, 
    dims=2, 
    transformMethod=c("autoLgcl", "cytofAsinh", "logicle", "arcsinh", "none"),
    method=c("tsne", "pca", "isomap", "diffusionmap", "umap", "NULL"), 
    k=10, 
    algorithm=c("kd_tree", "cover_tree", "CR", "brute")
)
{
  
  # Define file paths
  homefile_fdr <- file.path(fdrFCRdir, paste(fdrprojectname, "analyzedFCS", sep = "_"))
  
  # Load raw FCS files
  raw_files <- list.files(rawFCSdir, pattern = ".fcs$", full.names = TRUE)
  if (length(raw_files) == 0) stop("No raw FCS files found in the specified directory.")
  
  # Extract base names from raw files
  raw_file_names <- basename(raw_files)
  raw_file_base_names <- sub("_c[0-9]+_.*$", "", raw_file_names)
  
  # Read raw data
  raw_data <- lapply(raw_files, function(f) exprs(read.FCS(f, transformation = FALSE)))
  
  # Load processed FCS files with UMAP/TSNE coordinates
  fdr_files <- list.files(homefile_fdr, pattern = ".fcs$", full.names = TRUE)
  if (length(fdr_files) == 0) stop("No FDR FCS files found in the specified directory.")
  
  # Extract base names from FDR files
  fdr_file_names <- basename(fdr_files)
  fdr_file_base_names <- sub("^cytofkit_", "", fdr_file_names)
  fdr_file_base_names <- sub("_c[0-9]+_.*$", "", fdr_file_base_names)
  
  # Read FDR data
  fdr_data <- lapply(fdr_files, function(f) exprs(read.FCS(f, transformation = FALSE)))
  
  # Match raw and FDR files by base names
  file_mapping <- match(raw_file_base_names, fdr_file_base_names)
  if (any(is.na(file_mapping))) stop("Some raw files do not match any FDR files.")
  
  # Generate UMAP/TSNE column names based on the number of dimensions
  pseudo_columns <- paste0(method,"_", 1:dims, "_linear")
  pseudo_coords <- do.call(rbind, lapply(fdr_data[file_mapping], function(df) df[, pseudo_columns, drop = FALSE]))
  
  # Rename UMAP/TSNE columns
  colnames(pseudo_coords) <- paste0("pseudo_coord_", method,"_", 1:dims)
  
  # Check if the markers are present in both raw and FDR data
  if (is.null(markers)) stop("Markers must be specified.")
  params <- sub("<.+?>", "", markers)
  raw_params_present <- params[params %in% colnames(raw_data[[1]])]
  fdr_params_present <- params[params %in% colnames(fdr_data[[1]])]
  
  if (length(raw_params_present) == 0 || length(fdr_params_present) == 0) {
    stop("Specified markers are not present in both raw and FDR data.")
  }
  
  # Create matrices of markers for raw and FDR files
  raw_features_combined <- do.call(rbind, lapply(raw_data, function(df) df[, raw_params_present, drop = FALSE]))
  fdr_features_combined <- do.call(rbind, lapply(fdr_data, function(df) df[, fdr_params_present, drop = FALSE]))
  
  # Filter out empty or non-numeric columns
  raw_features_combined <- raw_features_combined[, colSums(is.na(raw_features_combined)) == 0, drop = FALSE]
  fdr_features_combined <- fdr_features_combined[, colSums(is.na(fdr_features_combined)) == 0, drop = FALSE]
  
  # Perform nearest-neighbor search
  knn_indices <- FNN::knnx.index(data = fdr_features_combined, query = raw_features_combined, k = k, algorithm = algorithm)
  
  # Map the nearest neighbors
  nearest_neighbors <- knn_indices[, 1]
  
  # Add UMAP/TSNE coordinates to the raw data
  raw_data_with_pseudo_coord <- as.data.frame(raw_features_combined)
  raw_data_with_pseudo_coord$pseudo_coord_1 <- pseudo_coords[nearest_neighbors, 1]
  raw_data_with_pseudo_coord$pseudo_coord_2 <- pseudo_coords[nearest_neighbors, 2]
  
  
  # Save the mapped files
  for (i in seq_along(raw_files)) {
    original_filename <- basename(raw_files[i])
    raw_file_data <- raw_data[[i]]
    
    # Combine raw data with pseudo coordinates
    raw_file_with_pseudo_coord <- cbind(raw_file_data,
                                        pseudo_coord_1 = pseudo_coords[nearest_neighbors, 1],
                                        pseudo_coord_2 = pseudo_coords[nearest_neighbors, 2]
    )
    
    # Generate new file name and path
    new_filename <- paste0(method,"_", original_filename)
    new_fcs_path <- file.path(resultDir, new_filename)
    
    # Save as flowFrame and write FCS file
    flow_data <- flowFrame(as.matrix(raw_file_with_pseudo_coord))
    write.FCS(flow_data, new_fcs_path)
    
    # Save as RData
    rdata_path <- sub("\\.fcs$", ".RData", new_fcs_path)
    save(raw_file_with_pseudo_coord, file = rdata_path)
    
    message("File saved: ", new_fcs_path)
    message("RData saved: ", rdata_path)
  }
  
  # Load saved FCS files and modify descriptions
  fcs_files_map <- list.files(resultDir, pattern = ".fcs$", full.names = TRUE)
  
  for (i in seq_along(fcs_files_map)) {
    raw_file <- flowCore::read.FCS(raw_files[i], transformation = FALSE)
    raw_desc <- flowCore::pData(flowCore::parameters(raw_file))$desc 
    
    fcs_file_map <- flowCore::read.FCS(fcs_files_map[i], transformation = FALSE)
    map_params <- flowCore::pData(flowCore::parameters(fcs_file_map))
    
    # Update marker descriptions
    map_desc <- map_params$desc
    map_desc[1:length(raw_desc)] <- raw_desc
    map_params$desc <- map_desc
    annotated_params <- Biobase::AnnotatedDataFrame(map_params)
    flowCore::parameters(fcs_file_map) <- annotated_params
    
    # Save FCS file with updated descriptions
    flowCore::write.FCS(fcs_file_map, fcs_files_map[i])
    message("FCS file with updated descriptions saved: ", fcs_files_map[i])
  }
  return(raw_data_with_pseudo_coord)
}