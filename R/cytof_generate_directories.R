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