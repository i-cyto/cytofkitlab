#' A Shiny APP to interactively visualize the analysis results
#'
#' Take the the RData object file saved by cytofkit as input, automatically load the data and allow
#' exploration of the analysis results with interactive control
#'
#'
#' @param RData Either the RData object file or data object, if missing, RData file need to be
#'   loaded on the ShinyAPP
#' @param onServer Logical value, if \verb{TRUE}, sets shinyApp host to 0.0.0.0 for other clients to
#'   access, otherwise defaults to 127.0.0.1 (local host)
#' @param port Integer value, specifies a port for shinyApp for other clients to access, otherwise
#'   automatic selection
#'
#' @return Opens shinyApp session for data visualisation
#'
#' @import shiny
#' @import shinyFiles
#' @importFrom grDevices dev.copy2pdf
#' @importFrom graphics plot
#'
#' @author Hao Chen
#'
#' @export
#'
#' @examples
#' d <- system.file('extdata',package='cytofkitlab')
#' Rdata <- list.files(d, pattern = '.RData$', full.names = TRUE)
#' #only for interactive sessions, remove hash to run
#' #cytofkitShinyAPP(Rdata)
#' 
cytofkitShinyAPP  <- function(RData = NULL, onServer = FALSE, port = NULL) {

  # Pass RData to the server environment
  # TODO: avoid duplication of usage of RData; pass the file name?
  analysis_results <- NULL
  sampleInformation <- NULL
  progCluster <- NULL
  serverObj <- NULL
  roots <- c(wd=getwd(), Home = "~")
  if(!missing(RData)){
    browser()
    if(is.character(RData)) {
      if(file.exists(RData))
        stop("RData file doesn't exist! Please check your obj file")
      if(!grepl("\\.RData$", RData))
        stop("Argument is not .RData file!")
      load(RData)
      direct_analysis_results <- analysis_results
      message(".RData loaded!")
    }else{
      analysis_results <- RData
    }
    
    if(is.null(analysis_results$projectName)){
      analysis_results$projectName <- "cytofkit_shinyAPP_output"
    }
    
    if(!is.null(analysis_results$progressionRes)){
      ## default the first cluster results are used for progression analysis
      # TODO: check if it's OK
      progCluster <- names(analysis_results$clusterRes)[1]
    }
    
    sampleInformation <- data.frame(
      cellID = row.names(analysis_results$expressionData),
      cellSample = factor(sub("_[0-9.]+$", "", row.names(analysis_results$expressionData))),
      stringsAsFactors = FALSE)
    analysis_results$sampleInfo <- sampleInformation
  }

  # Load all the components of a shiny app; could add others modules
  # See https://github.com/asardaes/dtwclust/tree/master/inst/ssdtwclust
  file_path <- system.file("shiny", "app.R", package = "cytofkitlab")
  if (!nzchar(file_path)) stop("Shiny app not found")
  ui <- server <- NULL # avoid NOTE about undefined globals
  source(file_path, local = TRUE, chdir = TRUE)
  
  # Pass values to the server environment
  server_env <- environment(server)
  server_env$analysis_results <- analysis_results
  server_env$progCluster <- progCluster
  server_env$serverObj <- serverObj
  server_env$roots <- roots
  server_env$sampleInformation <- sampleInformation

  # add options
  host <- ifelse(isTRUE(onServer), "0.0.0.0", "127.0.0.1")
  options(shiny.launch.browser = TRUE, shiny.host = host, shiny.maxRequestSize=1024^10)
  if(!is.null(port) && is.integer(port)) options(shiny.port = port)
  
  # launch
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app)
}
