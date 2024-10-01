#' Function to merge default options with dots arguments
#'
#' Arguments in the dots list beginning withng the given prefix are retrieved.
#' Their value replaces the value of the options in the defaults list when the
#' same name exists in both lists. Otherwise, options of both lists are kept in
#' the resulting merged list where the prefix is removed
#'
#' @param prefix a character string that identifies the options to retrieve and
#'   merge. The prefix does not contain the separator.
#' @param defaults a list of options that hold the default values.
#' @param dots a list of options, typically the '...' arguments passed to a
#'   function. Each option/value is named and the names are considered beginning
#'   with prefix followed by the '.' separator.
#'
#' @return a list of named values aka options.
#'
#' @examples
#' # None
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

#' cytof_verbose gets/sets the verbosity level for functions that need it
#'
#' @param lev integer level of verbosity. When defined, this set the level. When
#'   missing the current level is reported. If level has not been defined, 0 is
#'   returned.
#'
#' @return verbosity
#' @export
#'
#' @examples
#' cytof_verbose()
#' cytof_verbose(1)
#' cytof_verbose()
cytof_verbose <- function(lev) {
    if (missing(lev)) {
        lev <- options("cytofkit.verbose")[[1]]
        if (is.null(lev)) lev <- 0
        return(lev)
    }
    options(cytofkit.verbose = lev)
}
