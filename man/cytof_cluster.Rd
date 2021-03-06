% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cytof_cluster.R
\name{cytof_cluster}
\alias{cytof_cluster}
\title{Subset detection by clustering}
\usage{
cytof_cluster(
  ydata = NULL,
  xdata = NULL,
  method = c("Rphenograph", "ClusterX", "DensVM", "FlowSOM", "NULL", "FlowSOMDR"),
  Rphenograph_k = 30,
  FlowSOM_k = 40,
  flowSeed = NULL,
  ...
)
}
\arguments{
\item{ydata}{A matrix of the dimension reduced data.}

\item{xdata}{A matrix of the expression data.}

\item{method}{Cluster method including \code{DensVM}, \code{densityClustX},
\code{Rphenograph} and \code{FlowSOM}.}

\item{Rphenograph_k}{Integer number of nearest neighbours to pass to
Rphenograph.}

\item{FlowSOM_k}{Number of clusters for meta clustering in FlowSOM.}

\item{flowSeed}{Integer to set a seed for FlowSOM for reproducible results.}

\item{...}{Parameters passed to clustering methods.}
}
\value{
a vector of the clusters assigned for each row of the ydata
}
\description{
Apply clustering algorithms to detect cell subsets. \code{DensVM} clustering
is based on the transformed ydata and uses xdata to train the model.
\code{ClusterX} clustering works on the transformed ydata (data obtained from
a tSNE reduction). \code{Rphenograph} and \code{FlowSOM} directly work on high
dimensional xdata. \code{FlowSOM} is integrated from FlowSOM package
(https://bioconductor.org/packages/release/bioc/html/FlowSOM.html).
}
\examples{
d<-system.file('extdata',package='cytofkitlab')
fcsFile <- list.files(d, pattern='.fcs$', full=TRUE)
parameters <- list.files(d, pattern='.txt$', full=TRUE)
markers <- as.character(read.table(parameters, header = FALSE)[, 1])
xdata <- cytof_exprsMerge(fcsFile, mergeMethod = 'fixed', fixedNum = 100)
ydata <- cytof_dimReduction(xdata, markers = markers, method = "tsne")
clusters <- cytof_cluster(ydata, xdata, method = "ClusterX")
}
