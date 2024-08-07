% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complexity_graphs.R
\name{graph_complexity_tree}
\alias{graph_complexity_tree}
\title{Export Tree Map.}
\usage{
graph_complexity_tree(year, region)
}
\arguments{
\item{year}{year}

\item{region}{which region to draw the map. Only Australian States supported.}
}
\value{
ggraph2 object
}
\description{
Colours adjusted to match Atlas of Economic Complexity.
}
\examples{
\dontrun{
graph_complexity_tree(2021, "SA")
}
}
