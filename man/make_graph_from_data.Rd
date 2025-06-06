% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_graph_from_data.R
\name{make_graph_from_data}
\alias{make_graph_from_data}
\title{Make a graph object from data set containing weighted edgelist}
\usage{
make_graph_from_data(filepath)
}
\arguments{
\item{filepath}{Path to the input file}
}
\value{
Graph representing the transition matrix
}
\description{
Upload a dataset where the first row is the destination firm and the second
column is the origin firm, and the third column is the number of workers that
transitioned from the origin firm to the destination firm, and make a graph object.
}
