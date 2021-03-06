% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_cendist.R
\name{cendist}
\alias{cendist}
\alias{cendist.numeric}
\alias{cendist.matrix}
\alias{cendist.data.frame}
\alias{cendist.grouped_df}
\title{Compute distances from centroid}
\usage{
cendist(x, ...)

\method{cendist}{numeric}(x, ...)

\method{cendist}{matrix}(x, ...)

\method{cendist}{data.frame}(x, ...)

\method{cendist}{grouped_df}(x, ...)
}
\arguments{
\item{x}{numeric vector, matrix or data frame}

\item{...}{for data frame method, columns that store cooridinates}
}
\value{
numeric vector of distances
}
\description{
Generic function that, given a set of pionts,
finds distances between them and their centroid.
Calls \code{centroid} and \code{euclidean}.
}
\details{
A set of coordinates in n-dimensional space can be provided as a matrix or a data frame
with points listed in rows and dimensions listed in columns (see \code{Column specification}).
First the centroid coordinates is found by \code{centroid}
and then the point-centroid distances are computed by \code{euclidean}.
}
\section{Methods (by class)}{
\itemize{
\item \code{numeric}: returns deviations from the mean

\item \code{matrix}: finds the centroid and applies the \code{catresian} function across the matrix rows;
if \code{x} only has one column, drops dimensions and calls vector method

\item \code{data.frame}: extracts the requested columns and converts them to a matrix and then calls the matrix method

\item \code{grouped_df}: see \code{\link[metamethods]{data.frame__to__grouped_df}}
}}

\section{Special cases}{

For a 1-dimensional set (one column matrix) deviations from the mean are returned.

For a single point (one row matrix) expect \code{Value} to be 0.

Any missing coordinates (NAs) will derail distance calculation for that point.
}

\section{Column specification}{

Matrices are accepted as is, all columns are considered.
If \code{x} is a data frame, columns that carry coordinates may be specified.
They can be given as strings, bare names or numerical indices but
they must all be specified in the same way.
Also, each column must be specified separately, vectors are not accepted.
If no columns are specified, all numeric ones are considered.
}

