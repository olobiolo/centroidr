### CREATING PACKAGE CALLED CENTROID ###
# author Aleksander Chlebowski
# concepts the idea is to express variance of a n-diemnsional data point that exists in an arbitrary number of replicates
#          the process is as follows:
#          1. coordinates of all replicates are supplied
#             some replicates may be missing for whatever reason
#          2. the centroid of the set is determined by averaging coordinates in each respective dimension
#          3. the point-centroid distances are computed
#I am unable to make it work to mutate a grouped data frame.

#' centroidr: centroids and distances
#'
#' The package finds the centroid of a set of points in n-dimensional space
#' and calculates the distances between the points and the centroid.
#' It provides two functinos: \code{centroid} and \code{cendist}, and has one internal function, \code{euclidean}.
#'
#' @author Aleksander Chlebowski
#'
#' @section functions:
#' \code{euclidean} is an internal function that calculte the euclidean distance between points.
#'
#' \code{centroid} finds the centroid and returns its coordinates.
#'
#' \code{centroid_distance} calls \code{centroid} and \code{euclidean}
#' and returns the distances between each point and the centroid.
#'
#' @docType package
#' @name centroidr
NULL
