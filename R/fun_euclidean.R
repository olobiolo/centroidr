#' Find euclidean distance between two points
#'
#' Given two sets of coordinates, find euclidean distance between them in n-dimensional space.
#'
#' This is a simple vectorised calculation that returns the square root
#' of the sum of squared differences between respective elements of x and y.
#' It is used internally to calculate distances between points in a set and the set's centroid.
#'
#' @param pt1,pt2     numeric vectors of length n; coordinates of points 1 and 2, respectively
#'
#' @return           a single number
#'
#' @aliases distance
#'
#' @keywords internal
#'
euclidean <- function(pt1, pt2) {
  if (!is.numeric(pt1) || !is.numeric(pt2)) stop('non-numeric input')
  if (length(pt1) != length(pt2)) stop('different lengths of pt1 nad pt2')
  sqrt(sum((pt1 - pt2) ^ 2))
}

#' @examples
#' a <- sample(1:10, 2)
#' b <- sample(1:10, 2)
#' plot(rbind(a,b))
#' lines(rbind(a,b), type = 'c')
#' euclidean(a,b)
