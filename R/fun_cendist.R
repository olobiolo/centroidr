#' @title Compute distances from centroid
#'
#' @description
#' Generic function that, given a set of pionts,
#' finds distances between them and their centroid.
#' Calls \code{centroid} and \code{euclidean}.
#'
#' @details
#' A set of coordinates in n-dimensional space can be provided as a matrix or a data frame
#' with points listed in rows and dimensions listed in columns (see \code{Column specification}).
#' First the centroid coordinates is found by \code{centroid}
#' and then the point-centroid distances are computed by \code{euclidean}.
#'
#' @param x numeric vector, matrix or data frame
#'
#' @return numeric vector of distances
#'
#' @inheritParams centroid
#'
#' @section Special cases:
#' For a 1-dimensional set (one column matrix) deviations from the mean are returned.
#'
#' For a single point (one row matrix) expect \code{Value} to be 0.
#'
#' Any missing coordinates (NAs) will derail distance calculation for that point.
#'
#' @inheritSection centroid Column specification
#'
#' @export
#'

cendist <- function(x, ...) {
  UseMethod('cendist')
}

#' @describeIn cendist
cendist.numeric <- function(x, ...) {
#' returns deviations from the mean
  if (!is.numeric(x)) stop('non-numeric input')
  C <- mean(x)
  x - C
}

#' @export
#' @describeIn cendist
cendist.matrix <- function(x, ...) {
#' finds the centroid and applies the \code{catresian} function across the matrix rows;
#' if \code{x} only has one column, drops dimensions and calls vector method
  if (!is.numeric(x)) stop('non-numeric input')
  if (ncol(x) == 1) {
   warning('one-dimensional data set, calling vector method')
   return(cendist.numeric(drop(x)))
  }
  C <- centroid(x)
  apply(x, 1, euclidean, pt2 = C)
}

#' @export
#' @describeIn cendist
cendist.data.frame <- function(x, ...) {
#' extracts the requested columns and converts them to a matrix and then calls the matrix method

  # capture coordinate column specification
  cols <- substitute(list(...))[-1]
  # if no columns are specified, use all numeric columns in the data frame
  if (length(cols) == 0) {
    m <- as.matrix(Filter(f = is.numeric, x = x))
  } else {
    # define function that will test consistency of the above specification
    f <- function(x, y) if (identical(x, y)) return(y) else stop('columns must be specified consistently', call. = FALSE)
    Reduce(f, sapply(cols, typeof))
    # define function that will convert column specification to character/numeric vector
    to.char <- function(X) X <- if (is.numeric(X) || is.character(X)) X else if (is.name(X)) deparse(X) else stop('wrong argument:', X)
    cols <- sapply(cols, to.char)
    if(!all(sapply(x[, cols], is.numeric))) stop('non-numeric columns selected')
    #isolate coordinates to a matrix and pass to matrix method
    m <- as.matrix(x[, cols])
  }
  cendist.matrix(m)
}

#' @examples
#' m <- matrix(rnorm(15), 5,3)
#' cendist(m)
#'
#' d <- as.data.frame(m)
#' names(d) <- c('one', 'two', 'three')
#' d$cat <- letters[1:5]
#' cendist(d)
#' cendist(d, one, two)
