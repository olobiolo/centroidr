#' @title Find centroid
#'
#' @description Generic function that, given a set of pionts, finds their centroid.
#'
#' @details
#' Centroid coordinates are derived as average coordinates of all points in respective dimensions.
#' If a point is missing a coordinate, the remainig dimensions are still considered.
#' Methods exist for matrix and data frame.
#'
#' The data frame method isolates relevant columns as a matrix and calls the matrix method.
#'
#' @param x     matrix or data frame with observations in rows and dimensions in columns
#'
#' @param ...   for data frame method, columns that store cooridinates
#'
#' @return      numeric vector of centroid coordinates
#'
#' @section Column specification:
#' Matrices are accepted as is, all columns are considered.
#' If \code{x} is a data frame, columns that carry coordinates may be specified.
#' They can be given as strings, bare names or numerical indices but
#' they must all be specified in the same way.
#' Also, each column must be specified separately, vectors are not accepted.
#' If no columns are specified, all numeric ones are considered.
#'
#' @export
#'

centroid <- function(x, ...) {
  UseMethod('centroid')
}

#' @export
#' @describeIn centroid
centroid.numeric <- function(x, ...) {
  #' simply returns the mean, NAs are omitted
  if (!is.numeric(x)) stop('non-numeric input')
  if (length(x) == 0) {
    warning('no points provided')
    return(NA)
  } else if (length(x) == 1) {
    warning('only one point, returning as is')
    return(x)
  } else mean(x, na.rm = T)
}

#' @export
#' @describeIn centroid
centroid.matrix <- function(x, ...) {
  #' simply averages columns, omitting NAs
  if (!is.numeric(x)) stop('non-numeric input')
  if (nrow(x) == 0 || ncol(x) == 0) {
    warning('no points provided')
    return(NULL)
  }
  if (nrow(x) == 1) warning('only one point, returning as is')
  if (ncol(x) == 1) warning('only one dimension')
  colMeans(x, na.rm = T)
}

#' @export
#' @describeIn centroid
centroid.data.frame <- function(x, ...) {
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
  centroid.matrix(m)
}

#' @examples
#' m <- matrix(rnorm(15), 5,3)
#' centroid(m)
#'
#' d <- as.data.frame(m)
#' names(d) <- c('one', 'two', 'three')
#' d$cat <- letters[1:5]
#' centroid(d)
#' centroid(d, one, two)
