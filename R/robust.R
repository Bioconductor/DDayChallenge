#' Check inputs against known limitations
#'
#' Checking inputs allows code to \sQuote{fail early} with clear error
#' messages, rather tha failing cryptically later.
#'
#' Update the code below to check that the variable used in the
#' \code{if} is scalar, to avoid a mysterious message \dQuote{the
#' condition has length > 1 and only the first element will be
#' used}. Use \code{stopifnot()} as a convenient way to test the pre-
#' conditions.
#'
#' Find examples on the Bioconductor support site where this error has
#' confused users.
#'
#' @param x a scalar (length 1) numeric vector to be compared to a
#'     random number.
#'
#' @return A scalar numeric vector. If the input is greater than a
#'     random normal deviate, the return value is the random normal
#'     deviate. Otherwise, the return value is the orignal input.
#'
#' @examples
#' preconditions(.1)
#' @importFrom stats rnorm
#' @export
preconditions <- function(x) {
    deviate <- rnorm(1)
    if (x > deviate)
        deviate
    else
        x
}

#' Generate sequences correctly
#'
#' A very common problem is the use of 1:n to generate a sequence. The
#' following illustrates the problem; what's the solution?
#'
#' @param x a numeric vector of any length
#' 
#' @return A character vector of the same length as \code{x}
#' 
#' @examples
#' sequences(1:5)         # success
#' sequences(integer(0))  # failure!
#' @export
sequences <- function(x) {
    stopifnot(is.numeric(x))
    as.character(1:length(x))
}
