#' Vectorize!
#'
#' People often confuse iteration and vectorization.
#'
#' The implementation below is an iteration. Vectorize it.
#'
#' Show that vectorization influences performance using the
#' microbenchmark package.
#'
#' Find one StackOverflow post where the question or answer confuses
#' iteration and vectorization.
#'
#' @param x a numeric vector of non-negative numbers of any length
#'
#' @return A numeric vector of \code{length(x)}, where each element is
#'     the square root of x
#' 
#' @examples
#' vectorize(1:10)
#' @export
vectorize <- function(x) {
    stopifnot(is.numeric(x), all(x >= 0))
    vapply(x, sqrt, numeric(1))
}

#' Arrgh! copy-and-append
#'
#' A very common problem with severe performance consequences is the
#' \sQuote{copy-and-append} pattern. Save me from this craziness, and
#' document that even the simple example below scales terribly. Do
#' this in three steps. (1) Write a for-loop but pre-allocate the
#' result vector and fill it in. (2) Use an sapply() to avoid thinking
#' about pre-allocation. (3) implement the obvious vectorization.
#'
#' Verify using \code{identical()} that each solution returns the same
#' result for each implementation.
#'
#' Verify the inefficiency of copy-and-append using microbenchmark to
#' evaluate the original function with different values of \code{n}
#' and plotting the result.
#'
#' Verify the performance gain for a single value of \code{n} across
#' the three implementations.
#'
#' Find one StackOverflow question where the solution to a significant
#' performance bottleneck is to avoid copy-and-append.
#'
#' @param n a scalar (length 1) numeric non-negative vector indicating
#'     the number of a number will be
#'
#' @examples
#' loopy(5)
#' @export
loopy <- function(n) {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 0)
    result <- integer()
    for (i in seq_len(n))
        result <- c(result, sqrt(i))
    result
}
