#' Helpers
#'
#' @name helpers
#'
#' @rdname helpers
#' @export
silent <- function(...){
  base::suppressWarnings(...)
}
#'
#' @rdname helpers
#' @export
is_numchar <- function(x, verbose = FALSE){
  assertthat::assert_that(is.character(x),
                          msg = "`x` must be a character vector.")
  if( verbose ) {
    x0 <- try(as.numeric(x))
  } else {
    x0 <- silent(try(as.numeric(x)))
  }
  !any(is.na(x0))
}
#' @rdname helpers
#' @export
is_numeric <- function(x){
  is.numeric(x) || all(is_numchar(x))
}
#'
#' @rdname helpers
#' @export
n_int_digits <- function(x, .fn = NULL, ...){
  assertthat::assert_that(is_numeric(x),
                          msg = "`x` must be coercible to a numeric vector.")
  if( length(x) == 0L ) {
    return(0)
  } else {
    x <- abs(round(x))
  }
  if ( !is.null(.fn) ) { x <- purrr::as_mapper(.fn, ...)(x) }
  out <- ceiling(log(x + 1L, base = 10L))
  as.integer(out)
}
#'
#' @rdname helpers
#' @export
max_int_digits <- function(x = numeric()){
  n_int_digits(x, ~ max(.x, na.rm = TRUE))
}
#

