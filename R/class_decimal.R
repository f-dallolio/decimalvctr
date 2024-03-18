#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name pizza-vctrs
NULL

# constructor
#' @export
new_decimal <- function(x = numeric(),
                        precision = 0L,
                        scale = 2L,
                        strict = FALSE){
  assert_that(is.numeric(x), msg = "`x` must be numeric")
  assert_that(is.logical(strict), msg = "`strict` must be logical")
  dgts_dec <- vec_cast(scale, integer())
  out <- round(x, digits = dgts_dec)
  dgts_int <- vec_cast(max_int_digits(out), integer())
  dgts_tot <- vec_cast(dgts_dec + dgts_int, integer())
  if ( strict ) {
    assertthat::assert_that(
      dgts_tot <= precision,
      msg = paste0("`precision` must be ", dgts_tot, " or greater")
    )
  } else {
    dgts_tot <- max(precision, dgts_tot)
  }
  dgts <- list(precision = dgts_tot,
               scale = dgts_dec)
  vctrs::new_vctr(.data = out, dgts = dgts,
                  strict = strict, class = "decimal")
}

# for compatibility with the S4 system
methods::setOldClass(c("decimal", "vctrs_vctr"))

#' `decimal` vector
#'
#' This creates a double vector that represents percentages so when it is
#' printed, it is multiplied by 100 and suffixed with `%`.
#'
#' @param x A numeric vector
#' @return An S3 vector of class `decimal`.
#' @export
decimal <- function(x = numeric(),
                    precision = 0L,
                    scale = 2L,
                    strict = FALSE){
  x <- vec_cast(x, numeric())
  new_decimal(x = x, precision = precision, scale = scale, strict = strict)
}

#' @export
is_decimal <- function(x) {
  inherits(x, "decimal")
}

#' @export
format.decimal <- function(x, ...){
  precision <- base::attr(x, "dgts")[["precision"]]
  # account for possible `-` sign and the decimal separator
  width <- precision + 2
  scale <- base::attr(x, "dgts")[["scale"]]
  sprintf(
    paste0("%", width, ".", scale, "f"),
    vec_data(x)
  )
}

#' @export
dgts <- function(x) {
  base::attr(x, "dgts")
}
#' @export
dgts_precision <- function(x){
  dgts(x)[["precision"]]
}
#' @export
dgts_scale <- function(x){
  dgts(x)[["scale"]]
}
#' @export
dgts_integer <- function(x){
  dgts(x)[["precision"]] - dgts(x)[["scale"]]
}
#' @export
dgts_strict <- function(x) {
  base::attr(x, "strict")
}

#' @export
vec_ptype_full.decimal <- function(x, ...){
  if( length(x) == 0 ) {
    "decimal"
  } else {
    base::paste0("decimal(", dgts_precision(x), ", ", dgts_scale(x), ")")
  }
}
#' @export
vec_ptype_abbr.decimal <- function(x, ...){
  if( length(x) == 0 ) {
    "dec"
  } else {
    base::paste0("dec(", dgts_precision(x), ",", dgts_scale(x), ")")
  }
}

# casting -------------------
#' @export
vec_ptype2.decimal.decimal <- function(x, y, ...){
  if( any( c(dgts_strict(x), dgts_strict(y))) ){
    dgts_strict <- TRUE
  } else {
    dgts_strict <- FALSE
  }
  new_decimal(precision = max(dgts_precision(x), dgts_precision(y)),
              scale = max(dgts_scale(x), dgts_scale(y)),
              strict = dgts_strict)
}
#' @export
vec_cast.decimal.decimal <- function(x, to, ...){
  vec_to <- vec_data(x)
  new_decimal(
    x = vec_data(x),
    precision = dgts_precision(to), scale = dgts_scale(to),
    strict = dgts_strict(to))
}

## double -----
### coecion ----
#' @export
vec_ptype2.decimal.double <- function(x, y, ...) { x }
vec_ptype2.double.decimal <- function(x, y, ...) { y }

### casting ----
#' @export
vec_cast.decimal.double  <- function(x, to, ...) {
  new_decimal(
    x,
    precision = dgts_precision(to), scale = dgts_scale(to),
    strict = dgts_strict(to)
  )
}
#' @export
vec_cast.double.decimal  <- function(x, to, ...) {
  vctrs::vec_data(x)
}

## integer ----
### coercion ----

#' @export
vec_ptype2.decimal.integer <- function(x, y, ...) { x }
#' @export
vec_ptype2.integer.decimal <- function(x, y, ...) { y }

### casting ----
#' @export
vec_cast.decimal.integer  <- function(x, to, ...) {
  new_decimal(
    x,
    precision = dgts_precision(to), scale = dgts_scale(to),
    strict = dgts_strict(to)
  )
}
#' @export
vec_cast.integer.decimal  <- function(x, to, ...) {
  vctrs::vec_cast(round(vctrs::vec_data(x)), integer())
}

## integer64 ----
### coercion ----
#' @export
vec_ptype2.decimal.integer64 <- function(x, y, ...) { x }
#' @export
vec_ptype2.integer64.decimal <- function(x, y, ...) { y }

### casting ----
#' @export
vec_cast.decimal.integer64  <- function(x, to, ...) {
  new_decimal(
    x,
    precision = dgts_precision(to), scale = dgts_scale(to),
    strict = dgts_strict(to)
  )
}
#' @export
vec_cast.integer64.decimal  <- function(x, to, ...) {
  vctrs::vec_cast(round(vctrs::vec_data(x)), bit64::integer64())
}

## character ----
### coercion ----
#' @export
vec_ptype2.decimal.character <- function(x, y, ...) { y }
#' @export
vec_ptype2.character.decimal <- function(x, y, ...) { x }

### casting ----
#' @export
vec_cast.decimal.character  <- function(x, to, ...) {
  assertthat::assert_that(is_numchar(x),
                          msg = "`x` must be a character vector of digits")
  x <- as.double(x)
  new_decimal(
    as.double(x),
    precision = dgts_precision(to), scale = dgts_scale(to),
    strict = dgts_strict(to)
  )
}
#' @export
vec_cast.character.decimal <- function(x, to, ...) {
  as.character(vctrs::vec_data(x))
}


## arithmentic ----
### default ----
#' @export
#' @method vec_arith decimal
vec_arith.decimal <- function(op, x, y, ...) {
  UseMethod("vec_arith.decimal", y)
}

#' @export
#' @method vec_arith.decimal default
vec_arith.decimal.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith character
vec_arith.character <- function(op, x, y, ...) {
  UseMethod("vec_arith.character", y)
}
#' @export
#' @method vec_arith.character default
vec_arith.character.default <- function(op, x, y, ...) {
  assertthat::assert_that(is_numchar(x),
                          msg = "`x` must be a character vector with digits only")
  x <- as.numeric(x)
  vctrs::stop_incompatible_op(op, x, y)
}


### self ----
#' @export
#' @method vec_arith.decimal decimal
vec_arith.decimal.decimal <- function(op, x, y, ...) {
  precision <- max(dgts_precision(x), dgts_precision(y))
  scale <- max(dgts_scale(x), dgts_scale(y))
  strict <- dgts_strict(x) || dgts_strict(y)
  switch(
    op,
    "+" = new_decimal(vctrs::vec_arith_base(op, x, y),
                      precision = precision, scale = scale,
                      strict = strict),
    "-" = new_decimal(vctrs::vec_arith_base(op, x, y),
                      precision = precision, scale = scale,
                      strict = strict),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

### numeric ----
#' @export
#' @method vec_arith.decimal numeric
vec_arith.decimal.numeric <- function(op, x, y, ...) {
  precision <- dgts_precision(x)
  scale <- dgts_scale(x)
  strict <- dgts_strict(x)
  new_decimal(vctrs::vec_arith_base(op, x, y),
              precision = precision,
              scale = scale,
              strict = strict)
}
#' @export
#' @method vec_arith.numeric decimal
vec_arith.numeric.decimal <- function(op, x, y, ...) {
  precision <- dgts_precision(y)
  scale <- dgts_scale(y)
  strict <- dgts_strict(y)
  new_decimal(vctrs::vec_arith_base(op, x, y),
              precision = precision,
              scale = scale,
              strict = strict)
}
#' @export
#' @method vec_arith.decimal character
vec_arith.decimal.character <- function(op, x, y, ...) {
  assertthat::assert_that(is_numchar(y),
                          msg = "`y` must be a character vector of digits")
  y <- as.numeric(y)
  precision <- dgts_precision(x)
  scale <- dgts_scale(x)
  strict <- dgts_strict(x)
  new_decimal(vctrs::vec_arith_base(op, x, y),
              precision = precision,
              scale = scale,
              strict = strict)
}

#' @export
#' @method vec_arith.character decimal
vec_arith.character.decimal <- function(op, x, y, ...) {
  assertthat::assert_that(is_numchar(x),
                          msg = "`x` must be a character vector of digits")
  x <- as.numeric(x)
  precision <- dgts_precision(y)
  scale <- dgts_scale(y)
  strict <- dgts_strict(y)
  new_decimal(vctrs::vec_arith_base(op, x, y),
              precision = precision,
              scale = scale,
              strict = strict)
}
