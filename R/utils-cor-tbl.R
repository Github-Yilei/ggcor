#' Helper function for cor_tbl
#' @param x a cor_tbl.
#' @return return attribute value.
#' @rdname get_attr
#' @examples
#' df <- fortify_cor(mtcars)
#' ## get rows names
#' get_row_name(df)
#'
#' ## get columns names
#' get_col_name(df)
#'
#' ## get show.diag parameter
#' get_show_diag(df)
#'
#' ## get type parameter
#' get_type(df)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
get_row_name <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, ".row.names")
}

#' @rdname get_attr
#' @export
get_col_name <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, ".col.names")
}

#' @rdname get_attr
#' @export
get_type <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, "type")
}

#' @rdname get_attr
#' @export
get_show_diag <- function(x) {
  stopifnot(is_cor_tbl(x))
  attr(x, "show.diag")
}

#' @rdname get_attr
#' @export
nrows <- function(x) {
  stopifnot(is_cor_tbl(x))
  length(get_row_name(x))
}

#' @rdname get_attr
#' @export
ncols <- function(x) {
  stopifnot(is_cor_tbl(x))
  length(get_col_name(x))
}

#' @rdname get_attr
#' @export
is_cor_tbl <- function(x) {
  inherits(x, "cor_tbl")
}

#' @rdname get_attr
#' @export
is_gcor_tbl <- function(x) {
  inherits(x, "gcor_tbl")
}




