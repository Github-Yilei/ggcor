#' Helper function to extract cor_tbl.
#' @description These functions are used to quickly obtain the upper
#'     trig, lower trig, diagonal, or remove the diagonal of the correlation
#'     coefficient matrix.
#' @param x a cor_tbl object.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @return a modified cor_tbl object.
#' @importFrom dplyr filter
#' @rdname extract_cor_tbl
#' @examples
#' df <- fortify_cor(mtcars)
#' quickcor(df) + geom_colour()
#'
#' ## exclude upper
#' df %>% get_lower_data() %>%
#'   quickcor() + geom_colour()
#'
#' ## exclude lower
#' df %>% get_upper_data(show.diag = FALSE) %>%
#'   quickcor() + geom_colour()
#'
#' ## get the diagonal
#' df %>% get_diag_data() %>%
#'   quickcor() + geom_colour()
#'
#' ## exclude the diagonal
#' df %>% get_diag_tri() %>%
#'   quickcor() + geom_colour()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
get_lower_data <- function(x, show.diag = TRUE)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  if(isTRUE(show.diag)) {
    x <- dplyr::filter(x, .row.id + .col.id <= n + 1)
  } else {
    x <- dplyr::filter(x, .row.id + .col.id < n + 1)
  }
  attr(x, "type") <- "lower"
  attr(x, "show.diag") <- show.diag
  x
}

#' @importFrom dplyr filter
#' @rdname extract_cor_tbl
#' @export
get_upper_data <- function(x, show.diag = TRUE)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  if(isTRUE(show.diag)) {
    x <- dplyr::filter(x, .row.id + .col.id >= n + 1)
  } else {
    x <- dplyr::filter(x, .row.id + .col.id > n + 1)
  }
  attr(x, "type") <- "upper"
  attr(x, "show.diag") <- show.diag
  x
}

#' @importFrom dplyr filter
#' @rdname extract_cor_tbl
#' @export
get_diag_tri <- function(x)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  x <- dplyr::filter(x, .row.id + .col.id != n + 1)
  if(get_type(x) %in% c("upper", "lower"))
    attr(x, "show.diag") <- FALSE
  x
}

#' @importFrom dplyr filter
#' @rdname extract_cor_tbl
#' @export
get_diag_data <- function(x)
{
  stopifnot(is_cor_tbl(x))
  if(!is_symmet(x)) {
    warning("Just supports symmetric matrix.", call. = FALSE)
    return(x)
  }
  n <- length(get_col_name(x))
  dplyr::filter(x, .row.id + .col.id == n + 1)
}

#' @rdname extract_cor_tbl
#' @export
is_symmet <- function(x) {
  stopifnot(is_cor_tbl(x))
  col.name <- get_col_name(x)
  row.name <- get_row_name(x)
  if((length(col.name) != length(row.name)) || !all(col.name == row.name)) {
    return(FALSE)
  }
  TRUE
}

#' Create cor_tbl extractor function
#' @description This function returns another function that can extract cor_tbl
#' subset from a cor_tbl object.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#' lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param ... extra filter params, see Details.
#' @details This function is mainly used in \code{ggplot2} geom_*() functions,
#' where data is filtered based on the \code{...} parameter, then subsets
#' are extracted based on the type and show.diag parameters.
#' @return extractor function
#' @importFrom dplyr filter
#' @rdname get_data
#' @examples
#' ## arrange different elements in upper and lower
#' quickcor(mtcars) +
#'   geom_colour(data = get_data(type = "lower")) +
#'   geom_ellipse2(data = get_data(type = "upper")) +
#'   add_diag_label() +
#'   remove_axis()
#'
#' quickcor(mtcars, cor.test = TRUE) +
#'   geom_ellipse2(data = get_data(type = "upper")) +
#'   geom_mark(data = get_data(type = "lower")) +
#'   add_diag_label() +
#'   remove_axis()
#' @seealso \code{\link[dplyr]{filter}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
get_data <- function(..., type = "full", show.diag = TRUE)
{
  type <- match.arg(type, c("full", "upper", "lower", "diag"))
  function(data) {
    data <- dplyr::filter(data, ...)
    switch (type,
            full  = if(isTRUE(show.diag)) data else get_diag_tri(data),
            upper = get_upper_data(data, show.diag = show.diag),
            lower = get_lower_data(data, show.diag = show.diag),
            diag  = get_diag_data(data)
    )
  }
}

