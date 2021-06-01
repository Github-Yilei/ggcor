#' @rdname anno_link
#' @format NULL
#' @usage NULL
#' @export
add_link <- function(...) {
  warning("`add_link()` is deprecated. ",
          "Please see `?anno_link()` for more detail.", call. = FALSE)
  NULL
}

#' @rdname anno_link
#' @format NULL
#' @usage NULL
#' @export
anno_link_label <- function(...) {
  warning("`anno_link_label()` is deprecated. ",
          "Please see `?anno_link()` for more detail.", call. = FALSE)
  NULL
}

#' @rdname mantel_test
#' @format NULL
#' @usage NULL
#' @export
fortify_mantel <- function(...) {
  warning("`fortify_mantel()` is deprecated. ",
          "Use `mantel_test()` instead.", call. = FALSE)
  mantel_test(...)
}

#' @rdname procrutes_test
#' @format NULL
#' @usage NULL
#' @export
fortify_procrutes <- function(...) {
  warning("`fortify_procrutes()` is deprecated. ",
          "Use `procrutes_test()` instead.", call. = FALSE)
  procrutes_test(...)
}

#' @rdname geom_panel_grid
#' @format NULL
#' @usage NULL
#' @export
add_grid <- function(...) {
  warning("`add_grid()` is deprecated. ",
          "Use `geom_panel_grid()` instead.", call. = FALSE)
  geom_panel_grid(...)
}

#' @rdname geom_diag_label
#' @format NULL
#' @usage NULL
#' @export
add_diag_label <- function(...) {
  warning("`add_diag_label()` is deprecated. ",
          "Use `geom_diag_label()` instead.", call. = FALSE)
  geom_diag_label(...)
}

#' @rdname anno_tree
#' @format NULL
#' @usage NULL
#' @export
anno_tree <- function(...) {
  warning("`anno_tree()` is deprecated. ",
          "Use `anno_row_tree()` or `anno_col_tree()` instead.", call. = FALSE)
  NULL
}
