#' Add diagnoal labels on correlation plot
#' @description \code{geom_diag_label} is mainly used with \code{ggcor} and
#'     \code{quickcor} functions to add diagnoal labels on correct position
#'     base on different type of cor_tbl object.
#' @param geom one of "text", "label" or "image".
#' @param remove.axis if TRUE, will remove the axis.
#' @param ... extra parameters.
#' @importFrom ggplot2 aes_string geom_label
#' @rdname geom_diag_label
#' @examples
#' quickcor(mtcars, type = "upper") + geom_colour() + geom_diag_label()
#' quickcor(mtcars, type = "lower") + geom_colour() + geom_diag_label()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_diag_label <- function(..., geom = "text", remove.axis = TRUE)
{
  structure(.Data = list(geom = geom, remove.axis = remove.axis, params = list(...)),
            class = "geom_diag_label")
}


