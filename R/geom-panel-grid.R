#' Add panel grid line on correlation plot
#' @description \code{geom_grid} is mainly used with \code{ggcor} or \code{quickcor}
#'     function to add a panel grid line on plot region.
#' @param colour,color colour of grid lines.
#' @param size size of grid lines.
#' @importFrom ggplot2 geom_segment aes_string
#' @rdname geom_panel_grid
#' @examples
#' df <- fortify_cor(mtcars)
#' ggcor(df) + geom_panel_grid()
#' require(ggplot2, quietly = TRUE)
#' ggplot(df, aes(x, y)) + geom_panel_grid()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_panel_grid <- function(colour = "grey50",
                            size = 0.25,
                            color = NULL) {
  if(!is.null(color))
    colour <- color
  structure(.Data = list(colour = colour, size = size),
            class = "geom_panel_grid")
}
