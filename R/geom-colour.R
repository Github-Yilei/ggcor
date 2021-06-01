#' Colour Geom
#' @description This is the encapsulation of \code{geom_tile()}
#' function, and some fine-tuning has been done.
#' @param colour the colour of tile boder.
#' @inheritParams ggplot2::geom_tile
#' @importFrom ggplot2 geom_tile
#' @section Aesthetics:
#' \code{geom_colour()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'       \item \code{width}
#'       \item \code{height}
#'    }
#' @rdname geom_colour
#' @examples
#' quickcor(mtcars) + geom_colour()
#' @seealso \code{\link[ggplot2]{geom_tile}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_colour <- function(mapping = NULL,
                        data = NULL,
                        colour = "grey60",
                        ...)
{
  geom_tile(mapping = mapping,
            data = data,
            colour = colour,
            ...)
}

#' @rdname geom_colour
#' @export
geom_color <- geom_colour
