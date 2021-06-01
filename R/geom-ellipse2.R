#' Ellipse Geom
#'
#' @param n the number of ellipse path.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_ellipse2()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{r0}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto GeomPolygon aes
#' @importFrom grid grobTree
#' @rdname geom_ellipse2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_ellipse2 <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          n = 60,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomEllipse2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_ellipse2
#' @format NULL
#' @usage NULL
#' @export
GeomEllipse2 <- ggproto(
  "GeomEllipse2", GeomPolygon,
  default_aes = aes(r0 = 0.5, colour = "grey35", fill = NA, size = 0.25, linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, n = 60, linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y", "group", "subgroup"))
    dd <- point_to_ellipse(data$x, data$y, data$r0, n)
    aes <- data[rep(1:nrow(data), each = n), aesthetics, drop = FALSE]
    GeomPolygon$draw_panel(cbind(dd, aes), panel_params, coord)
  },

  draw_key = draw_key_ellipse
)

#' @noRd
point_to_ellipse <- function(x, y, r0, n = 60) {
  nn <- length(x)
  x <- rep(x, each = n)
  y <- rep(y, each = n)
  r0 <- rep(r0, each = n)
  t <- rep(seq(0, 2 * pi, length = n), nn)
  new_data_frame(list(
    x = 0.5 * cos(t + acos(r0) / 2)  + x,
    y = 0.5 * cos(t - acos(r0) / 2)  + y,
    group = rep(1:nn, each = n)
  ))
}
