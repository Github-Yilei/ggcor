#' Cross Geom
#'
#' @param sig.level significance threshold.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @section Aesthetics:
#'     \code{geom_cross()} understands the following aesthetics (required
#'     aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{p.value}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto GeomSegment aes draw_key_blank
#' @importFrom grid grobTree
#' @rdname geom_cross
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_cross <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       sig.level = 0.05,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCross,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sig.level = sig.level,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_cross
#' @format NULL
#' @usage NULL
#' @export
GeomCross <- ggproto(
  "GeomCross", GeomSegment,
  default_aes = aes(colour = "red", size = 0.5, linetype = 1, alpha = NA),
  required_aes = c("x", "y", "p.value"),

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre",
                        sig.level = 0.05, r0 = 0.6) {
    aesthetics <- setdiff(names(data), c("x", "y", "p.value"))
    data <- with(data, subset(data, p.value >= sig.level))
    if(nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }
    dd <- point_to_cross(data$x, data$y, r0)
    aes <- data[rep(1:nrow(data), each = 2) , aesthetics, drop = FALSE]
    GeomSegment$draw_panel(cbind(dd, aes), panel_params, coord)
  },
  draw_key = draw_key_blank
)

#' @noRd
point_to_cross <- function(x, y, r = 0.6) {
  xx <- c(x - 0.5 * r, x - 0.5 * r)
  xend <- c(x + 0.5 * r, x + 0.5 * r)
  yy <- c(y - 0.5 * r, y + 0.5 * r)
  yend <- c(y + 0.5 * r, y - 0.5 * r)

  new_data_frame(list(
    x = xx,
    y = yy,
    xend = xend,
    yend = yend
  ))
}
