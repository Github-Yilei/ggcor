#' Segment2 layer
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @section Aesthetics:
#' \code{geom_segment2()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{xend}}
#'       \item \strong{\code{yend}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{linetype}
#'       \item \code{width}
#'    }
#' @importFrom ggplot2 layer ggproto geom_segment GeomSegment aes draw_key_path
#' @rdname geom_segment2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_segment2 <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegment2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_segment2
#' @format NULL
#' @usage NULL
#' @export
GeomSegment2 <- ggproto(
  "GeomSegment2", GeomSegment,
  default_aes = aes(colour = "grey35", size = 0.5, linetype = 1, alpha = NA),
  required_aes = c("x", "y", "xend", "yend"),
  draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    if(empty(data)) {
      return(ggplot2::zeroGrob())
    }
    coords <- coord$transform(data, panel_params)
    arrow.fill <- arrow.fill %||% coords$edge_colour
    x <- y <- xend <- yend <- NULL
    ends <- dplyr::rename(data[setdiff(names(data), c("x", "y"))], x = xend, y = yend)
    ends <- coord$transform(ends, panel_params)
    ends <- dplyr::rename(ends, xend = x, yend = y)
    coords <- cbind(coords[setdiff(names(coords), c("xend", "yend"))], ends[c("xend", "yend")])
    ggname("geom_segment2",
           grid::segmentsGrob(coords$x, coords$y, coords$xend, coords$yend,
                 default.units = "native",
                 gp = grid::gpar(
                   col = scales::alpha(coords$colour, coords$alpha),
                   fill = scales::alpha(arrow.fill, coords$alpha),
                   lwd = coords$size * ggplot2::.pt,
                   lty = coords$linetype,
                   lineend = lineend,
                   linejoin = linejoin
                 ),
                 arrow = arrow
                 )
           )
  },
  draw_key = draw_key_path
)
