#' Ring Geom
#'
#' @param remain.fill the fill colour of the remaining area.
#' @param start.radius inner circle radius.
#' @param end.radius outer circle radius.
#' @param steps radians per step.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_ring()} understands the following aesthetics (required aesthetics are in bold):
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
#' @rdname geom_ring
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_ring <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      remain.fill = NA,
                      start.radius = 0.25,
                      end.radius = 0.5,
                      steps = 0.1,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRing,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      remain.fill = remain.fill,
      start.radius = start.radius,
      end.radius = end.radius,
      steps = steps,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_ring
#' @format NULL
#' @usage NULL
#' @export
GeomRing <- ggproto(
  "GeomRing", GeomPolygon,
  default_aes = aes(r0 = 1, colour = "grey60", fill = NA, size = 0.25,
                    linetype = 1, alpha = NA),
  required_aes = c("x", "y"),
  draw_panel = function(self, data, panel_params, coord, remain.fill = NA,
                        start.radius = 0.25, end.radius = 0.5, steps = 0.1,
                        linejoin = "mitre") {
    aesthetics <- setdiff(names(data), c("x", "y", "fill", "group", "subgroup"))
    polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
      dd <- point_to_ring(row$x, row$y, row$r0, start.radius, end.radius, steps)
      dd$fill <- c(row$fill, remain.fill)[dd$group]
      aes <- new_data_frame(row[aesthetics])[rep(1, nrow(dd)), ]
      GeomPolygon$draw_panel(cbind(dd, aes), panel_params, coord)
    })
    ggname("geom_ring", do.call("grobTree", polys))
  },
  draw_key = draw_key_ring
)

#' @noRd
point_to_ring <- function(cx,
                          cy,
                          r0,
                          start.radius = 0.25,
                          end.radius = 0.5,
                          steps = 0.1)
{
  start.angle <- c(0.5 * pi,  0.5 * pi + r0 * 2 * pi)
  end.angle <- c(0.5 * pi + r0 * 2 * pi, 0.5 * pi  + sign(r0) * 2 * pi)

  df <- purrr::pmap_dfr(list(start.angle, end.angle, 1:2), function(.start, .end, .n) {
    get_sector_coord(.start, .end, start.radius, end.radius, steps) %>%
      dplyr::mutate(group = .n)
  })

  df$x <- df$x + cx
  df$y <- df$y + cy
  df
}

#' @noRd
get_sector_coord <- function(start.angle,
                             end.angle,
                             start.radius,
                             end.radius,
                             steps = 0.1
) {
  if(start.angle > end.angle) {
    temp <- start.angle
    start.angle <- end.angle
    end.angle <- temp
  }
  if(start.radius > end.radius) {
    temp <- start.radius
    start.radius <- end.radius
    end.radius <- temp
  }
  angles <- seq(start.angle, end.angle, by = steps)
  angles[length(angles)] <- end.angle
  xpos <- c(cos(angles) * start.radius, cos(rev(angles)) * end.radius)
  ypos <- c(sin(angles) * start.radius, sin(rev(angles)) * end.radius)
  tibble::tibble(x = xpos, y = ypos)
}
