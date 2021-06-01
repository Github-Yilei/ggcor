#' Confident-Box Geom
#'
#' @param width the width of confident box.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#'     \code{geom_confbox()} understands the following aesthetics (required
#'     aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{r}}
#'       \item \strong{\code{lower.ci}}
#'       \item \strong{\code{upper.ci}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{confline.col}
#'       \item \code{midline.col}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer ggproto Geom GeomPolygon GeomSegment aes draw_key_polygon
#' @importFrom grid grobTree
#' @rdname geom_confbox
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_confbox <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        width = 0.5,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomConfbox,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_confbox
#' @format NULL
#' @usage NULL
#' @export
GeomConfbox <- ggproto(
  "GeomConfbox", Geom,
  default_aes = aes(confline.col = "grey30", midline.col = "grey50",
                    colour = NA, fill = "grey60", size = 0.5,
                    midline.type = "dotted", linetype = 1,
                    alpha = NA),
  required_aes = c("x", "y", "r", "lower.ci", "upper.ci"),
  draw_panel = function(self, data, panel_params, coord, width = 0.5) {
    aesthetics <- setdiff(
      names(data), c("x", "y", "r", "lower.ci", "upper.ci")
    )
    grobs <- lapply(split(data, seq_len(nrow(data))), function(row) {
      d <- point_to_confbox(row$x, row$y, row$r, row$lower.ci, row$upper.ci, width)
      confbox <- d$conf_box
      confline <- d$conf_line
      midline <- d$mid_line
      ## draw mid line
      mid_aes <- cbind(midline, new_data_frame(row[aesthetics]))
      mid_aes$colour <- row$midline.col
      mid_aes$linetype <- row$midline.type
      mid <- GeomSegment$draw_panel(mid_aes, panel_params, coord)
      ## draw conf box
      confbox_aes <- cbind(confbox,
                           new_data_frame(row[aesthetics])[rep(1, 5), ])
      confbox_aes$colour <- NA
      box <- GeomPolygon$draw_panel(confbox_aes, panel_params, coord)
      ## draw conf line
      confline_aes <- cbind(confline,
                            new_data_frame(row[aesthetics])[rep(1, 3), ])
      confline_aes$colour <- row$confline.col
      line <- GeomSegment$draw_panel(confline_aes, panel_params, coord)
      grid::gList(mid, box, line)
    })
    ggplot2:::ggname("geom_confbox", do.call("grobTree", grobs))
  },
  draw_key = draw_key_polygon
)

#' @noRd
point_to_confbox <- function(x, y, r, lower.ci, upper.ci, width = 0.5) {
  r <- r / 2
  lower.ci <- lower.ci / 2
  upper.ci <- upper.ci / 2
  ## confidence box
  xmin <- - 0.5 * width  + x
  xmax <-  0.5 * width  + x
  ymin <- lower.ci + y
  ymax <- upper.ci + y
  conf_box <- new_data_frame(list(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  ))
  ## confidence line
  xx <- rep_len(xmin, 3)
  xend <- rep_len(xmax, 3)
  yy <- c(lower.ci, r, upper.ci) + y
  yend <- c(lower.ci, r, upper.ci) + y

  conf_line <- new_data_frame(list(
    x = xx,
    y = yy,
    xend = xend,
    yend = yend
  ))
  ## mid line
  mid_line <- new_data_frame(list(
    x = x - 0.5,
    y = y,
    xend = x + 0.5,
    yend = y
  ))
  ## return confbox, confline, midline list
  list(conf_box = conf_box,
       conf_line = conf_line,
       mid_line = mid_line)
}

