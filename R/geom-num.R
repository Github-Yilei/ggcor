#' Format number Geom
#'
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used, the default value is 2.
#' @param nsmall the minimum number of digits to the right of the decimal
#'     point in formatting real/complex numbers in non-scientific formats,
#'     the default value is 2.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @section Aesthetics:
#'     \code{geom_number()} understands the following aesthetics (required
#'     aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{num}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{size}
#'       \item \code{angle}
#'       \item \code{hjust}
#'       \item \code{vjust}
#'       \item \code{family}
#'       \item \code{fontface}
#'       \item \code{lineheight}
#'    }
#' @importFrom ggplot2 layer ggproto position_nudge GeomText aes draw_key_text
#' @rdname geom_number
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_number <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        nudge_x = 0,
                        nudge_y = 0,
                        digits = 2,
                        nsmall = 2,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomNumber,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      digits = digits,
      nsmall = nsmall,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_number
#' @export
geom_number2 <- function(...) {
  structure(.Data = list(...), class = "geom_number2")
}

#' @rdname geom_number
#' @format NULL
#' @usage NULL
#' @export
GeomNumber <- ggproto("GeomNumber", GeomText,
                   required_aes = c("x", "y", "num"),
                   default_aes = aes(colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                     vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2),
                   draw_panel = function(data, panel_params, coord, digits = 2,
                                         nsmall = 2, na.rm = FALSE) {
                     data$label <- format_number(data$num, digits, nsmall)
                     GeomText$draw_panel(data, panel_params, coord)
                    },
                   draw_key = draw_key_text
)



