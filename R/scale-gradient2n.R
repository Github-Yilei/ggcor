#' Colour scales for correlation plot
#'
#' @description This set of scales defines new fill scales for correlation matrix plot
#' equivalent to the ones already defined by ggplot2.
#'
#' @return A ggproto object inheriting from `Scale`
#' @inheritParams ggplot2::scale_fill_gradient2
#' @param colours,colors vector of colours to use for n-colour gradient.
#' @param limits a numeric vector of length two providing limits of the scale.
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_colour
#' @examples
#' df <- data.frame(x = rep(1:10, 10),
#'                  y = rep(1:10, each = 10),
#'                  z = runif(100, -1, 1))
#' library(ggplot2)
#' ggplot(df, aes(x, y, fill = z)) +
#'        geom_tile() +
#'        scale_fill_gradient2n()
#'
#' ggplot(df, aes(x, y, colour = z)) +
#'        geom_point(size = 4) +
#'        scale_colour_gradient2n()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
scale_colour_gradient2n <- function(...,
                                    colours,
                                    midpoint = 0,
                                    limits  = NULL,
                                    space = "Lab",
                                    values = NULL,
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "colour",
                                    colors = NULL) {
  colours <- if (missing(colours)) colors else colours
  colours <- colours %||% red_blue()
  continuous_scale(aesthetics,
                   "gradient2n",
                   scales::gradient_n_pal(colours, values, space),
                   na.value = na.value,
                   guide = guide,
                   ...,
                   limits = limits,
                   rescaler = mid_rescaler(mid = midpoint))
}

#' @rdname scale_colour
#' @export
scale_color_gradient2n <- scale_colour_gradient2n

#' @rdname scale_colour
#' @export
scale_fill_gradient2n <- function(...,
                                  colours,
                                  midpoint = 0,
                                  limits = NULL,
                                  space = "Lab",
                                  values = NULL,
                                  na.value = "grey50",
                                  guide = "colourbar",
                                  aesthetics = "fill",
                                  colors = NULL) {
  colours <- if (missing(colours)) colors else colours
  colours <- colours %||% red_blue()
  continuous_scale(aesthetics,
                   "gradient2n",
                   scales::gradient_n_pal(colours, values, space),
                   na.value = na.value,
                   guide = guide,
                   ...,
                   limits = limits,
                   rescaler = mid_rescaler(mid = midpoint))
}

#' @noRd
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

