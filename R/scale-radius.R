#' Scale radius
#' @inheritParams ggplot2::scale_size
#' @param midpoint the midpoint (in data value) of the diverging scale. Defaults to 0.
#' @rdname scale_radius
#'
#' @importFrom scales rescale_pal
#' @importFrom ggplot2 continuous_scale
#' @export
scale_radius_area <- function(..., range = c(-1, 1), midpoint = 0, guide = "legend") {
  ggplot2::continuous_scale(c("r0", "upper_r0", "lower_r0"), "radius",
                            palette = scales::rescale_pal(range),
                            rescaler = mid_rescaler(midpoint), guide = guide, ...)
}
