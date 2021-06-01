#' Colour pal
#' @description Wrapper for the diverging palettes provided by \code{\link{RColorBrewer}}.
#' @param n number of different colors in the palette, minimum 3, maximum 11.
#' @return a palette.
#' @rdname colour-pal
#' @export
brown_blue <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "BrBG")
}
#' @rdname colour-pal
#' @export
pink_green <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "PiYG")
}
#' @rdname colour-pal
#' @export
purple_green <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "PRGn")
}
#' @rdname colour-pal
#' @export
brown_purple <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "PuOr")
}
#' @rdname colour-pal
#' @export
red_blue <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "RdBu")
}
#' @rdname colour-pal
#' @export
red_grey <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "RdGy")
}
#' @rdname colour-pal
#' @export
red_yellow_blue <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "RdYlBu")
}
#' @rdname colour-pal
#' @export
red_yellow_green <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "RdYlGn")
}
#' @rdname colour-pal
#' @export
spectral <- function(n = 11) {
  RColorBrewer::brewer.pal(n, "Spectral")
}
#' @rdname colour-pal
#' @export
link_colour_pal <- function(n)
{
  stopifnot(n <= 6)
  colors <- c("#D95F02", "#1B9E77", "#7570B3",
              "#E7298A", "#A6761D", "#CCCCCC99")
  if(n == 1)
    return(colors[1])
  col <- c(colors[1:(n - 1)], colors[6])
  col
}
