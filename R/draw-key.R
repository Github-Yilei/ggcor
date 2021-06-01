#' @noRd
draw_key_square <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  grid::rectGrob(
    x = 0.5,
    y = 0.5,
    width = grid::unit(data$r0, "npc") - grid::unit(data$size, "mm"),
    height = grid::unit(data$r0, "npc") - grid::unit(data$size, "mm"),
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre",
                    lineend = if (identical(params$linejoin, "round")) "round" else "square"))
}

#' @noRd
draw_key_circle <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  grid::circleGrob(
    x = 0.5,
    y = 0.5,
    r = grid::unit(0.5 * data$r0, "npc") - grid::unit(data$size, "mm"),
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt))
}

#' @noRd
draw_key_ellipse <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  data$zoom <- 1 - data$size / 5
  ellipseGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$r0,
    zoom = data$zoom,
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
ellipseGrob <- function(x = 0.5, y = 0.5, r0 = 0.5, zoom = 0.98, gp = grid::gpar()) {
  xy <- point_to_ellipse(0, 0, r0)
  px <- zoom * xy$x + x
  py <- zoom * xy$y + y
  grid::polygonGrob(px, py, gp = gp)
}

#' @noRd
draw_key_ring <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  data$zoom <- 1 - data$size / 5
  fill <- c(scales::alpha(data$fill %||% "grey40", data$alpha),
            params$remain.fill %||% NA)
  ringGrob(
    x = 0.5,
    y = 0.5,
    r0 = data$r0,
    start.radius = params$start.radius %||% 0.25,
    end.radius = params$end.radius %||% 0.5,
    zoom = data$zoom,
    gp = grid::gpar(col = data$colour %||% NA,
                    fill = fill,
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
ringGrob <- function(x = 0.5, y = 0.5, r0 = 0.5, start.radius = 0.25,
                     end.radius = 0.5, zoom = 0.98, gp = grid::gpar()) {
  xy <- point_to_ring(0, 0, r0, start.radius, end.radius, steps = 0.1)
  x <- xy$x * zoom + x
  y <- xy$y * zoom + y
  grid::polygonGrob(x, y, xy$group, gp = gp)
}

#' @noRd
draw_key_star <- function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  data$zoom <- 1 - data$size / 5
  starGrob(
    x = 0.5,
    y = 0.5,
    n = data$n %||% 5,
    r0 = data$r0 %||% 0.5,
    ratio = data$ratio %||% 0.618,
    zoom = data$zoom,
    gp = grid::gpar(col = data$colour %||% "grey25",
                    fill = scales::alpha(data$fill %||% "grey40", data$alpha),
                    lty = data$linetype %||% 1,
                    lwd = data$size * ggplot2::.pt,
                    linejoin = params$linejoin %||% "mitre"))
}

#' @noRd
starGrob <- function(x = 0.5, y = 0.5, n = 5, r0 = 1,
                     ratio = 0.618, zoom = 0.98, gp = grid::gpar()) {
  xy <- point_to_star(0, 0, n, r0, ratio)
  px <- zoom * xy$x + x
  py <- zoom * xy$y + y
  grid::polygonGrob(px, py, gp = gp)
}
