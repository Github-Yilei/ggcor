### modified from ComplexHeatmap packages
### see https://github.com/jokergoo/ComplexHeatmap/blob/master/R/grid.dendrogram.R for details.
#' @importFrom stats as.dendrogram as.hclust is.leaf nobs order.dendrogram
#' @noRd
dend_tbl <- function(dend,
                     bcols = NULL,
                     direction = "top",
                     hrange = c(1, 3),
                     circular = FALSE) {
  direction <- match.arg(direction, c("top", "bottom", "left", "right"))
  if(!inherits(dend, "dendrogram")) {
    dend <- stats::as.dendrogram(dend)
  }
  if(is.null(attr(dend, "x"))) {
    dend <- adjust_dend(dend)
  }
  if(!is.null(bcols)) {
    branches_color <- get_function("dendextend", "branches_color")
    bcols <- unique(bcols)
    dend <- branches_color(dend, k = length(bcols), col = bcols)
  }
  env <- as.environment(list(x = NULL, y = NULL, xend = NULL, yend = NULL,
                             col = NULL, lty = NULL, lwd = NULL))
  generate_children_dendrogram_segments <- function(dend, env = NULL) {
    if(is.leaf(dend)) {
      return(NULL)
    }
    height <- attr(dend, "height")
    nc <- length(dend)
    xl <- vapply(seq_len(nc), function(i) attr(dend[[i]], "x"), numeric(1))
    yl <- vapply(seq_len(nc), function(i) attr(dend[[i]], "height"), numeric(1))
    max_x <- max(xl)
    min_x <- min(xl)
    mid_x <- (max_x + min_x) * 0.5
    # graphic parameters for current branch
    edge_gp_list <- lapply(seq_len(nc), function(i) as.list(attr(dend[[i]], "edgePar")))
    for(i in c(setdiff(seq_len(nc), c(1, nc)), c(1, nc))) {
      for(gp_name in c("col", "lwd", "lty")) {
        # gp for two segments
        if(is.null(edge_gp_list[[i]][[gp_name]])) {
          gpa <- rep(default_gpar(gp_name), 2)
        } else {
          gpa <- rep(edge_gp_list[[i]][[gp_name]], 2)
        }
        env[[gp_name]] <- c(env[[gp_name]], gpa)
      }
      env$x <- c(env$x, xl[i], xl[i])
      env$xend <- c(env$xend, xl[i], mid_x)
      env$y <- c(env$y, yl[i], height)
      env$yend <- c(env$yend, height, height)
      generate_children_dendrogram_segments(dend[[i]], env)
    }
  }
  generate_children_dendrogram_segments(dend, env)
  col <- lwd <- lty <- NULL
  data <- new_data_frame(as.list(env)) %>%
    dplyr::rename(colour = col, size = lwd, linetype = lty)


  xmax <- max(data$x, data$xend, na.rm = TRUE)
  hmax <- max(data$y, data$yend, na.rm = TRUE)
  hrange <- switch(direction,
                   top = sort(hrange),
                   bottom = sort(hrange, TRUE),
                   left = sort(hrange, TRUE),
                   right = sort(hrange))
  data$y <- scales::rescale(data$y, hrange, c(0, hmax))
  data$yend <- scales::rescale(data$yend, hrange, c(0, hmax))

  if(direction %in% c("right", "left")) {
    temp.x <- xmax - data$x + 1
    temp.xend <- xmax - data$xend + 1
    data$x <- data$y
    data$xend <- data$yend
    data$y <- temp.x
    data$yend <- temp.xend
  }
  data
}

#' @noRd
subset_dendrogram <- function(x, ind) {
  if(is.null(ind)) x else x[[ind]]
}

#' @noRd
default_gpar <- function(name) {
  switch (name,
          col = "black",
          lwd = 0.5,
          lty = "solid"
  )
}

#' @noRd
adjust_dend <- function(dend) {
  n <- nobs(dend)
  leaves_pos <- 1:n
  dend_order <- order.dendrogram(dend)
  od2index <- NULL
  od2index[dend_order] <- 1:n
  env <- as.environment(list(dend = dend))
  adj_dend <- function(ind = NULL) {
    d <- subset_dendrogram(env$dend, ind)
    n_node <- length(d)
    if(is.leaf(d)) {
      i <- od2index[ d[][[1]] ]
      x <- leaves_pos[i]
    } else {
      nc <- length(d)
      for(i in seq_len(nc)) {
        adj_dend(c(ind, i))
      }
      d <- subset_dendrogram(env$dend, ind)

      xl <- vapply(1:nc, function(i) attr(d[[i]], "x"), numeric(1))
      x <- (max(xl) + min(xl))*0.5
    }
    if(is.null(ind)) {
      attr(env$dend, "x") <- x
    } else {
      attr(env$dend[[ind]], "x") <- x
    }
    x
  }
  adj_dend()
  dend <- env$dend
  return(dend)
}


#' @noRd
build_dendro <- function(dend,
                         circular,
                         bcols,
                         direction,
                         fixed.xy,
                         hrange,
                         colour = NULL,
                         size = NULL,
                         linetype = NULL) {
  n <- stats::nobs(as.dendrogram(dend))
  data <- dend_tbl(dend, bcols, direction, hrange)
  hmax <- max(data$y, data$yend, na.rm = TRUE)
  colour <- suppressWarnings(data$colour %||% colour %||% "black")
  size <- suppressWarnings(data$size %||% size %||% 0.5)
  linetype <- suppressWarnings(data$linetype %||% linetype %||% "solid")
  mapping <- aes_string(x = "x", y = "y", xend = "xend", yend = "yend")

  if(isTRUE(circular)) {
    return(
      geom_segment(mapping = mapping,
                   colour = colour,
                   size = size,
                   linetype = linetype,
                   data = data, inherit.aes = FALSE)
    )
  }
  p <- ggplot(data, mapping = mapping) +
    geom_segment(colour = colour,
                 size = size,
                 linetype = linetype) +
    theme_anno2()
  if(direction %in% c("top", "bottom")) {
    p <- p + scale_x_continuous(limits = c(0.5, n + 0.5), expand = c(0, 0))
    if(direction == "top") {
      p <- p + scale_y_continuous(expand = c(0, 0.05))
    } else {
      p <- p + scale_y_continuous(expand = c(0.05, 0))
    }
  } else {
    p <- p + scale_y_continuous(limits = c(0.5, n + 0.5), expand = c(0, 0))
    if(direction == "left") {
      p <- p + scale_x_continuous(expand = c(0.05, 0))
    } else {
      p <- p + scale_x_continuous(expand = c(0, 0.05))
    }
  }
  if(isTRUE(fixed.xy)) {
    p <- p + coord_fixed()
  }
  p
}
