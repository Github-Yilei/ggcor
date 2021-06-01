#' @importFrom ggplot2 ggplot_add geom_segment
#' @export
ggplot_add.geom_panel_grid <- function(object, plot, object_name) {
  obj <- geom_segment(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
                      data = get_grid_data(plot$data, drop = drop),
                      colour = object$colour, size = object$size,
                      inherit.aes = FALSE)
  ggplot_add(object = obj, plot = plot, object_name = object_name)
}

#' @noRd
get_grid_data <- function(data, drop) {
  if(!is_cor_tbl(data))
    stop("Need a cor_tbl.", call. = FALSE)
  n <- length(get_col_name(data))
  m <- length(get_row_name(data))
  type <- get_type(data)
  show.diag <- get_show_diag(data)

  if(type == "full") {
    xx <- c(0:n + 0.5, rep_len(0.5, m + 1))
    yy <- c(rep_len(0.5, n + 1), 0:m + 0.5)
    xxend <- c(0:n + 0.5, rep_len(n + 0.5, m + 1))
    yyend <- c(rep_len(m + 0.5, n + 1), 0:m + 0.5)
  } else if(type == "upper") {
    if(show.diag) {
      xx <- c(0:n + 0.5, c(n:1 - 0.5, 0.5))
      yy <- c(c(m:1 - 0.5, 0.5), 0:m + 0.5)
      xxend <- c(0:n + 0.5, rep_len(n + 0.5, m + 1))
      yyend <- c(rep_len(m + 0.5, n + 1), 0:m + 0.5)
    } else {
      xx <- c(1:n + 0.5, c(n:2 - 0.5, 1.5))
      yy <- c(c(m:2 - 0.5, 1.5), 1:m + 0.5)
      xxend <- c(1:n + 0.5, rep_len(n + 0.5, m))
      yyend <- c(rep_len(m + 0.5, n), 1:m + 0.5)
    }
  } else {
    if(show.diag) {
      xx <- c(0:n + 0.5, rep_len(0.5, m + 1))
      yy <- c(rep_len(0.5, n + 1), 0:m + 0.5)
      xxend <- c(0:n + 0.5, c(n + 0.5, n:1 + 0.5))
      yyend <- c(c(m + 0.5, m:1 + 0.5), 0:m + 0.5)
    } else {
      xx <- c(1:n - 0.5, rep_len(0.5, m))
      yy <- c(rep_len(0.5, n), 1:m - 0.5)
      xxend <- c(1:n - 0.5, c(n - 0.5, n:2 - 0.5))
      yyend <- c(c(m - 0.5, m:2 - 0.5), 1:m - 0.5)
    }
  }
  new_data_frame(list(x = xx, y = yy, xend = xxend, yend = yyend))
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_diag_label <- function(object, plot, object_name) {
  geom <- match.arg(object$geom, c("text", "label", "image"))
  geom_fun <- switch (object$geom,
                      text = get_function("ggplot2", "geom_text"),
                      label = get_function("ggplot2", "geom_label"),
                      image = get_function("ggimage", "geom_image")
  )

  type <- get_type(plot$data)
  row.names <- get_row_name(plot$data)
  show.diag <- get_show_diag(plot$data)

  n <- length(row.names)
  d <- new_data_frame(list(x = 1:n, y = n:1, label = row.names))
  if(object$geom == "image") {
    if(!"image" %in% names(object$params)) {
      stop("Did you forget to set the 'image' parameter?", call. = FALSE)
    }
    if(!is.null(names(object$params$image))) {
      object$params$image <- object$params$image[row.names]
    }
    mapping <- aes_string(x = "x", y = "y")
  } else {
    mapping <- aes_string(x = "x", y = "y", label = "label")
  }
  oparams <- list(mapping = aes_string(x = "x", y = "y", label = "label"),
                  data = d, inherit.aes = FALSE)
  params <- utils::modifyList(oparams, object$params)
  obj <- do.call(geom_fun, params)
  if(isTRUE(object$remove.axis)) {
    plot <- plot + remove_axis()
  }
  ggplot_add(object = obj, plot = plot)
}

#' @importFrom ggplot2 ggplot_add aes_string
#' @export
ggplot_add.anno_link <- function(object, plot, object_name) {
  if(isTRUE(plot$plot_env$circlar)) {
    stop("`anno_link()` should be used in cartesian coordinates.", call. = FALSE)
  }
  pdata <- plot$data
  type <- get_type(pdata)
  show.diag <- get_show_diag(pdata)
  n <- ncols(pdata)
  m <- nrows(pdata)

  data <- link_tbl(object$data, pdata, object$start.var, object$start.name, object$end.var)
  node.data <- attr(data, "node.pos")
  mapping <- aes_modify(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
                        object$mapping)
  params <- modifyList(list(data = data, mapping = mapping, inherit.aes = FALSE),
                       object$params)

  xmin <- min(data$x, na.rm = TRUE)
  xmax <- max(data$x, na.rm = TRUE)
  if(type == "upper") {
    if(isTRUE(show.diag)) {
      xrange <- c(min(-0.5, xmin - 0.2 * n), n + 0.5)
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(min(-0.5, xmin - 0.2 * n), n - 0.5)
      yrange <- c(-0.5, n - 0.5)
    }

  } else if (type == "lower") {
    if(isTRUE(show.diag)) {
      xrange <- c(0.5, max(xmax + 0.2 * n, n + 1.5))
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(0.5, max(xmax + 0.2 * n, n + 0.5))
      yrange <- c(0.5, n + 0.5)
    }
  }
  nudge_x <- object$nudge_x
  pos <- object$pos
  if(type == "upper") {
    nudge_x <- - nudge_x
  }
  if(type == "full") {
    if(object$pos == "left")
      nudge_x <- - nudge_x
  }
  hjust <- switch (type,
    lower = 0,
    upper = 1,
    full = if(pos == "right") 0 else 1
  )

  obj <- list(
    do.call(geom_curve2, params),
    do.call(geom_text,
            list(mapping = aes_string(x = "x", y = "y", label = "label"),
                 data = node.data, hjust = hjust, size = object$label.size,
                 colour = object$label.colour, family = object$label.family,
                 fontface = object$label.fontface, nudge_x = nudge_x,
                 inherit.aes = FALSE))
    )
  if(type == "full") {
    width <- object$width
    if(is.null(object$expand)) {
      expand <- if(pos == "left") c(0.5, 0, 0.05, 0) else c(0.05, 0, 0.5, 0)
    }
    p <- ggplot() + obj +
      scale_x_continuous(expand = expand) +
      scale_y_continuous(limits = c(0.5, m + 0.5), expand = c(0, 0)) +
      theme_anno2()
    if(isTRUE(plot$plot_env$fixed.xy)) {
      p <- p + coord_fixed()
    }
    plot + anno_row_custom(p, width = width, pos = pos)
  } else {
    plot <- plot + expand_axis(x = xrange, y = yrange)

    # reset scale
    new_scale <- new_scales("colour", "size", "linetype", "alpha")
    obj <- c(new_scale, obj)
    ggplot_add(object = obj, plot = plot, object_name = object_name)
  }
}

#' @importFrom ggplot2 ggplot_add geom_text
#' @importFrom stats order.dendrogram as.dendrogram cutree as.hclust
#' @export
ggplot_add.p_xaxis <- function(object, plot, object_name) {
  stopifnot(is_quickcor(plot))
  if(!isTRUE(plot$plot_env$circular)) {
    stop("Only supports for polar coordinates.", call. = FALSE)
  }
  col.hc <- attr(plot$data, "hclust")$col.hc
  data <- plot$plot_env$polar.args$xaxis_df
  if(isTRUE(object$stretch)) {
    data$x <- seq(1, plot$plot_env$polar.args$xlim[2], length.out = nrow(data))
  }
  bcols <- if(is.list(object$bcols)) object$bcols$col.bcols else object$bcols
  if(!is.null(bcols)) {
    if(!is.null(col.hc)) {
      order <- order.dendrogram(as.dendrogram(col.hc))
      ctree <- cutree(as.hclust(col.hc), length(bcols))[order]
      times <- table(ctree)[unique(ctree)]
      id <- rlang::set_names(rep(seq_along(bcols), times = times), names(ctree))
      object$params$colour <- bcols[id]
    }
  }
  args <- list(mapping = aes_modify(ggplot2::aes_all(names(data)), object$mapping),
               data = data, inherit.aes = FALSE)
  obj <- do.call(geom_text, modifyList(args, object$params))
  ggplot_add(obj, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add geom_text
#' @importFrom stats order.dendrogram as.dendrogram cutree as.hclust
#' @export
ggplot_add.p_yaxis <- function(object, plot, object_name) {
  stopifnot(is_quickcor(plot))
  if(!isTRUE(plot$plot_env$circular)) {
    stop("Only supports for polar coordinates.", call. = FALSE)
  }
  row.hc <- attr(plot$data, "hclust")$row.hc
  data <- plot$plot_env$polar.args$yaxis_df
  bcols <- if(is.list(object$bcols)) object$bcols$row.bcols else object$bcols
  if(!is.null(bcols)) {
    if(!is.null(row.hc)) {
      bcols <- rev(bcols)
      order <- order.dendrogram(as.dendrogram(row.hc))
      ctree <- rev(cutree(as.hclust(row.hc), length(bcols))[order])
      times <- table(ctree)[unique(ctree)]
      id <- rlang::set_names(rep(seq_along(bcols), times = times), names(ctree))
      object$params$colour <- bcols[id]
    }
  }
  args <- list(mapping = aes_modify(ggplot2::aes_all(names(data)), object$mapping),
               data = data, inherit.aes = FALSE)
  obj <- do.call(geom_text, modifyList(args, object$params))
  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_mark2 <- function(object, plot, object_name) {
  stopifnot(is_quickcor(plot))
  args <- plot$plot_env$polar.args
  if(!isTRUE(plot$plot_env$circular)) {
    warning("`geom_mark2()` only supports for polar coordinates, ",
            "use `geom_mark()` instead.", call. = FALSE)
    obj <- do.call(geom_mark, object)
  } else {
    angle <- args$angle[plot$data$.row.id] + 90 - 180 / diff(args$ylim)
    obj <- do.call(geom_mark, modifyList(object, list(angle = angle)))
  }

  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.geom_number2 <- function(object, plot, object_name) {
  stopifnot(is_quickcor(plot))
  args <- plot$plot_env$polar.args
  if(!isTRUE(plot$plot_env$circular)) {
    warning("`geom_number2()` only supports for polar coordinates, ",
            "use `geom_number()` instead.", call. = FALSE)
    obj <- do.call(geom_number, object)
  } else {
    angle <- args$angle[plot$data$.row.id] + 90 - 180 / diff(args$ylim)
    obj <- do.call(geom_number, modifyList(object, list(angle = angle)))
  }

  ggplot_add(obj, plot)
}

#' @importFrom ggplot2 ggplot_add coord_flip
#' @export
ggplot_add.anno_row_tree <- function(object, plot, object_name) {
  hc <- attr(plot$data, "hclust")$row.hc
  check_tree_params("row", hc)

  pdata <- plot$data
  n <- ncols(pdata)
  type <- get_type(pdata)
  circular <- plot$plot_env$circular
  bcols <- object$bcols
  bcols <- if(is.list(bcols)) bcols$row.bcols else bcols
  width <- object$width %||% 0.3
  pos <- object$pos

  if(inherits(hc, "ggtree")) {
    if(isTRUE(circular)) {
      stop("Now, circular plot don't support for 'ggtree' object.", call. = FALSE)
    }
    pos <- pos %||% "right"
    obj <- hc +
           scale_y_continuous(limits = yrange(plot), expand = c(0, 0)) +
           theme(plot.margin = ggplot2::margin())
    if(!plot$coordinates$is_free()) {
      mrsd <- hc$plot_env$mrsd
      if(!is.null(mrsd) && class(hc$data$x) == "Date") {
        xtrans <- if(pos == "left") {
          liner_trans(xrange(hc), c(0, width * n))
        } else {
          liner_trans(xrange(hc), c(width * n, 0))
        }
        obj <-  obj +
          scale_x_continuous(trans = xtrans) +
          coord_fixed()
      }
    } else {
      if(pos == "right") {
        obj <- obj + scale_x_reverse()
      }
    }
  } else {
    if(isTRUE(circular)) {
      args <- plot$plot_env$polar.args
      pos <- "left"
      hrange <- c(args$xlim[1], 0.5)
    } else {
      if(is.null(pos)) {
        pos <- switch (type, lower = "left", "right")
      }
      hrange <- c(0.5, width * n + 0.5)
    }

    obj <- build_dendro(hc, circular, bcols, pos, plot$plot_env$fixed.xy, hrange)
  }

  if(isTRUE(circular)) {
    ggplot_add(obj, plot)
  } else {
    .anno_row(plot, obj, pos = pos, width = width)
  }
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.anno_col_tree <- function(object, plot, object_name) {
  hc <- attr(plot$data, "hclust")$col.hc
  check_tree_params("col", hc)

  pdata <- plot$data
  n <- nrows(pdata)
  type <- get_type(pdata)
  circular <- plot$plot_env$circular
  bcols <- object$bcols
  bcols <- if(is.list(bcols)) bcols$col.bcols else bcols
  height <- object$height %||% 0.3
  pos <- object$pos
  if(inherits(hc, "ggtree")) {
    if(isTRUE(circular)) {
      stop("Now, circular plot don't support for 'ggtree' object.", call. = FALSE)
    }
    pos <- pos %||% "top"
    obj <- hc +
           scale_y_reverse(expand = c(0, 0)) +
           ggplot2::expand_limits(y = xrange(plot)) +
           coord_flip() +
           theme(plot.margin = ggplot2::margin())

    if(!plot$coordinates$is_free()) {
      mrsd <- hc$plot_env$mrsd
      if(!is.null(mrsd) && class(hc$data$x) == "Date") {
        xtrans <- if(pos == "top") {
          liner_trans(xrange(hc), c(n * height, 0))
        } else {
          liner_trans(xrange(hc), c(0, n * height))
        }
        obj <- obj + scale_x_continuous(trans = xtrans) + theme(aspect.ratio = 1)
      }
    } else {
      if(pos == "top") {
        obj <- obj + scale_x_reverse()
      }
    }
  } else {
    if(isTRUE(circular)) {
      args <- plot$plot_env$polar.args
      pos <- "top"
      col.shift <- args$col.shift %||% 0
      shift <- height * (args$ylim[2] - n)
      hrange <- c(0, shift) + n + 0.5 + col.shift
      plot$plot_env$polar.args$col.shift <- col.shift + shift
    } else {
      if(is.null(pos)) {
        pos <- switch (type, lower = "bottom", "top")
      }
      hrange <- if(pos == "bottom") c(- height * n + 0.5, 0.5) else c(0.5, height * n + 0.5) + n
    }

    obj <- build_dendro(hc, circular, bcols, pos, plot$plot_env$fixed.xy, hrange, circular)
  }

  if(isTRUE(circular)) {
    ggplot_add(obj, plot)
  } else {
    .anno_col(plot, obj, pos = pos, height = height)
  }
}

#' @noRd
check_tree_params <- function(index, hc) {
  if(index == "row" && is.null(hc)) {
    stop("Did you forget to cluster rows of the matrix?", call. = FALSE)
  }
  if(index == "col" && is.null(hc)) {
    stop("Did you forget to cluster columns of the matrix?", call. = FALSE)
  }
}

#' @importFrom ggplot2 ggplot_add geom_rect
#' @importFrom stats cutree
#' @export
ggplot_add.anno_hc_rect <- function(object, plot, object_name) {
  pdata <- plot$data
  if(!is_symmet(pdata)) {
    stop("Just supports for symmetric correlation matrix.", call. = FALSE)
  }
  hc <- attr(pdata, "hclust")$row.hc %||% attr(pdata, "hclust")$col.hc
  if(is.null(hc)) {
    return(plot)
  }
  n <- length(get_col_name(pdata))
  k <- object$k
  tree <- cutree(hc, k = k)
  v <- table(tree)[unique(tree[hc$order])]
  cv <- c(0, cumsum(v))
  data <- data.frame(xmin = cv[-(k + 1)] + 0.5, ymin = n - cv[-(k + 1)] + 0.5,
                     xmax = cv[-1] + 0.5, ymax = n - cv[-1] + 0.5)
  mapping <- aes_string(xmin = "xmin", ymin = "ymin", xmax = "xmax", ymax = "ymax")
  obj <- ggplot2::geom_rect(mapping = mapping, data = data, fill = object$fill,
                            colour = object$colour, size = object$size, inherit.aes = FALSE)
  ggplot_add(obj, plot)
}

#' @export
ggplot_add.anno_hc_bar <- function(object, plot, object_name) {
  pdata <- plot$data
  pos <- object$pos
  k <- object$k
  on.row <- pos %in% c("left", "right")
  hc <- if(on.row) attr(pdata, "hclust")$row.hc else attr(pdata, "hclust")$col.hc
  if(is.null(hc)) {
    return(plot)
  }
  tree <- cutree(hc, k = k)
  cv <- table(tree)[unique(tree[hc$order])]

  mappping <- aes_string(x = "x", y = "y")
  if(on.row) {
    data <- data.frame(x = object$width,
                       y = seq_len(nrows(pdata)))
    fill <- rep(rev(object$fill), times = rev(cv))
    width <- object$width
    height <- 1
  } else {
    data <- data.frame(x = seq_len(ncols(pdata)),
                       y = object$height)
    fill <- rep(rev(object$fill), times = cv)
    width <- 1
    height <- object$height
  }

  if(isTRUE(plot$plot_env$circular)) {
    polar.args <- plot$plot_env$polar.args
    shift <- if(on.row) polar.args$row.shift %||% 0 else polar.args$col.shift %||% 0
    half <- if(on.row) polar.args$row.half %||% 0.5 else polar.args$col.half %||% 0.5
    adj <- if(on.row) - 0.5 * object$width + half else - 0.5 * object$height + half
    if(on.row) {
      data$x <- data$x + ncols(plot$data) + adj + object$space + shift
      if(isTRUE(object$shift)) {
        polar.args$row.shift <- shift + object$width + adj + object$space
        polar.args$row.half <- object$width / 2
        polar.args$yaxis_df$x <- polar.args$yaxis_df$x + object$width + object$space + adj
      }
    } else {
      data$y <- data$y + nrows(plot$data) + adj + object$space + shift
      if(isTRUE(object$shift)) {
        polar.args$col.shift <- shift + object$height + adj + object$space
        polar.args$col.half <- object$height / 2
      }
    }
    plot$plot_env$polar.args <- polar.args
    obj <- geom_tile(mapping = mappping, data = data,
                     fill = fill, colour = fill, size = object$size,
                     width = width, height = height, inherit.aes = FALSE)
    ggplot_add(obj, plot)
  } else {

    obj <- ggplot(data, mappping) +
      geom_tile(fill = fill, colour = fill, size = object$size,
                width = width, height = height)
    if(on.row) {
      obj <- obj +
             scale_x_continuous(limits = c(0.5 * object$width, 1.5 * object$width), expand = c(0, 0)) +
             scale_y_continuous(limits = yrange(plot), expand = c(0, 0)) +
             theme_void()
      if(!plot$coordinates$is_free()) {
        obj <- obj + coord_fixed()
      }
      .anno_row(plot, obj, object$width / ncols(plot$data), pos = pos)
    } else {
      obj <- obj +
        scale_x_continuous(limits = xrange(plot), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0.5 * object$height, 1.5 * object$height), expand = c(0, 0)) +
        theme_void()
      if(!plot$coordinates$is_free()) {
        obj <- obj + coord_fixed()
      }
      .anno_col(plot, obj, object$height / nrows(plot$data), pos = pos)
    }
  }
}

#' @importFrom ggplot2 ggplot geom_bar scale_x_reverse scale_y_reverse
#' @export
ggplot_add.anno_bar <- function(object, plot, object_name) {
  stopifnot(is_quickcor(plot) && !isTRUE(plot$plot_env$circular))
  type <- get_type(plot$data)
  trans <- object$trans
  pos <- object$pos
  flip <- object$flip

  if(is.null(pos)) {
    if(!"y" %in% names(object$mapping)) {
      pos <- switch (type, lower = "bottom", "top")
    }
    if(!"x" %in% names(object$mapping)) {
      pos <- switch (type, lower = "left", "right")
    }
  }
  vertical <- pos %in% c("bottom", "top")
  name <- if(vertical) rlang::as_name(object$mapping$x) else rlang::as_name(object$mapping$y)

  p <- ggplot(object$data, object$mapping) + do.call(geom_bar, object$params)

  if(!plot$coordinates$is_free()) {
    if(is.null(trans)) {
      from <- c(0, max(table(object$data[[name]])))
      to <- if(vertical) {
        c(0, object$height * nrows(plot$data))
      } else {
        c(0, object$width * ncols(plot$data))
      }

      trans <- if(isTRUE(flip)) reverse_liner_trans(from, to) else liner_trans(from, to)
    }
  }

  if(!is.null(trans)) {
    breaks <- labels <- pretty(c(0, max(table(object$data[[name]]))))
  }

  if(vertical) {
    if(isTRUE(object$align)) {
      p <- p + scale_x_discrete(limits = get_col_name(plot$data), expand = c(0, 0))
    } else {
      p <- p + scale_x_discrete(expand = c(0, 0))
    }
    if(!is.null(trans)) {
      p <- p + scale_y_continuous(trans = trans,
                                  breaks = breaks,
                                  labels = labels)
    }

  } else {
    if(isTRUE(object$align)) {
      p <- p + scale_y_discrete(limits = rev(get_row_name(plot$data)), expand = c(0, 0))
    } else {
      p <- p + scale_y_discrete(expand = c(0, 0))
    }
    if(!is.null(trans)) {
      p <- p + scale_x_continuous(trans = trans,
                                  breaks = breaks,
                                  labels = labels)
    }
  }

  if(!plot$coordinates$is_free()) {
    if(isTRUE(object$align)) {
      if(vertical) {
        p <- p + coord_fixed(xlim = xrange(plot))
      } else {
        p <- p + coord_fixed(ylim = yrange(plot))
      }
    } else {
      p <- p + coord_fixed()
    }
  }

  if(!is.null(object$scale)) {
    p <- p + object$scale
  }

  p <- p + theme_anno()
  if(is.character(object$theme)) {
    p <- p + match.fun(object$theme)()
  } else {
    p <- p + object$theme
  }

  if(isTRUE(object$align)) {
    if(vertical) {
      p <- p + remove_x_axis()
    } else {
      p <- p + remove_y_axis()
    }
  }

  if(vertical) {
    .anno_col(plot, p, height = object$height, pos = pos)
  } else {
    .anno_row(plot, p, width = object$width, pos = pos)
  }
}

#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 scale_x_reverse
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_discrete
#' @export
ggplot_add.anno_bar2 <- function(object, plot, object_name) {
  stopifnot(inherits(plot, "quickcor") && !isTRUE(plot$plot_env$circular))
  type <- get_type(plot$data)
  data <- object$data
  trans <- object$trans
  pos <- object$pos
  flip <- object$flip
  xname <- rlang::as_name(object$mapping$x)
  yname <- rlang::as_name(object$mapping$y)
  if((!is_binary(data[[xname]]) && !is_binary(data[[yname]])) ||
     (is_binary(data[[xname]]) && is_binary(data[[yname]]))) {
    stop("`anno_bar2()` should have one binary and one numeric position vars .",
         call. = FALSE)
  }
  if(is.null(pos)) {
    if(is_binary(data[[xname]])) {
      pos <- switch (type, lower = "bottom", "top")
    } else {
      pos <- switch (type, lower = "left", "right")
    }
  }
  vertical <- pos %in% c("bottom", "top")

  p <- ggplot(object$data, object$mapping) + do.call(geom_col, object$params)

  if(!plot$coordinates$is_free()) {
    if(is.null(trans)) {
      all_geq_zero <- if(vertical) all(data[[yname]] >= 0) else all(data[[xname]] >= 0)
      all_leq_zero <- if(vertical) all(data[[yname]] <= 0) else all(data[[xname]] <= 0)
      if(vertical) {
        from <- if(all_geq_zero) {
          c(0, max(data[[yname]], na.rm = TRUE))
        } else if(all_leq_zero) {
          c(min(data[[yname]], na.rm = TRUE), 0)
        } else {
          range(data[[yname]], na.rm = TRUE)
        }
        to <- if(all_geq_zero) {
          c(0, object$height * nrows(plot$data))
        } else if(all_leq_zero) {
          c(- object$height * nrows(plot$data), 0)
        } else {
          c(from[1] / diff(from), from[2] / diff(from)) * object$height * nrows(plot$data)
        }
      } else {
        from <- if(all_geq_zero) {
          c(0, max(data[[xname]], na.rm = TRUE))
        } else if(all_leq_zero) {
          c(min(data[[xname]], na.rm = TRUE), 0)
        } else {
          range(data[[xname]], na.rm = TRUE)
        }
        to <- if(all_geq_zero) {
          c(0, object$width * ncols(plot$data))
        } else if(all_leq_zero) {
          c(- object$width * ncols(plot$data), 0)
        } else {
          c(from[1] / diff(from), from[2] / diff(from)) * object$width * ncols(plot$data)
        }
      }
      trans <- if(isTRUE(flip)) reverse_liner_trans(from, to) else liner_trans(from, to)
    }
  }

  if(!is.null(trans)) {
    if(vertical) {
      breaks <- labels <- pretty(range(c(0, range(data[[yname]], na.rm = TRUE))))
    } else {
      breaks <- labels <- pretty(range(c(0, range(data[[xname]], na.rm = TRUE))))
    }
  }
  if(vertical) {
    if(isTRUE(object$align)) {
      p <- p + scale_x_discrete(limits = get_col_name(plot$data), expand = c(0, 0))
    } else {
      p <- p + scale_x_discrete(expand = c(0, 0))
    }
    if(!is.null(trans)) {
      p <- p + scale_y_continuous(trans = trans,
                                  breaks = breaks,
                                  labels = labels)
    }

  } else {
    if(isTRUE(object$align)) {
      p <- p + scale_y_discrete(limits = rev(get_row_name(plot$data)), expand = c(0, 0))
    } else {
      p <- p + scale_y_discrete(expand = c(0, 0))
    }
    if(!is.null(trans)) {
      p <- p + scale_x_continuous(trans = trans,
                                  breaks = breaks,
                                  labels = labels)
    }
  }

  if(!plot$coordinates$is_free()) {
    if(isTRUE(object$align)) {
      if(vertical) {
        p <- p + coord_fixed(xlim = xrange(plot))
      } else {
        p <- p + coord_fixed(ylim = yrange(plot))
      }
    } else {
      p <- p + coord_fixed()
    }
  }

  if(!is.null(object$scale)) {
    p <- p + object$scale
  }

  p <- p + theme_anno()
  if(is.character(object$theme)) {
    p <- p + match.fun(object$theme)()
  } else {
    p <- p + object$theme
  }

  if(isTRUE(object$align)) {
    if(vertical) {
      p <- p + remove_x_axis()
    } else {
      p <- p + remove_y_axis()
    }
  }

  if(vertical) {
    .anno_col(plot, p, height = object$height, pos = pos)
  } else {
    .anno_row(plot, p, width = object$width, pos = pos)
  }
}

#' @importFrom ggplot2 geom_boxplot
#' @export
ggplot_add.anno_boxplot <- function(object, plot, object_name) {
  stopifnot(inherits(plot, "quickcor") && !isTRUE(plot$plot_env$circular))
  type <- get_type(plot$data)
  data <- object$data
  trans <- object$trans
  pos <- object$pos
  xname <- rlang::as_name(object$mapping$x)
  yname <- rlang::as_name(object$mapping$y)
  if((!is_binary(data[[xname]]) && !is_binary(data[[yname]])) ||
     (is_binary(data[[xname]]) && is_binary(data[[yname]]))) {
    stop("`anno_boxplot()` should have one binary and one numeric position vars .",
         call. = FALSE)
  }
  if(is.null(pos)) {
    if(is_binary(data[[xname]])) {
      pos <- switch (type, lower = "bottom", "top")
    } else {
      pos <- switch (type, lower = "left", "right")
    }
  }
  vertical <- pos %in% c("bottom", "top")

  p <- ggplot(object$data, object$mapping) + do.call(geom_boxplot, object$params)

  if(!plot$coordinates$is_free()) {
    if(is.null(trans)) {
      if(vertical) {
        from <- range(data[[yname]], na.rm = TRUE)
        to <- c(from[1] / diff(from), from[2] / diff(from)) * object$height * nrows(plot$data)
      } else {
        from <- range(data[[xname]], na.rm = TRUE)
        to <- c(from[1] / diff(from), from[2] / diff(from)) * object$width * ncols(plot$data)
      }
      trans <- liner_trans(from, to)
    }
  }

  if(!is.null(trans)) {
    if(vertical) {
      breaks <- labels <- pretty(range(data[[yname]], na.rm = TRUE))
    } else {
      breaks <- labels <- pretty(range(data[[xname]], na.rm = TRUE))
    }
  }

  if(vertical) {
    if(isTRUE(object$align)) {
      p <- p + scale_x_discrete(limits = get_col_name(plot$data), expand = c(0, 0))
    } else {
      p <- p + scale_x_discrete(expand = c(0, 0))
    }
    if(!is.null(trans)) {
      p <- p + scale_y_continuous(trans = trans,
                                  breaks = breaks,
                                  labels = labels)
    }

  } else {
    if(isTRUE(object$align)) {
      p <- p + scale_y_discrete(limits = rev(get_row_name(plot$data)), expand = c(0, 0))
    } else {
      p <- p + scale_y_discrete(expand = c(0, 0))
    }
    if(!is.null(trans)) {
      p <- p + scale_x_continuous(trans = trans,
                                  breaks = breaks,
                                  labels = labels)
    }
  }

  if(!plot$coordinates$is_free()) {
    if(isTRUE(object$align)) {
      if(vertical) {
        p <- p + coord_fixed(xlim = xrange(plot))
      } else {
        p <- p + coord_fixed(ylim = yrange(plot))
      }
    } else {
      p <- p + coord_fixed()
    }
  }

  if(!is.null(object$scale)) {
    p <- p + object$scale
  }

  p <- p + theme_anno()
  if(is.character(object$theme)) {
    p <- p + match.fun(object$theme)()
  } else {
    p <- p + object$theme
  }

  if(isTRUE(object$align)) {
    if(vertical) {
      p <- p + remove_x_axis()
    } else {
      p <- p + remove_y_axis()
    }
  }

  if(vertical) {
    .anno_col(plot, p, height = object$height, pos = pos)
  } else {
    .anno_row(plot, p, width = object$width, pos = pos)
  }
}

#' @export
ggplot_add.anno_row_heat <- function(object, plot, object_name) {
  if(!inherits(plot, "quickcor") && !isTRUE(plot$plot_env$circular)) {
    stop("`anno_row_heat()` just support for circular plot.", call. = FALSE)
  }
  data <- object$data
  if(isTRUE(object$align)) {
    if(!identical(get_row_name(data), get_row_name(plot$data))) {
      data$.row.id <- as.integer(factor(data$.row.names, levels = rev(get_row_name(plot$data))))
    }
  }

  width <- ncols(plot$data) * object$width / ncols(data)

  polar.args <- plot$plot_env$polar.args
  row.shift <- polar.args$row.shift %||% 0
  half <- polar.args$row.half %||% 0.5
  adj <- - 0.5 * width + half
  polar.args$row.half <- 0.5 * width
  data$.col.id <- data$.col.id * width + ncols(plot$data) +
                  object$space + row.shift + adj
  # reset y axis parameters
  polar.args$yaxis_df$x <- polar.args$yaxis_df$x + ncols(data) * width + object$space + adj
  if(isTRUE(object$label)) {
    df <- data.frame(x = seq_len(ncols(data)) * width + ncols(plot$data) + object$space + row.shift + adj,
                     y = unique(polar.args$xaxis_df$y),
                     label = get_col_name(data),
                     angle = 0,
                     hjust = 0, stringsAsFactors = FALSE)
    label <- geom_text(mapping = aes_string(x = "x", y = "y", label = "label",
                                            angle = "angle", hjust = "hjust"), data = df,
                       size = object$label.size, colour = object$label.colour,
                       family = object$label.family, fontface = object$label.fontface,
                       inherit.aes = FALSE)
  }
  polar.args$row.shift <- ncols(data) * width + object$space + row.shift + adj
  plot$plot_env$polar.args <- polar.args

  # calc the layer
  mapping <- aes_modify(aes_string(x = ".col.id", y = ".row.id"), object$mapping)
  params <- modifyList(list(mapping = mapping, data = data, inherit.aes = FALSE),
                       object$params)
  if(object$geom == "point") {
    obj <- do.call(ggplot2::geom_point, params)
    border <- geom_tile(aes_string(x = ".col.id", y = ".row.id"), data = data,
                        fill = NA, colour = "grey50", size = 0.25, inherit.aes = FALSE)
    if(isTRUE(object$label)) {
      obj <- list(border, obj, label)
    }
  } else {
    params$width <- width
    geom <- paste0("geom_", object$geom)
    obj <- do.call(geom, params)
    if(isTRUE(object$label)) {
      obj <- list(obj, label)
    }
  }
  if(!is.list(obj)) {
    obj <- list(obj)
  }

  mark <- object$mark
  if(is.null(mark)) mark <- as.list(NULL)
  if(inherits(mark, "Layer")) mark <- list(mark)
  if(is.list(mark)) {
    is.layer <- vapply(mark, inherits, logical(1), "Layer")
    if(length(mark) > sum(is.layer)) {
      warning("'mark' should be layer object.", call. = FALSE)
    }

    mark <- mark[is.layer]
    if(length(mark) > 0) {
      mark <- lapply(mark, function(.mark) {
        .mark$data <- data
        .mark$mapping <- aes_modify(aes_string(x = ".col.id", y = ".row.id"), .mark$mapping)
        .mark$inherit.aes <- FALSE
        .mark
      })
      obj <- c(obj, mark)
    }
  }

  new_scale <- new_scales("fill", "colour", "alpha", "size")
  obj <- c(new_scale, obj)
  ggplot_add(obj, plot, object_name)
}

#' @export
ggplot_add.anno_col_heat <- function(object, plot, object_name) {
  if(!inherits(plot, "quickcor") && !isTRUE(plot$plot_env$circular)) {
    stop("`anno_col_heat()` just support for circular plot.", call. = FALSE)
  }
  data <- object$data
  if(isTRUE(object$align)) {
    if(!identical(get_col_name(data), get_col_name(plot$data))) {
      data$.col.id <- as.integer(factor(data$.col.names, levels = get_col_name(plot$data)))
    }
  }

  height <- nrows(plot$data) * object$height / nrows(data)
  polar.args <- plot$plot_env$polar.args
  col.shift <- polar.args$col.shift %||% 0
  half <- polar.args$col.half %||% 0.5
  adj <- - 0.5 * height + half
  data$.row.id <- data$.row.id * height + nrows(plot$data) + object$space + col.shift + adj
  # reset y axis parameters
  if(isTRUE(object$label)) {
    t <- (seq_len(nrows(data)) * height + col.shift + nrows(plot$data) + adj) * 360 / diff(polar.args$ylim) + 90
    df <- data.frame(x = 0.5 + 1.05 * ncols(plot$data),
                     y = seq_len(nrows(data)) * height + nrows(plot$data) + object$space + col.shift + adj,
                     label = get_row_name(data),
                     angle = ifelse(t > 90 & t < 270, t + 180, t) -
                             0.5 / diff(polar.args$ylim) * 360,
                     hjust = ifelse(t > 90 & t < 270, 1, 0),
                     stringsAsFactors = FALSE)
    label <- geom_text(mapping = aes_string(x = "x", y = "y", label = "label",
                                            angle = "angle", hjust = "hjust"), data = df,
                       size = object$label.size, colour = object$label.colour,
                       family = object$label.family, fontface = object$label.fontface,
                       inherit.aes = FALSE)
  }
  plot$plot_env$polar.args$col.shift <- nrows(data) * height + object$space + col.shift + adj
  plot$plot_env$polar.args$col.half <- 0.5 * height

  # calc the layer
  mapping <- aes_modify(aes_string(x = ".col.id", y = ".row.id"), object$mapping)
  params <- modifyList(list(mapping = mapping, data = data, inherit.aes = FALSE),
                       object$params)
  if(object$geom == "point") {
    obj <- do.call(ggplot2::geom_point, params)
    border <- geom_tile(aes_string(x = ".col.id", y = ".row.id"), data = data,
                        height = height, fill = NA, colour = "grey50",
                        size = 0.25, inherit.aes = FALSE)
    if(isTRUE(object$label)) {
      obj <- list(border, obj, label)
    }
  } else {
    params$height <- height
    geom <- paste0("geom_", object$geom)
    obj <- do.call(geom, params)
    if(isTRUE(object$label)) {
      obj <- list(obj, label)
    }
  }
  if(!is.list(obj)) {
    obj <- list(obj)
  }

  mark <- object$mark
  if(is.null(mark)) mark <- as.list(NULL)
  if(inherits(mark, "Layer")) mark <- list(mark)
  if(is.list(mark)) {
    is.layer <- vapply(mark, inherits, logical(1), "Layer")
    if(length(mark) > sum(is.layer)) {
      warning("'mark' should be layer object.", call. = FALSE)
    }

    mark <- mark[is.layer]
    if(length(mark) > 0) {
      mark <- lapply(mark, function(.mark) {
        .mark$data <- data
        .mark$mapping <- aes_modify(aes_string(x = ".col.id", y = ".row.id"), .mark$mapping)
        .mark$inherit.aes <- FALSE
        .mark
      })
      obj <- c(obj, mark)
    }
  }

  new_scale <- new_scales("fill", "colour", "alpha", "size")
  obj <- c(new_scale, obj)
  ggplot_add(obj, plot, object_name)
}

#' @importFrom ggplot2 geom_point
#' @export
ggplot_add.anno_network <- function(object, plot, object_name) {
  if(!inherits(plot, "quickcor") || !isTRUE(plot$plot_env$circular)) {
    stop("`anno_network()` just support for circular plot.", call. = FALSE)
  }
  data <- object$data
  if(!inherits(data, "cor_network"))
    data <- as_cor_network(data)

  row.name <- rev(get_row_name(plot$data))
  id <- rlang::set_names(seq_len(nrows(plot$data)), row.name)
  id2 <- get_order(data$nodes$name, row.name)

  nodes <- data.frame(x = 0.5 - object$space,
                      y = id2,
                      name = row.name[id2],
                      stringsAsFactors = FALSE)
  edges <- data.frame(x = 0.5 - object$space,
                      y = id[data$edges$from],
                      xend = 0.5 - object$space,
                      yend = id[data$edges$to])
  nodes <- dplyr::left_join(nodes, data$nodes[setdiff(names(data$nodes), c("x", "y"))],
                            by = c(name = "name"))
  edges <- cbind(edges, data$edges)

  args <- object$params
  name <- names(args)
  edge.name <- c("edge_width", "edge_colour", "edge_linetype", "edge_alpha", "arrow",
                 "arrow.fill", "lineend", "linejion", "na.rm")
  params <- long_to_short(args[name %in% edge.name])
  params2 <- args[!(name %in% edge.name) || (name == "na.rm")]
  mapping <- aes_modify(aes_string(x = "x", y = "y", xend = "xend", yend = "yend"),
                        object$mapping)
  mapping2 <- aes_modify(aes_string(x = "x", y = "y"), object$mapping2)
  params <- modifyList(list(mapping = mapping, data = edges, inherit.aes = FALSE), params)
  params2 <- modifyList(list(mapping = mapping2, data = nodes, inherit.aes = FALSE), params2)
  obj <- list(
    new_scales("size", "colour", "linetype", "alpha"),
    do.call(geom_segment2, params),
    new_scales("size", "colour", "fill", "shape"),
    do.call(geom_point, params2)
  )

  ggplot_add(obj, plot, object_name)
}

#' @noRd
long_to_short <- function(params) {
  nm <- names(params)
  if("edge_color" %in% nm) {
    nm[which(nm == "edge_color")] <- "edge_colour"
  }
  if("edge_width" %in% nm) {
    nm[which(nm == "edge_width")] <- "edge_size"
  }
  id <- nm %in% c("edge_colour", "edge_size", "edge_linetype", "edge_alpha")
  nm[id] <- sub("edge_", "", nm[id], fixed = TRUE)
  names(params) <- nm
  params
}
