#' @noRd
.anno_circular <- function(data,
                           mapping = NULL,
                           geom = "tile",
                           orientation = NULL,
                           position = "auto",
                           space = 1,
                           width = 0.2,
                           height = 0.2,
                           ...
) {
  structure(.Data = list(data = data,
                         mapping = mapping,
                         geom = geom,
                         orientation = orientation,
                         position = position,
                         space = space,
                         width = width,
                         height = height,
                         params = list(...)),
            class = "anno_circular")
}

#' @noRd
ggplot_add.anno_circular <- function(object, plot, object_name) {
  stopifnot(is_quickcor(plot) && isTRUE(plot$plot_env$circular))
  pdata <- plot$data
  data <- object$data
  geom <- object$geom
  mapping <- object$mapping
  orientation <- object$orientation

  if(is_cor_tbl(data)) {
    mapping <- aes_modify(aes_string(x = ".col.id", y = ".row.id"), mapping)
  }
  x <- rlang::eval_tidy(mapping$x, data)
  y <- rlang::eval_tidy(mapping$y, data)
  xnm <- rlang::as_name(mapping$x)
  ynm <- rlang::as_name(mapping$y)
  aes_name <- names(mapping)
  if(geom %in% c("col", "violin", "boxplot")) {
    if(is_binary(x) || !is_binary(y)) {
      stop("`anno_circular` error: ",
           geom, "annotation only supports for adding on rows.",
           call. = FALSE)
    }
    orientation <- "y"
    data[[ynm]] <- as.integer(factor(y, levels = rev(get_row_name(pdata))))
    if(geom %in% c("violin", "boxplot")) {
      data$group <- as.integer(factor(y, levels = rev(get_row_name(pdata))))
      mapping <- aes_modify(mapping, aes_string(group = "group"))
    }
  }

  if(geom %in% c("point", "tile")) {
    if(is.null(orientation)) {
      if(is_cor_tbl(data)) {
        if(identical(sort(get_row_name(data)), sort(get_row_name(pdata))) &&
           !identical(sort(get_col_name(data)), sort(get_col_name(pdata)))) {
          orientation <- "y"
          data$.row.id <- as.numeric(factor(data$.row.names, levels = rev(get_row_name(pdata))))
        } else if(!identical(sort(get_row_name(data)), sort(get_row_name(pdata))) &&
                  identical(sort(get_col_name(data)), sort(get_col_name(pdata)))){
          orientation <- "x"
          data$.col.id <- as.numeric(factor(data$.col.names, levels = get_col_name(pdata)))
        } else {
          stop("Please set orientation params by yourself.", call. = F)
        }
      } else {
        if(is_binary(y) && all(y %in% get_row_name(pdata))) {
          orientation <- "y"
          data[[ynm]] <- as.integer(factor(y, levels = rev(get_row_name(pdata))))
          if(!is.numeric(x)) {
            data[[xnm]] <- as.numeric(factor(x))
          }
        } else if(is_binary(x) && all(x %in% get_col_name(pdata))) {
          orientation <- "x"
          data[[xnm]] <- as.integer(factor(x, levels = get_col_name(pdata)))
          if(!is.numeric(y)) {
            data[[ynm]] <- as.numeric(factor(y))
          }
        }
      }
    } else {
      if(orientation == "x") {
        if(is_cor_tbl(data)) {
          data$.col.id <- as.numeric(factor(data$.col.names, levels = get_col_name(pdata)))
        } else {
          data[[xnm]] <- as.integer(factor(x, levels = get_col_name(pdata)))
          if(!is.numeric(y)) {
            data[[ynm]] <- as.numeric(factor(y))
          }
        }
      } else {
        if(is_cor_tbl(data)) {
          data$.row.id <- as.numeric(factor(data$.row.names, levels = rev(get_row_name(pdata))))
        } else {
          data[[ynm]] <- as.integer(factor(y, levels = rev(get_row_name(pdata))))
          if(!is.numeric(x)) {
            data[[xnm]] <- as.numeric(factor(x))
          }
        }
      }
    }
  }

  if(orientation == "x") {
    shift <- plot$plot_env$polar.args$yshift %||% nrows(pdata)
    shift <- diff(plot$plot_env$polar.args$ylim) / 360 * object$space + shift

    scale <- (plot$plot_env$polar.args$ylim[2] - nrows(pdata) - 0.5) * object$height
    data[[ynm]] <- scales::rescale(data[[ynm]], c(0.5, scale + 0.5),
                                   from = range(data[[ynm]], na.rm = TRUE))

    position <- if(identical(object$position, "auto")) {
      position_shift_identity(yshift = shift)
    } else {
      object$position
    }
    plot$plot_env$polar.args$yshift <- shift + object$height * nrows(pdata)
  } else {
    shift <- plot$plot_env$polar.args$xshift %||% ncols(pdata) + object$space + 0.5
    scale <- ncols(pdata) * object$width
    rng <- range(data[[xnm]], na.rm = TRUE)
    if(geom == "col") {
      if(inherits(object$position, "position_shift_stack") || identical(object$position, "auto")){
        rng <- get_range_col(x, y)
      }
      data[[xnm]] <- scales::rescale(data[[xnm]], rng / diff(rng) * scale,
                                     from = range(data[[xnm]], na.rm = TRUE))
      if(rng[1] < 0) {
        shift <- abs(rng[1] / diff(rng)) * ncols(pdata) * object$width + shift
      }
    } else {
      data[[xnm]] <- scales::rescale(data[[xnm]], c(0, scale),
                                     from = range(data[[xnm]], na.rm = TRUE))
    }
    if(identical(object$position, "auto")) {
      position <- if(geom == "col") {
        position_shift_stack(xshift = shift)
      } else if(geom == "violin") {
        position_shift_dodge(xshift = shift)
      } else if(geom == "boxplot") {
        position_shift_dodge2(xshift = shift)
      } else {
        position_shift_identity(xshift = shift)
      }
    } else {
      object$position
    }
    plot$plot_env$polar.args$xshift <- shift + object$width * ncols(pdata)
  }

  geom <- paste0("geom_", object$geom)
  obj <- if(geom %in% c("geom_col", "geom_violin", "geom_boxplot")) {
    do.call(geom, c(list(mapping = mapping, data = data, position = position,
                         orientation = orientation, inherit.aes = FALSE),
                    object$params))
  } else {
    do.call(geom, c(list(mapping = mapping, data = data, position = position,
                         inherit.aes = FALSE), object$params))
  }
  ggplot_add(obj, plot, object_name)
}


#' @importFrom dplyr summarize group_by
get_range_col <- function(xx, yy) {
  if(!is.numeric(x)) {
    temp <- xx
    xx <- yy
    yy <- temp
  }
  if(all(xx >= 0) || all(xx <= 0)) {
    rng <-  range(tapply(xx, yy, sum, na.rm = TRUE, simplify = TRUE), na.rm = TRUE)
    rng <- if(rng[1] >= 0)  c(0, rng[2]) else c(rng[1], 0)
  } else {
    id <- xx >= 0
    rng1 <- min(tapply(xx[!id], yy[!id], sum, na.rm = TRUE, simplify = TRUE), na.rm = TRUE)
    rng2 <- max(tapply(xx[id], yy[id], sum, na.rm = TRUE, simplify = TRUE), na.rm = TRUE)
    rng <- c(rng1, rng2)
  }
  unname(rng)

}
