## These functions are inspired by the position_*() functions in the ggtreeExtra
## package and have been subtly modified for ggcor. And this function will be removed
## when ggtreeExtra package is committed to CRAN or Bioconductor.

## Shuangbin Xu and Guangchuang Yu (2020). ggtreeExtra: An R Package To Add Geom
##   Layers On Circular Or Other Layout Tree Of "ggtree". R package version 0.0.0.9.
##   https://github.com/YuLab-SMU/ggtreeExtra/


#' Shift stack position function for annotation.
#' @title Pisition function with some shift
#' @inheritParams ggplot2::position_stack
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @rdname position_shift_stack
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_stack <- function(vjust = 1,
                                 xshift = NA,
                                 yshift = NA,
                                 reverse = FALSE) {
  ggproto(NULL,
          PositionShiftStack,
          vjust = vjust,
          xshift = xshift,
          yshift = yshift,
          reverse = reverse)
}

#' @export
#' @rdname position_shift_stack
position_shift_fill <- function(vjust = 1,
                                xshift = NA,
                                yshift = NA,
                                reverse = FALSE) {
  ggproto(NULL,
          PositionShiftFill,
          vjust = vjust,
          xshift = xshift,
          yshift = yshift,
          reverse = reverse)
}

#' @rdname position_shift_stack
#' @format NULL
#' @usage NULL
#' @export
PositionShiftStack <- ggproto(
  "PositionShiftStack", Position,
  type = NULL,
  vjust = 1,
  fill = FALSE,
  reverse = FALSE,
  xshift = NA,
  yshift = NA,

  setup_params = function(self, data) {
    flipped_aes <- ggplot2::has_flipped_aes(data)
    data <- ggplot2::flip_data(data, flipped_aes)
    list(
      var = self$var %||% stack_var(data),
      fill = self$fill,
      vjust = self$vjust,
      flipped_aes = flipped_aes,
      xshift = self$xshift,
      yshift = self$yshift,
      reverse = self$reverse
    )
  },

  setup_data = function(self, data, params) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    if(is.null(params$var)) {
      return(data)
    }

    data$ymax <- switch(params$var,
                        y = data$y,
                        ymax = ifelse(data$ymax == 0, data$ymin, data$ymax)
    )

    data <- ggplot2::remove_missing(
      data,
      vars = c("x", "xmin", "xmax", "y"),
      name = "position_shift_stack"
    )
    ggplot2::flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    if(is.null(params$var)) {
      return(data)
    }

    negative <- data$ymax < 0
    negative[is.na(negative)] <- FALSE

    neg <- data[negative, , drop = FALSE]
    pos <- data[!negative, , drop = FALSE]

    if(any(negative)) {
      neg <- collide(neg, NULL, "position_shift_stack", pos_stack,
                               vjust = params$vjust,
                               fill = params$fill,
                               reverse = params$reverse
      )
    }
    if(any(!negative)) {
      pos <- collide(pos, NULL, "position_shift_stack", pos_stack,
                               vjust = params$vjust,
                               fill = params$fill,
                               reverse = params$reverse
      )
    }

    data <- rbind(neg, pos)[match(seq_len(nrow(data)), c(which(negative), which(!negative))),]
    data <- ggplot2::flip_data(data, params$flipped_aes)

    if(!is.na(params$yshift)){
      data$y <- data$y + params$yshift
      data$ymin <- data$ymin + params$yshift
      data$ymax <- data$ymax + params$yshift
    }
    if(!is.na(params$xshift)){
      data$x <- data$x + params$xshift
      data$xmin <- data$xmin + params$xshift
      data$xmax <- data$xmax + params$xshift
    }
    data <- data.frame(data, check.names=FALSE)
  }
)

#' @rdname position_shift_stack
#' @format NULL
#' @usage NULL
#' @export
PositionShiftFill <- ggproto("PositionShiftFill",
                             PositionShiftStack,
                             fill = TRUE
)

#' Shift dodge position function for annotation.
#' @title Dodge position with some shift
#' @inheritParams ggplot2::position_dodge
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @rdname position_shift_dodge
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_dodge <- function(width = NULL,
                                 xshift = NA,
                                 yshift = NA,
                                 preserve = c("total", "single")) {
  ggproto(NULL,
          PositionShiftDodge,
          width = width,
          xshift = xshift,
          yshift = yshift,
          preserve = match.arg(preserve)
  )
}

#' @rdname position_shift_dodge
#' @format NULL
#' @usage NULL
#' @export
PositionShiftDodge <- ggproto(
  "PositionShiftDodge", Position,
  width = NULL,
  xshift = NA,
  yshift = NA,
  preserve = "total",
  setup_params = function(self, data) {
    flipped_aes <- ggplot2::has_flipped_aes(data)
    data <- ggplot2::flip_data(data, flipped_aes)
    if(is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_shift_dodge(width = ?)`")
    }

    if(identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      ns <- vapply(panels, function(panel) max(table(panel$xmin)), double(1))
      n <- max(ns)
    }

    list(
      width = self$width,
      xshift = self$xshift,
      yshift = self$yshift,
      n = n,
      flipped_aes = flipped_aes
    )
  },

  setup_data = function(self, data, params) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    if(!"x" %in% names(data) && all(c("xmin", "xmax") %in% names(data))) {
      data$x <- (data$xmin + data$xmax) / 2
    }
    ggplot2::flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    collided <- collide(
      data,
      params$width,
      name = "position_shift_dodge",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE
    )
    data <- ggplot2::flip_data(collided, params$flipped_aes)
    data <- pos_shift_dodge(data = data,
                            xshift = params$xshift,
                            yshift = params$yshift)
  }
)



#' Shift dodge2 position function for annotation.
#' @title Dodge2 position with some shift
#' @inheritParams ggplot2::position_dodge2
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @rdname position_shift_dodge2
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_dodge2 <- function(width = NULL,
                                  preserve = c("total", "single"),
                                  xshift = NA,
                                  yshift = NA,
                                  padding = 0.1,
                                  reverse = FALSE) {
  ggproto(NULL,
          PositionShiftDodge2,
          width = width,
          preserve = match.arg(preserve),
          xshift = xshift,
          yshift = yshift,
          padding = padding,
          reverse = reverse
  )
}

#' @rdname position_shift_dodge2
#' @format NULL
#' @usage NULL
#' @export
PositionShiftDodge2 <- ggproto(
  "PositionShiftDodge2",
  PositionShiftDodge,
  preserve = "total",
  padding = 0.1,
  reverse = FALSE,

  setup_params = function(self, data) {
    flipped_aes <- ggplot2::has_flipped_aes(data)
    data <- ggplot2::flip_data(data, flipped_aes)
    if(is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_shift_dodge2(width = ?)`")
    }

    if(identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      if("x" %in% names(data)) {
        groups <- lapply(panels, function(panel) table(panel$x))
      } else {
        groups <- lapply(panels, find_x_overlaps)
      }
      n_groups <- vapply(groups, max, double(1))
      n <- max(n_groups)
    }

    list(
      width = self$width,
      n = n,
      xshift = self$xshift,
      yshift = self$yshift,
      padding = self$padding,
      reverse = self$reverse,
      flipped_aes = flipped_aes
    )
  },

  compute_panel = function(data, params, scales) {
    data <- ggplot2::flip_data(data, params$flipped_aes)
    collided <- collide2(
      data,
      params$width,
      name = "position_shift_dodge2",
      strategy = pos_dodge2,
      n = params$n,
      padding = params$padding,
      check.width = FALSE,
      reverse = params$reverse
    )
    data <- ggplot2::flip_data(collided, params$flipped_aes)
    data <- pos_shift_dodge(data = data,
                            xshift = params$xshift,
                            yshift = params$yshift)
  }
)

#' Shift identity position function for annotation.
#' @title identity position with some shift
#' @param xshift,yshift the offsets on x-axis and y-axis.
#' @return a position object.
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Position
#' @rdname position_shift_identity
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
position_shift_identity <- function(xshift = NA,
                                    yshift = NA) {
  ggproto(NULL,
          PositionShiftIdentity,
          xshift = xshift,
          yshift = yshift)
}

#' @rdname position_shift_identity
#' @format NULL
#' @usage NULL
#' @export
PositionShiftIdentity <- ggproto(
  "PositionShiftIdentity",
  Position,
  xshift = NA,
  yshift = NA,
  setup_params = function(self, data){
    list(xshift = self$xshift,
         yshift = self$yshift)
  },

  compute_layer = function(self, data, params, layout) {
    data <- pos_shift_identity(data,
                               xshift = params$xshift,
                               yshift = params$yshift)

    data
  }
)

#' @noRd
pos_shift_dodge <- function(data, xshift, yshift){
  name <- names(data)
  ypos <- intersect(c("ymin", "ymax", "lower", "middle", "upper", "y", "notchupper",
                      "notchlower", "outliers", "ymin_final", "ymax_final"), name)
  xpos <- intersect(c("xmin", "xmax", "xlower", "xmiddle", "xupper", "x", "notchupper",
                      "notchlower", "outliers", "xmin_final", "xmax_final"), name)

  if(!is.na(yshift)){
    data <- purrr::map2(data, name, function(y, name) {
      if(!name %in% ypos) {
        return(y)
      }
      if(name == "outliers") {
        purrr::map(y, function(.y) {
          if(length(.y) == 0) {
            return(.y)
          }
          .y + yshift
        })
      } else {
        y + yshift
      }
    })
  }

  if(!is.na(xshift)){
    data <- purrr::map2(data, name, function(x, name) {
      if(!name %in% xpos) {
        return(x)
      }
      if(name == "outliers") {
        purrr::map(x, function(.x) {
          if(length(.x) == 0) {
            return(.x)
          }
          .x + xshift
        })
      } else {
        x + xshift
      }
    })
  }
  data <- new_data_frame(data)
}

#' @noRd
pos_shift_identity <- function(data, xshift, yshift){
  name <- names(data)
  ypos <- intersect(c("ymin", "ymax", "y", "yend"), name)
  xpos <- intersect(c("xmin", "xmax", "x", "xend"), name)

  if(!is.na(yshift)){
    data <- purrr::map2(data, name, function(y, name) {
      if(!name %in% ypos) {
        return(y)
      }
      y + yshift
    })
  }

  if(!is.na(xshift)){
    data <- purrr::map2(data, name, function(x, name) {
      if(!name %in% xpos) {
        return(x)
      }
      x + xshift
    })
  }
  data <- new_data_frame(data)
}

#' @noRd
collide <- getFromNamespace("collide", "ggplot2")

#' @noRd
collide2 <- getFromNamespace("collide2", "ggplot2")

#' @noRd
stack_var <- getFromNamespace("stack_var", "ggplot2")

#' @noRd
pos_stack <- getFromNamespace("pos_stack", "ggplot2")

#' @noRd
pos_dodge <- getFromNamespace("pos_dodge", "ggplot2")

#' @noRd
pos_dodge2 <- getFromNamespace("pos_dodge2", "ggplot2")
