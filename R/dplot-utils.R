#' Decoration plot
#' @title Decoration plot
#' @param plot,x a "gg" or "dplot" object.
#' @param obj a "gg" object.
#' @param width,height scala numeric value.
#' @param pos one of "left", "right", "bottom" or "top".
#' @param ... extra parameters.
#' @return a "dplot" object.
#' @rdname dplot_utils
#' @importFrom ggplot2 ggplot geom_blank theme_void ggplot_add
#' @importFrom patchwork plot_layout
#' @examples \dontrun{
#' library(ggplot2)
#' d <- matrix(sample(c(LETTERS[1:3], NA), 33, replace = TRUE), nrow = 11)
#' p <- gcor_tbl(d, "var") %>%
#'   quickcor(mapping = aes(colour = var)) +
#'   geom_point(size = 3)
#' quickcor(mtcars) +
#'   geom_square() +
#'   anno_row_custom(p) +
#'   anno_col_custom(p)
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_dplot <- function(plot) {
  if(is_dplot(plot))
    return(plot)
  if(!inherits(plot, "gg")) {
    stop("'plot' must be a 'gg' object.", call. = FALSE)
  }
  structure(.Data = plot,
            .anno_info = list(row.anno = list(),
                              col.anno = list(),
                              width = NULL,
                              height = NULL,
                              multi = list(),
                              and = list(),
                              r = 0,
                              l = 0,
                              t = 0,
                              b = 0),
            class = c("dplot", class(plot)))
}

#' @rdname dplot_utils
#' @export
is_dplot <- function(plot) {
  inherits(plot, "dplot")
}

#' @rdname dplot_utils
#' @export
anno_col_custom <- function(obj,
                            height = 0.2,
                            pos = "top") {
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  structure(.Data = list(obj = obj, height = height, pos = pos),
            class = "anno_col_custom")
}

#' @rdname dplot_utils
#' @export
anno_row_custom <- function(obj,
                            width = 0.2,
                            pos = "right") {
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  structure(.Data = list(obj = obj, width = width, pos = pos),
            class = "anno_row_custom")
}

#' @noRd
.anno_row <- function(plot,
                      obj,
                      width = 0.2,
                      pos = "right") {
  plot <- as_dplot(plot)
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  pos <- match.arg(pos, c("left", "right"))
  .anno_info <- attr(plot, ".anno_info")
  if(pos == "left") {
    .anno_info$l <- .anno_info$l + 1
    .anno_info$width <- c(width, .anno_info$width)
    .anno_info$row.anno <- c(list(obj), .anno_info$row.anno)

  } else {
    .anno_info$r <- .anno_info$r + 1
    .anno_info$width <- c(.anno_info$width, width)
    .anno_info$row.anno <- c(.anno_info$row.anno, list(obj))

  }
  attr(plot, ".anno_info") <- .anno_info
  plot
}

#' @noRd
.anno_col <- function(plot,
                      obj,
                      height = 0.2,
                      pos = "top") {
  plot <- as_dplot(plot)
  if(!inherits(obj, "gg")) {
    stop("'obj' must be a 'gg' object.", call. = FALSE)
  }
  pos <- match.arg(pos, c("top", "bottom"))
  .anno_info <- attr(plot, ".anno_info")
  if(pos == "bottom") {
    .anno_info$b <- .anno_info$b + 1
    .anno_info$height <- c(.anno_info$height, height)
    .anno_info$col.anno <- c(.anno_info$col.anno, list(obj))
  } else {
    .anno_info$t <- .anno_info$t + 1
    .anno_info$height <- c(height, .anno_info$height)
    .anno_info$col.anno <- c(list(obj), .anno_info$col.anno)
  }
  attr(plot, ".anno_info") <- .anno_info
  plot
}

#' @export
ggplot_add.anno_row_custom <- function(object, plot, object_name) {
  .anno_row(plot, object$obj, object$width, object$pos)
}

#' @export
ggplot_add.anno_col_custom <- function(object, plot, object_name) {
  .anno_col(plot, object$obj, object$height, object$pos)
}

#' @rdname dplot_utils
#' @export
empty_plot <- function()
{
  ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_void()
}

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.dplot <- function(plot) {
  plot <- dplot_build(plot)
  ggplot_build(plot)
}
#' @rdname dplot_utils
#' @export
print.dplot <- function(x, ...) {
  x <- dplot_build(x)
  print(x)
}

#' @importFrom ggplot2 is.theme
#' @noRd
#' @export
`&.gg` <- function(e1, e2) {
  if(is_dplot(e1)) {
    .anno_info <- attr(e1, ".anno_info")
    .anno_info$and <- c(.anno_info$and, list(e2))
    attr(e1, ".anno_info") <- .anno_info
    return(e1)
  }
  if (is_patchwork(e1)) {
    if (is.theme(e2)) {
      e1$patches$annotation$theme <- e1$patches$annotation$theme +
        e2
    }
    e1$patches$plots <- lapply(e1$patches$plots, function(p) {
      if (is_patchwork(p)) {
        p <- p & e2
      }
      else {
        p <- p + e2
      }
      p
    })
  }
  e1 + e2
}

#' @noRd
#' @export
`*.gg` <- function(e1, e2) {
  if(is_dplot(e1)) {
    .anno_info <- attr(e1, ".anno_info")
    .anno_info$multi <- c(.anno_info$multi, list(e2))
    attr(e1, ".anno_info") <- .anno_info
    return(e1)
  }
  if (is_patchwork(e1)) {
    e1$patches$plots <- lapply(e1$patches$plots, function(p) {
      if (!is_patchwork(p))
        p <- p + e2
      p
    })
  }
  e1 + e2
}

#' @noRd
dplot_build <- function(plot) {
  .anno_info <- attr(plot, ".anno_info")
  row.anno <- .anno_info$row.anno
  col.anno <- .anno_info$col.anno
  r <- .anno_info$r
  l <- .anno_info$l
  t <- .anno_info$t
  b <- .anno_info$b
  width <- .anno_info$width
  height <- .anno_info$height

  class(plot) <- setdiff(class(plot), "dplot")
  if(length(row.anno) == 0 && length(col.anno) == 0) {
    return(plot)
  }

  n <- length(row.anno) + 1
  m <- length(col.anno) + 1

  plot.list <- vector("list", n * m)
  plot.list <- lapply(seq_len(n * m), function(.id) {
    plot.list[[.id]] <- empty_plot()
  })

  row.anno <- c(row.anno[rev(seq_len(l))], list(plot), row.anno[seq_len(r) + l])
  col.anno <- c(col.anno[seq_len(t)], list(plot), col.anno[seq_len(b) + t])
  plot.list[t * n + seq_len(n)] <- row.anno
  plot.list[l + 1 + n * (seq_len(m) - 1)] <- col.anno

  width <- c(width[seq_len(l)], 1, width[l + seq_len(r)])
  height <- c(height[seq_len(t)], 1, height[seq_len(b) + t])


  if(!plot$coordinates$is_free()) {
    width <- height <- NULL
  }
  p <- Reduce("+", plot.list) +
    plot_layout(ncol = n,
                nrow = m,
                byrow = TRUE,
                widths = width,
                heights = height,
                guides = "collect")
  if(length(.anno_info$multi) > 0) {
    p <- Reduce("*", .anno_info$multi, init = p)
  }
  if(length(.anno_info$and) > 0) {
    p <- Reduce("&", .anno_info$and, init = p)
  }
  p
}

#' @noRd
is_patchwork <- function(.plot) {
  inherits(.plot, "patchwork")
}
