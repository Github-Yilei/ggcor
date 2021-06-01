#' Significant marks Geom
#'
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used, the default value is 2.
#' @param nsmall the minimum number of digits to the right of the decimal
#'     point in formatting real/complex numbers in non-scientific formats,
#'     the default value is 2.
#' @param sig.level significance level，the default values is [0.05, 0.01, 0.001].
#' @param mark significance mark，the default values is ["*", "**", "***"].
#' @param sig.thres if not NULL, just when p.value is not larger than sig.thres will be ploted.
#' @param sep a character string to separate the number and mark symbols.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @section Aesthetics:
#'     \code{geom_mark()} understands the following aesthetics (required
#'     aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{p.value}}
#'       \item \code{r}
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
#' @note \code{geom_mark} is good in cartesian coordinates, and \code{geom_mark2}
#' is good in polar coordinates.
#' @importFrom ggplot2 layer ggproto GeomText aes draw_key_text
#' @rdname geom_mark
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
geom_mark <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      digits = 2,
                      nsmall = 2,
                      sig.level = c(0.05, 0.01, 0.001),
                      mark = c("*", "**", "***"),
                      sig.thres = NULL,
                      sep = "",
                      parse = FALSE,
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
    geom = GeomMark,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      digits = digits,
      nsmall = nsmall,
      sig.level = sig.level,
      mark = mark,
      sig.thres = sig.thres,
      sep = sep,
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_mark
#' @export
geom_mark2 <- function(...) {
  structure(.Data = list(...), class = "geom_mark2")
}

#' @rdname geom_mark
#' @format NULL
#' @usage NULL
#' @export
GeomMark <- ggproto("GeomMark", GeomText,
                   required_aes = c("x", "y", "p.value"),

                   default_aes = aes(
                     r = NA, colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                     vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                   ),

                   draw_panel = function(data, panel_params, coord, digits = 2,
                                         nsmall = 2, sig.level = c(0.05, 0.01, 0.001),
                                         mark = c("*", "**", "***"), sig.thres = NULL,
                                         sep = "", parse = FALSE, na.rm = FALSE) {
                     stopifnot(length(sig.level) == length(mark))
                     if(!is.null(sig.thres))
                       data <- dplyr::filter(data, p.value <= sig.thres)
                     star <- sig_mark(data$p.value, sig.level, mark)
                     na_idx <- is.na(data$r)
                     num <- ifelse(na_idx, "", format_number(data$r, digits, nsmall))
                     if(parse) {
                       if(!requireNamespace("latex2exp", quietly = TRUE))
                         warning("Need latex2exp package.", call. = FALSE)
                       parse <- FALSE
                     }
                     if(parse) {
                       label <- paste(num, paste0("{", star, "}"), sep = sep)
                       data$label <- latex2exp::TeX(label, output = "text")
                     } else {
                       data$label <- paste(num, star, sep = sep)
                     }
                     GeomText$draw_panel(data, panel_params, coord)
                   },
                   draw_key = draw_key_text
)


