#' Set axis labels on circular plot
#' @title Set axis labels
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param bcols colours of branchs.
#' @param ... extra parameters passing to \code{ggplot2::geom_text()}.
#' @rdname set_axis
#' @examples
#' p <- quickcor(mtcars, circular = TRUE, paxis = "none")
#' p + set_p_xaxis()
#' p + set_p_yaxis()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
set_p_xaxis <- function(mapping = NULL, bcols = NULL, ...) {
  structure(.Data = list(mapping = mapping, bcols = bcols,
                         params = list(...)), class = "p_xaxis")
}

#' @rdname set_axis
#' @export
set_p_yaxis <- function(mapping = NULL, bcols = NULL, ...) {
  structure(.Data = list(mapping = mapping, bcols = bcols, params = list(...)),
            class = "p_yaxis")
}

#' @noRd
calc_polar_params <- function(cor_tbl,
                              open =  90,
                              inner = 0.6,
                              outer = 0.3) {
  row.names <- get_row_name(cor_tbl)
  col.names <- get_col_name(cor_tbl)
  rows <- length(row.names)
  cols <- length(col.names)
  open <- open %% 360
  ratio <- open / 360
  xlim <- c(- cols * inner, cols * (1 + outer)) + 0.5
  ylim <- c(0.5, max(rows / (1 - ratio) + 0.5, rows + 0.5))

  start <- one_degree()
  ut.degree <- diff(ylim) / 360
  t <- (1:rows) * 360 / diff(ylim) + 90

  yaxis_df <- new_data_frame(
    list(x = 0.5 + 1.05 * cols,
         y = 1:rows,
         label = rev(row.names),
         angle = ifelse(t > 90 & t < 270, t + 180, t) - 0.5 / ut.degree,
         hjust = ifelse(t > 90 & t < 270, 1, 0)))
  xaxis_df <- new_data_frame(
    list(x = 1:cols,
         y = ylim[2] - ut.degree,
         label = col.names,
         angle = 0,
         hjust = 0)
    )
  list(xlim = xlim, ylim = ylim, start = start, xaxis_df = xaxis_df,
       yaxis_df = yaxis_df, angle = t)
}

#' @noRd
one_degree <- function() 1 / 180 * pi
