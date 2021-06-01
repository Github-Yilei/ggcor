#' Quick heatmap
#' @title Quick heatmap
#' @param data a cor_tbl object.
#' @param mapping aesthetic mappings parameters.
#' @param geom short name of layer function, the default is "tile".
#' @param zoom_x,zoom_y the ratio of zoom.
#' @param ... extra parameters for layer function.
#' @note This function is mainly used to add annotation.
#' @return a gg object.
#' @rdname qheatmap
#' @examples
#' data <- gcor_tbl(mtcars)
#' qheatmap(data, aes(fill = value))
#'
#' library(ggplot2)
#' p <- matrix(sample(LETTERS[1:3], 33, replace = TRUE), nrow = 11) %>%
#'      gcor_tbl() %>%
#'      qheatmap(aes(fill = value), zoom_x = 0.5) +
#'      remove_y_axis() +
#'      coord_fixed()
#' quickcor(mtcars) +
#'   geom_square() +
#'   anno_row_custom(p)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
qheatmap <- function(data,
                     mapping = NULL,
                     geom = "tile",
                     zoom_x = 1,
                     zoom_y = 1,
                     ...) {
  if(!is_cor_tbl(data)) {
    stop("Need a cor_tbl object.", call. = FALSE)
  }
  n <- nrows(data)
  m <- ncols(data)
  data$.row.id <- data$.row.id * zoom_y
  data$.col.id <- data$.col.id * zoom_x
  xbrks <- seq_len(m) * zoom_x
  ybrks <- seq_len(n) * zoom_y
  xlabel <- get_col_name(data)
  ylabel <- rev(get_row_name(data))
  geom <- paste0("geom_", geom)
  mapping <- aes_modify(aes_string(x = ".col.id", y = ".row.id"), mapping)
  params <- list(...)
  if(geom == "tile") {
    params <- modifyList(list(width = zoom_x, height = zoom_y), params)
  }
  obj <- do.call(geom, params)
  p <- ggplot(data = data, mapping = mapping) +
    obj +
    scale_x_continuous(limits = c(0.5, m + 0.5) * zoom_x,
                       breaks = xbrks,
                       labels = xlabel,
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5, n + 0.5) * zoom_y,
                       breaks = ybrks,
                       labels = ylabel,
                       expand = c(0, 0)) +
    theme_cor(plot.margin = ggplot2::margin())
  p
}

