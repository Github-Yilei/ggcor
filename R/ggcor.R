#' Create a correlation plot
#' @description This function is the equivalent of \code{\link[ggplot2]{ggplot}}
#'     in ggplot2. It takes care of setting up the position of axis and legend for
#'     the plot based on the plot type.
#' @param data cor_tbl object.
#' @param mapping NULL (default) or a list of aesthetic mappings to use for plot.
#' @param axis.x.position,axis.y.position the position of the axis. 'auto' (default)
#'     is set according to the plot type, 'bottom' or 'top' for x axes, 'left' or 'right'
#'     for y axes.
#' @param axis.label.drop logical value (default is TRUE). When type of plot is 'upper'
#'     or 'lower' and 'show.diag' is FALSE, do you need to remove the blank coordinate
#'     label.
#' @return an object of class gg onto which layers, scales, etc. can be added.
#' @importFrom ggplot2 ggplot ggplot_add aes_string scale_x_continuous scale_y_continuous
#' @importFrom utils modifyList
#' @rdname ggcor
#' @examples
#' df <- fortify_cor(mtcars)
#' ggcor(df)
#' df01 <- fortify_cor(mtcars, type = "lower", show.diag = FALSE)
#' ggcor(df01, axis.label.drop = TRUE)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
ggcor <- function(data,
                  mapping = NULL,
                  axis.x.position = "auto",
                  axis.y.position = "auto",
                  axis.label.drop = TRUE)
{
  if(!is_cor_tbl(data))
    stop("'data' needs a cor_tbl.", call. = FALSE)
  type <- get_type(data)
  show.diag <- get_show_diag(data)
  col.names <- get_col_name(data)
  row.names <- rev(get_row_name(data))
  base.aes <- aes_string(".col.id", ".row.id")
  mapping <- if(is.null(mapping)) base.aes else modifyList(base.aes, mapping)
  # handle axis setting
  axis.x.position <- match.arg(axis.x.position, c("auto", "bottom", "top"))
  axis.y.position <- match.arg(axis.y.position, c("auto", "left", "right"))
  if(axis.x.position == "auto") {
    axis.x.position <- switch (type,
                               full = "bottom",
                               lower = "bottom",
                               upper = "top")
  }
  if(axis.y.position == "auto") {
    axis.y.position <- switch (type,
                               full = "left",
                               lower = "left",
                               upper = "right")
  }
  axis.x.breaks <- 1:length(col.names)
  axis.x.labels <- col.names
  axis.y.breaks <- 1:length(row.names)
  axis.y.labels <- row.names
  if(axis.label.drop) {
    if(isFALSE(show.diag)) {
      if(type == "upper") {
        axis.x.breaks <- axis.x.breaks[-1]
        axis.x.labels <- axis.x.labels[-1]
        axis.y.breaks <- axis.y.breaks[-1]
        axis.y.labels <- axis.y.labels[-1]
      }
      if(type == "lower") {
        axis.x.breaks <- axis.x.breaks[-length(col.names)]
        axis.x.labels <- axis.x.labels[-length(col.names)]
        axis.y.breaks <- axis.y.breaks[-length(row.names)]
        axis.y.labels <- axis.y.labels[-length(row.names)]
      }
    }
  }

  p <- ggplot(data = data, mapping = mapping, environment = parent.frame()) +
    scale_x_continuous(expand = c(0, 0), breaks = axis.x.breaks, labels = axis.x.labels,
                       position = axis.x.position)+
    scale_y_continuous(expand = c(0, 0), breaks = axis.y.breaks, labels = axis.y.labels,
                       position = axis.y.position)
  p
}
