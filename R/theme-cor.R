#' Create the default ggcor theme
#' @details The theme_cor, with no axis title, no background, no grid,
#'     made some adjustments to the x-axis label.
#' @param legend.position the position of legends ("none", "left", "right",
#'     "bottom", "top", or two-element numeric vector).
#' @param ... extra params passing to \code{\link[ggplot2]{theme}}.
#' @return The theme.
#' @importFrom ggplot2 theme element_text element_blank element_rect element_line
#' @rdname theme_cor
#' @examples
#' require(ggplot2, quietly = TRUE)
#' df <- fortify_cor(mtcars)
#' ggcor(df) + geom_raster(aes(fill = r)) + theme_cor()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
theme_cor <- function(legend.position = "right",
                      ...)
{
  theme(
    axis.text = element_text(size = 10.5, colour = "black"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
    axis.text.x.bottom = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = NA),
    legend.position = legend.position,
    ...
  )
}

#' @rdname theme_cor
#' @export
theme_anno <- function()
{
  ggplot2::theme_bw() + theme(plot.margin = ggplot2::margin())
}

#' @rdname theme_cor
#' @export
theme_anno2 <- function()
{
  ggplot2::theme_void() + theme(plot.margin = ggplot2::margin())
}
