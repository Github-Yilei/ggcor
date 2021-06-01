#' Annotation for correlation matrix plot
#' @title Annotation for correlation matrix plot
#' @param bcols branch colours.
#' @param width,height width or height of tree.
#' @param pos position of tree.
#' @param colour,color colour of segments.
#' @param size width of segments.
#' @param linetype line type of segments.
#' @return anno_tree object.
#' @rdname anno_tree
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_row_tree <- function(bcols = NULL,
                          width = NULL,
                          pos = NULL,
                          colour = NULL,
                          size = NULL,
                          linetype = NULL,
                          color
                          ) {
  if(!missing(color))
    colour <- color
  if(!is.null(pos)) {
    pos <- match.arg(pos, c("left", "right"))
  }
  structure(.Data = list(bcols = bcols, width = width, pos = pos,
                         colour = colour, size = size, linetype = linetype),
            class = "anno_row_tree")
}

#' @rdname anno_tree
#' @export
anno_col_tree <- function(bcols = NULL,
                          height = NULL,
                          pos = NULL,
                          colour = NULL,
                          size = NULL,
                          linetype = NULL,
                          color
) {
  if(!missing(color))
    colour <- color
  if(!is.null(pos)) {
    pos <- match.arg(pos, c("top", "bottom"))
  }
  structure(.Data = list(bcols = bcols, height = height, pos = pos,
                         colour = colour, size = size, linetype = linetype),
            class = "anno_col_tree")
}

#' Link annotation
#' @title Link annotation
#' @param data a data frame.
#' @param mapping aesthetic mappings parameters.
#' @param width width of link plot.
#' @param pos position of link plot.
#' @param start.var,end.var character to specify which variable is the starting
#' points and which is the ending points. if the variable is not character, it
#' will be converted.
#' @param start.name character vector.
#' @param label.size,label.colour,label.family,label.fontface parameters for label.
#' @param nudge_x horizonal justification of label.
#' @param expand expansion of x axis.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @note Loading the \code{ggraph} package first makes this function even more functional.
#' @rdname anno_link
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_link <- function(data,
                      mapping = NULL,
                      width = 0.3,
                      pos = "right",
                      label.size = 3.88,
                      label.colour = "black",
                      label.family = "",
                      label.fontface = 1,
                      nudge_x = 0.1,
                      expand = NULL,
                      start.var = NULL,
                      start.name = NULL,
                      end.var = NULL,
                      ...)
{
  start.var <- rlang::enquo(start.var)
  end.var <- rlang::enquo(end.var)
  structure(.Data = list(mapping = mapping, data = data, width = width, pos = pos,
                         label.size = label.size, label.colour = label.colour,
                         label.family = label.family, label.fontface = label.fontface,
                         nudge_x = nudge_x, start.var = start.var, start.name = start.name,
                         end.var = end.var, params = list(...)),
            class = "anno_link")
}

#' Square annotation
#' @title Square annotation
#' @description Draw the cluster square mark on the correlation matrix plot.
#' @param pos position of the bar.
#' @param k an integer scalar or vector with the desired number of groups.
#' @param fill the colour of fill.
#' @param colour,color the colour of boder.
#' @param width,height width or height of the bar.
#' @param space scala numeric value.
#' @param size size of square boder line.
#' @param shift logical value.
#' @return square layer.
#' @rdname anno_hc_rect
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_hc_rect <- function(k = 2,
                         fill = NA,
                         colour = "black",
                         size = 2,
                         color)
{
  if(!missing(color))
    colour <- color
  structure(.Data = list(k = k, fill = fill, colour = colour,
                         size = size), class = "anno_hc_rect")
}

#' @rdname anno_hc_rect
#' @export
anno_hc_bar <- function(k = 2,
                        fill = NULL,
                        size = 0.125,
                        pos = "right",
                        width = 1,
                        height = 1,
                        space = 0,
                        shift = TRUE)
{
  if(is.null(fill)) {
    fill <- sample(grDevices::colors(TRUE), k)
  }
  pos <- match.arg(pos, c("bottom", "top", "left", "right"))
  structure(.Data = list(pos = pos, k = k, fill = fill, size = size,
                         width = width, height = height, space = space,
                         shift = shift),
            class = "anno_hc_bar")
}

#' Bar and boxplot annotation
#' @title Bar and boxplot annotation
#' @param mapping aesthetic mappings parameters.
#' @param data a data frame.
#' @param align align base on main plot.
#' @param flip a logical value. If TRUE, the annotation on the left or on
#' the bottom side will be flipped.
#' @param pos position of annotation.
#' @param width,height width and height of annotation.
#' @param trans the name of a transformation object or the object itself.
#' @param scale scale object.
#' @param theme theme object or the name.
#' @param ... extra parameters.
#' @return a anno_* object.
#' @rdname anno_bar
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_bar <- function(data,
                     mapping,
                     align = TRUE,
                     flip = FALSE,
                     pos = NULL,
                     width = 0.2,
                     height = 0.2,
                     trans = NULL,
                     scale = NULL,
                     theme = NULL,
                     ...) {
  structure(.Data = list(mapping = mapping, data = data, align = align,
                         flip = flip, pos = pos, width = width, height = height,
                         trans = trans, scale = scale, theme = theme,
                         params = list(...)),
            class = "anno_bar")
}

#' @rdname anno_bar
#' @export
anno_bar2 <- function(data,
                      mapping,
                      align = TRUE,
                      flip = FALSE,
                      pos = NULL,
                      width = 0.2,
                      height = 0.2,
                      trans = NULL,
                      scale = NULL,
                      theme = NULL,
                      ...) {
  structure(.Data = list(mapping = mapping, data = data, align = align,
                         flip = flip, pos = pos, width = width, height = height,
                         trans = trans, scale = scale, theme = theme,
                         params = list(...)),
            class = "anno_bar2")
}

#' @rdname anno_bar
#' @export
anno_boxplot <- function(data,
                         mapping,
                         align = TRUE,
                         pos = NULL,
                         width = 0.2,
                         height = 0.2,
                         trans = NULL,
                         scale = NULL,
                         theme = NULL,
                         ...) {
  structure(.Data = list(mapping = mapping, data = data, align = align,
                         pos = pos, width = width, height = height,
                         trans = trans, scale = scale, theme = theme,
                         params = list(...)),
            class = "anno_boxplot")
}


#' Heatmap annotation
#' @title Heatmap annotation
#' @param data a data frame.
#' @param mapping aesthetic mappings parameters.
#' @param align align base on main plot.
#' the bottom side will be flipped.
#' @param width,height width and height of annotation.
#' @param geom one of "anno_tile", "anno_tile2" or "point".
#' @param mark a layer instance.
#' @param space scala numeric value.
#' @param label logical value, if TRUE will add label on plot.
#' @param label.size,label.colour,label.family,label.fontface parameters for label.
#' @param ... extra parameters.
#' @return a anno_*_heat object.
#' @rdname anno_heatmap
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
anno_row_heat <- function(data,
                          mapping = NULL,
                          align = TRUE,
                          geom = "tile",
                          mark = NULL,
                          space = 0.5,
                          width = 0.2,
                          label = TRUE,
                          label.size = 3.88,
                          label.colour = "black",
                          label.family = "",
                          label.fontface = 1,
                          ...) {
  if(!is_cor_tbl(data)) {
    stop("Invalid data input.", call. = FALSE)
  }
  geom <- match.arg(geom, c("tile", "point"))
  structure(.Data = list(data = data, mapping = mapping, align = align,
                         geom = geom, space = space, mark = mark,
                         width = width, label = label, label.size = label.size,
                         label.colour = label.colour, label.family = label.family,
                         label.fontface = label.fontface, params = list(...)),
            class = "anno_row_heat")
}

#' @rdname anno_heatmap
#' @export
anno_col_heat <- function(data,
                          mapping = NULL,
                          align = TRUE,
                          geom = "tile",
                          mark = NULL,
                          space = 0.5,
                          height = 0.2,
                          label = TRUE,
                          label.size = 3.88,
                          label.colour = "black",
                          label.family = "",
                          label.fontface = 1,
                          ...) {
  if(!is_cor_tbl(data)) {
    stop("Invalid data input.", call. = FALSE)
  }
  geom <- match.arg(geom, c("tile", "point"))
  structure(.Data = list(data = data, mapping = mapping, align = align,
                         geom = geom, space = space, height = height,
                         label = label, label.size = label.size,
                         label.colour = label.colour, label.family = label.family,
                         label.fontface = label.fontface, params = list(...)),
            class = "anno_col_heat")
}

#' Network annotation
#' @title Network annotation
#' @param data a cor_network object.
#' @param mapping aesthetic mappings parameters for edges.
#' @param mapping2 aesthetic mappings parameters for nodes.
#' @param space gap between heatmap and network nodes.
#' @param ... extra parameters.
#' @return a anno_network object.
#' @note Loading the \code{ggraph} package first makes this function even more functional.
#' @rdname anno_network
#' @seealso \code{\link{geom_segment2}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export

anno_network <- function(data,
                         mapping = NULL,
                         mapping2 = NULL,
                         space = 0.5,
                         ...) {
  structure(.Data = list(data = data, mapping = mapping,
                         mapping2 = mapping2, space = space,
                         params = list(...)), class = "anno_network")
}
