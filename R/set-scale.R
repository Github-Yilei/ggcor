#' Set the continuous fill scale of correlation matrix plot
#' @title Set scale
#' @param pal NULL or colours.
#' @param type a scale function or character of scale name (should be one of
#' "gradient", "viridis" or "gradient2n").
#' @param ... extra parameters for scale function.
#' @importFrom ggplot2 guides guide_colorbar scale_fill_gradient scale_fill_viridis_c
#' @rdname set_scale
#' @examples
#' set_scale()
#' quickcor(mtcars) + geom_square()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
set_scale <- function(pal = NULL,
                      type = "gradient2n",
                      ...
                      ) {
  if(!is.function(type)) {
    if(!type %in% c("gradient", "viridis", "gradient2n")) {
      stop("Unknown scale type.", call. = FALSE)
    }
  }
  if(is.null(colours)) {
    colours <- red_blue()
  }
  scale <- if(is.function(type)) {
    do.call(type, list(...))
  } else if(identical(type, "gradient")) {
    scale_fill_gradient(...)
  } else if(identical(type, "viridis")) {
    scale_fill_viridis_c(...)
  } else {
    scale_fill_gradient2n(colours = pal, ...)
  }

  options(scale_fill_continuous = scale)
}

#' @rdname set_scale
#' @export
reset_scale <- function() {
  options(scale_fill_continuous = NULL)
}
