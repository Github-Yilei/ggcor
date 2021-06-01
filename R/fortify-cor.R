#' Convert to cor_tbl based on input type.convert
#' @description The fortify_cor function is a deep encapsulation of
#' the \code{as_cor_tbl} function and also supports converting raw
#' data into cor_tbl objects by calculation.
#' @param x any \code{R} object.
#' @param y NULL (default) or a matrix or data frame with compatible
#' dimensions to x.
#' @param is.cor logical value (default to FALSE) indicating wheater
#' \code{x} is a correlation matrix.
#' @param group NULL (default) or a vector that has the same number
#' of rows as x.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#' lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param cor.test logical value (default to TRUE) indicating whether test
#' for the correlation.
#' @param cluster logical value (default to FALSE) indicating whether reorder
#' the correlation matrix by cluster.
#' @param cluster.method the agglomeration method to be used. This should be
#' (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single",
#' "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC)
#' or "centroid" (= UPGMC).
#' @param ... extra params passing to \code{matrix_order}.
#' @return cor_tbl object.
#' @importFrom dplyr %>% mutate
#' @rdname fortify_cor
#' @examples
#' fortify_cor(mtcars)
#' fortify_cor(iris[-5], group = iris[[5]])
#' fortify_cor(mtcars, type = "lower", cluster = TRUE)
#' m <- cor(mtcars)
#' fortify_cor(m, is.cor = TRUE)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @seealso \code{\link[ggcor]{matrix_order}}, \code{\link[stats]{hclust}},
#'     \code{\link[ggcor]{as_cor_tbl}}.
#' @export
fortify_cor <- function(x,
                        y = NULL,
                        is.cor = FALSE,
                        group = NULL,
                        type = "full",
                        show.diag = TRUE,
                        cor.test = FALSE,
                        cluster = FALSE,
                        cluster.method = "complete",
                        ...)
{
  type <- match.arg(type, c("full", "upper", "lower"))
  if(is_cor_tbl(x)) {
    return(
      switch (type,
      full = x,
      upper = get_upper_data(x, show.diag),
      lower = get_lower_data(x, show.diag)
    ))
  }
  clss <- c("correlate", "rcorr", "corr.test", "mantel_tbl", "pro_tbl")
  if(any(clss %in% class(x)) || is.cor) {
    return(as_cor_tbl(x, type = type, show.diag = show.diag, cluster = cluster,
                      cluster.method = cluster.method, ...))
  }
  y <- y %||% x
  if(!is.data.frame(x))
     x <- as.data.frame(x)
  if(!is.data.frame(y)) {
    y <- as.data.frame(y)
    if(nrow(x) != nrow(y))
      stop("'y' must have the same rows as 'x'.", call. = FALSE)
  }
  if(!is.null(group)) {
    if(length(group) != nrow(x))
      stop("'group' must have the same length as rows of 'x'.", call. = FALSE)
    x <- split(x, group, drop = FALSE)
    y <- split(y, group, drop = FALSE)
    dfs <- purrr::pmap(list(x, y, as.list(names(x))),
                      function(.x, .y, .group) {
                        correlate(.x, .y, cor.test, ...) %>%
                          as_cor_tbl(type = type, show.diag = show.diag, cluster = cluster,
                                     cluster.method = cluster.method) %>%
                          mutate(.group = .group)
                      })
    attrs <- attributes(dfs[[1]])
    df <- suppressMessages(
      set_attrs(dplyr::bind_rows(dfs), attrs)
    )
  } else {
    corr <- correlate(x, y, cor.test, ...)
    df <- as_cor_tbl(corr, type = type, show.diag = show.diag, cluster = cluster,
               cluster.method = cluster.method)
  }
  attr(df, "grouped") <- if(is.null(group)) FALSE else TRUE
  df
}
