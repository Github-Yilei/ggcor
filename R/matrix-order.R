#' Reorder Matrices
#' @description  Tries to find an order for matrix by different cluster methods.
#' @param x a matrix-like object.
#' @param is.cor logical value (defaults to TRUE) indicating wheater
#' \code{x} is a correlation matrix.
#' @param cluster.method a character string with the name of agglomeration method.
#' @param ... extra params passing to \code{\link[stats]{hclust}}.
#' @details Now it just supports for square matrix.
#' @return a list of hclust object.
#' @importFrom stats as.dist dist hclust
#' @rdname matrix_order
#' @examples
#' m <- matrix(rnorm(25), nrow = 5)
#' matrix_order(m)
#' @seealso \code{\link[stats]{hclust}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
matrix_order <- function(x,
                         is.cor = TRUE,
                         cluster.method = "complete",
                         ...)
{
  if(!is.matrix(x))
    x <- as.matrix(x)
  if(isTRUE(is.cor)) {
    if(!isSymmetric(x) || any(colnames(x) != rownames(x))) {
      row.hc <- hclust(dist(x), cluster.method, ...)
      col.hc <- hclust(dist(t(x)), cluster.method, ...)
    } else {
      row.hc <- col.hc <- hclust(as.dist(1 - x), cluster.method, ...)
    }
  } else {
    row.hc <- hclust(dist(x))
    col.hc <- hclust(dist(t(x)))
  }
  list(row.hc = row.hc,
       col.hc = col.hc)
}
