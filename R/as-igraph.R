#' Corece to a igraph object
#' @description Functions to coerce a object to igraph if possible.
#' @param x \code{R} object.
#' @param directed 	logical value, whether or not to create a directed graph.
#' @param ... extra params.
#' @return igraph object.
#' @importFrom igraph graph_from_data_frame as.igraph
#' @rdname as_igraph
#' @examples
#' fortify_cor(mtcars) %>% as.igraph()
#' correlate(mtcars, cor.test = TRUE) %>% as.igraph()
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as.igraph.cor_tbl <- function(x, directed = FALSE, ...)
{
  x <- as_cor_network(x, ...)
  igraph::graph_from_data_frame(x$edges, directed = directed,
                                vertices = x$nodes)
}

#' @rdname  as_igraph
#' @export
as.igraph.mantel_tbl <- function(x, directed = FALSE, ...)
{
  as.igraph(as_cor_tbl(x), directed = directed, ...)
}

#' @rdname  as_igraph
#' @export
as.igraph.pro_tbl <- function(x, directed = FALSE, ...)
{
  as.igraph(as_cor_tbl(x), directed = directed, ...)
}

#' @rdname  as_igraph
#' @importFrom tidygraph tbl_graph
#' @export
as.igraph.rcorr <- function(x, directed = FALSE, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  cor_network(x$r, p.value, directed = directed, ..., val.type = "igraph")
}

#' @rdname  as_igraph
#' @export
as.igraph.corr.test <- function(x, directed = FALSE, ...)
{
  cor_network(x$r, x$p, directed = directed, ..., val.type = "igraph")
}

#' @rdname  as_igraph
#' @export
as.igraph.correlate <- function(x, directed = FALSE, ...)
{
  cor_network(x$r, x$p.value, directed = directed, ..., val.type = "igraph")
}

#' @importFrom igraph graph_from_data_frame
#' @rdname  as_igraph
#' @export
as.igraph.cor_network <- function(x, directed = FALSE, ...)
{
  igraph::graph_from_data_frame(x$edges, directed, x$nodes)
}
