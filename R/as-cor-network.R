#' Coerce to a cor_network object
#' @description Functions to coerce a object to cor_network if possible.
#' @param x any \code{R} object.
#' @param directed 	logical value, whether or not to create a directed graph.
#' @param simplify logical value (defaults to TRUE) indicating whether to
#'     delete nodes without edge connections.
#' @param weight NULL (default) or name of column in edges which will be renamed
#'     to "weight".
#' @param r.thres a numeric value.
#' @param r.abs logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param ... extra params passing to \code{\link[ggcor]{cor_network}}.
#' @return a cor_network object.
#' @importFrom dplyr filter rename %>%
#' @importFrom tibble tibble
#' @importFrom rlang sym !!
#' @rdname as_cor_network
#' @examples
#' ll <- correlate(mtcars)
#' as_cor_network(ll)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_cor_network <- function(x, ...) {
  UseMethod("as_cor_network")
}

#' @rdname  as_cor_network
#' @export
#' @method as_cor_network cor_tbl
as_cor_network.cor_tbl <- function(x,
                                   directed = FALSE,
                                   simplify = TRUE,
                                   weight = NULL,
                                   r.thres = 0.6,
                                   r.abs = TRUE,
                                   p.thres = 0.05,
                                   ...)
{

  if(is_gcor_tbl(x)) {
    edges <- if("p.value" %in% names(x) && is.finite(p.thres)) {
      dplyr::filter(x, p.value < p.thres)
    } else x
  } else {
    edges <- if(is.finite(r.thres)) {
      if("p.value" %in% names(x) && is.finite(p.thres)) {
        if(r.abs) {
          dplyr::filter(x, abs(r) > r.thres, p.value < p.thres)
        } else {
          dplyr::filter(x, r > r.thres, p.value < p.thres)
        }
      } else {
        if(r.abs) {
          dplyr::filter(x, abs(r) > r.thres)
        } else {
          dplyr::filter(x, r > r.thres)
        }
      }
    } else {
      if("p.value" %in% names(x) && is.finite(p.thres)) {
        dplyr::filter(x, p.value < p.thres)
      } else {
        x
      }
    }
  }

  # rename
  edges <- rename_cor_network_edge(edges, ".row.names", ".col.names")

  nodes <- if(simplify) {
    tibble::tibble(name = unique(c(edges$from, edges$to)))
  } else {
    tibble::tibble(name = unique(c(x$.col.names, x$.row.names)))
  }

  if(!is.null(weight)) {
    if(!weight %in% names(edges)) {
      stop("don't find ", weight, " in egdes table.", call. = FALSE)
    }
    weight <- rlang::sym(weight)
    edges <- dplyr::rename(edges, weight = !!weight)
  }

  structure(.Data = list(nodes = nodes, edges  = edges),
            directed = directed, class = "cor_network")
}

#' @rdname  as_cor_network
#' @export
#' @method as_cor_network mantel_tbl
as_cor_network.mantel_tbl <- function(x, directed = FALSE, ...) {
  as_cor_network(as_cor_tbl(x), directed = directed, ...)
}

#' @rdname  as_cor_network
#' @export
#' @method as_cor_network pro_tbl
as_cor_network.pro_tbl <- function(x, directed = FALSE, ...) {
  as_cor_network(as_cor_tbl(x), directed = directed, ...)
}

#' @rdname  as_cor_network
#' @export
#' @method as_cor_network correlate
as_cor_network.correlate <- function(x, directed = FALSE, ...) {
  cor_network(corr = x$r, p.value = x$p.value, directed = directed, ...,
              val.type = "list")
}

#' @rdname  as_cor_network
#' @export
#' @method as_cor_network rcorr
as_cor_network.rcorr <- function(x, directed = FALSE, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  cor_network(corr = x$r, p.value = p.value, directed = directed, ...,
              val.type = "list")
}

#' @rdname  as_cor_network
#' @export
#' @method as_cor_network corr.test
as_cor_network.corr.test <- function(x, directed = FALSE, ...)
{
  cor_network(corr = x$r, p.value = x$p, directed = directed, ...,
              val.type = "list")
}

#' @importFrom tibble as_tibble
#' @importFrom igraph as_data_frame is.directed
#' @rdname  as_cor_network
#' @export
#' @method as_cor_network igraph
as_cor_network.igraph <- function(x, ...)
{
  nodes <- tibble::as_tibble(igraph::as_data_frame(x, "vertices"))
  edges <- tibble::as_tibble(igraph::as_data_frame(x, "edges"))
  structure(.Data = list(nodes = nodes, edges = edges),
            directed = igraph::is.directed(x), class = "cor_network")
}

#' @rdname  as_cor_network
#' @export
#' @method as_cor_network tbl_graph
as_cor_network.tbl_graph <- function(x, ...)
{
  as_cor_network(igraph::as.igraph(x), ...)
}

#' @rdname as_cor_network
#' @export
#' @method as_cor_network default
as_cor_network.default <- function(x, ...) {
  stop(class(x)[1], " hasn't been realized yet.", call. = FALSE)
}

#' @noRd
rename_cor_network_edge <- function(x, from, to)
{
  stopifnot(is.data.frame(x))
  name <- names(x)
  name[name %in% c(from, to)] <- c("from", "to")
  names(x) <- name
  new <- c(c("from", "to"), setdiff(name, c("from", "to")))
  x[new]
}
