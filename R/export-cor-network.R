#' Output co-network object
#' @description Functions to output edges or nodes data of co-network
#'     to a file.
#' @param x \code{R} object can be convert to \code{cor_network}.
#' @param file a character string naming a file for writing. "" indicates
#'     output to the console.
#' @param what either "edges" (default) or "nodes" will be output.
#' @param sep the field separator string (defaults to ",").
#' @param row.names,col.names row and column names of correlation matrix.
#' @param rm.dup logical (defaults to TRUE) indicating whether remove duplicate
#'     rows. If TRUE, the correlation between A-B and B-A is retained only A-B.
#' @param simplify logical value (defaults to TRUE) indicating whether to
#'     delete nodes without edge connections.
#' @param r.thres a numeric value.
#' @param r.absolute logical value (defaults to TRUE).
#' @param p.thres a numeric value.
#' @param ... extra params passing to \code{\link[utils]{write.table}}.
#' @importFrom utils write.table
#' @rdname export_cor_network
#' @seealso \code{\link[utils]{write.table}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
export_cor_network <- function(x, file = "", what = "edges", sep = ",", ...)
{
  UseMethod("export_cor_network")
}

#' @rdname  export_cor_network
#' @export
#' @method export_cor_network cor_network
export_cor_network.cor_network <- function(x,
                                           file = "",
                                           what = "edges",
                                           sep = ",",
                                           ...)
{
  what <- match.arg(what, c("edges", "nodes"))
  x <- x[[what]]
  utils::write.table(x, file, sep = sep, row.names = FALSE, ...)
}

#' @rdname  export_cor_network
#' @export
#' @method export_cor_network cor_tbl
export_cor_network.cor_tbl <- function(x,
                                       file = "",
                                       what = "edges",
                                       sep = ",",
                                       simplify = TRUE,
                                       r.thres = 0.6,
                                       r.absolute = TRUE,
                                       p.thres = 0.05,
                                       ...)
{
  what <- match.arg(what, c("edges", "nodes"))
  x <- as_cor_network(x, simplify, r.thres, r.absolute, p.thres)
  export_cor_network(x, file, what, sep, ...)
}

#' @rdname  export_cor_network
#' @export
#' @method export_cor_network mantel_tbl
export_cor_network.mantel_tbl <- function(x,
                                          file = "",
                                          what = "edges",
                                          sep = ",",
                                          simplify = TRUE,
                                          r.thres = 0.6,
                                          r.absolute = TRUE,
                                          p.thres = 0.05,
                                          ...)
{
  what <- match.arg(what, c("edges", "nodes"))
  x <- as_cor_network(x, simplify, r.thres, r.absolute, p.thres)
  export_cor_network(x, file, what, sep, ...)
}

#' @rdname  export_cor_network
#' @export
#' @method export_cor_network pro_tbl
export_cor_network.pro_tbl <- function(x,
                                       file = "",
                                       what = "edges",
                                       sep = ",",
                                       simplify = TRUE,
                                       r.thres = 0.6,
                                       r.absolute = TRUE,
                                       p.thres = 0.05,
                                       ...)
{
  what <- match.arg(what, c("edges", "nodes"))
  x <- as_cor_network(x, simplify, r.thres, r.absolute, p.thres)
  export_cor_network(x, file, what, sep, ...)
}

#' @rdname  export_cor_network
#' @export
#' @method export_cor_network correlate
export_cor_network.correlate <- function(x,
                                         file = "",
                                         what = "edges",
                                         sep = ",",
                                         row.names = NULL,
                                         col.names = NULL,
                                         rm.dup = TRUE,
                                         simplify = TRUE,
                                         r.thres = 0.6,
                                         r.absolute = TRUE,
                                         p.thres = 0.05,
                                         ...)
{
  what <- match.arg(what, c("edges", "nodes"))
  x <- as_cor_network(x, row.names = row.names, col.names = col.names,
                      rm.dup = rm.dup, simplify = simplify, r.thres = r.thres,
                      r.absolute = r.absolute, p.thres = p.thres)
  export_cor_network(x, file, what, sep, ...)
}

#' @rdname  export_cor_network
#' @export
#' @method export_cor_network rcorr
export_cor_network.rcorr <- function(x,
                                     file = "",
                                     what = "edges",
                                     sep = ",",
                                     row.names = NULL,
                                     col.names = NULL,
                                     rm.dup = TRUE,
                                     simplify = TRUE,
                                     r.thres = 0.6,
                                     r.absolute = TRUE,
                                     p.thres = 0.05,
                                     ...)
{
  what <- match.arg(what, c("edges", "nodes"))
  x <- as_cor_network(x, row.names = row.names, col.names = col.names,
                      rm.dup = rm.dup, simplify = simplify, r.thres = r.thres,
                      r.absolute = r.absolute, p.thres = p.thres)
  export_cor_network(x, file, what, sep, ...)
}

#' @rdname  export_cor_network
#' @export
#' @method export_cor_network corr.test
export_cor_network.corr.test <- function(x,
                                         file = "",
                                         what = "edges",
                                         sep = ",",
                                         row.names = NULL,
                                         col.names = NULL,
                                         rm.dup = TRUE,
                                         simplify = TRUE,
                                         r.thres = 0.6,
                                         r.absolute = TRUE,
                                         p.thres = 0.05,
                                         ...)
{
  what <- match.arg(what, c("edges", "nodes"))
  x <- as_cor_network(x, row.names = row.names, col.names = col.names,
                      rm.dup = rm.dup, simplify = simplify, r.thres = r.thres,
                      r.absolute = r.absolute, p.thres = p.thres)
  export_cor_network(x, file, what, sep, ...)
}
