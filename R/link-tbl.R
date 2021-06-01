#' Transform data base on different layout
#' @description These layout functions are not layout in the network diagram,
#' it just converts the original data into a form that makes it easy to draw
#' a curve graph.
#' @param data a data frame.
#' @param cor_tbl a col_tbl object.
#' @param start.var,end.var character to specify which variable is the starting
#' points and which is the ending points. if the variable is not character, it
#' is forced to be converted.
#' @param start.name names of start point.
#' @param pos position of link.
#' @return a data frame.
#' @importFrom rlang enquo eval_tidy set_names quo_is_null
#' @importFrom dplyr filter
#' @rdname link_tbl
#' @examples \dontrun{
#' data(varespec, package = "vegan")
#' data(varechem, package = "vegan")
#' m <- mantel_test(varespec, varechem, spec.select = list(1:10, 11:44))
#' corr <- fortify_cor(varechem)
#' link_tbl(m, corr) ## full
#' link_tbl(m, get_upper_data(corr)) ## upper
#' link_tbl(m, get_lower_data(corr)) ## lower
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
link_tbl <- function(data,
                     cor_tbl,
                     start.var = NULL,
                     start.name = NULL,
                     end.var = NULL,
                     pos = "right")
{
  if(!is.data.frame(data))
    data <- as.data.frame(data)
  if(!is_cor_tbl(cor_tbl))
    stop("Need a cor_tbl.", call. = FALSE)
  row.names <- rev(get_row_name(cor_tbl))
  col.names <- get_col_name(cor_tbl)
  type <- get_type(cor_tbl)
  show.diag <- get_show_diag(cor_tbl)

  if(!rlang::is_quosure(start.var))
    start.var <- enquo(start.var)
  if(!rlang::is_quosure(end.var))
    end.var <- enquo(end.var)
  start <- if(quo_is_null(start.var)) {
    data[[1]]
  } else {
    eval_tidy(start.var, data)
  }
  end <- if(quo_is_null(end.var)) {
    data[[2]]
  } else {
    eval_tidy(end.var, data)
  }
  if(!is.character(start))
    start <- as.character(start)
  if(!is.character(end))
    end <- as.character(end)
  spec.name <- start.name[!is.na(start.name)] %||% unique(start[!is.na(start)])
  n <- length(row.names)
  m <- length(spec.name)
  if(type == "full") {
    x1 <- max(length(col.names), n) * 0.45 + length(col.names)
    x2 <- length(col.names) + 1
    if(pos == "left") {
      temp <- x1
      x1 <- x2
      x2 <- x1
    }

    start.pos <- set_names(seq(1, n, length.out = m + 2)[-c(1, m + 2)], spec.name)
    end.pos <- set_names(seq(1, n, length.out = n), row.names)

    edge.pos <- tibble(x = x1, y = start.pos[start], xend = x2, yend = end.pos[end])
    node.pos <- tibble(x = rep(x1, m),
                       y = start.pos[spec.name],
                       label = spec.name)
  } else {
    if(type == "upper") {
      if(m == 1) {
        x <- 0.5 + 0.18 * n
        y <- 0.5 + 0.3 * n
      } else if(m == 2) {
        x <- c(0.5 - 0.02 * n, 0.5 + 0.2 * n)
        y <- c(0.5 + 0.46 * n, 0.5 + 0.2 * n)
      } else {
        y <- seq(0.5 + n * (1 - 0.3), 0.5 + n * 0.1, length.out = m)
        x <- seq(0.5 - 0.25 * n, 0.5 + 0.3 * n, length.out = m)
      }
    } else if(type == "lower") {
      if(m == 1) {
        x <- 0.5 + 0.82 * n
        y <- 0.5 + 0.7 * n
      } else if(m == 2) {
        x <- c(0.5 + 0.8 * n, 0.5 + 1.02 * n)
        y <- c(0.5 + 0.8 * n, 0.5 + 0.54 * n)
      } else {
        y <- seq(0.5 + n * (1 - 0.1), 0.5 + n * 0.3, length.out = m)
        x <- seq(0.5 + 0.75 * n, 0.5 + 1.3 * n, length.out = m)
      }
    }
    x <- set_names(x, spec.name)
    y <- set_names(y, spec.name)

    ## get position of env point
    xend <- n:1
    yend <- 1:n
    if(type == "upper") {
      if(show.diag) {
        xend <- xend - 1
      }
    } else {
      if(show.diag) {
        xend <- xend + 1
      }
    }
    xend <- set_names(xend, row.names)
    yend <- set_names(yend, row.names)

    ## bind postion end data
    edge.pos <- tibble::tibble(x = x[start], y = y[start],
                               xend = xend[end], yend = yend[end])
    node.pos <- tibble::tibble(x = x[spec.name],
                               y = y[spec.name],
                               label = spec.name)
  }

  structure(.Data = dplyr::bind_cols(edge.pos, data), node.pos = node.pos,
            class = c("link_tbl", class(edge.pos)))
}
