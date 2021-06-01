#' @importFrom tibble as_tibble
#' @export
as_tibble.cor_tbl <- function(x, ...)
{
  class(x) <- setdiff(class(x), "cor_tbl")
  attrs <- attributes(x)
  excludes <- attrs[setdiff(names(attrs), c("names", "class", "row.names"))]
  if(length(excludes) > 0) {
    for (nm in names(excludes)) {
      attr(x, nm) <- NULL
    }
  }
  x
}

#' @importFrom dplyr filter
filter.cor_tbl <- function(.data, ...)
{
  attrs <- attributes(.data)
  .data <- filter(as_tibble(.data), ...)
  set_attrs(.data, attrs, .excludes = c("names", "row.names"))
}

#' @importFrom dplyr mutate
#' @export
mutate.cor_tbl <- function(.data, ...)
{
  attrs <- attributes(.data)
  .data <- dplyr::mutate(as_tibble(.data), ...)
  set_attrs(.data, attrs, .excludes = c("names", "row.names"))
}

#' @importFrom dplyr group_by
#' @export
group_by.cor_tbl <- function(.data, add = FALSE, ...)
{
  attrs <- attributes(.data)
  .data <- group_by(as_tibble(.data), add = FALSE)
  structure(.Data = .data,
            class = c("grouped_cor_tbl", class(.data)),
            attrs = attrs)
}

#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_cor_tbl <- function(x, ...)
{
  attrs <- attr(x, "attrs")
  class(x) <- setdiff(class(x), "grouped_cor_tbl")
  x <- ungroup(x, ...)
  class(.data) <- c("grouped_cor_tbl", class(.data))
  set_attrs(.data, attrs, .excludes = c("names", "row.names"))
}
#' @noRd
set_attrs <- function(.data, .attrs = list(), .excludes = NULL)
{
  .excludes <- .excludes %||% c("names", "class", "row.names")
  new.attrs <- .attrs[setdiff(names(.attrs), .excludes)]
  if(length(new.attrs) > 0) {
    for (nm in names(new.attrs)) {
      attr(.data, nm) <- new.attrs[[nm]]
    }
  }
  .data
}
#' @noRd
remove_attrs <- function(.data, .excludes = NULL)
{
  attrs <- attributes(.data)
  .excludes <- .excludes %||% c("names", "class", "row.names")
  rm.attr.name <- setdiff(names(attrs), .excludes)
  if(length(rm.attr.name) > 0) {
    for (nm in rm.attr.name) {
      attr(.data, nm) <- NULL
    }
  }
  .data
}

## extended dplyr for network
#' @importFrom dplyr filter
#' @export
filter.cor_network <- function(.data,
                               ...,
                               what = "nodes",
                               simplify = TRUE)
{
  nodes <- .data$nodes
  edges <- .data$edges
  if(what == "nodes") {
    nodes <- filter(nodes, ...)
    e.id <- edges$from %in% nodes$name &
      edges$to   %in% nodes$name
    edges <- filter(edges, e.id)
  } else {
    edges <- filter(edges, ...)
    if(simplify) {
      n.id <- nodes$name %in% c(edges$from, edges$to)
      nodes <- filter(nodes, n.id)
    }
  }
  structure(.Data = list(nodes = nodes,
                         edges = edges), class = "cor_network")
}

#' @importFrom dplyr mutate
#' @export
mutate.cor_network <- function(.data, what = "nodes", ...)
{
  check_mutate_var_name(what, ...)
  if(what == "nodes") {
    .data$nodes <- dplyr::mutate(.data$nodes, ...)
  } else {
    .data$edges <- dplyr::mutate(.data$edges, ...)
  }
  .data
}

#' @noRd
check_mutate_var_name <- function(what = "nodes", ...) {
  var.name <- list(...)
  if(what == "nodes") {
    if("name" %in% var.name) {
      stop("variable of 'name' is preserved.", call. = FALSE)
    }
  } else {
    if(any(c("from", "to") %in% var.name)) {
      stop("variable of 'from' and 'to' are preserved.", call. = FALSE)
    }
  }
}
