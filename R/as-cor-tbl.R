#' Coerce to a cor_tbl object
#' @description Functions to coerce a object to cor_tbl if possible.
#' @param x any \code{R} object.
#' @param extra.mat any other matrix-like data with same dimmsion as \code{x}.
#' @param byrow a logical value indicating whether arrange the 'spec' columns on y axis.
#' @param ... extra params passing to \code{\link[ggcor]{cor_tbl}}.
#' @return a cor_tbl object.
#' @importFrom utils modifyList
#' @rdname as_cor_tbl
#' @examples
#' cor(mtcars) %>% as_cor_tbl()
#' correlate(mtcars, cor.test = TRUE) %>% as_cor_tbl()
#' correlate(mtcars, type = "upper") %>% as_cor_tbl()
#' \dontrun{
#' ## S3 method for rcorr object
#' require(Hmisc)
#' rcorr(mtcars) %>% as_cor_tbl()
#'
#' ## S3 method for corr.test object
#' require(psych)
#' corr.test(mtcars) %>% as_cor_tbl()
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
as_cor_tbl <- function(x, ...) {
  UseMethod("as_cor_tbl")
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl matrix
as_cor_tbl.matrix <- function(x, ...) {
  check_corr(x)
  cor_tbl(corr = x, ...)
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl data.frame
as_cor_tbl.data.frame <- function(x, ...) {
  check_corr(x)
  cor_tbl(corr = x, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl correlate
as_cor_tbl.correlate <- function(x, extra.mat = list(), ...) {
  anynull <- is.null(x$lower.ci) || is.null(x$upper.ci)
  conf.ci <- if(!anynull) {
    list(upper.ci = x$upper.ci, lower.ci = x$lower.ci)
  } else list()
  extra.mat <- modifyList(extra.mat, conf.ci)
  cor_tbl(corr = x$r, p.value = x$p.value, extra.mat = extra.mat, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl rcorr
as_cor_tbl.rcorr <- function(x, ...)
{
  p.value <- x$P
  diag(p.value) <- 0
  cor_tbl(corr = x$r, p.value = p.value, ...)
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl corr.test
as_cor_tbl.corr.test <- function(x, ...)
{
  cor_tbl(corr = x$r, p.value = x$p, ...)
}
#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl mantel_tbl
as_cor_tbl.mantel_tbl <- function(x, byrow = TRUE, ...) {
  env_nm <- unique(x$env)
  spec_nm <- unique(x$spec)
  if(byrow) {
    col.names <- env_nm
    row.names <- spec_nm
    .col.names <- x$env
    .row.names <- x$spec
    .col.id <- as.integer(factor(x$env, levels = col.names))
    .row.id <- as.integer(factor(x$spec, levels = rev(row.names)))
  } else {
    col.names <- spec_nm
    row.names <- env_nm
    .col.names <- x$spec
    .row.names <- x$env
    .col.id <- as.integer(factor(x$spec, levels = col.names))
    .row.id <- as.integer(factor(x$env, levels = rev(row.names)))
  }
  df <- tibble::tibble(.col.names = .col.names, .row.names = .row.names,
                       r = x$r, p.value = x$p.value, .row.id = .row.id,
                       .col.id = .col.id) %>%
    dplyr::bind_cols(x[setdiff(names(x), c("spec", "env", "r", "p.value"))])
  structure(
    .Data = df,
    .row.names = row.names,
    .col.names = col.names,
    type = "full",
    show.diag = TRUE,
    grouped = attr(x, "grouped"),
    class = c("cor_tbl", setdiff(class(df), "mantel_tbl"))
  )
}

#' @rdname  as_cor_tbl
#' @export
#' @method as_cor_tbl pro_tbl
as_cor_tbl.pro_tbl <- function(x, byrow = TRUE, ...) {
  env_nm <- unique(x$env)
  spec_nm <- unique(x$spec)
  if(byrow) {
    col.names <- env_nm
    row.names <- spec_nm
    .col.names <- x$env
    .row.names <- x$spec
    .col.id <- as.integer(factor(x$env, levels = col.names))
    .row.id <- as.integer(factor(x$spec, levels = rev(row.names)))
  } else {
    col.names <- spec_nm
    row.names <- env_nm
    .col.names <- x$spec
    .row.names <- x$env
    .col.id <- as.integer(factor(x$spec, levels = col.names))
    .row.id <- as.integer(factor(x$env, levels = rev(row.names)))
  }
  df <- tibble::tibble(.col.names = .col.names, .row.names = .row.names,
                       r = x$r, p.value = x$p.value, .row.id = .row.id,
                       .col.id = .col.id) %>%
    dplyr::bind_cols(x[setdiff(names(x), c("spec", "env", "r", "p.value"))])
  structure(
    .Data = df,
    .row.names = row.names,
    .col.names = col.names,
    type = "full",
    show.diag = TRUE,
    grouped = attr(x, "grouped"),
    class = c("cor_tbl", setdiff(class(df), "pro_tbl"))
  )
}

#' @rdname as_cor_tbl
#' @export
#' @method as_cor_tbl default
as_cor_tbl.default <- function(x, ...) {
  stop(class(x)[1], " hasn't been realized yet.", call. = FALSE)
}

#' @noRd
check_corr <- function(x) {
  if(!is.matrix(x))
    x <- as.matrix(x)
  rng <- range(x, na.rm = TRUE)
  if(!(rng[1] >= -1 && rng[2] <= 1)) {
    stop("Not a correlation matrix.", call. = FALSE)
  }
}
