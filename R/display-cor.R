#' Display correlation matrix with nice format
#' @description Functions to display correlation object.
#' @param x any \code{R} object.
#' @param byrow a logical value indicating whether arrange the 'spec' columns on y axis.
#' @description Format an correlation matrix for printing.
#' @param corr a correlation matrix.
#' @param p.value NULL (default) or a matrix of p value.
#' @param type a string, "full" (default), "upper" or "lower", display full,
#'     lower triangular or upper triangular matrix.
#' @param show.diag a logical value indicating whether keep the diagonal.
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used, the default value is 2.
#' @param nsmall the minimum number of digits to the right of the decimal
#'     point in formatting real/complex numbers in non-scientific formats,
#'     the default value is 2.
#' @param sig.level significance level，the defaults values is [0.05, 0.01, 0.001].
#' @param mark significance mark，the defaults values is ["*", "**", "***"].
#' @param coef string to specifies which column is the coefficient when "x"
#'    is a general_cor_tbl.
#' @param nice.format a logical value indicating whether the output needs to be
#'     automatically optimized.
#' @param ... extra params passing to \code{format_cor}.
#' @return a data frame.
#' @rdname display_cor
#' @examples
#' corr <- correlate(mtcars, cor.test = TRUE)
#' format_cor(corr$r, corr$p.value, type = "lower")
#' display_cor(corr)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
display_cor <- function(x, ...) {
  UseMethod("display_cor")
}

#' @rdname  display_cor
#' @export
#' @method display_cor matrix
display_cor.matrix <- function(x, ...) {
  format_cor(corr = x, ...)
}

#' @rdname  display_cor
#' @export
#' @method display_cor data.frame
display_cor.data.frame <- function(x, ...) {
  format_cor(corr = x, ...)
}

#' @rdname  display_cor
#' @export
#' @method display_cor correlate
display_cor.correlate <- function(x, ...) {
  format_cor(corr = x$r, p.value = x$p.value, ...)
}

#' @rdname  display_cor
#' @export
#' @method display_cor rcorr
display_cor.rcorr <- function(x, ...) {
  p.value <- x$P
  diag(p.value) <- 0
  format_cor(corr = x$r, p.value = p.value, ...)
}

#' @rdname  display_cor
#' @export
#' @method display_cor corr.test
display_cor.corr.test <- function(x, ...) {
  format_cor(corr = x$r, p.value = x$p, ...)
}

#' @importFrom purrr walk map_dfr
#' @rdname  display_cor
#' @export
#' @method display_cor cor_tbl
display_cor.cor_tbl <- function(x,
                                type = "full",
                                show.diag = FALSE,
                                digits = 2,
                                nsmall = 2,
                                sig.level = c(0.05, 0.01, 0.001),
                                mark = c("*", "**", "***"),
                                coef = NULL,
                                nice.format = TRUE,
                                ...) {
  if(is_gcor_tbl(x)) {
    if(is.null(coef)) {
      stop("Must set 'coef' parameter for general_cor_tbl.", call. = FALSE)
    }
    if(!coef %in% names(x)) {
      stop("Don't find ", coef, " in data table.", call. = FALSE)
    }
    coef <- rlang::sym(coef)
    x <- dplyr::rename(x, r = !!coef)
  }
  type <- match.arg(type, c("full", "upper", "lower"))
  x <- switch (type,
    full  = x,
    upper = get_upper_data(x, show.diag),
    lower = get_lower_data(x, show.diag)
  )
  if(nrow(x) == 0)
    return()
  grouped <- attr(x, "grouped")
  row.name <- get_row_name(x)
  col.name <- get_col_name(x)
  n <- length(row.name)
  m <- length(col.name)
  corr <- format_number(x$r, digits, nsmall)
  if(nice.format) {
    corr <- ifelse(x$r >= 0, paste0("", corr), corr)
  }
  if("p.value" %in% names(x) && is.numeric(x$p.value)) {
    corr <- paste0(corr,
                   sig_mark(x$p.value, sig.level, mark))
  }
  max.len <- max(nchar(corr), na.rm = TRUE)
  if(!is.finite(max.len)) {
    nice.format <- FALSE
  }
  if(nice.format) {
    corr <- purrr::map_chr(corr, function(.corr) {
      if(!is.na(.corr) && nchar(.corr) < max.len) {
        paste0(.corr, paste0(rep_len(" ", max.len - nchar(.corr)), collapse = ""))
      } else
        .corr
    })
  }

  if(grouped) {
    ngroup <- length(unique(x$.group))

    mat <- matrix("", nrow = n * ngroup, ncol = m,
                  dimnames = list(paste(rep(unique(x$.group), each = n),
                                         rep(row.name, ngroup), sep = "-"),
                                  col.name))
    x <- split(x, x$.group)
    name <- names(x)
    x <- purrr::map_dfr(1:ngroup, function(.id) {
      x[[.id]]$.row.id <<- n - x[[.id]]$.row.id + 1 + (.id - 1) * n
      x[[.id]]
    })

    purrr::walk(1:nrow(x), function(.id) {
      mat[x$.row.id[.id], x$.col.id[.id]] <<- corr[.id]
    })
  } else {
    mat <- matrix("", nrow = n, ncol = m, dimnames = list(row.name, col.name))
    purrr::walk(1:nrow(x), function(.id) {
      mat[n - x$.row.id[.id] + 1, x$.col.id[.id]] <<- corr[.id]
    })
  }

  as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
}

#' @rdname  display_cor
#' @export
#' @method display_cor mantel_tbl
display_cor.mantel_tbl <- function(x, byrow = TRUE, ...) {
  x <- as_cor_tbl(x, byrow = byrow)
  display_cor(x, ...)
}

#' @rdname  display_cor
#' @export
#' @method display_cor pro_tbl
display_cor.pro_tbl <- function(x, byrow = TRUE, ...) {
  x <- as_cor_tbl(x, byrow = byrow)
  display_cor(x, ...)
}

#' @importFrom purrr map_chr
#' @rdname display_cor
#' @export
format_cor <- function(corr,
                       p.value = NULL,
                       type = "full",
                       show.diag = FALSE,
                       digits = 2,
                       nsmall = 2,
                       sig.level = c(0.05, 0.01, 0.001),
                       mark = c("*", "**", "***"),
                       nice.format = TRUE)
{
  if(!is.matrix(corr))
    corr <- as.matrix(corr)
  if(!is.null(p.value) && !is.matrix(p.value))
    p.value <- as.matrix(p.value)
  type <- match.arg(type, c("full", "upper", "lower"))

  idx <- corr >= 0
  corr[] <- format_number(corr, digits, nsmall)
  if(nice.format) {
    corr[] <- ifelse(idx, paste0("", corr), corr)
  }
  if(!is.null(p.value)) {
    corr[] <- paste0(corr,
                     sig_mark(p.value, sig.level, mark))
  }

  max.len <- max(nchar(corr), na.rm = TRUE)
  if(!is.finite(max.len)) {
    warning("Don't have finite value.", call. = FALSE)
    nice.format <- FALSE
  }

  if(nice.format) {
    nn <- nchar(corr)
    corr[] <- purrr::map_chr(1:length(nn), function(.idx) {
      if(nn[.idx] < max.len) {
        paste0(corr[.idx], paste0(rep_len(" ", max.len - nn[.idx]), collapse = ""))
      } else
        corr[.idx]
    })
  }

  if(type == "upper") {
    corr[lower.tri(corr, !show.diag)] <- ""
  }
  if(type == "lower") {
    corr[upper.tri(corr, !show.diag)] <- ""
  }
  as.data.frame(corr, stringsAsFactors = FALSE, check.names = FALSE)
}

