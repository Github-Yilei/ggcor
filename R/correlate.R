#' Matrix of Correlations, P-values and confidence intervals
#' @description \code{correlate} uses \code{cor} to find the correlations and use \code{cor.test} to find
#' the p values, confidence intervals for all possible pairs of columns ofmatrix.
#' @param x,y a matrix object or NULL.
#' @param cor.test logical, if TRUE (default) will test for correlation.
#' @param method a character string indicating which correlation coefficient is to be used
#' for the test. One of "pearson", "kendall", or "spearman".
#' @param use an optional character string giving a method for computing covariances in the presence of missing values.
#' @param p.adjust logical, if TRUE (default) will adjust p value for multiple comparisons.
#' @param p.adjust.method correction method.
#' @param ... extra params, see Details.
#' @details The columns of 'x' will be tested for each pair when y is NULL(the default),
#' otherwise each column in 'x' and each column in 'y' is tested for each pair.
#' @return a list with correlation matrix, P values matrix, confidence intervals matrix.
#' @importFrom stats cor cor.test p.adjust p.adjust.methods
#' @importFrom purrr walk2
#' @rdname corrlate
#' @examples
#' correlate(mtcars)
#' m1 <- matrix(rnorm(100), nrow = 10)
#' m2 <- matrix(rnorm(60), nrow = 10)
#'
#' ## not test for correlation matrix
#' correlate(m1, m2)
#'
#' ## test for correlation matrix
#' correlate(m1, m2, cor.test = TRUE)
#'
#' ## fast compute correlation
#' \dontrun{
#' require(WGCNA)
#' fast_correlate(m1, m2)
#'
#' require(picante)
#'   fast_correlate2(m1)
#' }
#' @seealso \code{\link[stats]{cor}}, \code{\link[stats]{cor.test}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
correlate <- function(x,
                      y = NULL,
                      cor.test = FALSE,
                      method = "pearson",
                      use = "everything",
                      p.adjust = FALSE,
                      p.adjust.method = "holm",
                      ...)
{
  missing.y <- is.null(y)
  y <- y %||% x
  if(!is.matrix(x))
    x <- as.matrix(x)
  if(!is.matrix(y))
    y <- as.matrix(y)
  n <- ncol(x)
  m <- ncol(y)
  r <- cor(x, y, use = use, method = method)
  if(isTRUE(cor.test)) {
    p.value <- lower.ci <- upper.ci <- matrix(NA, ncol = m, nrow = n)
    id <- expand.grid(1:n, 1:m)
    if(missing.y) {
      id <- id[id$Var1 > id$Var2, , drop = FALSE]
      purrr::walk2(id$Var1, id$Var2, function(.idx, .idy) {
        tmp <- cor.test(x = x[ , .idx], y = y[ , .idy], method = method, ...)
        p.value[c(.idx, .idy), c(.idy, .idx)] <<- tmp$p.value
        if(method == "pearson") {
          if (nrow(x) > 3) {
            lower.ci[c(.idx, .idy), c(.idy, .idx)] <<- tmp$conf.int[1]
            upper.ci[c(.idx, .idy), c(.idy, .idx)] <<- tmp$conf.int[2]
          } else {
            warning("correlation test interval needs 4 observations at least.", call. = FALSE)
          }
        }
      })
      diag(p.value) <- 0
      if(method == "pearson") {
        diag(lower.ci) <- diag(upper.ci) <- 1
      }
    } else {
      purrr::walk2(id$Var1, id$Var2, function(.idx, .idy) {
        tmp <- cor.test(x = x[ , .idx], y = y[ , .idy], method = method, ...)
        p.value[.idx, .idy] <<- tmp$p.value
        if(method == "pearson") {
          if (nrow(x) > 3) {
            lower.ci[.idx, .idy] <<- tmp$conf.int[1]
            upper.ci[.idx, .idy] <<- tmp$conf.int[2]
          } else {
            warning("correlation test interval needs 4 observations at least.", call. = FALSE)
          }
        }
      })
    }
  }
  if(isTRUE(cor.test)) {
    lower.ci <- if(method == "pearson") lower.ci else NULL
    upper.ci <- if(method == "pearson") upper.ci else NULL
    if(isTRUE(p.adjust)) {
      p.adjust.method <- match.arg(p.adjust.method, p.adjust.methods)
      p.value[] <- p.adjust(p.value, p.adjust.method)
    }
  } else {
    p.value <- lower.ci <- upper.ci <- NULL
  }
  structure(
    .Data = list(
      r = r,
      p.value = p.value,
      lower.ci = lower.ci,
      upper.ci = upper.ci
    ), class = "correlate"
  )
}

#' @rdname corrlate
#' @export
fast_correlate <- function(x,
                           y = NULL,
                           p.adjust = FALSE,
                           p.adjust.method = "holm",
                           use = "everything",
                           ...)
{
  if(!requireNamespace("WGCNA", quietly = TRUE)) {
    stop("'fast_correlate' needs 'WGCNA' package.", call. = FALSE)
  }
  corr <- WGCNA::corAndPvalue(x, y, use, ...)
  if(isTRUE(p.adjust)) {
    p.adjust.method <- match.arg(p.adjust.method, p.adjust.methods)
    corr$p.value <- p.adjust(corr$p.value, p.adjust.method)
  }
  structure(.Data = list(r = corr$cor, p.value = corr$p),
            class = "correlate")
}

#' @rdname corrlate
#' @export
fast_correlate2 <- function (x,
                             method = "pearson",
                             p.adjust = FALSE,
                             p.adjust.method = "holm",
                             ...)
{
  if(!requireNamespace("picante", quietly = TRUE)) {
    stop("'fast_correlate2' needs 'picante' package.", call. = FALSE)
  }
  corr <- picante::cor.table(x, method, ...)
  if(isTRUE(p.adjust)) {
    p.adjust.method <- match.arg(p.adjust.method, p.adjust.methods)
    corr$p.value <- p.adjust(corr$p.value, p.adjust.method)
  }
  structure(.Data = list(r = corr$r, p.value = corr$P),
            class = "correlate")
}

#' Print correlate object.
#' @param x a correlate object.
#' @param all if FALSE (default) just print correlation matrix, else will
#'     print all values.
#' @param ... extra params passing to \code{print}.
#' @examples
#' m <- correlate(mtcars, cor.test = TRUE)
#' print(m)
#' print(m, TRUE)
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
print.correlate <- function(x, all = FALSE, ...) {
  if(all) print(x, ...) else print(x$r, ...)
}
