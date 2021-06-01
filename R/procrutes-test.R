#' Procrutes test for dissimilarity matrices
#' @description Perform procrutes test quickly and tidy up the data to
#' data frame.
#' @param spec,env data frame object.
#' @param group vector for rows grouping.
#' @param procrutes.fun string, name of procrutes test function.
#'    \itemize{
#'      \item{\code{"protest"} will use \code{vegan::protest()} (default).}
#'      \item{\code{"procuste.randtest"} will use \code{ade4::procuste.randtest()}.}
#'      \item{\code{"procuste.rtest"} will use \code{ade4::procuste.rtest()}.}
#'   }
#' @param spec.select,env.select NULL (default), numeric or character vector index of columns.
#' @param use one of "everything", "complete" or "pairwise".
#' @param spec.pre.fun,env.pre.fun string, function name of transform the input data.
#' @param spec.pre.params,env.pre.params list, extra parameters for \code{spec/env.pre.fun}.
#' @param seed a integer value.
#' @param ... extra params passing to \code{procrutes.fun}.
#' @return a data frame.
#' @importFrom dplyr %>% mutate
#' @importFrom purrr map pmap pmap_dfr
#' @rdname procrutes_test
#' @examples \dontrun{
#' library(vegan)
#' data("varespec")
#' data("varechem")
#' procrutes_test(varespec, varechem)
#' procrutes_test(varespec, varechem, procrutes.fun = "procuste.randtest")
#' procrutes_test(varespec, varechem,
#'             spec.select = list(spec01 = 1:6, spec02 = 7:12))
#' procrutes_test(varespec, varechem, spec.pre.fun = "mono_mds",
#'             spec.select = list(spec01 = 1:6, spec02 = 7:12),
#'             env.select = list(env01 = 1:4, env02 = 5:14))
#' set.seed(20191224)
#' sam_grp <- sample(paste0("sample", 1:3), 24, replace = TRUE)
#' procrutes_test(varespec, varechem, group = sam_grp)
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
procrutes_test <- function(spec,
                           env,
                           group = NULL,
                           procrutes.fun = "protest",
                           spec.select = NULL, # a list of index vector
                           env.select = NULL,
                           use = "everything",
                           spec.pre.fun = "identity",
                           spec.pre.params = list(),
                           env.pre.fun = spec.pre.fun,
                           env.pre.params = spec.pre.params,
                           seed = 123,
                           ...)
{
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)
  if(nrow(spec) != nrow(env)) {
    stop("'spec' must have the same rows as 'env'.", call. = FALSE)
  }

  if(!is.null(group)) {
    if(length(group) != nrow(spec))
      stop("Length of 'group' and rows of 'spec' must be same.", call. = FALSE)
    spec <- split(spec, group, drop = FALSE)
    env <- split(env, group, drop = FALSE)

    df <- suppressMessages(
      purrr::pmap_dfr(
        list(spec, env, as.list(names(spec))),
        function(.spec, .env, .group) {
          .procrutes_test(spec = .spec, env = .env, procrutes.fun = procrutes.fun,
                          spec.select = spec.select, env.select = env.select,
                          spec.pre.fun = spec.pre.fun, spec.pre.params = spec.pre.params,
                          env.pre.fun = env.pre.fun, env.pre.params = env.pre.params,
                          use = use, seed = seed, ...) %>%
            dplyr::mutate(.group = .group)
          })
      )
  } else {
    df <- .procrutes_test(spec = spec, env = env, procrutes.fun = procrutes.fun,
                          spec.select = spec.select, env.select = env.select,
                          spec.pre.fun = spec.pre.fun, spec.pre.params = spec.pre.params,
                          env.pre.fun = env.pre.fun, env.pre.params = env.pre.params,
                          use = use, seed = seed, ...)
  }
  grouped <- if(!is.null(group)) TRUE else FALSE
  attr(df, "grouped") <- grouped
  df
}

#' @noRd
.procrutes_test <- function(spec,
                            env,
                            procrutes.fun = "protest",
                            spec.select = NULL,
                            env.select = NULL,
                            use = "everything",
                            spec.pre.fun = "identity",
                            spec.pre.params = list(),
                            env.pre.fun = spec.pre.fun,
                            env.pre.params = list(),
                            seed = 123,
                            ...)
{
  .FUN <- switch (procrutes.fun,
                  protest = get_function("vegan", "protest"),
                  procuste.randtest = get_function("ade4", "procuste.randtest"),
                  procuste.rtest = get_function("ade4", "procuste.rtest"),
                  stop("Invalid 'procrutes.fun' parameter.", call. = FALSE)
  )
  use <- match.arg(use, c("everything", "complete", "pairwise"))
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)
  if(nrow(spec) != nrow(env)) {
    stop("'spec' must have the same rows as 'env'.", call. = FALSE)
  }

  if(!is.list(spec.select) && !is.null(spec.select))
    stop("'spec.select' needs a list or NULL.", call. = FALSE)
  if(!is.list(env.select) && !is.null(env.select))
    stop("'env.select' needs a list or NULL.", call. = FALSE)
  if(is.null(spec.select)) {
    spec.select <- list(spec = 1:ncol(spec))
  }

  if(use == "complete") {
    non.na <- complete.cases(spec) & complete.cases(env)
    spec <- spec[non.na, , drop = FALSE]
    env <- env[non.na, , drop = FALSE]
  }

  if(is.null(env.select)) {
    env.select <- as.list(setNames(1:ncol(env), names(env)))
  }
  spec.select <- make_list_names(spec.select, "spec")
  env.select <- make_list_names(env.select, "env")
  spec.name <- rep(names(spec.select), each = length(env.select))
  env.name <- rep(names(env.select), length(spec.select))
  spec <- purrr::map(spec.select, function(.x) {
    subset(spec, select = .x, drop = FALSE)})
  env <- purrr::map(env.select, function(.x) {
    subset(env, select = .x, drop = FALSE)})

  set.seed(seed)
  seeds <- 10000 * round(runif(length(spec.name)))

  rp <- purrr::pmap(list(spec.name, env.name, seeds), function(.x, .y, .seed) {
    .spec <- spec[[.x]]
    .env <- env[[.y]]
    if(use == "pairwise") {
      .non.na <- complete.cases(.spec) & complete.cases(.env)
      .spec <- .spec[.non.na, , drop = FALSE]
      .env <- .env[.non.na, , drop = FALSE]
    }
    .x <- do.call(spec.pre.fun, modifyList(list(.spec), spec.pre.params))
    .y <- do.call(env.pre.fun, modifyList(list(.env), env.pre.params))

    if(procrutes.fun != "protest") {
      if(!is.data.frame(.x)) {
        .x <- as.data.frame(as.matrix(.x))
      }
      if(!is.data.frame(.y)) {
        .y <- as.data.frame(as.matrix(.y))
      }
    }

    set.seed(.seed)
    .FUN(.x, .y, ...)
  }) %>% extract_procrutes(procrutes.fun)

  structure(.Data = tibble::tibble(spec = spec.name,
                                   env = env.name,
                                   r = rp$r,
                                   p.value = rp$p.value),
            grouped = FALSE,
            class = c("pro_tbl", "tbl_df", "tbl", "data.frame"))
}

#' Helper functions for procrutes test
#' @description \code{mono_mds} is used to transform data by \code{vegan::monoMDS()},
#' and \code{dudi_pca} is used to transform data by \code{ade4::dudi.pca()}.
#' @param x a data frame.
#' @param method the distance measure to be used.
#' @param ... extra parameters
#' @return a matrix.
#' @rdname procrutes_helper
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
mono_mds <- function(x,
                     method="bray",
                     ...) {
  vegdist <- get_function("vegan", "vegdist")
  monoMDS <- get_function("vegan", "monoMDS")
  d <- vegdist(x, method = method)
  monoMDS(d, ...)
}
#' @rdname procrutes_helper
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
dudi_pca <- function(x, ...) {
  dudi.pca <- get_function("ade4", "dudi.pca")
  pca <- dudi.pca(df = x, scannf = FALSE, ...)
  pca$tab
}

#' @importFrom purrr map_dbl
#' @noRd
extract_procrutes <- function(x, .f = "procrutes") {
  .f <- match.arg(.f, c("protest", "procuste.randtest", "procuste.rtest"))
  if(.f == "protest") {
    r <- purrr::map_dbl(x, `[[`, "t0")
    p.value <- purrr::map_dbl(x, `[[`, "signif")
  } else {
    r <- purrr::map_dbl(x, `[[`, "obs")
    p.value <- purrr::map_dbl(x, `[[`, "pvalue")
  }
  list(r = r, p.value = p.value)
}
