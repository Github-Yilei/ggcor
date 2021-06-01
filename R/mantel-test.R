#' Mantel and partial mantel test for dissimilarity matrices
#' @title Mantel test
#' @param spec,env data frame object.
#' @param group vector for grouping the rows.
#' @param env.ctrl NULL (default), data frame.
#' @param mantel.fun string, function of mantel test.
#'    \itemize{
#'      \item{\code{"mantel"} will use \code{vegan::mantel()} (default).}
#'      \item{\code{"mantel.randtest"} will use \code{ade4::mantel.randtest()}.}
#'      \item{\code{"mantel.rtest"} will use \code{ade4::mantel.rtest()}.}
#'      \item{\code{"mantel.partial"} will use \code{vegan::mantel.partial()} (default).}
#'   }
#' @param spec.select,env.select NULL (default), numeric or character vector index of columns.
#' @param use one of "everything", "complete" or "pairwise".
#' @param spec.dist.method dissimilarity index (default is 'bray'), passing to \code{method}
#'     params of \code{vegan::vegdist}.
#' @param env.dist.method dissimilarity index (default is euclidean'), passing to \code{method}
#'     params of \code{vegan::vegdist()}.
#' @param seed a integer value.
#' @param ... extra params passing to \code{mantel.fun}.
#' @return a data.frame.
#' @importFrom dplyr %>% mutate
#' @importFrom purrr pmap pmap_dfr
#' @importFrom stats complete.cases runif
#' @rdname mantel_test
#' @examples \dontrun{
#' library(vegan)
#' data("varespec")
#' data("varechem")
#' mantel_test(varespec, varechem,
#'   spec.select = list(spec01 = 1:5, spec02 = 6:12))
#' mantel_test(varespec, varechem,
#'   spec.select = list(spec01 = 1:5, spec02 = 6:12),
#'   env.select = list(env01 = 1:5, env02 = 6:10, env03 = 11:14))
#' set.seed(20191224)
#' sam_grp <- sample(paste0("sample", 1:3), 24, replace = TRUE)
#' mantel_test(varespec, varechem, group = sam_grp)
#' }
#' @seealso \code{\link{mantel_test}}.
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
mantel_test <- function(spec,
                        env,
                        group = NULL,
                        env.ctrl = NULL, # named list if grouped
                        mantel.fun = "mantel",
                        spec.select = NULL, # a list of index vector
                        env.select = NULL,
                        use = "everything",
                        spec.dist.method = "bray",
                        env.dist.method = "euclidean",
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
  if(mantel.fun == "mantel.partial") {
    if(is.null(env.ctrl))
      stop("Did you forget to set the 'env.ctrl' param?", call. = FALSE)
    if(!is.data.frame(env.ctrl) && !is.list(env.ctrl))
      stop("'env.ctrl' needs a list or data.frame.", call. = FALSE)
  }
  if(!is.null(group)) {
    if(length(group) != nrow(spec))
      stop("Length of 'group' and rows of 'spec' must be same.", call. = FALSE)
    spec <- split(spec, group, drop = FALSE)
    env <- split(env, group, drop = FALSE)
    if(mantel.fun == "mantel.partial") {
      if(is.data.frame(env.ctrl)) {
        env.ctrl <- rep_len(list(env.ctrl), length(names(spec)))
      } else {
        env.ctrl <- env.ctrl[names(spec)]
      }
    } else {
      env.ctrl <- as.list(rep(NA, length(names(spec))))
    }
    df <- suppressMessages(
      purrr::pmap_dfr(
        list(spec, env, env.ctrl, as.list(names(spec))),
        function(.spec, .env, .env.ctrl, .group) {
          .mantel_test(spec = .spec, env = .env, env.ctrl = .env.ctrl,
                       mantel.fun = mantel.fun, spec.select = spec.select,
                       env.select = env.select, spec.dist.method = spec.dist.method,
                       env.dist.method = env.dist.method, use = use, seed = seed, ...) %>%
            dplyr::mutate(.group = .group)
          })
      )
  } else {
    df <- .mantel_test(spec = spec, env = env, env.ctrl = env.ctrl,
                       mantel.fun = mantel.fun, spec.select = spec.select,
                       env.select = env.select, spec.dist.method = spec.dist.method,
                       env.dist.method = env.dist.method, use = use, seed = seed, ...)
  }
  grouped <- if(!is.null(group)) TRUE else FALSE
  attr(df, "grouped") <- grouped
  df
}

#' @noRd
.mantel_test <- function(spec,
                        env,
                        env.ctrl = NULL, # named list if grouped
                        mantel.fun = "mantel",
                        spec.select = NULL, # a list of index vector
                        env.select = NULL,
                        use = "everything",
                        spec.dist.method = "bray",
                        env.dist.method = "euclidean",
                        seed = 123,
                        ...)
{
  .FUN <- switch (mantel.fun,
    mantel = get_function("vegan", "mantel"),
    mantel.partial = get_function("vegan", "mantel.partial"),
    mantel.randtest = get_function("ade4", "mantel.randtest"),
    mantel.rtest = get_function("ade4", "mantel.rtest"),
    stop("Invalid 'mantel.fun' parameter.", call. = FALSE)
  )
  use <- match.arg(use, c("everything", "complete", "pairwise"))

  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)
  if(nrow(spec) != nrow(env)) {
    stop("'spec' must have the same rows as 'env'.", call. = FALSE)
  }
  if(mantel.fun == "mantel.partial") {
    if(is.null(env.ctrl))
      stop("Did you forget to set the 'env.ctrl' param?", call. = FALSE)
    if(!is.data.frame(env.ctrl))
      env.ctrl <- as.data.frame(env.ctrl)
  }
  if(!is.list(spec.select) && !is.null(spec.select))
    stop("'spec.select' needs a list or NULL.", call. = FALSE)
  if(!is.list(env.select) && !is.null(env.select))
    stop("'env.select' needs a list or NULL.", call. = FALSE)
  if(is.null(spec.select)) {
    spec.select <- list(spec = 1:ncol(spec))
  }
  if(is.null(env.select)) {
    env.select <- as.list(setNames(1:ncol(env), names(env)))
  }

  if(use == "complete") {
    non.na <- complete.cases(spec) & complete.cases(env)
    if(mantel.fun == "mantel.partial") {
      non.na <- non.na & complete.cases(env.ctrl)
    }
    spec <- spec[non.na, , drop = FALSE]
    env <- env[non.na, , drop = FALSE]
    if(mantel.fun == "mantel.partial") {
      env.ctrl <- env.ctrl[non.na, , drop = FALSE]
    }
  }

  spec.select <- make_list_names(spec.select, "spec")
  env.select <- make_list_names(env.select, "env")
  spec.name <- rep(names(spec.select), each = length(env.select))
  env.name <- rep(names(env.select), length(spec.select))

  set.seed(seed)
  seeds <- 10000 * round(runif(length(spec.name)))

  spec <- purrr::map(spec.select, function(.x) {
    subset(spec, select = .x, drop = FALSE)})
  env <- purrr::map(env.select, function(.x) {
    subset(env, select = .x, drop = FALSE)})

  rp <- purrr::pmap(list(spec.name, env.name, seeds), function(.x, .y, .seed) {
    .spec <- spec[[.x]]
    .env <- env[[.y]]
    if(use == "pairwise") {
      .non.na <- complete.cases(.spec) & complete.cases(.env)
      if(mantel.fun == "mantel.partial") {
        .non.na <- .non.na & complete.cases(env.ctrl)
      }
      .spec <- .spec[.non.na, , drop = FALSE]
      .env <- .env[.non.na, , drop = FALSE]
      if(mantel.fun == "mantel.partial") {
        env.ctrl <- env.ctrl[.non.na, , drop = FALSE]
      }
    }
    spec.dist <- vegan::vegdist(.spec, method = spec.dist.method)
    env.dist <- vegan::vegdist(.env, method = env.dist.method)

    set.seed(.seed)
    if(mantel.fun == "mantel.partial") {
      env.ctrl.dist <- vegan::vegdist(env.ctrl, method = env.dist.method)
      .FUN(spec.dist, env.dist, env.ctrl.dist, ...)
    } else {
      .FUN(spec.dist, env.dist, ...)
    }
  }) %>% extract_mantel(mantel.fun)

    structure(.Data = tibble::tibble(spec = spec.name,
                                     env = env.name,
                                     r = rp$r,
                                     p.value = rp$p.value),
              grouped = FALSE,
              class = c("mantel_tbl", "tbl_df", "tbl", "data.frame"))
}

#' @importFrom purrr map_dbl
#' @noRd
extract_mantel <- function(x, .f = "mantel") {
  .f <- match.arg(.f, c("mantel", "mantel.partial",
                        "mantel.randtest", "mantel.rtest"))
  if(.f %in% c("mantel", "mantel.partial")) {
    r <- purrr::map_dbl(x, `[[`, "statistic")
    p.value <- purrr::map_dbl(x, `[[`, "signif")
  } else {
    r <- purrr::map_dbl(x, `[[`, "obs")
    p.value <- purrr::map_dbl(x, `[[`, "pvalue")
  }
  list(r = r, p.value = p.value)
}
