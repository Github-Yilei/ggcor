#' Random forests
#' @title Random forests
#' @param spec,env a data.frame object.
#' @param byrow a logical value, if TRUE, the 'spec' on the rows.
#' @param seed a integer value.
#' @param x a rand_forest object.
#' @param ... extra parameters.
#' @return a rand_forest object.
#' @rdname random_forest
#' @examples \dontrun{
#' spec <- mtcars[c(1, 3, 4, 5)]
#' env <- mtcars[6:11]
#' random_forest(spec, env)
#' }
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
random_forest <- function(spec,
                        env,
                        byrow = TRUE,
                        seed = 123,
                        ...)
{
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)

  n <- length(spec)
  m <- length(env)
  if(any(n < 1, m < 1)) {
    stop("Zero length data.", call. = FALSE)
  }

  if(nrow(spec) != nrow(env)) {
    stop("'env' shold have the same rows as 'spec'.", call. = FALSE)
  }

  rfPermute <- get_function("rfPermute", "rfPermute")
  rp.importance <- get_function("rfPermute", "rp.importance")
  set.seed(seed)
  seeds <- as.integer(stats::runif(n) * 10000)

  explained <- vector(length = n)
  importance <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  p.value <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))

  for (i in seq_len(n)) {
    set.seed(seeds[i])
    rf <- rfPermute(spec[[i]] ~ ., data = env, importance = TRUE, ...)

    type <- rf$type
    imp <- rp.importance(rf, scale = TRUE)[names(env), , drop = FALSE]
    if(type == "classification") {
      explained[i] <- 100 - 100 * rf$err.rate[rf$ntree, "OOB"]
    } else {
      explained[i] <- 100 * rf$rsq[length(rf$rsq)]
    }
    if(type == "classification") {
      importance[i, ] <- imp[, "MeanDecreaseAccuracy"]
      p.value[i, ] <- imp[, "MeanDecreaseAccuracy.pval"]
    } else {
      importance[i, ] <- imp[, "%IncMSE"]
      p.value[i, ] <- imp[, "%IncMSE.pval"]
    }
  }

  if(isFALSE(byrow)) {
    importance <- t(importance)
    p.value <- t(p.value)
  }
  structure(.Data = list(explained = data.frame(name = names(spec),
                                                explained = explained,
                                                stringsAsFactors = FALSE),
                         importance = as.data.frame(importance),
                         p.value = as.data.frame(p.value)),
            byrow = byrow,
            class = "random_forest")
}

#' @method print random_forest
#' @rdname random_forest
#' @export
print.random_forest <- function(x, ...) {
  cat("Var explained (%):\n")
  print(x$explained)
  cat("\n")
  cat("Var importance:\n")
  print(x$importance)
  cat("\n")
  cat("Var importance p value:\n")
  print(x$p.value)
}

#' @method plot random_forest
#' @rdname random_forest
#' @importFrom graphics plot
#' @export
plot.random_forest <- function(x, ...) {
  byrow <- attr(x, "byrow")
  data <- gcor_tbl(x$importance, "importance", p.value = x$p.value)
  importance <- p.value <- explained <- name <- NULL
  p <- quickcor(data) +
    geom_colour(aes(fill = importance)) +
    geom_cross(aes(p.value = p.value))
  if(isTRUE(byrow)) {
    p <- p + anno_bar2(x$explained, aes(x = explained, y = name), fill = "#377EB8")
  } else {
    p <- p + anno_bar2(x$explained, aes(x = name, y = explained), fill = "#377EB8")
  }
  p
}
