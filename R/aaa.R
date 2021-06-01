#' @noRd
get_function <- function(pkg, fun) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(pkg, " package has not been installed", call. = FALSE)
  }
  eval(parse(text = paste0(pkg, "::", fun)))
}

#' @importFrom ggnewscale new_scale
#' @noRd
new_scales <- function(...) {
  scales <- list(...)
  if(length(scales) == 0) 
    return(NULL)
  lapply(scales, new_scale)
}
