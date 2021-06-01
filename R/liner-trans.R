#' Scale continuous with liner transform
#' @title Scale continuous
#' @param from input range.
#' @param to output range.
#' @return liner trans.
#' @importFrom scales trans_new
#' @rdname liner_trans
#' @author Houyun Huang, Lei Zhou, Jian Chen, Taiyun Wei
#' @export
liner_trans <- function (from, to)
{
  force(from)
  force(to)
  nm <- paste0("liner-trans:\nfrom ",
               paste0("(", format(from[1]), "->", format(from[2]), ")"),
               " to ",
               paste0("(", format(to[1]), "->", format(to[2]), ")"))
  trans <- function(x) scales::rescale(x, from = from, to = to)
  inv <- function(x) scales::rescale(x, from = to, to = from)
  scales::trans_new(nm,
                    transform = trans,
                    inverse = inv,
                    minor_breaks = scales::regular_minor_breaks())
}

#' @noRd
reverse_liner_trans <- function (from, to)
{
  force(from)
  force(to)
  nm <- paste0("reverse-liner-trans:\nfrom ",
               paste0("(", format(from[1]), "->", format(from[2]), ")"),
               " to ",
               paste0("(", format(to[1]), "->", format(to[2]), ")"))
  trans <- function(x) - scales::rescale(x, from = from, to = to)
  inv <- function(x) - scales::rescale(x, from = to, to = from)
  scales::trans_new(nm,
                    transform = trans,
                    inverse = inv,
                    minor_breaks = scales::regular_minor_breaks(reverse = TRUE))
}
