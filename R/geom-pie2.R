#' @rdname geom_ring
#' @export
geom_pie2 <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      remain.fill = NA,
                      end.radius = 0.5,
                      steps = 0.1,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  geom_ring(mapping = mapping, data = data,
            stat = stat, position = position,
            ...,
            remain.fill = remain.fill,
            start.radius = 0,
            end.radius = end.radius,
            steps = steps,
            na.rm = na.rm,
            show.legend = show.legend,
            inherit.aes = inherit.aes)
}
