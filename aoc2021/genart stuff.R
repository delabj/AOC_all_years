library(tidyverse)


#' Draw Tangent Lines
#'
#' @param start Where does the sine wave start
#' @param end where does the sine wave end
#' @param n_lines how many lines to draw?
#' @param line_alpha what alpha should be used for all lines
#' @param expand_start Where does the plot begin?
#' @param expand_end Where does the plot end?

draw_tangent_lines_to_sine <- function(
  start = -pi*4,
  stop = pi*4,
  n_lines = 1000,
  line_alpha = 1,
  expand_start = 0,
  expand_end = 0
){
  tibble(
    point=seq(start, stop, length.out = n_lines ),
    slope = cos(point),
    intercept = sin(point) - cos(point)*point
  ) %>%
    ggplot()+
    geom_abline(
      aes(slope=slope, intercept = intercept,

          #size = -slope+point^2
          ), alpha = line_alpha
    )+
    theme_void()+
    delabj::legend_none()+
    delabj::scale_color_delabj(discrete = F)+
    ylim(-5, 5)+
    xlim(start-expand_start,stop+expand_end)+
    scale_size(range = c(0,1))


}


sine <- tibble(
  x=seq(-pi*4, pi*4, length.out= 1000),
  y=sin(x)
)


draw_tangent_lines() +
  geom_line(data = sine, aes(x=x, y=y), color = "red")
