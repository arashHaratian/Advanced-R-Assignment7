#' visualize airport delays
#'
#' This function visualizes the mean departure delay of different airports in US on the map
#'
#' @return a `ggplot2` plot
#' @importFrom stats na.omit
#' @export
visualize_airport_delays <- function() {
  fig <- nycflights13::flights %>%
    na.omit() %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise("mean_dep_delay" = mean(dep_delay)) %>%
    dplyr::inner_join(nycflights13::airports, c("dest" = "faa")) %>%
    dplyr::select("lat", "lon", "mean_dep_delay") %>%
    ggplot2::ggplot() +
    ggplot2::geom_polygon(data = ggplot2::map_data("usa"),
                          ggplot2::aes_string(x="long", y = "lat", group = "group"),
                          alpha = 0.8) +
    ggplot2::geom_point(ggplot2::aes_string("lon", "lat", colour = "mean_dep_delay")) +
    ggplot2::labs(x = "longitude", y = "latitude", color = "mean delay of \nflights (in minutes)")

  return(fig)
}



