#' Summarises by unit for graph.
#' @param data dataset containing admission and discharge info one row per patient.
#' @param expected_name name of the variable containing expected number of deaths
#' @import dplyr
#' @noRd
smr_graph <- function(by_split, expected_name, title = "",
                      max = NA, automatic_y_lim = FALSE) {
  custom_colours <- c("#48a9dd", "#004c77", "#7fbc41", "#4d9221", "#F8766D")
  by_split <- by_split %>%
    mutate(
      expected = get(expected_name),
      smr = n_dead / expected
    )

  if(is.na(max)){
    # Deciding max based on data
    max <- ceiling(max(by_split$expected, na.rm = T))
    max <- ifelse(!is.na(max) & !is.infinite(max), max, 10)
  }
  ci <- data.frame(id = seq(1, max, 1)) %>%
    mutate(
      lower_95 = ((qpois(0.025, lambda = id) - 0.975) / id),
      upper_95 = ((qpois(0.975, lambda = id) - 0.025) / id)
    )

  smr <- by_split %>%
    ggplot(aes(x = expected, y = smr)) +
    geom_point(size = 1.5) +
    scale_color_manual(values = c(custom_colours[2], custom_colours[5])) +
    geom_line(aes(y = 1, linetype = "dashed"), color = custom_colours[1]) +
    geom_line(data = ci, aes(x = id, y = lower_95, linetype = "solid"),
              color = custom_colours[1]) +
    geom_line(data = ci, aes(x = id, y = upper_95, linetype = "solid"),
              color = custom_colours[1]) +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      labels = c("Mean SMR", "95% CI"),
      name = NULL
    ) +
    labs(y = "SMR", x = "Expected number of deaths") +
    scale_x_continuous(
      limits = c(0, max),  # Set the x-axis limits
      breaks = seq(0, max, by = max/4)  # Set the x-axis tick positions
    ) +
    ggtitle(title) +
    theme_classic() +
    theme(
      legend.justification = "center", legend.margin = margin(-5, 0, 0, 0),
      legend.box = "vertical", legend.box.just = "center",
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent')
    )

  # Adding pre-defined limit if required for shared plots.
  if(automatic_y_lim){
    smr <- smr +
      scale_y_continuous(
        limits = c(0.25, 3),  # Set the y-axis limits
        breaks = seq(0.25, 3, by = 0.5)  # Set the y-axis tick positions
      )
  }

  smr
}
