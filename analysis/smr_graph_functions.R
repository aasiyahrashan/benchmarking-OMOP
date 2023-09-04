# Setting a default theme
#' @param graph A ggplot object to be formatted
#' @import ggplot2
#' @noRd
custom_theme <- function(graph){
  formatted_graph <- graph +
    theme_classic() +
    theme(axis.text.x = element_text(color = "azure4", size = 8),
          axis.text.y = element_text(color = "azure4"),
          axis.title.x = element_text(color = "azure4", size = 10),
          axis.title.y = element_text(color = "azure4", size = 10),
          axis.line = element_line(color = "azure4"),
          plot.title = element_text(color = "azure4", hjust = 0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = "italic", color = "azure4"),
          legend.position = "bottom")
  formatted_graph
}

#' Summarises by unit for graph.
#' @param data dataset containing admission and discharge info one row per patient.
#' @import dplyr
#' @noRd
smr_graph <- function(by_split, unit_id, title ="", custom_colours){

  custom_colours <- c("#48a9dd", "#004c77", "#7fbc41", "#4d9221", "#F8766D")
  by_split <- by_split %>%
    mutate(expected = expected_ap2,
           smr = n_dead/expected)

  # Dealing with case where expected N is empty.
  max <- ceiling(max(by_split$total, na.rm = T))
  max <- ifelse(!is.na(max) & !is.infinite(max), max, 10)
  ci <- data.frame(id=seq(1, max, 1)) %>%
    mutate(lower_95 = ((qpois(0.025,lambda = id) - 0.975)/id),
           upper_95 = ((qpois(0.975,lambda = id) - 0.025)/id))

  smr <- by_split %>%
    ggplot(aes(x=total, y= smr)) +
    geom_point(size = 1.5) +
    scale_color_manual(values = c(custom_colours[2], custom_colours[5])) +
    geom_line(aes(y = 1, linetype = "dashed"), color = custom_colours[1]) +
    geom_line(data = ci, aes(x=id, y=lower_95, linetype = "solid"), color = custom_colours[1]) +
    geom_line(data = ci, aes(x=id, y=upper_95, linetype = "solid"), color = custom_colours[1]) +
    scale_linetype_manual(values = c("dashed", "solid"),
                          labels = c( "Mean SMR", "95% CI")) +
    labs(y = "SMR", x = 'Number of encounters') +
    ggtitle(title)

  custom_theme(smr) +
    theme(legend.justification = 'center', legend.margin = margin(-5, 0, 0, 0),
          legend.box = 'vertical', legend.box.just = 'center')
}

