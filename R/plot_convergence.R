#' plot_convergence
#' @description bildet die Entwicklung der Score ueber mehrere Generationen ab
#' @param best_individuals tibble, mit Individuen verschiedener Generationen
#'     (keep_best())
#' @param title String: Standard 'Genetic Algorithm: Convergence', Titel fuer Plot
#' @param subtitle String, Standard '', Untertitel fuer Plot
#' @export
#' @import ggplot2
plot_convergence <- function(best_individuals,
                             title="Genetic Algorithm: Convergence",
                             subtitle=""){
    p <- best_individuals %>%
        ggplot2::ggplot(ggplot2::aes(x=GENERATION,y=SCORE)) + 
        ggplot2::geom_line(color="#686868",size=0.2) +
        ggplot2::labs(title=title,
                      subtitle=subtitle,
                      x="Generation",
                      y="Score") +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid=ggplot2::element_line(linetype="dashed",size=0.3))
    return(p)
}
