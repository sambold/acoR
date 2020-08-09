#' plot_genes
#' @description plottet die Gene (init_genes()) als Punkte. Optional kann ein
#'     bestimmtes Chromosome mit uebergeben werden. Die Informationen des 
#'     Chromosome werden dann genutzt, um einen Pfad zu zeichnen. 
#' @param genes tibble mit Informationen zu Genen (init_genes)
#' @param chromosome Numeric, Vektor mit IDs der Gene in einer bestimmten 
#'     Reihenfolge, die zur Darstellung des Pfades verwendet wird.
#' @param title String, Standard "Genetic Algorithm", Titel fuer Plot
#' @param subtitle String, Standard "", Untertitel fuer Plot
#' @export
#' @import dplyr
#' @import ggplot2
#' @import ggsci
plot_genes <- function(genes,
                       chromosome=NULL,
                       title="Genetic Algorithm",
                       subtitle=""){
    
    if (is.null(chromosome)) {
        order <- genes$CID %>%
            rep(length.out=length(.)+1)
    } else {
        order <- chromosome %>%
            rep(length.out=length(.)+1)
    }
    
    p <- genes[order,] %>%
        dplyr::mutate(POINT_COL=dplyr::case_when(STARTING_POINT ~ "STARTING_POINT",
                                                 CAPITOL ~ "CAPITOL",
                                                 TRUE ~ "")) %>%
        ggplot2::ggplot(ggplot2::aes(x=X,y=Y,color=POINT_COL)) +
        ggplot2::geom_point(size=1,alpha=1) +
        ggplot2::scale_color_manual(labels=c("Start",
                                             "Capitol",
                                             "normaler Punkt"),
                                    values=ggsci::pal_d3(alpha=0.5)(3)) 
        # Beschriftungen 
    if (!is.null(chromosome)){
        p <- p +
            ggplot2::geom_path(color="#686868",alpha=0.5,size=0.2)
    }
    p <- p +
        ggplot2::labs(title=title,
                      subtitle=subtitle,
                      x="",
                      y="") +
        ggplot2::guides(color=F) +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid = ggplot2::element_line(linetype="dashed",
                                                          size=0.5))
    return(p)
}
