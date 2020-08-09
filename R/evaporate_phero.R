#' evaporate_phero: Anteil an Pheromonen auf Teilstuecken der Route verdampfen
#' @description Verdampft Pheromone auf Teilstuecken zwischen allen 
#'     Staedten im definierten Ausmass (evaporation rate)
#' @param phero_matrix matrix, enthaelt den zur Generation g aktuellsten Stand
#'     an Pheromonen fuer die einzelnen Teilstrecken
#' @param evaporation_rate numberic, Anteil der vorhandenen Pheromone, die 
#'     verdampft werden sollen 
#' @export
evaporate_phero <- function(phero_matrix, evaporation_rate){
    res <- (1-evaporation_rate)*phero_matrix
    return(res)
}