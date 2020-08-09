#' update_phero
#' @description aktualisiert die Pheromonkonzentration zwischen den einzelnen
#'     Teilstrecken/Staedten (zuvor wurden schon Pheromone verdampft). 
#' @param phero_matrix
#' @param ants
#' @param Q
#' @param dist_matrix
#' @param min_phero
#' @param max_phero
#' @export
#' @importFrom usedist dist_get
#' @import dplyr
#' 
update_phero <- function(phero_matrix,
                         ants,
                         Q,
                         dist_matrix,
                         min_phero,
                         max_phero){
    sapply(seq(1,nrow(ants)), function(aid){
        ant <- ants %>%
            dplyr::filter(dplyr::row_number()==aid)
        idx <- embed(ant$TOUR[[1]],2) %>%
            t() %>%
            dplyr::as_tibble()
        lapply(idx,function(edge){
            i <- edge[1]
            j <- edge[2]
            val <- phero_matrix[i,j]
            score <- Q/usedist::dist_get(dist_matrix,i,j)
            phero_matrix[i,j] <<- max(min_phero,min(score,max_phero))#val +  score #ant$PHERO
            phero_matrix[j,i] <<- phero_matrix[i,j]#val +  score #ant$PHERO
        })
    })
    return(phero_matrix)
}


    
    