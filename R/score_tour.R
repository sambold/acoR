#' score_tour
#' @description bewertet die gefundenen Touren der einzelnen Ants
#' @param ants
#' @param world
#' @export
#' @importFrom usedist dist_get
#' @import dplyr
score_tour <- function(ants,
                       world){
    # Touren der Ameisen bewerten ----
    ants <- ants %>%
        dplyr::mutate(
            SCORE=lapply(AID,function(ant){
                tour <- ants$TOUR[[ant]] %>%
                    c(.,.[1]) %>%
                    embed(dimension=2)
                score <- usedist::dist_get(world$dist_matrix,
                                           idx1=tour[,2],
                                           idx2=tour[,1]) %>%
                    sum()
                return(score)
                }) %>%
                unlist())
    return(ants)
}