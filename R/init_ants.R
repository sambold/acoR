#' init_ants
#' @description Initialisiert Ants der Generation 0 mit lauter fehlenden Werten.
#'     Der dataframe dient als Schema fuer die weiteren Befuellungen der nachfolgenden
#'     Generationen
#' @export
#' @import dplyr
init_ants <- function(n,
                      seed=NULL){
    
    if (!is.null(seed)) set.seed(seed)
    
    ants <- dplyr::tibble(
        AID=seq(n),
        GENERATION=0,
        TOUR=NA,
        SCORE=NA,
        PHERO=NA)
    return(ants)
}
