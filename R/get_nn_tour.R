#' get_nn_tour
#' @description findet nearest neighbor tour fuer gegebenes TSP-Problem. Die 
#'     nn-Tour dient lediglich dazu, um min-/max-Pherowerte fuer MMAS zu bestimmen
#' @param destinations
#' @param dist_matrix
#' @export
#' @importFrom usedist dist_get
get_nn_tour <- function(destinations=world$cities$CID,
                        dist_matrix=world$dist_matrix){
    start_city <- sample(destinations,size=1)
    curr_city <- start_city
    destinations <- destinations[!destinations %in% curr_city]
    tour_len <- length(destinations)
    tour <- lapply(seq(1,tour_len),function(step){
        #print(glue::glue("Step: {step} - Current City: {curr_city}"))
        if (length(destinations)==1){
            next_city <- destinations
        } else {
            dmmy <- usedist::dist_get(dist_matrix,
                                      idx1=curr_city,
                                      idx2=destinations) 
            next_city <- destinations[which.min(dmmy)]
        }
        curr_city <<- next_city
        destinations <<- destinations[!destinations %in% curr_city]
        return(curr_city)
    }) %>%
        unlist() %>%
        c(start_city,.)
    return(tour)
}