#' find_tour: 
#' @description 
#' @param ants
#' @param world
#' @param random_factor
#' @param alpha
#' @param beta
#' @export
#' @importFrom magrittr "%>%" divide_by raise_to_power
#' @importFrom usedist dist_get
#' @import dplyr
find_tour <- function(ants,
                      world,
                      random_factor,
                      alpha,
                      beta){
    # Init ----
    destinations <- world$cities$CID
    min_value <- 1e-100
    # Touren fuer alle Ameisen finden ---- 
    ants <- ants %>%
        dplyr::mutate(TOUR=lapply(ants$AID, function(ant){
            start_city <- sample(destinations,size=1)
            curr_city <- start_city
            destinations <- destinations[!destinations %in% curr_city]
            tour_len <- length(destinations)-1
            tour <- lapply(seq(1,tour_len),function(step){
                if (runif(1) <= random_factor){
                    next_city <- sample(destinations,size=1)
                } else {
                    dist_score <- 1 %>%
                        magrittr::divide_by(usedist::dist_get(world$dist_matrix,
                                                              idx1=curr_city,
                                                              idx2=destinations) %>%
                                                dplyr::if_else(.==0,min_value,.)) %>%
                        magrittr::raise_to_power(beta) %>%
                        dplyr::if_else(. < min_value, min_value,.)
                    phero_score <- world$phero_matrix[curr_city,destinations] %>%
                        magrittr::raise_to_power(alpha) %>%
                        dplyr::if_else(. < min_value, min_value,.)
                    prob <- dist_score*phero_score/sum(dist_score*phero_score)
                    next_city <- destinations %>%
                        sample(size=1,prob=prob)
                }
                curr_city <<- next_city
                destinations <<- destinations[!destinations %in% curr_city]
                return(curr_city)
            }) %>%
                unlist() %>%
                c(start_city,.,destinations)
        }))

    # Output
    return(ants)
}