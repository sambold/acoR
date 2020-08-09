#' start_mmas: startet mmas, um TSP-Problem zu loesen
#' 
#' @description Wrapper, um alle mmas-Einzelfunktionen miteinander in Verbindung
#'     zu setzen und TSP-Problemloesung zu starten
#' @param alpha numeric, Standard = 9, Faktor, um Bedeutung von Pheromonen (im
#'     Verhaeltnis zur Distanz) bei der Suche nach der naechsten Stadt festzulegen
#' @param beta numeric, Standard = 12, Faktor, um Bedeutung von Distanz (im 
#'     Verhaeltnis zu Pheromonen) bei der Suche nach der naechsten Stadt festzulegen
#' @param evaporation_rate numeric, Standard = 0.25, Wert zwischen 0 und 1. Gibt 
#'     an, welcher Anteil an Pheromonen jeden Zug verdampft. In Literatur: rho
#' @param Q numeric, Standard = 2, Anzahl der Pheromone, die auf Teilabschnitt 
#'     (zwischen zwei Staedten) verteilt werden koennen. Je kuerzer die Strecke,
#'     desto hoeher die verteilten Pheormone.
#'     gesamten Stecke zur Initialisierung verteilt werden
#' @param ant_factor numeric, Standardwert = 0.8, Verhaeltnis zwischen Anzahl 
#'     der Staedte und Anzahl der Ameisen (0.8 bedeutet, dass bei 100 Staedten, 
#'     80 Ameisen ausschwaermen)
#' @param random_factor numeric, Standardwert = 0.3, Wert zwischen 0 und 1. 
#'     Bestimmt, ob naechste Stadt in Tour rein zufaellig gewaehlt wird oder durch
#'     die Verbindung von Distanz und Pheromondichte
#' @param max_generations numeric, Standardwert = 1000, Anzahl maximal zu 
#'     berechnender Generationen. Sobald das Maximum erreicht ist, werden keine
#'     neuen Ameisen mehr ausgeschickt.
#' @param local_mina_thld numeric, Standardwert 10% der Anzahl maximaler Generationen.
#'     Sobald die gleiche Tour n Mal hintereinander auftaucht und der Threshold 
#'     erreicht wird, bricht die Suche ab und es wurde ein neues lokales Minimum
#'     gefunden
#' @param print_info Boolean, Standardwert = T, gibt an, ob Information ueber
#'     die aktuelle Runde ausgegeben werden soll oder nicht
#' @param print_plot Boolean, Standardwert = T, gibt an, ob Plot zur aktuellen
#'     Runde ausgegeben werden soll oder nicht
#' @param fname String, Standardwert = "data/dj38.tsp", gibt an welcher 
#'     Inputdatenbestand zur Berechnung verwendet werden soll. Wenn NULL, werden
#'     n zufaellige Staedte gewaehlt.
#' @param max_phero numeric, maximaler Wert an Pheromonen, der auf Tourabschnitt
#'     zwischen zwei Staedten sein darf. Werte, die ueber diesen Wert hinausgehen,
#'     werden automatisch gedeckelt. Wenn NULL wird Standardwert zur Berechnung
#'     herangezogen: 1/(evaporation_rate*nn_score/10)
#' @param min_phero numeric, minimaler Wert an Pheromonen, der auf Tourabschnitt
#'     zwischen zwei Staedten sein muss. Werte, die unter diesen Wert liegen,
#'     werden automatisch aufgestockt. Wenn NULL wird Standardwert zur Berechnung
#'     herangezogen: max_phero/(nrow(world$cities))
#' @param ants_n numeric, Standardwert = NULL, absoluter Wert an Ameisen, die 
#'     nach Tourloesung suchen sollen. Wenn NULL, floor(ant_factor*nrow(world$cities))
#' @param local_min_prob numeric, Standardwert = NULL, Wert zwischen 0 und 1. 
#'     Gibt die Wahrscheinlichkeit an, dass in die Liste bester Tours das lokale
#'     Optimum (der jeweiligen Generation) einfliesst, obwohl das globale Optimum
#' @export
#' @importFrom magrittr "%>%" is_greater_than
#' @importFrom glue glue
#' @import dplyr
start_mmas <- function(fname="dj38.tsp",
                       ants_n=NULL,
                       ant_factor=0.8,
                       alpha=9,
                       beta=12,
                       Q=2,
                       evaporation_rate=0.25,
                       random_factor=0.03,
                       max_generations=1000,
                       local_min_thld=NULL,
                       max_phero=NULL,
                       min_phero=NULL,
                       local_min_prob=NULL,
                       print_info=TRUE,
                       print_plot=TRUE){
    
    # Init ----
    `%>%` <- magrittr::`%>%`
    fun_list <- list.files("R",full.names=T)
    sapply(fun_list, source)
    
    # MMAS initialisieren ----
    # Timer initialisieren ####
    start <- Sys.time()
    # Welt initialisieren ####
    world <- init_world(fname=file.path("data",fname))
    # Defaultwerte fuer Parameter setzen, falls nicht explizit definiert ####
    if (is.null(ants_n)) ants_n <- floor(ant_factor*nrow(world$cities))
    if (is.null(local_min_thld)) local_min_thld <- floor(max_generations*0.1)
    if (is.null(min_phero) | is.null(max_phero)){
        nn_score <- dplyr::tibble(AID=1,
                                  GENERATION=0,
                                  TOUR=get_nn_tour(destinations=world$cities$CID,
                                                   dist_matrix=world$dist_matrix) %>%
                                      list(),
                                  SCORE=0,
                                  PHERO=0) %>%
            score_tour(world=world) %>%
            .$SCORE
        if (is.null(max_phero)) max_phero <- 1/(evaporation_rate*nn_score/10)
        if (is.null(min_phero)) min_phero <- max_phero/(nrow(world$cities))
        thld_dmmy <- c(min_phero, max_phero)
        min_phero <- min(thld_dmmy)
        max_phero <- max(thld_dmmy)
    }
    if (is.null(local_min_prob)) local_min_prob <- 0.2
    # Ameisen initiieren ####
    ants <- init_ants(n=ants_n)
    # Konstanten setzen
    local_min_counter <- 1
    is_local_min <- FALSE
    best_tour <- ants %>%
        dplyr::filter(1==2) %>%
        dplyr::select(GENERATION,TOUR,SCORE)
    
    # Ameisen aussenden ----
    for (generation in 1:max_generations){
        Sys.sleep(0.001)
        
        ants <- ants %>%
            find_tour(world=world,
                      random_factor=random_factor,
                      alpha=alpha,
                      beta=beta) %>%
            score_tour(world=world) %>%
            dplyr::mutate(GENERATION=generation, 
                          PHERO=Q/SCORE)
        
        # Beste Tour der Runde speichern ####
        best_tour <- ants %>%
            dplyr::filter(AID==which.min(SCORE)) %>%
            dplyr::select(GENERATION,TOUR,SCORE) %>%
            dplyr::bind_rows(best_tour)
        
        # Tour fuer Update waehlen ####
        # immer global beste Tour verwenden (mit einer Chance p, dass auch ein
        # schlechteres, lokales Optimum (erste Zeile aus best_tour) verwendet werden kann)
        best_row <- which.min(best_tour$SCORE)
        if (runif(1) <= local_min_prob) best_row <- 1
        update_ant <- best_tour %>%
            dplyr::filter(dplyr::row_number()==best_row) %>%
            dplyr::mutate(AID=nrow(ants)+1,
                          PHERO=Q/SCORE)
        if (best_row!=1){
            best_tour[1,] <- best_tour[best_row,]
            best_tour$GENERATION[1] <- ants$GENERATION[1]
        }
        
        # Pheromone updaten ####
        world$phero_matrix <- world$phero_matrix %>%
            evaporate_phero(evaporation_rate) %>%
            update_phero(ants=update_ant,
                         Q=Q,
                         dist_matrix=world$dist_matrix,
                         min_phero=min_phero,
                         max_phero=max_phero)
        
        # Print Tour Info ####
        if (print_info){
            print(glue::glue("Generation {generation} - \\
                     Score: {score} - \\
                     Local-Minium-Counter: {local_min_counter}",
                             generation=best_tour$GENERATION[1],
                             score=fmt(best_tour$SCORE[1])))}

        if (nrow(best_tour) > 1){
            is_new_tour <-lapply(best_tour$TOUR[c(1,2)],function(tour){
                pos_1 <- which(tour==1)
                tour <- tour[c(pos_1:length(tour),1:(pos_1-1))]
                return(tour)}) %>%
                unique() %>%
                length() %>%
                magrittr::is_greater_than(1)
            
            if (is_new_tour){
                if (print_plot){
                    # Plot Tour ####
                    name <- ifelse(is.null(fname),"Random Genes",fname)
                    plot_genes(genes=world$cities,
                               best_tour$TOUR[[1]],
                               title=glue::glue("Genetic Algorithm: {name}"),
                               subtitle=glue::glue("Generation: {generation} - \\
                                       Score: {score}",
                                                   generation=best_tour$GENERATION[1],
                                                   score=fmt(best_tour$SCORE[1]))) %>%
                        print()
                }
                local_min_counter <- 1
            } else {
                local_min_counter <- local_min_counter+1
                if (local_min_counter>=local_min_thld) {
                    is_local_min <- TRUE
                    break
                }
            }
        }
    }
    return(list(world=world,best_tour=best_tour))
}