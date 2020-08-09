#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   ACO
#
#
#
#
#
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   INIT ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Parameter ----
alpha <- 9                # pheromone importance
beta <- 12                # distance importance (beta > alpha)
evaporation_rate <- 0.25  # pheromone evaporating in every iteration (rho)
Q <- 2                    # total pheromone left on trail by each ant
ant_factor <- 0.8         # ants per city
random_factor <- 0.03     # 

max_generations <- 2000
local_min_thld=floor(max_generations*0.1)
print_info <- T
print_plot <- T
fname <- "data/qa194.tsp"
max_phero <- NULL
min_phero <- NULL
ants_n <- NULL
local_min_prob <- NULL

# Startwerte ----
local_min_counter <- 1
is_local_min <- FALSE

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   FUNKTIONEN ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`%>%` <- magrittr::`%>%`
fun_list <- list.files("R",full.names=T)
sapply(fun_list, source)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   C ACO STARTEN ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start <- Sys.time()

# MMAS initialisieren ----
# Welt initialisieren ####
world <- init_world(fname=fname)
# Defaultwerte fuer Parameter setzen, falls nicht explizit definiert ####
if (is.null(ants_n)) ants_n <- floor(0.8*nrow(world$cities))
if (is.null(min_phero) | is.null(max_phero)){
    nn_score <- get_nn_tour() %>%
        score_tour(dist_matrix=world$dist_matrix)
    if (is.null(max_phero)) max_phero <- 1/(evaporation_rate*nn_score/10)
    if (is.null(min_phero)) min_phero <- max_phero/(nrow(world$cities))
    thld_dmmy <- c(min_phero, max_phero)
    min_phero <- min(thld_dmmy)
    max_phero <- max(thld_dmmy)
}
if (is.null(local_min_prob)) local_min_prob <- 0.2

# Ameisen ausschicken und scoren ####
ants <- init_ants(n=ants_n) %>%
    dplyr::mutate(
        TOUR=lapply(AID,function(ant) {
            find_tour(destinations=world$cities$CID,dist_matrix=world$dist_matrix)}),
        SCORE=sapply(TOUR,function(tour) {
            score_tour(tour=tour,dist_matrix=world$dist_matrix)}),
        PHERO=Q/SCORE)
# Beste Tour der Runde speichern ####
best_tour <- ants %>%
    dplyr::filter(AID==which.min(SCORE)) %>%
    dplyr::select(GENERATION,TOUR,SCORE)

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
# Plot Tour ####
if (print_plot){
    name <- ifelse(is.null(fname),"Random Genes",fname)
    plot_genes(genes=world$cities, 
               best_tour$TOUR[[1]],
               title=glue::glue("Genetic Algorithm: {name}"),
               subtitle=glue::glue("Generation: {generation} - \\
                                           Score: {score}",
                                   generation=best_tour$GENERATION[1],
                                   score=fmt(best_tour$SCORE[1]))) %>%
        print()}

# Weitere Generationen durchlaufen ----
for (generation in 2:max_generations){
    Sys.sleep(0.001)
    # Ameisen ausschicken und scoren ####
    ants <- ants %>%
        dplyr::mutate(
            GENERATION=GENERATION+1,
            TOUR=lapply(AID,function(ant) {
                find_tour(destinations=world$cities$CID,dist_matrix=world$dist_matrix)}),
            SCORE=sapply(TOUR,function(tour) {
                score_tour(tour=tour,dist_matrix=world$dist_matrix)}),
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
end <- Sys.time()


# Abschluss-Report ----
# Information zu lokalem Minimum ####
if (is_local_min) {
    print(glue::glue("Genetic Algorithm nach {generation} Generationen \\
                         (Abbruch durch lokales Minimum) abgeschlossen. \n \\
                         Dauer: {duration} \\
                         Score zu Beginn: {score_begin} \n \\
                         Score am Ende: {score_end}",
                     duration=end-start,
                     score_begin=best_tour %>% 
                         tail(1) %>%
                         .$SCORE %>%
                         fmt(),
                     score_end=best_tour %>%
                         head(1) %>%
                         .$SCORE %>%
                         fmt()))
} else {
    print(glue::glue("Genetic Algorithm nach {generation} Generationen //
                         (Maximale Anzahl an Iterationen erreicht) abgeschlossen. \n //
                         Dauer: {duration} \\
                         Score zu Beginn: {score_begin} \n \\
                         Score am Ende: {score_end}",
                     duration=end-start,
                     score_begin=best_tour %>% 
                         tail(1) %>%
                         .$SCORE %>%
                         fmt(),
                     score_end=best_tour %>%
                         head(1) %>%
                         .$SCORE %>%
                         fmt()))
}

# Global beste Tour plotten ####
if (print_plot){
    pos_best <- which.min(best_tour$SCORE)
    # Plot Tour ####
    name <- ifelse(is.null(fname),"Random Genes",fname)
    plot_genes(genes=world$cities, 
               best_tour$TOUR[[pos_best]],
               title=glue::glue("Genetic Algorithm: {name}"),
               subtitle=glue::glue("Generation: {generation} - \\
                                       Score: {score}",
                                   generation=best_tour$GENERATION[pos_best],
                                   score=fmt(best_tour$SCORE[pos_best]))) %>%
        print()
    # Plot Convergence ####
    plot_convergence(best_tour)
}





