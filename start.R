#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#   AUFRUF FUER MMAS
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   INIT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Funktionen laden
library(magrittr)
files <- list.files("R",full.names = T)
for (f in files){
    source(f)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   MMAS STARTEN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fname <- "berlin52.tsp"
res <- start_mmas(
    fname=fname,
    alpha=9,
    beta=12,
    Q=2,
    evaporation_rate=0.25,
    random_factor=0.03,
    max_generations=100,
    print_info=TRUE,
    print_plot=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ERGEBNIS PLOTTEN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_convergence(res$best_tour)
best_tour <- res$best_tour %>%
    dplyr::filter(dplyr::row_number()==which.min(SCORE))
plot_genes(genes=res$world$cities,
           res$best_tour$TOUR[[1]],
           title=glue::glue("Genetic Algorithm: {fname}"),
           subtitle=glue::glue("Generation: {generation} - \\
                                       Score: {score}",
                               generation=res$best_tour$GENERATION[1],
                               score=fmt(res$best_tour$SCORE[1]))) %>%
    print()
