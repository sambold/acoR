#' init_world
#' @description initialisiert Genpool. Notwendige Merkmale eines Gens sind dabei
#'     jeweils ein Wert fuer Longitude und Latitude bzw. ein X und Y-Wert. Zudem 
#'     kann festgelegt werden, ob es sich bei einzelnen Genen um den Startpunkt
#'     der Tour bzw. um einen besonderen Punkt auf der Tour (Capitol) handelt. 
#' @param fname Standard NULL, wenn fname NULL ist, wird ein zufaelliger Genpool
#'     mit n Genen initialisiert. Handelt es sich bei fname um einen 
#'     Character-String, wird dieser als Pfadangabe zu einem Input-File interpretiert
#'     und es wird - je nach Endung - versucht, die Daten einzulesen und in die
#'     geforderte Struktur zu bringen. Ebenso kann ein fertiger Dataframe 
#'     uebergeben werden. In diesem Fall wird versucht ueber ein Mapping den 
#'     dataframe in die geforderte Struktur zu bringen.
#' @param n numeric, Standard 10, wird nur verwendet, wenn fname NULL. Gibt
#'     die Anzahl der zu initialisierenden Gene an.
#' @param capitol_list Numeric, Standard NULL, ein Vector mit IDs/Zeilennummern
#'     jener Gene, die als 'Capitol' markiert werden sollen. Z.B. um fuer diese
#'     Gene eine abweichende Scoring-Funktion zu verwenden etc.
#' @param starting_point Numeric, Standard NULL, Zeilennummer jenes Gens, das als
#'     Startpunkt markiert werden soll. Wenn NULL wird die erste Zeile als
#'     Startpunkt gewaehlt. Hat auf die Berechnung der Route keinen Einfluss.
#' @param mapping noch nicht umgesetzt - fuer den Fall, dass ein dataframe 
#'     ubergeben wird, eine Liste, die die entsprechenden Spalten 
#'     CID, X, Y, STARTING_POINT, CAPITOL zuweist
#' @param seed Numeric, Standard NULL, wenn nicht NULL wird seed verwendet, um
#'     Ergebnisse von init_genes reproduzierbar zu machen. 
#' @export tibble mit der Struktur: CID, X, Y, CAPITOL, STARTING_POINT
#' @importFrom purrr is_scalar_atomic
#' @importFrom stringr str_extract
#' @import dplyr
init_world <- function(fname=NULL,
                       n=10,
                       capitol_list=NULL,
                       starting_point=NULL,
                       mapping=NULL,
                       init_phero=0.0001,
                       dist_matrix_thld=90000,
                       seed=NULL){
    
    if (is.null(starting_point)){
        starting_point <- 1
    } else {
        stopifnot(purrr::is_scalar_atomic(starting_point))
    }
    
    # Staedte einlesen
    if (is.null(fname)){
        # init rnd cities
        if (!is.null(seed)) set.seed(seed)
        cities <- dplyr::tibble(
            CID=seq(n),
            X=sample(10000,size=n,replace=T),
            Y=sample(10000,size=n,replace=T),
            CAPITOL=CID %in% capitol_list,
            STARTING_POINT=CID == starting_point)
    } else {
        # cities aus file/df laden
        if (is.character(fname)){
            # Dateiendung extrahieren
            fext <- stringr::str_extract(fname,"\\.[A-Za-z0-9]*$")
            cities <- switch(fext,
                             .tsp={
                                 cities <- readLines(con=fname) %>%
                                     .[grepl("^[0-9]",.)] %>%
                                     strsplit(split=" ") %>%
                                     lapply(function(cols){
                                         cols <- as.numeric(cols)
                                         cities <- dplyr::tibble(
                                             CID=cols[1],
                                             X=cols[2],
                                             Y=cols[3],
                                             CAPITOL=cols[1] %in% capitol_list,
                                             STARTING_POINT=cols[1]==starting_point)
                                         return(cities)
                                     }) %>%
                                     dplyr::bind_rows()
                                 #do.call("dplyr::bind_rows",.)
                                 if (nrow(cities) != tail(cities$CID,1)){
                                     warning(glue::glue(
                                         "Achtung: Die eingelesenen Daten sind mitunter fehlerhaft. \\
                                        Der Datensatz umfasst {cities_len} Datensaetze. Die \\
                                        ID des letzten Datensatzes lautet aber \\
                                        {last_cid}"))
                                 }
                                 cities
                             },
                             .csv={cities <- "b"},
                             {"c"})
        } else {
            # # Dataframe etc einlesen (Achtung num und Co ausschließen?)
            # # Format: ID,X,Y
            # if (!is.null(mapping)){
            #     # c(GID=1,X=2,Y=3,STARTING_POINT=5,CAPITOL=4)
            #     # c(X=1,Y=2)
            #     m <- rep(NA,5)
            #     m[mapping] <- names(mapping)
            #     m <- m[!is.na(m)]
            #     
            # }
            # genes <- dplyr::as_tibble(fname) %>%
            #     dplyr::select(tidyselect::all_of(m)) %>%
            #     magrittr::set_colnames(c("GID","X","Y","STARTING_POINT","CAPITOL")) %>%
            #     dplyr::mutate(STARTING_POINT=starting_point==GID,
            #                   CAPITOL=GID %in% capitol_list)
        }
    }
    
    n <- nrow(cities)
    
    # Distanzmatrix berechnen
    if (n <= dist_matrix_thld){
        dist_matrix <- cities %>%
            dplyr::select(X,Y) %>%
            dist()
    } else {
        dist_matrix <- NULL
    }
    
    # Pheromonmatrix initiieren
    phero_matrix <- rep(init_phero,n^2) %>%
        matrix(ncol=n)
    
    # Output
    res <- list(cities=cities,
                dist_matrix=dist_matrix,
                phero_matrix=phero_matrix)
    
    return(res)
}