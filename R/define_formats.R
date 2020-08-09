# verschiedene Formate zum Anzeigen 
fmt <- function(x) UseMethod("fmt")
fmt.default <- function(x){
    y <- abs(x)
    sprintf("%s%02d:%02d:%02d:%02d", 
            ifelse(x < 0, "-", ""), # sign
            y %/% 86400,  # days
            y %% 86400 %/% 3600,  # hours 
            y %% 3600 %/% 60,  # minutes
            y %% 60 %/% 1) # seconds
}
fmt.difftime <- function(x){
    units(x) <- "secs"
    x <- unclass(x)
    NextMethod()
}
fmt.numeric <- function(x){
    format(x,big.mark=".",decimal.mark=",",nsmall=2)
}
fmt.integer <- function(x){
    format(x,big.mark=".",decimal.mark=",",nsmall=0)
}