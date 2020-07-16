## Copyright (C) 2020 by Landmark Acoustics LLC

#' One term from a Fourier basis
#'
#' A finite Fourier fit uses some of the terms from a Discrete Fourier
#' Transform to approximate a smooth fit to some data. This function
#' renders one such part of a term.
#'
#' @param index The integer that corresponds to an item in the DFT.
#' @param phase The actual phase of the item from the DFT.
#' @param variable.name An optional symbol that defaults to 'a'.
#' @return A string representation of the term
#' @examples
#' build.term(1, 0)
#' build.term(42, 2.71828)
#' @seealso
#' \code{\link{fft}} for calculating a DFT
#' 
#' \code{\link{Arg}} for calculating the phase of an item in DFT
#' 
#' \code{\link{as.formula}} for using the output to build a formula
build.term <- function(index, phase, variable.name='a'){
    if(index>1){
        variable <- paste(variable.name, index, sep='')
        result <- paste(variable,
                        " * cos(",
                        index - 1, " * u(x) ",
                        ifelse(phase < 0, '-', '+'), ' ',
                        abs(phase),
                        ')', sep='')
    }
    else{
        variable <- result <- "b"
    }
    names(result) <- variable
    return(result)
}


build.rhs.list <- function(indices.to.use, all.phases){
    terms <- list()
    for(i in indices.to.use){
        terms <- append(terms, build.term(i, all.phases[i]))
    }
    return(terms)
}


build.formula <- function(response, rhs.list){
    return(as.formula(paste(response,
                            "~",
                            paste(rhs.list, collapse=" + "))))
}


build.starts <- function(term.order, term.var, variable.name='a'){
    n <- paste(variable.name, term.order, sep='')
    v <- as.list(term.var[term.order])
    names(v) <- n
    return(v)
}


best.fit <- function(fit.list){
    v <- sapply(fit.list, BIC)
    i <- argmin(v)
    return(c(index=i, BIC=v[i]))
}

