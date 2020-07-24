## Copyright (C) 2020 by Landmark Acoustics LLC

.arg.names <- list(a="a", f="f", p="p")


.trig.term <- function(i) {
    return(paste("a", i, "*cos(2*pi*f", i, "*x - p", i, ")", sep=""))
}


.named.item <- function(value, name, number) {
    return(structure(value, names=paste(name, number, sep="")))
}


.frequency.guesses <- function(indices, obj) {
    return(sapply(seq_along(indices),
                  function(i) {
                      ix <- obj$magnitude.order[i]
                      .named.item(obj$f[ix], "f", i)
                  }))
}


#' Summarize a DFT Term for Use in an FFFit
#'
#' Several pieces of information from each term go in to building an [fffit()],
#' and it makes sense to calculate and store them once.
#'
#' @param ranking An integer representing where the term falls in some ordering
#' @param obj A [fourier.summary()] object.
#' of the terms of the DFT.
#' @return an object of type `fffterm`, which has the following named members:
#' a
#' ~ The computed amplitude of the term
#'
#' p
#' ~ The phase of the term
#'
#' f
#' ~ The frequency of the term
#'
#' term
#' ~ A string with format $a\[n\]cos(2 pi f\[n\] x + p\[n\])$ for ranking `n`.
#'
#' @seealso [fourier.summary()]
#' @export
fffterm <- function(ranking, obj) {

    ix <- obj$magnitude.order[ranking]

    result <- if (ix==1) {
                  with(obj, list(a=.named.item(a[1], "b", ""),
                                p=NA,
                                f=NA,
                                term="b"))
              } else {
                  c(
                      lapply(.arg.names, function(n) {
                          .named.item(obj[[n]][ix], n, ranking)
                      }),
                      list(term=.trig.term(ranking))
                  )
              }

    class(result) <- append(class(result), "fffterm")

    return(result)
}


.get.part <- function(term, name) {
    return(term[[name]])
}


.build.formula <- function(response, term.list) {
    rhs <- paste(sapply(term.list, .get.part, "term"), collapse=" + ")
    return(formula(paste(response, "~", rhs)))
}


.concatenate.starts <- function(name, term.list) {
    result <- as.list(sapply(term.list, .get.part, name))
    return(result[!is.na(result)])
}


.build.starts <- function(term.list) {
    result <- sapply(as.character(.arg.names),
                     .concatenate.starts,
                     term.list,
                     simplify=FALSE)
    ##    return(result[sapply(result, function(x) !is.null(x))])
    return(c(result$a, result$f, result$p))
}
