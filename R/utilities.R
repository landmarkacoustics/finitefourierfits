## Copyright (C) 2020 by Landmark Acoustics LLC

#' The index of the minimum element in a container
#'
#' \code{argmin} acts just like the mathematical concept it is named after.
#'
#' @param x Some collection, probably a \code{\link{vector}} of some kind.
#' @return the index of the smallest item in the collection.
#' @examples
#' argmin(seq(-1, 10, 10)^2)
argmin <- function(x){
    return(match(min(x), x))
}


frequencies <- function(fft.size){
    return( 2*pi*seq(0, fft.size-1)/fft.size )
}

