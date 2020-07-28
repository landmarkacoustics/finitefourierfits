## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(finitefourierfits)
set.seed(42)

## ---- eval=FALSE, fig.dim=c(5, 5), out.width="95%"----------------------------
#  x <- seq(-2, 2, length.out=401)
#  N <- length(x)
#  fft.size <- 4 * nextn(N, 2)
#  half <- floor(fft.size / 2)
#  omegas <- finitefourierfits::omegas(fft.size)
#  b <- x[1]
#  Hz <- (N-1)/(x[N]-b)
#  d.omega <- (fft.size-1)/omegas[fft.size]
#  m <- Hz / d.omega
#  u <- function(x) {m*(x-b)}
#  plot(x, u(x), las=1, pch=16, col="blue",
#       xlab=expression(x), ylab=expression(u(x)))
#  y <- (x-rnorm(1))*(x-rnorm(1))*(x-rnorm(1))
#  mu <- mean(y)
#  S <- fft(c(y - mu, rep(0, fft.size-N)))
#  a <- finitefourierfits::.amplitudes(S)
#  p <- finitefourierfits::.phases(S)
#  mag.order <- order(a, decreasing=TRUE)[1:10]
#  all.terms <- build.term.list(mag.order, p)
#  all.starts <- a[mag.order]
#  names(all.starts) <- names(all.terms)
#  all.starts
#  step.fits <- list()
#  for(i in 1:10) {
#      ix <- 1:i
#      tmp <- tryCatch(nls(formula(paste("y-mu ~",
#                                        paste(all.terms[ix], collapse=" + "))),
#                          data.frame(w=u(x), y=y),
#                          start=all.starts[ix]),
#                      error=function(e) NULL)
#      if(!is.null(tmp)){
#          step.fits <- append(step.fits, list(tmp))
#      }
#  }
#  if (length(step.fits)) {
#      scores <- sapply(step.fits, BIC)
#      tops <- match(min(scores), scores)
#      plot(x, y,
#  	     las=1, col="blue",
#  		 xlab=expression(x), ylab=expression(f(x)))
#      lines(x, predict(step.fits[[tops]])+mu, col="orange", lwd=2)
#  	summary(step.fits[[tops]])
#  }

## ----eval=FALSE---------------------------------------------------------------
#  my.linters <- lintr::with_defaults(
#    object_name_linter=lintr::object_name_linter(styles="dotted.case"),
#    infix_spaces_linter=NULL,
#    commented_code_linter=NULL
#  )
#  lintr::lint_package(linters=my.linters)

## ----eval=FALSE---------------------------------------------------------------
#  my.fit <- fffit(my.data$x, my.data$y, model.selector=BIC)
#  my.fit$n.terms
#  coef(my.fit)

## ---- fig.dim=c(6, 6), out.width="95%"----------------------------------------
x <- seq(-2, 2, length.out=401)
y <- 10*(x/2)^2 + rnorm(length(x))
tmp <- finitefourierfits::fffit(x, y-mean(y))
summary(tmp)
plot(x, y, las=1, col="blue", pch=16, xlab=expression(x), ylab=expression(f(x)))
lines(x, predict(tmp)+mean(y), col="orange", lwd=2)

## ---- fig.dim=c(6, 6), out.width="95%"----------------------------------------
x <- seq(-2, 2, length.out=401)
f <- function(x, a=c(1, 1), b=c(0, 1)){
  a[1]*cos(a[2]*2*pi*x) + b[1]*cos(b[2]*2*pi*x)
}
phi <- c(0.6, 2.5)
y <- 5*f(x) + rnorm(length(x))
yfit <- fffit(x, y)
z <- 5*f(x, c(1, phi[1]), c(1, phi[2])) + rnorm(length(x))
zfit <- fffit(x, z)
plot(x, y,
     las=1,
     ylim=10*c(-1, 2),
     xlab=expression(x), ylab=expression(f(x)),
     pch=1, col="blue")
points(x, z,
      pch=1, col="orange")
lines(x, predict(yfit), col="blue", lwd=2)
lines(x, predict(zfit), col="orange", lwd=2)
legend("topright",
       c(expression(cos(2*pi*x)),
         bquote(cos(.(2*phi[1])*pi*x) + cos(.(2*phi[2])*pi*x))),
       pch=1,
       lty=1,
       col=c("blue", "orange"))
summary(yfit)
summary(zfit)

## -----------------------------------------------------------------------------
unwrap <- function(phases) {
    return((phases + pi) %% (2*pi) - pi)
}

## ---- fig.dim=c(6, 6), out.width="95%"----------------------------------------
a <- 30
n <- sqrt(2)
u <- seq(0, 10, 0.01)
v <- a * u^n * exp(-u) + rnorm(length(u))
parts <- list(low = u < 1,
	          mid = u > 0.5 & u < 7,
			  high = u > 6)
fits <- lapply(parts, function(f){
	fffit(u[f], v[f] - mean(v[f]), pad.multiplier=3)
	})
fits$all <- fffit(u, v - mean(v), pad.multiplier=3)
fits$nls <- nls(v ~ a*u^n*exp(-u), start=list(a=1, n=1))
colors <- list(all="cyan",
	           low="orange",
	           mid="green",
			   high="darkred",
			   nls="black")
plot(v ~ u, col="blue", las=1, cex=2)
invisible(sapply(names(colors),
	             function(n) {
				     if (n %in% names(parts)) {
					     f <- parts[[n]]
					     x <- u[f]
						 y <- predict(fits[[n]]) + mean(v[f])
					 } else {
					     x <- u
						 y <- predict(fits[[n]]) + ifelse(n=="all", mean(v), 0)
					 }
					 lines(x, y,
					       col=colors[[n]],
					       lwd=ifelse(n=="nls", 2, 3),
						   lty=ifelse(n=="nls", 2, 1))
	             }))
legend("topright",
       legend=names(colors),
	   lty=ifelse(names(colors)=="nls", 2, 1),
	   lwd=2,
	   col=as.character(colors),
	   title="Fit",
	   bty="n")

