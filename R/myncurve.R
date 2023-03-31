#' @title myncurve
#'
#' @param mu mu for the dnorm
#' @param sigma sigma for the dnorm
#' @param a x value for shaded area
#'
#' @return curve
#' @export
#'
#'
#' @examples
#' \dontrun{myncurve(mu,sigma,a)}
myncurve = function(mu, sigma, a){
  library(ggplot2)
  z <- seq(-4,4,0.01)
  fz <- dnorm(z,mean=mu,sd=sigma)
  q <- qnorm(0.25,mean=mu,sd=sigma) # the quantile
  x <- seq(-4, q, 0.01)
  y <- c(dnorm(x,mean=mu,sd=sigma), 0, 0)
  x <- c(x, q, -4)


  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  ggplot() + geom_line(aes(z, fz)) +
    geom_polygon(data = data.frame(x=x, y=y), aes(x, y), fill='blue')
}
