#' Ticket_Approximation
#'
#' @param N number of seats
#' @param gamma probability of total overbooking
#' @param p probability of show up
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{ntickets(200,0.02,0.95)}
#'
ntickets <- function(N,gamma,p) {

  n=c(N:(N+N/10))
  x<-abs(N-qbinom(1-gamma,n,p))
  idx<-which.min(x)
  nd<-n[idx]
  t1<-(1-gamma-pbinom(N,n,p))

  plot(n,t1,xlab="Objective",ylab="n")
  abline(v=nd , h = 0,col="Red")

  axis(side = 1,
       labels = TRUE,
       at = 2.5,
       col.ticks = "Red",
       lwd = 4)

  title(main = paste0("Objective Vs n to find optimal tickets sold \n(", nd,") gamma =",gamma," N=",N," discrete"))

  f<-function(n)(abs(N+0.5-qnorm(1-gamma,n*p,sqrt(n*p*(1-p)))))

  f_1<-function(n)(1-gamma-pnorm(N,(n-0.5)*p,sqrt((n-0.5)*p*(1-p))))


  curve(f_1,xlim=c(N,(N+N/10)),xlab="n", ylab="Objective")

  op<-optimize(f,interval = c(N:(N+N/10)))
  nc=op$minimum
  abline(v = op$minimum , h = 0,col="blue")

  axis(side = 1,
       labels = TRUE,
       at = 2.5,
       col.ticks = "Red",
       lwd = 4)

  title(main = paste0("Objective Vs n to find optimal tickets sold \n(", op$min,") gamma =",gamma," N=",N," continuous"))
  result <-list(nd=nd, nc = nc, N = N, p = p, gamma = gamma)
  print(result)
}
