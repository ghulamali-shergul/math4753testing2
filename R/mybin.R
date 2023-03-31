#' create barplots
#'
#' @param iter
#' @param n
#' @param p
#'
#' @return
#' @export
#'
#' @examples
mybin = function(iter=100,n=10,p=0.5) {
  #create a matrix to hold the sample
  #initialize with NA

  sam.mat = matrix(NA,nr=n,nc=iter,byrow = TRUE)

  #make a vector to store the number of successes for each trial

  succ=c()
  for (i in 1:iter) {
    #fill each column with a new sample data
    sam.mat[,i]=sample(c(1,0),n,replace = TRUE, prob = c(p,1-p))

    #calculate statistic from sample (sum)
    succ[i]=sum(sam.mat[,i])
  }

  #make a table of successes
  succ.tab = table(factor(succ,levels = 0:n))

  #make a bar-plot of proportions
  barplot(succ.tab/(iter), col = rainbow(n+1), main = "Binomial simulation", xlab = "Number of successes")

  succ.tab/iter

}
