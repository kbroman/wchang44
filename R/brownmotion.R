#plot 2d brownian motion
plotBM <- function(x)
{
  subdude1 <- x[1,]
  subdude2 <- x[nrow(x),]
  ggplot(x, aes_string(x="x",y="y"))+geom_point()+theme_bw()+ggtitle("2D Brownian Motion")+
    geom_point(data=subdude1, size=5,shape=5)+
    geom_text(data=subdude1,label="First",hjust=1,vjust=1)+
    geom_point(data=subdude2, size=5,shape=6)+
    geom_text(data=subdude2,label="Last",hjust=1,vjust=1)
  
}

#' Simulate Brownian Motion
#'
#' Input number of trials and sigma and simulate 2D Brownian motion
#'
#' @param n Number of trials
#' 
#' @param sigma Standard Deviation
#' 
#' @return Returns a dataframe with 2 columns "x" and "y" containing the results
#' of n simulations of Brownian Motion
#' 
#' @examples
#' simBM(100)
#' x <- simBM(100)
#' 
#' @export
#'
#' @import ggplot2

#simulate 2d brownian motion
simBM <- function(n, sigma=1)
{
  stopifnot(n>=2)
  stopifnot(sigma >=0)
  x <- matrix(rnorm(2*n,0,sigma),ncol=2)
  colnames(x) <- c("x","y")
  x <- apply(x,2,cumsum)
  x <- as.data.frame(x)
  print(plotBM(x))
  return(x)
  
}
