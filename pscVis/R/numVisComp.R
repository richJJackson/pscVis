numVisComp <- function(p,x){
  dnew <- data.frame("xnew"=x)
  p + geom_density(aes(x=xnew,y=-..density..),data=dnew, fill="#404080",color="#404080" )
}
