numVis <- function(x,nm){
  cls <- brewer.pal(3,"BuGn")
  df <- data.frame("y"=x)
  p <- ggplot(aes(x=y),data=df) +
    geom_density(aes(x=y,y=..density..), fill=cls[1],colour=cls[1] ) +
    xlab("")+
    ylab("")+
    ggtitle(nm)+
    theme_void()+
    theme(plot.title = element_text(hjust = 0.05))
  p
}
