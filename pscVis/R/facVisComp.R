

facVisComp <- function(p,x){

  old.d <- p$data
  tit <- p$labels$title
  old.d$source="CFM"
  dbnew <- data.frame(table(x));dbnew
  dbnew$source <- "DC"
  df <- rbind(old.d,dbnew)

  cls <- brewer.pal(max(3,nrow(df)),"BuGn")


  p <- ggplot(data=df,aes(fill=x,values=Freq))+
    geom_waffle(color="white",size=0.33,n_rows=10)+
    theme_void()+
    scale_fill_manual(values = cls) +
    facet_wrap(~source,ncol=1)+
    ggtitle(tit)+
    theme(legend.position="right") +
    theme(legend.title=element_blank())+
    theme(plot.title = element_text(hjust = 0.07))
  p
}
