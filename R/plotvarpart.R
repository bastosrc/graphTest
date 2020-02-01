#' @title Create Venn driagrams graph
#' @name plotvarpart
#' @description Function to create graphs type Ven diagram using objects of
#' class varpart. The function has one argument that you to use to make graphs
#' that show the variance partition in a colors gradient.
#' @param x One object of class varpart.
#' @param x1 Partition name.
#' @param x2 Other partition name.
#' @param resi Risduals name.
#' @param colour Circle colour.
#' @param fill Circle fill.
#' @param grad Logical argument to indicate if the graph must be in gradient. The default is \code{TRUE}.
#' @param fill1 Circle fill if it be in gradient. To the minor values.
#' @param fill2 Circle fill if it be in gradient. To the major values.
#' @param font Change the texts family in graph.
#' @return The function will return a graphic type Venn diagram.
#' @author Rafael Costa Bastos
#' @examples
#' require(vegan)
#' require(ggplot2)
#' require(ggforce)
#' data(mite)
#' data(mite.env)
#' data(mite.pcnm)
#' mod <- varpart(mite, mite.env, mite.pcnm, transfo="hel")
#' plotvarpart(mod)
#' @export
#'
plotvarpart=function(x, x1="X1", x2="X2", resi="Residuals",
                     colour=fill1, fill=NA, grad="/", fill1="gray",
                     fill2="black", font="serif"){
library(ggplot2)
library(ggforce)
values<-round(x$part$indfract$Adj.R.squared, 2)
paleta<-data.frame(x=c(0,2.5), y=c(0,0),labels=c(100,0))
dados<-data.frame(x=c(0,2.5), y=c(0,0),labels=c(values[1]*100,values[3]*100))

if(missing(resi)){
    resi<-paste("Residuals =", values[4], sep=" ")}
else{
    resi<-paste(resi,"=", values[4], sep=" ")}

show(

if(missing(grad)||grad==TRUE){
      ggplot(paleta, aes(x0 = x, y0 = y, r = 2, fill = labels))+
        geom_circle(alpha = 0)+
        geom_circle(data=dados, aes(x0=x, y0=y, fill=labels), alpha=0.6, size = 1, colour = colour, show.legend=F)+ coord_fixed()+
        scale_fill_gradient(low = fill1, high = fill2)+ 
        theme_void()+
        theme(legend.position="top", legend.title=element_text(family=font, size=15, face="bold"),
        legend.text=element_text(family=font, size=12))+
        labs(fill=expression(R^2))+
        annotate("text", x=-0.6, y=0, label=if((values[1]>=0)==TRUE){values[1]}else{},
        family=font, size=6)+
        annotate("text", x=1.3, y=0, label=if((values[2]>=0)==TRUE){values[2]}else{},
        family=font, size=6)+
        annotate("text", x=3.1, y=0, label=if((values[3]>=0)==TRUE){values[3]}else{},
        family=font, size=6)+
        annotate("text", x=0, y=2.3, label=x1, family=font, size=6)+
        annotate("text", x=2.5, y=2.3, label=x2, family=font, size=6)+
        annotate("text", x=3, y=-2.5, label=resi, family=font, size=6)

}else{

      ggplot(dados, aes(x0 = x, y0 = y, r = 2))+
        geom_circle(alpha=0.6, size = 1, fill=fill, colour = colour, show.legend=F)+ coord_fixed()+
        theme_void()+
        theme(legend.position="top", legend.title=element_text(family=font, size=15, face="bold"),
        legend.text=element_text(family=font, size=12))+
        labs(fill=expression(R^2))+
        annotate("text", x=-0.6, y=0, label=if((values[1]>=0)==TRUE){values[1]}else{},
        family=font, size=6)+
        annotate("text", x=1.3, y=0, label=if((values[2]>=0)==TRUE){values[2]}else{},
        family=font, size=6)+
        annotate("text", x=3.1, y=0, label=if((values[3]>=0)==TRUE){values[3]}else{},
        family=font, size=6)+
        annotate("text", x=0, y=2.3, label=x1, family=font, size=6)+
        annotate("text", x=2.5, y=2.3, label=x2, family=font, size=6)+
        annotate("text", x=3, y=-2.5, label=resi, family=font, size=6)
}
)
return("Values <0 not shown")
}
