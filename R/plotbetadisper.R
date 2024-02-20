#' @title Ordenation from an object of the betadisper class. 
#' @name plotbetadisper
#' @description Function to create PERMDISP graphs using ggplot2.
#' The function has one argument that you to use to make graphs of ordination type.
#' @param x One object of betadisper class.
#' @return The function will return a graphic type ordination.
#' @author Rafael Costa Bastos
#' @examples
#' require(vegan)
#' require(ggplot2)
#' require(ggConvexHull)
#' data(varespec)
#' dist <- vegdist(varespec)
#' gp <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))
#' mod <- betadisper(dist, gp)
#' plotbetadisper(mod)
#' @export
#'

plotbetadisper=function(x){
library(ggConvexHull)
	ord<-as.data.frame(x$vectors[,c(1,2)])
		ord<-as.data.frame(cbind(ord, x$group))

	ord2<-ord[order(ord[,3]),]# Classificando em ordem alfabética.
	print(levels(ord2[,3]))
	levels(ord2[,3])[1]
	levels(ord2[,3])[2]
	levels(ord2[,3])[3]

######## Dividindo a tabela, para criar as setas e centróides.
trat1<-ord2[1:sum(ifelse((ord[,3]==(levels(ord[,3])[1]))==TRUE,1,0)),]
trat2<-ord2[(1+nrow(trat1)):(sum(ifelse((ord[,3]==(levels(ord[,3])[1]))==TRUE,1,0))+sum(ifelse((ord[,3]==(levels(ord[,3])[2]))==TRUE,1,0))),]
trat3<-ord2[(1+nrow(trat1)+nrow(trat2)):nrow(ord2),]

######## Plotando o gráfico.
plot1<-ggplot(ord, aes(x=PCoA1, y=PCoA2, colour=ord[,3]))+
	theme_test(base_family="serif", base_size=18)+
	theme(panel.border = element_rect(size=1, fill = NA), axis.title=element_text(family=serif))+
	guides(colour="none")+
	geom_convexhull(alpha=0, size=1)

if(length(levels(ord2[,3]))==2){
plot1<-plot1+geom_segment(data=trat1, aes(x=x$centroids[1], y=x$centroids[3], xend=PCoA1, yend=PCoA2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray")+
	annotate("text", x=x$centroids[1], y=x$centroids[3], label=levels(ord[,3])[1], family="serif", size=6)+
	geom_segment(data=trat2, aes(x=x$centroids[2], y=x$centroids[4], xend=PCoA1, yend=PCoA2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray")+
	annotate("text", x=x$centroids[2], y=x$centroids[4], label=levels(ord[,3])[2], family="serif", size=6)+
	geom_point(shape=2, size=3)
}
if(length(levels(ord2[,3]))==3){
plot1<-plot1+geom_segment(data=trat1, aes(x=x$centroids[1], y=x$centroids[4], xend=PCoA1, yend=PCoA2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray")+
	annotate("text", x=x$centroids[1], y=x$centroids[4], label=levels(ord[,3])[1], family="serif", size=6)+
	geom_segment(data=trat2, aes(x=x$centroids[2], y=x$centroids[5], xend=PCoA1, yend=PCoA2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray")+
	annotate("text", x=x$centroids[2], y=x$centroids[5], label=levels(ord[,3])[2], family="serif", size=6)+
	geom_segment(data=trat3, aes(x=x$centroids[3], y=x$centroids[6], xend=PCoA1, yend=PCoA2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray")+
	annotate("text", x=x$centroids[3], y=x$centroids[6], label=levels(ord[,3])[3], family="serif", size=6)+
	geom_point(shape=2, size=3)
}
return(plot1)
}

