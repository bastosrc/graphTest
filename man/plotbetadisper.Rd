% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotbetadisper.R
\name{plotbetadisper}
\alias{plotbetadisper}
\title{Plot a betadisper object}
\usage{
plotbetadisper(x)
}
\arguments{
\item{x}{One object of class betadisper.}
}
\value{
The function will return a ordinaition of ggplot class.
}
\description{
Function to create PERMDISP graphs using ggplot2. The function has one argument that you to use to make graphs of ordination type.
}
\examples{
require(vegan)
require(ggplot2)
require(ggConvexHull)
data(varespec)
dist <- vegdist(varespec)
gp <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))
mod <- betadisper(dist, gp)
plotbetadisper(mod)
}
\author{
Rafael Costa Bastos
}
