# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
library(ggplot2)
library(plyr)
library(reshape)
#library(scales)
library(grid)
library(gridExtra)
library(stats4)
library(lme4)
library(lmtest)
library(plm)
library(magrittr)

# basic functions
fisherz <- function(r){0.5*log((1+r)/(1-r))}
ifisherz <- function(z){(exp(2*z)-1)/(exp(2*z)+1)}
logistic <- function(x){1/(1+exp(-x))}
logit <- function(p){log(p/(1-p))}

# matrix functions
offdiagonal <- function(mat, n){mat[(row(mat)-col(mat))==(-n)]}

# data processing functions.
cbind.fill<-function(...){
  nm <- list(...) 
  rnames<-unique(unlist(lapply(nm, rownames)))
  nm <- lapply(nm, function(x){newrows <- rnames[! rnames %in% rownames(x)]
                               newentries <- matrix(nrow=length(newrows), ncol=ncol(x))
                               rownames(newentries) <- newrows
                               colnames(newentries) <- colnames(x)
                               x <- rbind(x, newentries)
                               return(x)})
  nm <- lapply(nm, function(x){y<-data.frame(x[order(as.numeric(rownames(x))),])
                               rownames(y) <- as.character(sort(as.numeric(rownames(x))))
                               colnames(y) <- colnames(x)
                               return(y)})
  return(do.call(cbind, nm))
}

# ggplot functions

mytheme <- theme_bw()+
  theme(panel.grid.minor = element_line(colour="gray90", size=0.5), 
        panel.grid.major = element_line(colour="gray", size=0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        panel.border=element_rect(colour="black", size=0.6),
        strip.background= element_rect(colour = "black", fill = "gray90",
                                       size = 0.3, linetype = "solid"),
        strip.text=element_text(size=12, face="bold"))

my.log.breaks <- function(lims, marks){
  majors <- seq(floor(log10(lims[1])), ceiling(log10(lims[2])), by=1)
  n = floor(marks/(length(majors)-1))
  if(n > 1){
    rmajors <- log10(unlist(lapply(majors[-1], function(x){seq(10^(x-1), (n+1)*10^(x-1), by=10^(x-1))})))    
  } else {
    rmajors = majors
  }
  minors <- log10(unlist(lapply(majors[-1], function(x){seq(10^(x-1), 9*10^(x-1), by=10^(x-1))})))
  return(list(rmajors, minors))
}

mylogx <- function(lims, marks=6){
  breaks <- my.log.breaks(lims, marks)
  scale_x_log10(limits=lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks=breaks[[2]])
}
mylogy <- function(lims, marks=6){
  breaks <- my.log.breaks(lims, marks)
  scale_y_log10(limits=lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks=breaks[[2]])
}




#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

