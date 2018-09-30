
rm(list=ls())




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
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

setwd("C:/Users/a-bibeka/Dropbox/TTI_Projects/0-1-6934 TSC Settings Sunkari/Det_R_code")

Camera_Ht<-c(20,24,28,32,36,40)
STbar_dist<-c(50,100,150)
data<-expand.grid(Camera_Ht,STbar_dist)
colnames(data)<-c("Camera_Ht","STbar_dist")
car_Ht<-5
truck_Ht<-14.3
require(data.table)
data<-data.table(data)
data[,det_car:=round(car_Ht*(STbar_dist/Camera_Ht))]
data[,det_truck:=round(truck_Ht*(STbar_dist/Camera_Ht))]


require(ggplot2)

require(RColorBrewer)
darkcols <- brewer.pal(8, "Dark2")
g1<-ggplot(data=data,aes(x=Camera_Ht,y=det_car,group=as.factor(STbar_dist),color=as.factor(STbar_dist)))+geom_point(shape=17,cex=4)+scale_colour_hue(l=50)
g1<-g1+scale_color_manual("Distance of Camera from Stopbar (ft)",values=darkcols[1:3])
g1<-g1+labs(title="Affect of Video Detection Occlusion on Detector Length for Cars",x="Camera Height (ft)",y="Extra Detection Length (ft)")+ theme_bw()

g2<-ggplot(data=data,aes(x=Camera_Ht,y=det_truck,group=as.factor(STbar_dist),color=as.factor(STbar_dist)))+geom_point(shape=17,cex=4)+scale_colour_hue(l=50)
g2<-g2+scale_color_manual("Distance of Camera from Stopbar (ft)",values=darkcols[1:3])
g2<-g2+labs(title="Affect of Video Detection Occlusion on Detector Length for Trucks",x="Camera Height (ft)",y="Extra Detection Length (ft)")+ theme_bw()
pdf("Video_Occlusion.pdf")
multiplot(g1,g2,cols=1)
graphics.off()
data1=reshape(data,idvar="Camera_Ht",timevar=c("STbar_dist"),direction = "wide")


setnames(data1,colnames(data1),c("Camera Ht","50 ft Intersection Det Len Car","50 ft Intersection Det Len Truck"
                                 ,"100 ft Intersection Det Len Car","100 ft Intersection Det Len Truck",
                                 "150 ft Intersection Det Len Car","150 ft Intersection Det Len Truck"))
write.csv(data1,"Vid_Det_Occlusion.csv")





