#Create ggplot 

Simul_plots<-function(data1,x1,y1,fill1,facet1,xlab1="",ylab1,title1){
  require(RColorBrewer)
  darkcols <- brewer.pal(8, "Dark2")
  
  ggplt<-ggplot(data1,aes_(x=substitute(x1),y=substitute(y1),fill=substitute(fill1)))+
    geom_bar(stat="Identity",colour="black",position=position_dodge())+
    facet_grid(facet1)+
    scale_fill_manual(name="Direction",values=darkcols[1:4])+
    labs(x=xlab1, y=ylab1)+ theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
  ggplt
}


det_plots<-function(data1,x1,y1,fill1,facet1,xlab1="EBT Passage Time (sec)",ylab1,title1,vol_label=c(`400`="EBT = 800 veh/hr",`800`="EBT = 1600 veh/hr",`1200`="EBT = 2400 veh/hr")){
  require(RColorBrewer)
  darkcols <- brewer.pal(8, "Dark2")

  ggplt<-ggplot(data1,aes_(x=substitute(x1),y=substitute(y1),fill=substitute(fill1)))+
    geom_bar(stat="Identity",colour="black",position=position_dodge())+
    facet_grid(facet1,labeller=labeller(EBT_Vol=vol_label))+
    scale_fill_manual(name="Direction",values=darkcols[1:3])+
    labs(x=xlab1, y=ylab1)+ theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
  ggplt
}

det_plots_2<-function(data1,x1,y1,fill1,facet1,xlab1="EBT Passage Time (sec)",ylab1,title1,vol_label=c(`400`="EBT = 800 veh/hr",`800`="EBT = 1600 veh/hr",`1200`="EBT = 2400 veh/hr")
){
  require(RColorBrewer)
  darkcols <- brewer.pal(8, "Dark2")
  ggplt<-ggplot(data1,aes_(x=substitute(x1),y=substitute(y1),fill=substitute(fill1)))+
    geom_bar(stat="Identity",colour="black",position=position_dodge())+
    facet_grid(facet1,labeller=labeller(EBT_Vol=vol_label))+scale_fill_manual(guide=FALSE,values=darkcols[1:8])+
    labs(x=xlab1, y=ylab1)+ theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
  ggplt
}

TwoLndet_plots<-function(data1,x1,y1,fill1,facet1,xlab1="EBT Passage Time (sec)",ylab1,title1,vol_label=c(`400`="800 veh/hr",`800`="1600 veh/hr",`1200`="2400 veh/hr")){
  require(RColorBrewer)
  LnDist_label<-c(`50`="50-50 Lane\nDistribution",`75`="75-25 Lane\nDistribution")
  darkcols <- brewer.pal(8, "Dark2")
  ggplt<-ggplot(data1,aes_(x=substitute(x1),y=substitute(y1),fill=substitute(fill1)))+
    geom_bar(stat="Identity",colour="black",position=position_dodge())+
    facet_grid(facet1,labeller=labeller(EBT=vol_label,`Ln Dist`=LnDist_label))+
    scale_fill_manual(name="Direction",values=darkcols[1:3])+
    labs(x=xlab1, y=ylab1)+ theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
  ggplt
}

TwoLndet_plots_2<-function(data1,x1,y1,fill1,facet1,xlab1="EBT Passage Time (sec)",ylab1,title1,vol_label=c(`400`="EBT = 800 veh/hr",`800`="EBT = 1600 veh/hr",`1200`="EBT = 2400 veh/hr")
){
  LnDist_label<-c(`50`="50-50 Lane\nDistribution",`75`="75-25 Lane\nDistribution")
  require(RColorBrewer)
  darkcols <- brewer.pal(8, "Dark2")
  ggplt<-ggplot(data1,aes_(x=substitute(x1),y=substitute(y1),fill=substitute(fill1)))+
    geom_bar(stat="Identity",colour="black",position=position_dodge())+
    facet_grid(facet1,labeller=labeller(EBT=vol_label,`Ln Dist`=LnDist_label))+scale_fill_manual(guide=FALSE,values=darkcols[1:8])+
    labs(x=xlab1, y=ylab1)+ theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))
  ggplt
}