# Karst manuscript new plots

library(car)
library(MASS)
library(AICcmodavg)
library(ggplot2)
library(lattice)
library(grid)
library(scales)
library(plyr)
library(gtable)
library(reshape2)
library(factoextra)

############################# Fig 2 - detections of a few PPCPs by site

# load table of data 
reptable<- read.csv("Karst.all.data.csv", sep=",",head=T)
r<-reptable
#make variables categorical (character) or continuous(numeric/double)
r[,1] <- as.numeric(r[,1])
r[,2] <-as.character(r[,2])
r[,3] <-as.character(r[,3])
r[,4] <-as.character(r[,4])
r[,5] <-as.character(r[,5])
r[,6] <-as.character(r[,6])
r[,7] <-as.numeric(r[,7])
r[,8] <-as.numeric(r[,8])
r[,9] <-as.numeric(r[,9])
r[,10] <-as.numeric(r[,10])
r[,11] <-as.numeric(r[,11])
r[,12] <-as.numeric(r[,12])
r[,13] <-as.numeric(r[,13])
r[,14] <-as.numeric(r[,14])
r[,15] <-as.numeric(r[,15])
r[,16] <-as.numeric(r[,16])
r[,17] <-as.numeric(r[,17])
r[,18] <-as.numeric(r[,18])
r[,19] <-as.numeric(r[,19])
r[,20] <-as.numeric(r[,20])
r[,21] <-as.character(r[,21])
r[,22] <-as.numeric(r[,22])
r[,23] <-as.numeric(r[,23])
r[,24] <-as.numeric(r[,24])
r[,25] <-as.numeric(r[,25])
r[,26] <-as.numeric(r[,26])
r[,27] <-as.numeric(r[,27])
r[,28] <-as.numeric(r[,28])
r[,29] <-as.numeric(r[,29])
r[,30] <-as.numeric(r[,30])
r[,31] <-as.numeric(r[,31])
r[,32] <-as.numeric(r[,32])
r[,33] <-as.numeric(r[,33])
r[,34] <-as.numeric(r[,34])
r[,35] <-as.numeric(r[,35])
r[,36] <-as.numeric(r[,36])
r[,37] <-as.numeric(r[,37])
r[,38] <-as.numeric(r[,38])
r[,39] <-as.numeric(r[,39])
r[,40] <-as.numeric(r[,40])
r[,41] <-as.numeric(r[,41])
r[,42] <-as.numeric(r[,42])
r[,43] <-as.numeric(r[,43])
r[,44] <-as.numeric(r[,44])
r[,45] <-as.numeric(r[,45])
r[,46] <-as.numeric(r[,46])
r[,47] <-as.numeric(r[,47])
r[,48] <-as.numeric(r[,48])
r[,49] <-as.numeric(r[,49])
r[,50] <-as.numeric(r[,50])
r[,51] <-as.numeric(r[,51])
r[,52] <-as.numeric(r[,52])
r[,53] <-as.numeric(r[,53])
r[,54] <-as.numeric(r[,54])
r[,55] <-as.numeric(r[,55])
r[,56] <-as.numeric(r[,56])
r[,57] <-as.numeric(r[,57])
r[,58] <-as.numeric(r[,58])
r[,59] <-as.numeric(r[,59])
r[,60] <-as.numeric(r[,60])
r[,61] <-as.numeric(r[,61])
r[,62] <-as.numeric(r[,62])
r[,63] <-as.numeric(r[,63])
r[,64] <-as.numeric(r[,64])
r[,65] <-as.numeric(r[,65])
r[,66] <-as.numeric(r[,66])
r[,67] <-as.numeric(r[,67])
r[,68] <-as.numeric(r[,68])
r[,69] <-as.numeric(r[,69])
r[,70] <-as.numeric(r[,70])
r[,71] <-as.character(r[,71])
r[,72] <-as.character(r[,72])
r[,73] <-as.character(r[,73])
r[,74] <-as.character(r[,74])
r[,75] <-as.character(r[,75])   
r[,76] <-as.numeric(r[,76])
r[,77] <-as.numeric(r[,77])
r[,78] <-as.numeric(r[,78])
r[,79] <-as.numeric(r[,79])
r[,80] <-as.numeric(r[,80])
r[,81] <-as.numeric(r[,81])
r[,82] <-as.numeric(r[,82])
r[,83] <-as.numeric(r[,83])
r[,84] <-as.numeric(r[,84])
r[,85] <-as.numeric(r[,85])
r[,86] <-as.numeric(r[,86])
r[,83] <-as.numeric(r[,87])
r[,88] <-as.numeric(r[,88])
r[,89] <-as.numeric(r[,89])
r[,90] <-as.numeric(r[,90])
r[,91] <-as.numeric(r[,91])
r[,92] <-as.numeric(r[,92])
r[,93] <-as.numeric(r[,93])

# make any introduced NAs (from BDL or otherwise) into zero
for (i in 7:20) {
  r[is.na(r[,i]),i] <- 0
}
for (j in 22:70) {
  r[is.na(r[,j]),j] <- 0
}
for (l in 72:93) {
  r[is.na(r[,l]),l] <- 0
}

# some sites are NotMeasured not zeroes
r[1:19,"NVOC"] <- NA
r[1:19,"TKN"] <- NA
r[1:19,"Tl.ppm"] <- NA
r[1:19,"S.ppm"] <- NA
r[1:19,"P.ppm"] <- NA

# rearrange data so that PPCP data is long under a column called PPCP
longr <- melt(r[,], id=c("Site", "Year", "Month", "Day","Date", "Habitat", "Latitude"), variable="variable", value="ppt") 
# melt makes things characters so have to remake them numbers
longr$value <- as.numeric(as.character(longr$value))
# reorder dates to be chronological
longr$Date <- factor(longr$Date, levels=c("12/18/2013","1/19/2014","2/3/2014","2/12/2014","4/10/2014","5/20/2014","6/9/2014",
                                          "6/10/2014","9/29/2014","9/30/2014","1/13/2015","1/14/2015","4/21/2015","4/22/2015"))
longr$year.month <-paste(longr$Year, longr$Month, sep="-") 
# subset data to only sum PPCP and top 3 most frequently detected PPCP
ppcp <- longr[longr$variable %in% c("PPCP.Sum.ppt","Gemfibrozil","Triclocarban","Trimethoprim"),]
# correct name of two sites
ppcp[ppcp$Site=="Falling Springs","Site"] <- "Falling Spring"
ppcp[ppcp$Site=="Sparrow Spring","Site"] <- "Sparrow Creek Spring"

# make ppcp plot object
PPCP <- ggplot(data=ppcp[ppcp$variable =="PPCP.Sum.ppt",], 
               mapping=aes(x=year.month, y=value, group=Site)) +
  #   labs(title = "gemflocarban (ng/L)")
  ylab(expression("Sum of PPCPs")) +
  #         xlab(expression("Sample Site")) +
  geom_point(aes(shape=Site),size=3, position=position_jitter(width=.6,height=0)) +   # position_dodge(width=1)
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous(limits=c(0.0000001,150), breaks=c(0.0000001,50,100,150), labels=c("0","50","100","150")) +
  scale_x_discrete() +
  #   guides(fill = guide_legend(override.aes = list(colour = NULL),ncol=2)) +
  #     guides(group=guide_legend(ncol=3)) + 
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust= 0.5, vjust=1.3,size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=8, color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size=20, face="bold"),
        plot.margin = unit(c(0,0,.2,0), "cm"),
        legend.position = "none",
        #         legend.position= c(.87,.74),
        legend.text = element_text(size=8),
        legend.title=element_blank())
print(PPCP)
# make Gemfibrozil plot object
Gem <- ggplot(data=ppcp[ppcp$variable =="Gemfibrozil",], 
              mapping=aes(x=year.month, y=value, group=Site)) +
  #   labs(title = "gemflocarban (ng/L)")
  ylab(expression("Gemfibrozil")) +
  #         xlab(expression("Sample Site")) +
  geom_point(aes(shape=Site),size=3, position=position_jitter(width=.6,height=0)) +   # position_dodge(width=1)
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous(limits=c(0.0000001,150), breaks=c(0.0000001,50,100,150), labels=c("0","50","100","150")) +
  scale_x_discrete() +
  #   guides(fill = guide_legend(override.aes = list(colour = NULL),ncol=2)) +
  #     guides(group=guide_legend(ncol=3)) + 
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust= 0.5, vjust=1.3,size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=8, color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size=20, face="bold"),
        plot.margin = unit(c(0,0,.2,0), "cm"),
        legend.position = "none",
        #         legend.position= c(.87,.74),
        legend.text = element_text(size=8),
        legend.title=element_blank())
print(Gem)
# make Triclocarban plot object
Tcc <- ggplot(data=ppcp[ppcp$variable =="Triclocarban",], 
              mapping=aes(x=year.month, y=value, group=Site)) +
  #   labs(title = "gemflocarban (ng/L)")
  ylab(expression("Triclocarban")) +
  #         xlab(expression("Sample Site")) +
  geom_point(aes(shape=Site),size=3, position=position_jitter(width=.6,height=0)) +   # position_dodge(width=1)
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous(limits=c(0.0000001,10), breaks=c(0.0000001,2,4,6,8,10), labels=c("   0","   2","   4","   6","   8","  10") ) +
  scale_x_discrete() +
  #   guides(fill = guide_legend(override.aes = list(colour = NULL),ncol=2)) +
  #     guides(group=guide_legend(ncol=3)) + 
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust= 0.5, vjust=1.3,size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=8, color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size=20, face="bold"),
        plot.margin = unit(c(0,0,.2,0), "cm"),
        legend.position = "none",
        #         legend.position= c(.87,.74),
        legend.text = element_text(size=8),
        legend.title=element_blank())
print(Tcc)
# make Trimethoprim plot object
Tmp <- ggplot(data=ppcp[ppcp$variable =="Trimethoprim",], 
              mapping=aes(x=year.month, y=value, group=Site)) +
  #   labs(title = "gemflocarban (ng/L)")
  ylab(expression("Trimethoprim")) +
  #         xlab(expression("Sample Site")) +
  geom_point(aes(shape=Site),size=3, position=position_jitter(width=.6,height=0)) +   # position_dodge(width=1)
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous(limits=c(0.0000001,10), breaks=c(0.0000001,2,4,6,8,10), labels=c("  0","  2","  4","  6","  8"," 10") ) +
  scale_x_discrete(drop = F, labels=c("Dec","Jan","Feb","Apr","May","Jun","Sep","Jan","Apr") ) +
  #   scale_x_discrete(breaks=c(2013-12,2014-1,2014-2,2014-4,2014-5,2014-6,2014-9,2015-1,2015-4), 
  #                    labels=c("2013-12","2014-1","2014-2","2014-4","2014-5","2014-6","2014-9","2015-1","2015-4")  ) +
  #   #   guides(fill = guide_legend(override.aes = list(colour = NULL),ncol=2)) +
  #     guides(group=guide_legend(ncol=3)) + 
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust= 0.5, vjust=1.3,size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=9, color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=8, color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size=20, face="bold"),
        plot.margin = unit(c(0,0,.1,0), "cm"),
        legend.position = "none",
        #         legend.position= c(.87,.74),
        legend.text = element_text(size=8),
        legend.title=element_blank())
print(Tmp)
# make legend plot object
legend <- ggplot(data=ppcp[ppcp$variable =="Trimethoprim",], 
                 mapping=aes(x=year.month, y=value, group=Site)) +
  #   labs(title = "gemflocarban (ng/L)")
  ylab(expression("Trimethoprim")) +
  #         xlab(expression("Sample Site")) +
  geom_point(aes(shape=Site),size=3, position=position_jitter(width=.6,height=0)) +   # position_dodge(width=1)
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +
  scale_y_continuous(limits=c(0.0000001,10), breaks=c(0.0000001,2,4,6,8,10), labels=c("   0","   2","   4","   6","   8","  10") ) +
  scale_x_discrete() +
  #     guides(group=guide_legend(ncol=3)) + 
  guides(shape = guide_legend(byrow=F,ncol = 3,reverse=F, label.position="top",label.vjust = 0.5,label.hjust=.5)) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust= 0.5, vjust=1.3,size = 9),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size=8, color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=8, color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size=20, face="bold"),
        plot.margin = unit(c(0,0,.1,0), "cm"),
        legend.key = element_blank(),
        legend.position = "right",
        #         legend.position= c(.87,.74),
        legend.text = element_text(angle=90,size=10),
        legend.title=element_blank())
#   guides(size = guide_legend(override.aes = list(colour = NULL),ncol=10))
print(legend)
# steal the site legend from a plot
library(gridExtra)
g_legend <- function(a) {
  tmp <- ggplot_gtable(ggplot_build(a))
  leg <- which(sapply(tmp$grobs,function(x){x$name})=="guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
the.legend <- g_legend(legend)

jpeg("karst.manuscript.fig.2.PPCP.detecs.jpg",width=5,height=8, units= "in", res=150)

pushViewport(viewport(layout=grid.layout(nrow=4,ncol=2,widths=c(1,.3),heights=c(1,1,1,1))))
print(PPCP,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(Gem,vp=viewport(layout.pos.row=3,layout.pos.col=1))
print(Tcc,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(Tmp,vp=viewport(layout.pos.row=4,layout.pos.col=1))

pushViewport(viewport(layout.pos.row=c(1:4),layout.pos.col=2))
pushViewport(viewport(x=unit(0,"npc"),y=unit(0.5,"npc"),
                      width=gtable_width(the.legend),just="left"))
grid.draw(the.legend)
popViewport(2)

popViewport()
dev.off()

###################### Figure 3 dendrogram and biplot of clustering will be made by Don, code here

# load table of data 
reptable<- read.csv("Karst.all.data.csv", sep=",",head=T)
r<-reptable
#make variables categorical (character) or continuous(numeric/double)
r[,1] <- as.numeric(r[,1])
r[,2] <-as.character(r[,2])
r[,3] <-as.character(r[,3])
r[,4] <-as.character(r[,4])
r[,5] <-as.character(r[,5])
r[,6] <-as.character(r[,6])
r[,7] <-as.numeric(r[,7])
r[,8] <-as.numeric(r[,8])
r[,9] <-as.numeric(r[,9])
r[,10] <-as.numeric(r[,10])
r[,11] <-as.numeric(r[,11])
r[,12] <-as.numeric(r[,12])
r[,13] <-as.numeric(r[,13])
r[,14] <-as.numeric(r[,14])
r[,15] <-as.numeric(r[,15])
r[,16] <-as.numeric(r[,16])
r[,17] <-as.numeric(r[,17])
r[,18] <-as.numeric(r[,18])
r[,19] <-as.numeric(r[,19])
r[,20] <-as.numeric(r[,20])
r[,21] <-as.character(r[,21])
r[,22] <-as.numeric(r[,22])
r[,23] <-as.numeric(r[,23])
r[,24] <-as.numeric(r[,24])
r[,25] <-as.numeric(r[,25])
r[,26] <-as.numeric(r[,26])
r[,27] <-as.numeric(r[,27])
r[,28] <-as.numeric(r[,28])
r[,29] <-as.numeric(r[,29])
r[,30] <-as.numeric(r[,30])
r[,31] <-as.numeric(r[,31])
r[,32] <-as.numeric(r[,32])
r[,33] <-as.numeric(r[,33])
r[,34] <-as.numeric(r[,34])
r[,35] <-as.numeric(r[,35])
r[,36] <-as.numeric(r[,36])
r[,37] <-as.numeric(r[,37])
r[,38] <-as.numeric(r[,38])
r[,39] <-as.numeric(r[,39])
r[,40] <-as.numeric(r[,40])
r[,41] <-as.numeric(r[,41])
r[,42] <-as.numeric(r[,42])
r[,43] <-as.numeric(r[,43])
r[,44] <-as.numeric(r[,44])
r[,45] <-as.numeric(r[,45])
r[,46] <-as.numeric(r[,46])
r[,47] <-as.numeric(r[,47])
r[,48] <-as.numeric(r[,48])
r[,49] <-as.numeric(r[,49])
r[,50] <-as.numeric(r[,50])
r[,51] <-as.numeric(r[,51])
r[,52] <-as.numeric(r[,52])
r[,53] <-as.numeric(r[,53])
r[,54] <-as.numeric(r[,54])
r[,55] <-as.numeric(r[,55])
r[,56] <-as.numeric(r[,56])
r[,57] <-as.numeric(r[,57])
r[,58] <-as.numeric(r[,58])
r[,59] <-as.numeric(r[,59])
r[,60] <-as.numeric(r[,60])
r[,61] <-as.numeric(r[,61])
r[,62] <-as.numeric(r[,62])
r[,63] <-as.numeric(r[,63])
r[,64] <-as.numeric(r[,64])
r[,65] <-as.numeric(r[,65])
r[,66] <-as.numeric(r[,66])
r[,67] <-as.numeric(r[,67])
r[,68] <-as.numeric(r[,68])
r[,69] <-as.numeric(r[,69])
r[,70] <-as.numeric(r[,70])
r[,71] <-as.character(r[,71])
r[,72] <-as.character(r[,72])
r[,73] <-as.character(r[,73])
r[,74] <-as.character(r[,74])
r[,75] <-as.character(r[,75])   
r[,76] <-as.numeric(r[,76])
r[,77] <-as.numeric(r[,77])
r[,78] <-as.numeric(r[,78])
r[,79] <-as.numeric(r[,79])
r[,80] <-as.numeric(r[,80])
r[,81] <-as.numeric(r[,81])
r[,82] <-as.numeric(r[,82])
r[,83] <-as.numeric(r[,83])
r[,84] <-as.numeric(r[,84])
r[,85] <-as.numeric(r[,85])
r[,86] <-as.numeric(r[,86])
r[,83] <-as.numeric(r[,87])
r[,88] <-as.numeric(r[,88])
r[,89] <-as.numeric(r[,89])
r[,90] <-as.numeric(r[,90])
r[,91] <-as.numeric(r[,91])
r[,92] <-as.numeric(r[,92])
r[,93] <-as.numeric(r[,93])

# make any introduced NAs (from BDL or otherwise) into zero
for (i in 7:20) {
  r[is.na(r[,i]),i] <- 0
}
for (j in 22:70) {
  r[is.na(r[,j]),j] <- 0
}
for (l in 72:93) {
  r[is.na(r[,l]),l] <- 0
}

# some sites are NotMeasured not zeroes
r[1:19,"NVOC"] <- NA
r[1:19,"TKN"] <- NA
r[1:19,"Tl.ppm"] <- NA
r[1:19,"S.ppm"] <- NA
r[1:19,"P.ppm"] <- NA

# remove Collier Spring and Indina Hole from Site subsets; has only one sample so only one level for any variable
r <- r[r$Site!="Collier Spring",]
r <- r[r$Site!="Indian Hole Spring",]

# remove Year, Month, and Day and Date - not relevant factors
drops <- c("Year","Month","Day","Date")
r <- r[,!(names(r) %in% drops)]

#object to save to
glmsum <- NULL

# iterative performance of glm on each variable for each site, skipping factors that only have one level in a site
for (Site in levels(factor(r$Site))) {
  #   for (Variable in 4:ncol(r)) {
  for (Variable in names(r)) {
    if (length(levels(factor(r[r$Site==Site,Variable]))) < 2 ) {next;}
    else {
      #     glms <- glm(as.formula(paste0("PPCP.Sum.ppt ~ ",Variable,"+Site+0")), data=r)
      glms <- glm(as.formula(paste0("PPCP.Sum.ppt ~ ",Variable)), data=r[r$Site==Site,])
      Column <- Variable
      Coefficient <- NA
      try (Coefficient <- summary(glms)$coefficients[2,1], silent=T )
      Pr.T.Test <- NA
      try (Pr.T.Test <- summary(glms)$coefficients[2,4], silent=T )
      AICcScore <- NA
      try (AICcScore <- AICc(glms))
      glmsum <- rbind(glmsum,data.frame(Site,Column,Coefficient,Pr.T.Test,AICcScore)) } }}

# take glmsum and remove t.test and AIC values, then cast wide into rows of site, 
# then divide each coefficient by its absolute value to cast a matrix of -1,0,1
library(reshape2)
glmshort <- glmsum[,1:3]
coef <- dcast(data=glmshort, formula=Site~Column, fill=as.numeric(0),drop=F)
coeftable <- coef[,-1]/abs(coef[,-1])
rownames(coeftable) <- coef[,1]

## replace NaN with 0
coeftable[!is.finite(as.matrix(coeftable))] <- 0
# correct site names
rownames(coeftable) <-
  gsub("Falling Springs","Falling Spring",rownames(coeftable))
rownames(coeftable) <-
  gsub("Sparrow Spring","Sparrow Creek Spring",rownames(coeftable))

# random seed
# ## ggplot(coef.pca,aes(x=PC1,y=PC2,color=cluster))+geom_point()+stat_ellipse(type="norm")
# pdf(file="~/laurel_cluster_plot.pdf",width=6,height=4)
# layout(mat=matrix(1:2,nrow=1))
# par(mar=c(6,2,0,0))
# fviz_dend(hcut(coeftable,3),rect=TRUE,k_colors="black",ylab="",main="")
# grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
# print(factoextra::fviz_pca_ind(prcomp(coeftable),
#                                invisible="quali",
#                                axes.linetype=NA,
#                                repel=TRUE,
#                                habillage=coef.pca$cluster,
#                                labelsize=3,
#                                title=NULL
# )+theme_bw()
# + scale_colour_manual(name="Cluster",values=c("grey60","grey40","black"))
# + scale_shape_discrete(name="Cluster")
# + theme(legend.position="bottom"),
# vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
# dev.off()
# 

# second try with seed set to replicate first subsetting
# load("~/Laurel.Karst.Coefficient.Dataframe.Rdata")
# ## replace NaN with 0
# coeftable[!is.finite(as.matrix(coeftable))] <- 0
# ## plot(hclust(dist(coeftable)))
# coef.pca <- as.data.frame(prcomp(coeftable)$x[,1:2])
# set.seed(1)
# coef.pca$cluster <-
#   factor(kmeans(as.matrix(coeftable),3)$cluster)
# ## ggplot(coef.pca,aes(x=PC1,y=PC2,color=cluster))+geom_point()+stat_ellipse(type="norm")
# pdf(file="~/laurel_cluster_plot.pdf",width=6,height=4)
# layout(mat=matrix(1:2,nrow=1))
# par(mar=c(6,2,0,0))
# fviz_dend(hcut(coeftable,3,stand=TRUE),rect=TRUE,k_colors="black",ylab="",main="")
# grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
# print(factoextra::fviz_pca_ind(prcomp(coeftable),
#                                invisible="quali",
#                                axes.linetype=NA,
#                                repel=TRUE,
#                                habillage=coef.pca$cluster,
#                                labelsize=3,
#                                title=NULL
# )+theme_bw()
# + scale_colour_manual(name="Cluster",values=c("grey60","grey40","black"))
# + scale_shape_discrete(name="Cluster")
# + theme(legend.position="bottom"),
# vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
# dev.off()
# 
# ## determined that optimal clustering is from the first code because improved sum of squares (between_SS / total_SS =  48 %)
# 

# Don plots
# ## plot(hclust(dist(coeftable)))
# coef.pca <- as.data.frame(prcomp(coeftable)$x[,1:2])
# set.seed(4)
# coef.pca$cluster <-
#   factor(kmeans(as.matrix(coeftable),3)$cluster)
# ## ggplot(coef.pca,aes(x=PC1,y=PC2,color=cluster))+geom_point()+stat_ellipse(type="norm")
# set.seed(7)
# pdf(file="~/laurel_cluster_plot.pdf",width=6,height=4)
# layout(mat=matrix(1:2,nrow=1))
# par(mar=c(8.5,2,0,0))
# fviz_dend(hcut(coeftable,3),rect=TRUE,k_colors="black",ylab="",main="")
# grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
# print(factoextra::fviz_pca_ind(prcomp(coeftable),
#                                invisible="quali",
#                                axes.linetype=NA,
#                                repel=TRUE,
#                                habillage=coef.pca$cluster,
#                                labelsize=3.2,
#                                title=NULL
# )+theme_bw()
# + scale_colour_manual(name="Cluster",values=c("grey40","grey20","black"))
# + scale_shape_discrete(name="Cluster")
# + theme(legend.position="bottom"),
# vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
# dev.off()
# 
# 
# set.seed(7)
# jpeg(file="~/laurel_cluster.jpeg",width=6,height=4,units="in",res=300)
# layout(mat=matrix(1:2,nrow=1))
# par(mar=c(8.5,2,0,0))
# fviz_dend(hcut(coeftable,3),rect=TRUE,k_colors="black",ylab="",main="")
# grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
# print(factoextra::fviz_pca_ind(prcomp(coeftable),
#                                invisible="quali",
#                                axes.linetype=NA,
#                                repel=TRUE,
#                                habillage=coef.pca$cluster,
#                                labelsize=3.2,
#                                title=NULL
# )+theme_bw()
# + scale_colour_manual(name="Cluster",values=c("grey40","grey20","black"))
# + scale_shape_discrete(name="Cluster")
# + theme(legend.position="bottom"),
# vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
# dev.off()
# 
# # without tree y-axis
# load("~/Laurel.Karst.Coefficient.Dataframe.Rdata")
# ## replace NaN with 0
# rownames(coeftable) <-
#   gsub("Falling Springs","Falling Spring",rownames(coeftable))
# rownames(coeftable) <-
#   gsub("Sparrow Spring","Sparrow Creek Spring",rownames(coeftable))
# coeftable[!is.finite(as.matrix(coeftable))] <- 0
# ## plot(hclust(dist(coeftable)))
# coef.pca <- as.data.frame(prcomp(coeftable)$x[,1:2])
# set.seed(4)
# coef.pca$cluster <-
#   factor(kmeans(as.matrix(coeftable),3)$cluster)
# ## ggplot(coef.pca,aes(x=PC1,y=PC2,color=cluster))+geom_point()+stat_ellipse(type="norm")
# set.seed(7)
# pdf(file="~/laurel_cluster_plot.pdf",width=6,height=4)
# layout(mat=matrix(1:2,nrow=1))
# par(mar=c(8.5,0,0,0))
# fviz_dend(hcut(coeftable,3),rect=TRUE,k_colors="black",ylab="",main="",axes=FALSE)
# grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
# print(factoextra::fviz_pca_ind(prcomp(coeftable),
#                                invisible="quali",
#                                axes.linetype=NA,
#                                repel=TRUE,
#                                habillage=coef.pca$cluster,
#                                labelsize=3.2,
#                                title=NULL
# )+theme_bw()
# + scale_colour_manual(name="Cluster",values=c("grey40","grey20","black"))
# + scale_shape_discrete(name="Cluster")
# + theme(legend.position="bottom"),
# vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
# dev.off()
# 
# 
# set.seed(7)
# jpeg(file="~/laurel_cluster.jpeg",width=6,height=4,units="in",res=300)
# layout(mat=matrix(1:2,nrow=1))
# par(mar=c(8.5,0,0,0))
# fviz_dend(hcut(coeftable,3),rect=TRUE,k_colors="black",ylab="",main="",axes=FALSE)
# grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
# print(factoextra::fviz_pca_ind(prcomp(coeftable),
#                                invisible="quali",
#                                axes.linetype=NA,
#                                repel=TRUE,
#                                habillage=coef.pca$cluster,
#                                labelsize=3.2,
#                                title=NULL
# )+theme_bw()
# + scale_colour_manual(name="Cluster",values=c("grey40","grey20","black"))
# + scale_shape_discrete(name="Cluster")
# + theme(legend.position="bottom"),
# vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
# dev.off()

## plot(hclust(dist(coeftable)))
coef.pca <- as.data.frame(prcomp(coeftable)$x[,1:2])
set.seed(4)
coef.pca$cluster <-
  factor(kmeans(as.matrix(coeftable),3)$cluster)
# remake plots with rename clusters and cluster groups in dendrogram
## ggplot(coef.pca,aes(x=PC1,y=PC2,color=cluster))+geom_point()+stat_ellipse(type="norm")

# first plot with dendrogram forward
set.seed(7) # shows same relationships but jitters site labels slightly so they don't overlap on biplot
jpeg(file="karst.manuscript.older.fig.3.dendro.pca.jpeg",width=6,height=4,units="in",res=300)
layout(mat=matrix(1:2,nrow=1))
par(mar=c(9,0,0,0))
# fviz_dend(hcut(coeftable,3),rect=TRUE,k_colors="black",ylab="",xlab="C III      C II                  C I                ",
#           show_labels=T,main="",axes=T)
fviz_dend(hcut(coeftable,3),rect=TRUE,rect_border = c("black","grey25","grey40"),ylab="",k_colors=c("black","grey20","grey40"),
          show_labels=T,main="",axes=F)
grid.text(label="C III    C II               Cluster I", x=unit(0.035, "npc"),y=unit(0.015, "npc"), just=c("left","bottom"))
# grid.rect(x = unit(.2, "npc"), y = unit(.2, "npc"), width = unit(.2, "npc"), height = unit(.2, "npc"))
# segments(x0=unit(.2,"npc"), y0=unit(.2,"npc"), x1 = =unit(.2,"npc"), y1 = =unit(.2,"npc"),
#          col = par("npc")
grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
print(factoextra::fviz_pca_ind(prcomp(coeftable),
                               invisible="quali",
                               axes.linetype=NA,
                               repel=TRUE,
                               habillage=coef.pca$cluster,
                               labelsize=3.2,
                               title=NULL)
+theme_bw()
+ scale_colour_manual(name="Cluster",labels=c("I","II","III"),values=c("grey40","grey25","black"))
+ scale_shape_discrete(name="Cluster",labels=c("I","II","III"))
+ theme(legend.position="bottom"),
vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
dev.off()

# better second plot with dendrogram reversed; code by Don to reverse order and keep rect and color
special.rev <- function(x){temp <- rev(x); class(temp) <- class(x); 
                           temp$nbclust <- x$nbclust; temp$cluster <- x$cluster; temp$silinfo <- x$silinfo; temp}
set.seed(7)
jpeg(file="karst.manuscript.fig.3.dendro.pca.jpeg",width=6,height=4,units="in",res=300)
layout(mat=matrix(1:2,nrow=1))
par(mar=c(9,0,0,0))
# fviz_dend(hcut(coeftable,3),rect=TRUE,k_colors="black",ylab="",xlab="C III      C II                  C I                ",
#           show_labels=T,main="",axes=T)
fviz_dend(special.rev(hcut(coeftable,3)),rect=TRUE,rect_border = c("grey40","grey25","black"),ylab="",
          k_colors=c("grey40","grey25","black"),show_labels=T,main="",axes=F)
grid.text(label="Cluster I             C II     C III", x=unit(0.125, "npc"),y=unit(0.015, "npc"), just=c("left","bottom"))
# grid.rect(x = unit(.2, "npc"), y = unit(.2, "npc"), width = unit(.2, "npc"), height = unit(.2, "npc"))
# segments(x0=unit(.2,"npc"), y0=unit(.2,"npc"), x1 = =unit(.2,"npc"), y1 = =unit(.2,"npc"),
#          col = par("npc")
grid::pushViewport(grid::viewport(layout=grid::grid.layout(nrow=1,ncol=2)))
print(factoextra::fviz_pca_ind(prcomp(coeftable),
                               invisible="quali",
                               axes.linetype=NA,
                               repel=TRUE,
                               habillage=coef.pca$cluster,
                               labelsize=3.2,
                               title=NULL)
      +theme_bw()
      + scale_colour_manual(name="Cluster",labels=c("I","II","III"),values=c("grey40","grey25","black"))
      + scale_shape_discrete(name="Cluster",labels=c("I","II","III"))
      + theme(legend.position="bottom"),
      vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
dev.off()

################### Figure 4  - PPCP sum by a sig factor for each cluster

# load table of data 
reptable<- read.csv("Karst.all.data.csv", sep=",",head=T)
r<-reptable
#make variables categorical (character) or continuous(numeric/double)
r[,1] <- as.numeric(r[,1])
r[,2] <-as.character(r[,2])
r[,3] <-as.character(r[,3])
r[,4] <-as.character(r[,4])
r[,5] <-as.character(r[,5])
r[,6] <-as.character(r[,6])
r[,7] <-as.numeric(r[,7])
r[,8] <-as.numeric(r[,8])
r[,9] <-as.numeric(r[,9])
r[,10] <-as.numeric(r[,10])
r[,11] <-as.numeric(r[,11])
r[,12] <-as.numeric(r[,12])
r[,13] <-as.numeric(r[,13])
r[,14] <-as.numeric(r[,14])
r[,15] <-as.numeric(r[,15])
r[,16] <-as.numeric(r[,16])
r[,17] <-as.numeric(r[,17])
r[,18] <-as.numeric(r[,18])
r[,19] <-as.numeric(r[,19])
r[,20] <-as.numeric(r[,20])
r[,21] <-as.character(r[,21])
r[,22] <-as.numeric(r[,22])
r[,23] <-as.numeric(r[,23])
r[,24] <-as.numeric(r[,24])
r[,25] <-as.numeric(r[,25])
r[,26] <-as.numeric(r[,26])
r[,27] <-as.numeric(r[,27])
r[,28] <-as.numeric(r[,28])
r[,29] <-as.numeric(r[,29])
r[,30] <-as.numeric(r[,30])
r[,31] <-as.numeric(r[,31])
r[,32] <-as.numeric(r[,32])
r[,33] <-as.numeric(r[,33])
r[,34] <-as.numeric(r[,34])
r[,35] <-as.numeric(r[,35])
r[,36] <-as.numeric(r[,36])
r[,37] <-as.numeric(r[,37])
r[,38] <-as.numeric(r[,38])
r[,39] <-as.numeric(r[,39])
r[,40] <-as.numeric(r[,40])
r[,41] <-as.numeric(r[,41])
r[,42] <-as.numeric(r[,42])
r[,43] <-as.numeric(r[,43])
r[,44] <-as.numeric(r[,44])
r[,45] <-as.numeric(r[,45])
r[,46] <-as.numeric(r[,46])
r[,47] <-as.numeric(r[,47])
r[,48] <-as.numeric(r[,48])
r[,49] <-as.numeric(r[,49])
r[,50] <-as.numeric(r[,50])
r[,51] <-as.numeric(r[,51])
r[,52] <-as.numeric(r[,52])
r[,53] <-as.numeric(r[,53])
r[,54] <-as.numeric(r[,54])
r[,55] <-as.numeric(r[,55])
r[,56] <-as.numeric(r[,56])
r[,57] <-as.numeric(r[,57])
r[,58] <-as.numeric(r[,58])
r[,59] <-as.numeric(r[,59])
r[,60] <-as.numeric(r[,60])
r[,61] <-as.numeric(r[,61])
r[,62] <-as.numeric(r[,62])
r[,63] <-as.numeric(r[,63])
r[,64] <-as.numeric(r[,64])
r[,65] <-as.numeric(r[,65])
r[,66] <-as.numeric(r[,66])
r[,67] <-as.numeric(r[,67])
r[,68] <-as.numeric(r[,68])
r[,69] <-as.numeric(r[,69])
r[,70] <-as.numeric(r[,70])
r[,71] <-as.character(r[,71])
r[,72] <-as.character(r[,72])
r[,73] <-as.character(r[,73])
r[,74] <-as.character(r[,74])
r[,75] <-as.character(r[,75])   
r[,76] <-as.numeric(r[,76])
r[,77] <-as.numeric(r[,77])
r[,78] <-as.numeric(r[,78])
r[,79] <-as.numeric(r[,79])
r[,80] <-as.numeric(r[,80])
r[,81] <-as.numeric(r[,81])
r[,82] <-as.numeric(r[,82])
r[,83] <-as.numeric(r[,83])
r[,84] <-as.numeric(r[,84])
r[,85] <-as.numeric(r[,85])
r[,86] <-as.numeric(r[,86])
r[,83] <-as.numeric(r[,87])
r[,88] <-as.numeric(r[,88])
r[,89] <-as.numeric(r[,89])
r[,90] <-as.numeric(r[,90])
r[,91] <-as.numeric(r[,91])
r[,92] <-as.numeric(r[,92])
r[,93] <-as.numeric(r[,93])

# make any introduced NAs (from BDL or otherwise) into zero
for (i in 7:20) {
  r[is.na(r[,i]),i] <- 0
}
for (j in 22:70) {
  r[is.na(r[,j]),j] <- 0
}
for (l in 72:93) {
  r[is.na(r[,l]),l] <- 0
}

# some sites are NotMeasured not zeroes
r[1:19,"NVOC"] <- NA
r[1:19,"TKN"] <- NA
r[1:19,"Tl.ppm"] <- NA
r[1:19,"S.ppm"] <- NA
r[1:19,"P.ppm"] <- NA

# correct name of two sites
r[r$Site=="Falling Springs","Site"] <- "Falling Spring"
r[r$Site=="Sparrow Spring","Site"] <- "Sparrow Creek Spring"

Cluster1 <- c("Falling Spring","Camp Vandeventer Spring","Fogelpole Cave 1","Fogelpole Cave 2","Frog Spring",
              "Illinois Caverns","Kelly Spring")
Cluster2 <- c("Auctioneer Spring","Sparrow Creek Spring" )
Cluster3 <- c("Stemler Cave","Fogelpole Cave 3")

## CLuster 1 plot

GH <- ggplot(data=r[r$Site %in% Cluster1, ] ,
                   mapping=aes(x=TwoDay.Mean.Gage.Height.ft, 
                               y=PPCP.Sum.ppt))
GH <- GH + ylab(expression(paste("PPCP Sum (ng/L)"))) + 
  xlab(expression(paste("Stream Gauge Height, 2 day mean (feet)"))) + geom_point(size=5) +
  scale_y_continuous(breaks=c(0,10,20,30),labels=c("   0","  10","  20","  30"), limits=c(-5,35), oob=censor) +
  scale_x_continuous(breaks=c(25,26,27,28,29),labels=c("25","26","27","28","29"), limits=c(25,29)) +
  stat_smooth(data=r[r$Site %in% Cluster1, ], 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  theme(panel.background=NULL,
                             panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank(),
                             axis.title.y = element_text(hjust=0.5, vjust=0,size = 16),
                             axis.title.x = element_text(hjust=0.5, vjust=0,size = 16),
                             axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
                             axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
                             strip.text = element_blank(),
                             strip.background = element_blank(),
                             plot.title = element_blank(),
                             legend.background=element_blank(),
                             legend.key=element_blank(),
                             legend.title = element_blank(),
                             legend.position = "none",
                             # legend.position = c(1,1),
                             legend.margin=unit(.0, "in"),
                             legend.text = element_text(size=11, hjust=0),
                             legend.key.size = unit(.35, "in"),
                             legend.text.align = 0)   +
  geom_text(aes(label="Cluster I Sites",x=28,y=30),size=6,parse=F,inherit.aes=F)

print(GH)

Bo <- ggplot(data=r[r$Site %in% Cluster1, ] ,
             mapping=aes(x=B.ppm, 
                         y=PPCP.Sum.ppt))
Bo <- Bo + ylab(expression(paste("PPCP Sum (ng/L)"))) + 
  xlab(expression(paste("Boron (mg/L)"))) + geom_point(size=5) +
  scale_y_continuous(breaks=c(0,10,20,30),labels=c("0","10","20","30"), limits=c(-5,35), oob=censor) +
  scale_x_continuous(breaks=c(0,.03,.06,.09),labels=c("25","26","27",".09"), limits=c(0,.05)) +
  stat_smooth(data=r[r$Site %in% Cluster1, ], 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust=0.5, vjust=0,size = 16),
        axis.title.x = element_text(hjust=0.5, vjust=0,size = 16),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        # legend.position = c(1,1),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=11, hjust=0),
        legend.key.size = unit(.35, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="Cluster 1 Sites",x=.01,y=30),size=6,parse=F,inherit.aes=F) +
  geom_text(aes(label="p-value = 1.0 e-9",x=.01,y=24),size=4.5,parse=F,inherit.aes=F) +
  geom_text(aes(label="slope = -8.84",x=.01,y=20),size=4.5,parse=F,inherit.aes=F) 

print(Bo)

## CLuster 2 plot

Water <- ggplot(data=r[r$Site %in% Cluster2, ] ,
                  mapping=aes(x=Water.Temp.C, 
                              y=PPCP.Sum.ppt))
Water <- Water + ylab(expression(paste("PPCP Sum (ng/L)"))) + 
  xlab(expression(paste("Water Temperature (°C)"))) + geom_point(size=5) +
  scale_y_continuous(breaks=c(0,35,70,105,140),labels=c("0","35","70","105","140")) +
  scale_x_continuous(breaks=c(12,13,14,15,16,17), labels=c("12","13","14","15","16","17")) +
  stat_smooth(data=r[r$Site %in% Cluster2, ], 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust=0.5, vjust=2.5,size = 16),
        axis.title.x = element_text(hjust=0.5, vjust=0,size = 16),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        # legend.position = c(1,1),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=11, hjust=0),
        legend.key.size = unit(.35, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="Cluster II Sites",x=14.75,y=125),size=6,parse=F,inherit.aes=F) 

print(Water)


## CLuster 3 plot

Barium <- ggplot(data=r[r$Site %in% Cluster3, ] ,
                  mapping=aes(x=Ba.ppm, 
                              y=PPCP.Sum.ppt))
Barium <- Barium + ylab(expression(paste("PPCP Sum (ng/L)"))) + 
  xlab(expression(paste("Barium (mg/L)"))) + geom_point(size=5) +
  scale_y_continuous(breaks=c(0,12,24,36,48),labels=c("0","12","24","36","48"), limits=c(-5,55), oob=censor) +
  scale_x_continuous(breaks=c(0.06,.08,.10,.12), labels=c("0.06","0.08","0.10","0.12")) +
#   stat_smooth(data=r[r$Site %in% Cluster3, ], 
#               fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust=0.5, vjust=2.5,size = 16),
        axis.title.x = element_text(hjust=0.5, vjust=0,size = 16),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        # legend.position = c(1,1),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=11, hjust=0),
        legend.key.size = unit(.35, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="Cluster III Sites",x=.1,y=50),size=6,parse=F,inherit.aes=F) +
  geom_text(aes(label="p-value = 1.8 e-5",x=.1,y=42),size=5,parse=F,inherit.aes=F)  +
  geom_text(aes(label="slope = -1203",x=.1,y=38),size=5,parse=F,inherit.aes=F) +
  geom_segment(aes(x=0.20864, y=0, xend=0.16708, yend=50),size=1) 

print(Barium)

DO <- ggplot(data=r[r$Site %in% Cluster3, ] ,
                 mapping=aes(x=Dissolved.Oxygen.ppm, 
                             y=PPCP.Sum.ppt))
DO <- DO + ylab(expression(paste("PPCP Sum (ng/L)"))) + 
  xlab(expression(paste("Dissolved Oxygen (mg/L)"))) + geom_point(size=5) +
  scale_y_continuous(breaks=c(0,12,24,36,48),labels=c("   0","  12","  24","  36","  48"), limits=c(-5,55), oob=censor) +
  scale_x_continuous(breaks=c(4,6,8,10), labels=c("4","6","8","10")) +
  stat_smooth(data=r[r$Site %in% Cluster3, ], 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust=0.5, vjust=2.5,size = 16),
        axis.title.x = element_text(hjust=0.5, vjust=0,size = 16),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        # legend.position = c(1,1),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=11, hjust=0),
        legend.key.size = unit(.35, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="Cluster III Sites",x=9.4,y=49),size=6,parse=F,inherit.aes=F) 

print(DO)

jpeg("karst.manuscript.fig.4.clusters.jpg",width=5,height=10, units= "in", res=150)

pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1,widths=c(1),heights=c(1,1,1))))
print(GH,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(Water, vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(DO,vp=viewport(layout.pos.row=3,layout.pos.col=1))

popViewport()
dev.off()


################### Figure 5 - PPCP sum by other PPCPs subset by clusters

# load table of data 
reptable<- read.csv("Karst.all.data.csv", sep=",",head=T)
r<-reptable
#make variables categorical (character) or continuous(numeric/double)
r[,1] <- as.numeric(r[,1])
r[,2] <-as.character(r[,2])
r[,3] <-as.character(r[,3])
r[,4] <-as.character(r[,4])
r[,5] <-as.character(r[,5])
r[,6] <-as.character(r[,6])
r[,7] <-as.numeric(r[,7])
r[,8] <-as.numeric(r[,8])
r[,9] <-as.numeric(r[,9])
r[,10] <-as.numeric(r[,10])
r[,11] <-as.numeric(r[,11])
r[,12] <-as.numeric(r[,12])
r[,13] <-as.numeric(r[,13])
r[,14] <-as.numeric(r[,14])
r[,15] <-as.numeric(r[,15])
r[,16] <-as.numeric(r[,16])
r[,17] <-as.numeric(r[,17])
r[,18] <-as.numeric(r[,18])
r[,19] <-as.numeric(r[,19])
r[,20] <-as.numeric(r[,20])
r[,21] <-as.character(r[,21])
r[,22] <-as.numeric(r[,22])
r[,23] <-as.numeric(r[,23])
r[,24] <-as.numeric(r[,24])
r[,25] <-as.numeric(r[,25])
r[,26] <-as.numeric(r[,26])
r[,27] <-as.numeric(r[,27])
r[,28] <-as.numeric(r[,28])
r[,29] <-as.numeric(r[,29])
r[,30] <-as.numeric(r[,30])
r[,31] <-as.numeric(r[,31])
r[,32] <-as.numeric(r[,32])
r[,33] <-as.numeric(r[,33])
r[,34] <-as.numeric(r[,34])
r[,35] <-as.numeric(r[,35])
r[,36] <-as.numeric(r[,36])
r[,37] <-as.numeric(r[,37])
r[,38] <-as.numeric(r[,38])
r[,39] <-as.numeric(r[,39])
r[,40] <-as.numeric(r[,40])
r[,41] <-as.numeric(r[,41])
r[,42] <-as.numeric(r[,42])
r[,43] <-as.numeric(r[,43])
r[,44] <-as.numeric(r[,44])
r[,45] <-as.numeric(r[,45])
r[,46] <-as.numeric(r[,46])
r[,47] <-as.numeric(r[,47])
r[,48] <-as.numeric(r[,48])
r[,49] <-as.numeric(r[,49])
r[,50] <-as.numeric(r[,50])
r[,51] <-as.numeric(r[,51])
r[,52] <-as.numeric(r[,52])
r[,53] <-as.numeric(r[,53])
r[,54] <-as.numeric(r[,54])
r[,55] <-as.numeric(r[,55])
r[,56] <-as.numeric(r[,56])
r[,57] <-as.numeric(r[,57])
r[,58] <-as.numeric(r[,58])
r[,59] <-as.numeric(r[,59])
r[,60] <-as.numeric(r[,60])
r[,61] <-as.numeric(r[,61])
r[,62] <-as.numeric(r[,62])
r[,63] <-as.numeric(r[,63])
r[,64] <-as.numeric(r[,64])
r[,65] <-as.numeric(r[,65])
r[,66] <-as.numeric(r[,66])
r[,67] <-as.numeric(r[,67])
r[,68] <-as.numeric(r[,68])
r[,69] <-as.numeric(r[,69])
r[,70] <-as.numeric(r[,70])
r[,71] <-as.character(r[,71])
r[,72] <-as.character(r[,72])
r[,73] <-as.character(r[,73])
r[,74] <-as.character(r[,74])
r[,75] <-as.character(r[,75])   
r[,76] <-as.numeric(r[,76])
r[,77] <-as.numeric(r[,77])
r[,78] <-as.numeric(r[,78])
r[,79] <-as.numeric(r[,79])
r[,80] <-as.numeric(r[,80])
r[,81] <-as.numeric(r[,81])
r[,82] <-as.numeric(r[,82])
r[,83] <-as.numeric(r[,83])
r[,84] <-as.numeric(r[,84])
r[,85] <-as.numeric(r[,85])
r[,86] <-as.numeric(r[,86])
r[,83] <-as.numeric(r[,87])
r[,88] <-as.numeric(r[,88])
r[,89] <-as.numeric(r[,89])
r[,90] <-as.numeric(r[,90])
r[,91] <-as.numeric(r[,91])
r[,92] <-as.numeric(r[,92])
r[,93] <-as.numeric(r[,93])

# make any introduced NAs (from BDL or otherwise) into zero
for (i in 7:20) {
  r[is.na(r[,i]),i] <- 0
}
for (j in 22:70) {
  r[is.na(r[,j]),j] <- 0
}
for (l in 72:93) {
  r[is.na(r[,l]),l] <- 0
}

# some sites are NotMeasured not zeroes
r[1:19,"NVOC"] <- NA
r[1:19,"TKN"] <- NA
r[1:19,"Tl.ppm"] <- NA
r[1:19,"S.ppm"] <- NA
r[1:19,"P.ppm"] <- NA

# correct name of two sites
r[r$Site=="Falling Springs","Site"] <- "Falling Spring"
r[r$Site=="Sparrow Spring","Site"] <- "Sparrow Creek Spring"

# rearrange data so that PPCP data is long under a column called PPCP
longr <- melt(r[,], id=c("Site", "Year", "Month", "Day","Date", "Habitat", "Latitude","PPCP.Sum.ppt"),
              variable="Compound", value="ppt") 
# melt makes things characters so have to remake them numbers
longr$value <- as.numeric(as.character(longr$value))
longr$Compound <- as.character(longr$Compound)
# reorder dates to be chronological
longr$Date <- factor(longr$Date, levels=c("12/18/2013","1/19/2014","2/3/2014","2/12/2014","4/10/2014","5/20/2014","6/9/2014",
                                          "6/10/2014","9/29/2014","9/30/2014","1/13/2015","1/14/2015","4/21/2015","4/22/2015"))
longr$year.month <-paste(longr$Year, longr$Month, sep="-") 
# subset data to only PPCP data
ppcp <- longr[longr$Compound %in% c("Caffeine","Gemfibrozil","Sulfamethoxazole","Triclocarban"),]

r <-ppcp

Cluster1 <- c("Falling Spring","Camp Vandeventer Spring","Fogelpole Cave 1","Fogelpole Cave 2","Frog Spring",
              "Illinois Caverns","Kelly Spring")
Cluster2 <- c("Auctioneer Spring","Sparrow Creek Spring" )
Cluster3 <- c("Stemler Cave","Fogelpole Cave 3")


shapes<- c(0,1,2,6)
line <- c(1,3,4,5)

# make root-10 function
tenroot_trans = function() trans_new('tenroot', transform= function(x) x^(1/10), inverse = function(x) x^10 )
tworoot_trans = function() trans_new('tworoot', transform= function(x) x^(1/4), inverse = function(x) x^4 )


## CLuster 1 plot
ClusA <- NULL
ClusA <- ggplot(data=r[r$Site %in% Cluster1, ] ,
                mapping=aes(x=value, y=PPCP.Sum.ppt, group=Compound, 
                            shape=Compound, linetype=Compound))
ClusA <- ClusA + # labs(legend="All Sites") +
  ylab(expression(paste("Sum of Detected PPCPs (ng/L)"))) + 
  xlab(expression(paste("Individual PPCP (ng/L)"))) + geom_point(size=5) +
  scale_y_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01","0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=censor) +
  scale_x_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01   ","  0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=squish) +
  scale_shape_manual(values=shapes) +
  stat_smooth(aes(linetype=Compound),data=r[r$Site %in% Cluster1, ] , 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  scale_linetype_manual(values = line) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
        #         legend.title = element_text(hjust=0.5, vjust=0,size = 19),
        legend.title = element_blank(),
        legend.title.align = unit(.5,"in"),
        legend.background=element_blank(),
        legend.key=element_blank(),
        #         legend.position = "top",
        legend.position = c(.7,.13),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=12, hjust=0),
        legend.key.height = unit(.2, "in"),
        legend.key.width = unit(.6, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="Cluster 1 Sites",x=50,y=1),size=8,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.01",x=0.1,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +1.83",x=0.1,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.0001",x=13,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +1.16",x=13,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.24",x=108,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +5.16",x=108,y=.000001),size=3.75,parse=F,inherit.aes=F) +
  
  guides(shape=guide_legend(title="Cluster 1 Sites",byrow=T,ncol = 1,reverse=F), linetype=guide_legend(title="Cluster 1 Sites",byrow=T,ncol = 1,reverse=F) )  

print(ClusA)

## CLuster 2 plot
ClusB <- NULL
ClusB <- ggplot(data=r[r$Site %in% Cluster2, ] ,
                mapping=aes(x=value, y=PPCP.Sum.ppt, group=Compound, 
                            shape=Compound, linetype=Compound))
ClusB <- ClusB + # labs(legend="All Sites") +
  ylab(expression(paste("Sum of Detected PPCPs (ng/L)"))) + 
  xlab(expression(paste("Individual PPCP (ng/L)"))) + geom_point(size=5) +
  scale_y_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01","0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=censor) +
  scale_x_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01   ","  0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=squish) +
  scale_shape_manual(values=shapes) +
  stat_smooth(aes(linetype=Compound),data=r[r$Site %in% Cluster2, ], 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  scale_linetype_manual(values = line) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust=0.5, vjust=0,size = 16),
        axis.title.x = element_text(hjust=0.5, vjust=0,size = 16),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
        #         legend.title = element_text(hjust=0.5, vjust=0,size = 19),
        legend.title = element_blank(),
        legend.title.align = unit(.5,"in"),
        legend.background=element_blank(),
        legend.key=element_blank(),
        #         legend.position = "top",
        legend.position = c(.7,.13),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=12, hjust=0),
        legend.key.height = unit(.2, "in"),
        legend.key.width = unit(.6, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="Cluster 2 Sites",x=48,y=1),size=8,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.05",x=0.1,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +0.88",x=0.1,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.24",x=13,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +3.60",x=13,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.65",x=108,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +3.08",x=108,y=.000001),size=3.75,parse=F,inherit.aes=F) +
  
  guides(shape=guide_legend(title="Cluster 2 Sites",byrow=T,ncol = 1,reverse=F), linetype=guide_legend(title="Cluster 2 Sites",byrow=T,ncol = 1,reverse=F) )  

print(ClusB)

## CLuster 3 plot
ClusC <- NULL
ClusC <- ggplot(data=r[r$Site %in% Cluster3, ] ,
                mapping=aes(x=value, y=PPCP.Sum.ppt, group=Compound, 
                            shape=Compound, linetype=Compound))
ClusC <- ClusC + # labs(legend="All Sites") +
  ylab(expression(paste("Sum of Detected PPCPs (ng/L)"))) + 
  xlab(expression(paste("Individual PPCP (ng/L)"))) + geom_point(size=5) +
  scale_y_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01","0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=censor) +
  scale_x_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01   ","  0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=squish) +
  scale_shape_manual(values=shapes) +
  stat_smooth(aes(linetype=Compound),data=r[r$Site %in% Cluster3, ], 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  scale_linetype_manual(values = line) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.x = element_text(hjust=0.5, vjust=0,size = 16),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
        #         legend.title = element_text(hjust=0.5, vjust=0,size = 19),
        legend.title = element_blank(),
        legend.title.align = unit(.5,"in"),
        legend.background=element_blank(),
        legend.key=element_blank(),
        #         legend.position = "top",
        legend.position = c(.70,.13),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=12, hjust=0),
        legend.key.height = unit(.2, "in"),
        legend.key.width = unit(.6, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="Cluster 3 Sites",x=50,y=1),size=8,parse=F,inherit.aes=F) +
#   geom_text(aes(label="No Detection",x=0.1,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="No Detection",x=0.1,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.60",x=13,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +2.19",x=13,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.05",x=108,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = -4.81",x=108,y=.000001),size=3.75,parse=F,inherit.aes=F) +
  
  guides(shape=guide_legend(title="Cluster 3 Sites",byrow=T,ncol = 1,reverse=F), linetype=guide_legend(title="Cluster 3 Sites",byrow=T,ncol = 1,reverse=F) )  

print(ClusC)

## all sites plot
ClusAll <- NULL
ClusAll <- ggplot(data=r,
                mapping=aes(x=value, y=PPCP.Sum.ppt, group=Compound, 
                            shape=Compound, linetype=Compound))
ClusAll <- ClusAll + # labs(legend="All Sites") +
  ylab(expression(paste("Sum of Detected PPCPs (ng/L)"))) + 
  xlab(expression(paste("Individual PPCP (ng/L)"))) + geom_point(size=5) +
  scale_y_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01","0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=censor) +
  scale_x_continuous(trans=tworoot_trans(), breaks=c(.01,.1,1,4,10,40,80,140),
                     labels=c("0.01   ","  0.1","1","4","10","40","80","140"), 
                     limits=c(0,160), oob=squish) +
  scale_shape_manual(values=shapes) +
  stat_smooth(aes(linetype=Compound),data=r, 
              fullrange=T,size=0.75, colour="black", method="glm", se=F) +
  scale_linetype_manual(values = line) +
  theme(panel.background=NULL,
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title.y = element_text(hjust=0.5, vjust=0,size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust=0, size=14,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=14,color="black"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.title = element_blank(),
#         legend.title = element_text(hjust=0.5, vjust=0,size = 19),
        legend.title = element_blank(),
        legend.title.align = unit(.5,"in"),
        legend.background=element_blank(),
        legend.key=element_blank(),
                #         legend.position = "top",
        legend.position = c(.7,.13),
        legend.margin=unit(.0, "in"),
        legend.text = element_text(size=12, hjust=0),
        legend.key.height = unit(.2, "in"),
        legend.key.width = unit(.6, "in"),
        legend.text.align = 0)   +
  geom_text(aes(label="All Sites",x=25,y=1),size=8,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.001",x=0.1,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +1.27",x=0.1,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.0001",x=13,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +1.14",x=13,y=.000001),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="p-value < 0.17",x=108,y=.0005),size=3.75,parse=F,inherit.aes=F) +
#   geom_text(aes(label="slope = +4.74",x=108,y=.000001),size=3.75,parse=F,inherit.aes=F) +
  guides(shape=guide_legend(title="All Sites",byrow=T,ncol = 1,reverse=F), linetype=guide_legend(title="All Sites",byrow=T,ncol = 1,reverse=F) )  

print(ClusAll)

jpeg("karst.manuscript.fig.5.sum.by.cmpds.jpg",width=9,height=9, units= "in", res=150)

pushViewport(viewport(layout=grid.layout(nrow=2,ncol=2,widths=c(1,0.95),heights=c(0.95,1))))
print(ClusAll,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(ClusA, vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(ClusB,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(ClusC,vp=viewport(layout.pos.row=2,layout.pos.col=2))

popViewport()
dev.off()
