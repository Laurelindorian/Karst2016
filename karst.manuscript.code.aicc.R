# Karst Project                      
# build model of PPCP contamination by step-forward optimization with confirmation by step-back
# build with type II anova informed by Akaike's 2nd order crierion

# load necessary libraries
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
library(metap)


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

### hierarchical clustering 
plot(hclust(dist(coeftable)))
plot(hcut(coeftable,3))

### k-means clustering
kmeans(as.matrix(coeftable),3)$cluster
# itterative evaluation of optimization between to total sum of squares
set.seed(1)
kmeans(as.matrix(coeftable),3)
set.seed(2)
kmeans(as.matrix(coeftable),3)
#### etc. optimized at set.seed(4)
# biplot of PCA of factors with sites in black
biplot(prcomp(coeftable))



# glm is more appropriate because some factors are binary (e.g., "livestock" is yes/no) and data may be non-normal

########### glm of new group 1 sites #######################################

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

# Subset to only group 1 sites by removing others
# 3 cluster model seems most cohesive comparing biplot, dendrogram, and sum of squares
# Cluster 1 - Falling, Camp Vandeventer, Fogelpole 1, Fogelpole 2, Frog, Illinois, Kelly
# Cluster 2 - Auctioneer, Sparrow
# CLuster 3 - Stemler, Fogelpole Cave 3
drops <- c("Falling Springs","Camp Vandeventer Spring","Illinois Caverns",
           "Fogelpole Cave 1", "Fogelpole Cave 2","Frog Spring","Kelly Spring")
r <- r[r$Site %in% drops,]

# remove not relevant factors
drops <- c("Year","Month","Day","Date")
r <- r[,!(names(r) %in% drops)]

#object to save to
glmsum <- NULL

# iterative performance of glm on each variable
for (Variable in names(r)) {
  if (length( levels(factor(r[,Variable]))) <2 ) {next;}
  else{
    glms <- glm(as.formula(paste0("PPCP.Sum.ppt ~ ",Variable)), data=r)
    Factor <- Variable
    Coefficient <- NA
    try (Coefficient <- summary(glms)$coefficients[2,1], silent=T )
    Pr.T.Test <- NA
    try (Pr.T.Test <- summary(glms)$coefficients[2,4], silent=T )
    AICcScore <- NA
    try (AICcScore <- AICc(glms))
    glmsum <- rbind(glmsum,data.frame(Factor,Coefficient,Pr.T.Test,AICcScore)) }  }

# reorder glmsum by lowest to highest Pr.T.Test
orderglmsum <- glmsum[with(glmsum, order(Pr.T.Test, -Coefficient)),]

# first sig is  TwoDay.Mean.Gage.Height.ft -5.576291e+00 3.681312e-05  253.8129
# itteratively add variables one at a time, starting with lowest Pr.T.Test, but being sensible about choices
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="TwoDay.Mean.Gage.Height.ft")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ TwoDay.Mean.Gage.Height.ft+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[2], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[2], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# 2nd factor Mn.ppm 1.118859e-04  242.8228
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Mn.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ TwoDay.Mean.Gage.Height.ft+Mn.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[3], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[3], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# B.ppm 5.059812e-04  233.9218
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="B.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ TwoDay.Mean.Gage.Height.ft+Mn.ppm+B.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[4], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[4], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Specific.Conductivity.uSpercm 3.645808e-03  228.0911
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Specific.Conductivity.uSpercm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ TwoDay.Mean.Gage.Height.ft+Mn.ppm+B.ppm+Specific.Conductivity.uSpercm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[5], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[5], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Heteroplate.CFUpermL 2.019695e-02 225.1945
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Specific.Conductivity.uSpercm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ TwoDay.Mean.Gage.Height.ft+Mn.ppm+B.ppm+
                                 Specific.Conductivity.uSpercm+Heteroplate.CFUpermL+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[6], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[6], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# summary(glm()) shows spec.con has p-value of 0.34, in the model with AIC of 225.19
summary(glm(PPCP.Sum.ppt ~ TwoDay.Mean.Gage.Height.ft+Mn.ppm+B.ppm+Heteroplate.CFUpermL, data=r)) # AICc dropped to 223.2661

# model for Cluster 1 sites PPCP Sum~TwoDay.Mean.Gage.Height.ft+Mn.ppm+B.ppm+Heteroplate.CFUpermL

# calculate R2 and model p-value

Clustermodel <- glm(PPCP.Sum.ppt ~ TwoDay.Mean.Gage.Height.ft+Mn.ppm+B.ppm+Heteroplate.CFUpermL, data=r)
Nullmodel <- glm(PPCP.Sum.ppt ~ 1, data=r)
anova(Clustermodel, Nullmodel, test="F")


#################### new Cluster 2 sum model #######################

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

# Subset to only group 2 sites by removing others
# # Cluster 2 - Auctioneer, Sparrow
drops <- c("Auctioneer Spring","Sparrow Spring")
r <- r[r$Site %in% drops,]

# remove not relevant factors
drops <- c("Year","Month","Day","Date")
r <- r[,!(names(r) %in% drops)]

#object to save to
glmsum <- NULL

# iterative performance of glm on each variable
for (Variable in names(r)) {
  if (length( levels(factor(r[,Variable]))) <2 ) {next;}
  else{
    glms <- glm(as.formula(paste0("PPCP.Sum.ppt ~ ",Variable)), data=r)
    Factor <- Variable
    Coefficient <- NA
    try (Coefficient <- summary(glms)$coefficients[2,1], silent=T )
    Pr.T.Test <- NA
    try (Pr.T.Test <- summary(glms)$coefficients[2,4], silent=T )
    AICcScore <- NA
    try (AICcScore <- AICc(glms))
    glmsum <- rbind(glmsum,data.frame(Factor,Coefficient,Pr.T.Test,AICcScore)) }  }

# reorder glmsum by lowest to highest Pr.T.Test
orderglmsum <- glmsum[with(glmsum, order(Pr.T.Test, -Coefficient)),]

# first sig is S.ppm  3.585557e+00 4.377494e-03  68.84309, but it is partly NA
# next sig is Na.ppm    5.185387 7.081534e-03 103.05842
# itteratively add variables one at a time, starting with lowest Pr.T.Test, but being sensible about choices
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Na.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Na.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[2], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[2], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# second sig Specific.Conductivity.uSpercm 4.842138e-02  104.6342
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Specific.Conductivity.uSpercm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Na.ppm+Specific.Conductivity.uSpercm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[3], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[3], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# third Alkalinity.mgCaCO3perL 7.308817e-05 100.76525
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Alkalinity.mgCaCO3perL")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Na.ppm+Specific.Conductivity.uSpercm+Alkalinity.mgCaCO3perL+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[4], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[4], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# fourth Water.Temp.C 3.298394e-02  109.2978
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Water.Temp.C")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Na.ppm+Specific.Conductivity.uSpercm+Alkalinity.mgCaCO3perL+Water.Temp.C+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[5], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[5], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Ca.ppm 2.308554e-04  124.5035
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Ca.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Na.ppm+Specific.Conductivity.uSpercm+Alkalinity.mgCaCO3perL+Water.Temp.C+Ca.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[6], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[6], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}


summary(glm(PPCP.Sum.ppt ~ Na.ppm+Specific.Conductivity.uSpercm+Alkalinity.mgCaCO3perL+Water.Temp.C+Ca.ppm, data=r))
# AICc 124.50
summary(glm(PPCP.Sum.ppt ~ Na.ppm+Specific.Conductivity.uSpercm+Water.Temp.C+Ca.ppm, data=r))
# AICc 103.95
# model for Cluster 2 sites PPCP Sum~Na.ppm+Specific.Conductivity.uSpercm+Water.Temp.C+Ca.ppm
Clustermodel <- glm(PPCP.Sum.ppt ~ Na.ppm+Specific.Conductivity.uSpercm+Water.Temp.C+Ca.ppm, data=r)
Nullmodel <- glm(PPCP.Sum.ppt ~ 1, data=r)
anova(Clustermodel, Nullmodel, test="F")



################### new Cluster 3 sum model #######################

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

# Subset to only group 3 sites by removing others
# CLuster 3 - Stemler, Fogelpole Cave 3
drops <- c("Fogelpole Cave 3", "Stemler Cave")
r <- r[r$Site %in% drops,]

# remove not relevant factors
drops <- c("Year","Month","Day","Date")
r <- r[,!(names(r) %in% drops)]

#object to save to
glmsum <- NULL

# iterative performance of glm on each variable
for (Variable in names(r)) {
  if (length( levels(factor(r[,Variable]))) <2 ) {next;}
  else{
    glms <- glm(as.formula(paste0("PPCP.Sum.ppt ~ ",Variable)), data=r)
    Factor <- Variable
    Coefficient <- NA
    try (Coefficient <- summary(glms)$coefficients[2,1], silent=T )
    Pr.T.Test <- NA
    try (Pr.T.Test <- summary(glms)$coefficients[2,4], silent=T )
    AICcScore <- NA
    try (AICcScore <- AICc(glms))
    glmsum <- rbind(glmsum,data.frame(Factor,Coefficient,Pr.T.Test,AICcScore)) }  }

# reorder glmsum by lowest to highest Pr.T.Test
orderglmsum <- glmsum[with(glmsum, order(Pr.T.Test, -Coefficient)),]

# first sig is Dissolved.Oxygen.ppm -6.147750e+00 0.01651694  73.95780
# itteratively add variables one at a time, starting with lowest Pr.T.Test, but being sensible about choices
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Dissolved.Oxygen.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[2], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[2], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# 2nd TwoDay.TotalPrecip.cm 1.186941e-02  76.74681
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="TwoDay.TotalPrecip.cm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+TwoDay.TotalPrecip.cm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[3], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[3], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# 3rd K.ppm 6.304246e-08  78.46717
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="K.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+TwoDay.TotalPrecip.cm+K.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[4], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[4], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}


# 4th Ba.ppm 2.064238e-03 123.05567
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Ba.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+TwoDay.TotalPrecip.cm+K.ppm+Ba.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[5], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[5], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# 5th Ammonia.ppm  5.837287e-02       Inf
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Ammonia.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+TwoDay.TotalPrecip.cm+K.ppm+Ba.ppm+Ammonia.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[6], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[6], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Cu.ppm  1.109330e-03 -128.7895
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Cu.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+TwoDay.TotalPrecip.cm+K.ppm+Ba.ppm+Ammonia.ppm+Cu.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[7], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[7], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# no more sig
summary(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+TwoDay.TotalPrecip.cm+K.ppm+Ba.ppm+Ammonia.ppm+Cu.ppm, data=r))
AICc(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+TwoDay.TotalPrecip.cm+K.ppm+Ba.ppm+Ammonia.ppm+Cu.ppm, data=r))# AICc -128 
# two day least sig
summary(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+K.ppm+Ba.ppm+Ammonia.ppm+Cu.ppm, data=r))
AICc(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+K.ppm+Ba.ppm+Ammonia.ppm+Cu.ppm, data=r))# AICc Inf
# Cu least sig
summary(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+K.ppm+Ba.ppm+Ammonia.ppm, data=r))
AICc(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+K.ppm+Ba.ppm+Ammonia.ppm, data=r))# AICc 116
# K least sig
summary(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+Ba.ppm+Ammonia.ppm, data=r))
AICc(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+Ba.ppm+Ammonia.ppm, data=r))# AICc 68
# ammonia least sig
summary(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+Ba.ppm, data=r))
AICc(glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+Ba.ppm, data=r))# AICc 52
# this is finalized, but wonder if went down worng path by ignoring AIC in first part; redo of first part

# first sig is Dissolved.Oxygen.ppm -6.147750e+00 0.01651694  73.95780
# itteratively add variables one at a time, starting with lowest Pr.T.Test, but being sensible about choices
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Dissolved.Oxygen.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[2], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[2], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# 2nd factor that causes lower AICc Ba.ppm 1.648372e-56 51.8081737
model1 <- NULL
for (Row in (which(orderglmsum$Factor=="Ba.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Factor"]
  model <- glm(as.formula(paste0("PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+Ba.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[3], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[3], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# no more factors that cause lower AICc score
# so this method got to the same final model in fewer steps :P

# model for Cluster 3 site PPCP Sum~Dissolved.Oxygen.ppm+Ba.ppm,
Clustermodel <- glm(PPCP.Sum.ppt ~ Dissolved.Oxygen.ppm+Ba.ppm,, data=r)
Nullmodel <- glm(PPCP.Sum.ppt ~ 1, data=r)
anova(Clustermodel, Nullmodel, test="F")

########### glm of all sites #######################################

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

# remove not relevant factors
drops <- c("Year","Month","Day","Date")
r <- r[,!(names(r) %in% drops)]

#object to save to
glmsum <- NULL

# iterative performance of glm on each variable
for (Variable in names(r)) {
  if (length( levels(factor(r[,Variable]))) <2 ) {next;}
  else{
    glms <- glm(as.formula(paste0("PPCP.Sum.ppt ~ ",Variable)), data=r)
    Factor <- Variable
    Coefficient <- NA
    try (Coefficient <- summary(glms)$coefficients[2,1], silent=T )
    Pr.T.Test <- NA
    try (Pr.T.Test <- summary(glms)$coefficients[2,4], silent=T )
    AICcScore <- NA
    try (AICcScore <- AICc(glms))
    glmsum <- rbind(glmsum,data.frame(Factor,Coefficient,Pr.T.Test,AICcScore)) }  }

# reorder glmsum by lowest to highest Pr.T.Test
orderglmsum <- glmsum[with(glmsum, order(Pr.T.Test, -Coefficient)),]