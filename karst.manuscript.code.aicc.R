# Karst Project                      
# build model of PPCP contamination by step-forward optimization with confirmation by step-back
# build with type II anova informed by Akaike's 2nd order crierion

# load necessary libraries
library(car)
library(MASS)
library(AICcmodavg)
library(ggplot2)

# load table of data 
r<- read.csv("Karst.all.data.csv", sep=",",head=T)
#make variables categorical (character) or continuous(numeric/double) as appropriate
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

# make any introduced NAs in continuous variables (from BelowDetectionLimit) into zero
for (i in 7:20) {
  r[is.na(r[,i]),i] <- 0
}
for (j in 22:70) {
  r[is.na(r[,j]),j] <- 0
}
for (l in 72:93) {
  r[is.na(r[,l]),l] <- 0
}

# make a quick count of the number of times each PPCP was detected; 
# frequent detections will make it easier to discern relationships between its concentration and the other variables
# 58 samples in data frame
# for caffeine, 9 detections, 15.5%
sum(r[,7]!=0)
# carbamazepine, 12, 20.7%
# diphenhydramine, 3, 5.2%
# erythromycin, 0, 0%
# fluoxetine, 6, 10.3%
# gemfibrozil, 33, 56.9%
# ibuprofen, 1, 1.7%
# naproxen, 13, 22.4%
# sulfamethazine, 2, 3.4%
# sulfamethoxazole, 7, 12.1%
# triclocarban, 47, 81.0% 
# trimethoprim, 17, 29.3%

# gemfibrozil and triclocarban are the only 2 compounds that have greater than 50% detection, so good ones to use for modeling


# examing plots to see if any categorical variables are skewing the data (would need to be subset in models)
plt <- ggplot(data=r, mapping=aes(x=Latitude, y=Gemfibrozil))+geom_boxplot()
# levels of Latitude are different (North different than South and Middle)
plt <- ggplot(data=r, mapping=aes(x=Latitude, y=Gemfibrozil, fill=Livestock))+geom_boxplot()
# shows that GEM in North Latitude is due to large effects of livestock, but lstock not importnt in mid and south
plt <- ggplot(data=r, mapping=aes(x=Latitude, y=Gemfibrozil, fill=Habitat))+geom_boxplot()
# Habitat similar across all Latitudes
plt <- ggplot(data=r, mapping=aes(x=Latitude, y=Gemfibrozil, fill=Site))+geom_boxplot()
# Sparrow has high GEM and is in North
plt <- ggplot(data=r, mapping=aes(x=Livestock, y=Gemfibrozil, fill=Site))+geom_boxplot()
# Sparrow has much higher GEM than other North sites or other Livestock-Yes sites
plt <- ggplot(data=r, mapping=aes(x=Row.Crops, y=Gemfibrozil, fill=Site))+geom_boxplot()
# not very useful, because only Fogel3 doesn't have rowcrops
plt <- ggplot(data=r, mapping=aes(x=Grassland, y=Gemfibrozil, fill=Site))+geom_boxplot()
# also not very useful, because only 2 sites have
plt <- ggplot(data=r, mapping=aes(x=Houses, y=Gemfibrozil, fill=Site))+geom_boxplot()
# also not very useful because only fogel3 doesn't

# check if Sparrow is generally different, or just contain some large outliers that makes look different
plt <- ggplot(data=r, mapping=aes(x=row(r)[,1], y=Gemfibrozil, group=Site))+geom_point(size=12,aes(colour=Site))
plt <- ggplot(data=r[r$Site %in% c("Sparrow Spring"),],
              mapping=aes(x=row(r[r$Site %in% c("Sparrow Spring"),])[,1], y=Gemfibrozil, group=Site))+geom_point(size=12,aes(colour=Site))
plt <- ggplot(data=r[!r$Site %in% c("Sparrow Spring"),],
              mapping=aes(x=row(r[!r$Site %in% c("Sparrow Spring"),])[,1], y=Gemfibrozil, group=Site))+geom_point(size=12,aes(colour=Site))
# Sparrow has detections mostly in the 13-50ppt range, while other sites are principally less than 5,
# so yes, Sparrow is fundamentally different than others

# knowing this, will perform forward step-wise to create one model for Sparrow, and another for other sites

# glm more appropriate because some factors are binary (e.g., "livestock" is yes/no)
plot(plt)# provides many diagnostic plots
print(shapiro.test(studres(plt))) #shapiro test of normality; resiudals on plots look very good; 
# shapiro more likely to find non-normality with too many variables, so agrees with over-fitting 

############################# individual glm of each variable for gemfibrozil non-Sparrow Sites######################

#object to save to
glmsum <- NULL
# remove "Forest" because all Sites are Yes (only has one level)
r <- r[,-72]
# some sites are NotMeasured not zeroes
r[1:19,"NVOC"] <- NA
r[1:19,"TKN"] <- NA
r[1:19,"Tl.ppm"] <- NA
r[1:19,"S.ppm"] <- NA
r[1:19,"P.ppm"] <- NA
# subset to non-Sparrow sites
r <- r[!r$Site %in% c("Sparrow Spring"),]
# iterative performance of glm on each variable
for (Variable in 1:ncol(r)) {
  # perform glm
  glms <- glm(Gemfibrozil ~ r[,Variable], data=r)
  Column <- colnames(r)[Variable]
  Coefficient <- NA
  # fill in coefficient of variable if glm successful
  try (Coefficient <- summary(glms)$coefficients[2,1], silent=T )
  Pr.T.Test <- NA
  # fill in t-value if glm successful
  try (Pr.T.Test <- summary(glms)$coefficients[2,4], silent=T )
  AICcScore <- NA
  # fill in AICc score if glm succesfful
  try (AICcScore <- AICc(glms))
  # form object with coefficient, t-value, and AICc score for outcome against each variable
  glmsum <- rbind(glmsum,data.frame(Column,Coefficient,Pr.T.Test,AICcScore)) }

# reorder glmsum by lowest to highest Pr.T.Test
orderglmsum <- glmsum[with(glmsum, order(Pr.T.Test, -Coefficient)),]

# results of glmsum:
# triclocarban is highly related (T.Test 10*-15) suggesting sources of GEM and TCC are similar
# redox first significant, useful factor 

# itteratively add variables one at a time that lower AICc score or have significant Chi-sq
# (following order of variables in orderglmsum)
model1 <- NULL
for (Row in (which(orderglmsum$Column=="Oxidation.Reduction.Potential.mV")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[2], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[2], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model1 <- rbind (model1, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# after including redox, second significant factor (by either a < 0.05 ChiSq or lower AICc) is two d max t

model2 <- NULL
for (Row in (which(orderglmsum$Column=="TwoDay.MaxTemp.C")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[3], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[3], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model2 <- rbind (model2, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# next sig factor is PPCP.Sum.ppt (ChiSq < 10-6)
#    suggesting that gemfibrozil largely drives the sum detected, or is representative of PPCP contamianation
# alaklinity sig (ChSq 0.0543)(AICc 219.07)

model3 <- NULL
for (Row in (which(orderglmsum$Column=="Alkalinity.mgCaCO3perL")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[4], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[4], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model3 <- rbind (model3, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

#TwoDay.Mean.Discharge.cuft.per.s 1.447504e-02  215.3998
model4 <- NULL
for (Row in (which(orderglmsum$Column=="TwoDay.Mean.Discharge.cuft.per.s")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+TwoDay.Mean.Discharge.cuft.per.s+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[5], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[5], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model4 <- rbind (model4, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Water.pH 0.0001611677  204.0386
model5 <- NULL
for (Row in (which(orderglmsum$Column=="Water.pH")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+TwoDay.Mean.Discharge.cuft.per.s+Water.pH+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[6], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[6], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model5 <- rbind (model5, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Ca.ppm 5.479037e-02  202.7345
model6 <- NULL
for (Row in (which(orderglmsum$Column=="Ca.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+TwoDay.Mean.Discharge.cuft.per.s+Water.pH+Ca.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[7], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[7], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model6 <- rbind (model6, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Heteroplate.CFUpermL 6.847556e-02  201.8760
model7 <- NULL
for (Row in (which(orderglmsum$Column=="Heteroplate.CFUpermL")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+TwoDay.Mean.Discharge.cuft.per.s+Water.pH+Ca.ppm+Heteroplate.CFUpermL+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[8], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[8], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model7 <- rbind (model7, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# TwoDay.TotalPrecip.cm 2.092673e-09  173.3186
model8 <- NULL
for (Row in (which(orderglmsum$Column=="TwoDay.TotalPrecip.cm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+TwoDay.Mean.Discharge.cuft.per.s+Water.pH+Ca.ppm+Heteroplate.CFUpermL+TwoDay.TotalPrecip.cm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[9], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[9], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model8 <- rbind (model8, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Mg.ppm 0.001256835  165.0344
model9 <- NULL
for (Row in (which(orderglmsum$Column=="Mg.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+TwoDay.Mean.Discharge.cuft.per.s+Water.pH+Ca.ppm+Heteroplate.CFUpermL+TwoDay.TotalPrecip.cm+Mg.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[10], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[10], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model9 <- rbind (model9, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}

# Si.ppm 0.05627270  163.9856
model10 <- NULL
for (Row in (which(orderglmsum$Column=="Si.ppm")+1):nrow(orderglmsum)) {
  Variable <- orderglmsum[Row, "Column"]
  model <- glm(as.formula(paste0("Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+TwoDay.Mean.Discharge.cuft.per.s+Water.pH+Ca.ppm+Heteroplate.CFUpermL+TwoDay.TotalPrecip.cm+Mg.ppm+Si.ppm+",Variable)), data=r)
  ano <- Anova(model, type=2)
  Name <-NA
  try (Name <- attributes(ano)$row.names[11], silent=T )
  Pr.Chisq <- NA
  try (Pr.Chisq <- ano$`Pr(>Chisq)`[11], silent=T )
  AICcScore <- NA
  try (AICcScore <- AICc(model))
  model10 <- rbind (model10, data.frame(Variable, Name, Pr.Chisq, AICcScore))
}


# gemfibrozil related to sulfamethoxazole, but not to others (fluoxetine, carbamazepine) suggesting variety of sources
# no other sig factors
# now to step-wise remove factors from built model

model <- glm(Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+
               TwoDay.Mean.Discharge.cuft.per.s+Water.pH+Ca.ppm+Heteroplate.CFUpermL+TwoDay.TotalPrecip.cm+Mg.ppm+Si.ppm, data=r)
ano <- Anova(model, type=2)
AICcScore <- AICc(model)

# 2-d discharge is chi-sq 0.27
model <- glm(Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+
               Water.pH+Ca.ppm+Heteroplate.CFUpermL+TwoDay.TotalPrecip.cm+Mg.ppm+Si.ppm, data=r)
ano <- Anova(model, type=2)
AICcScore <- AICc(model)

# final model for non-sparrow sites is 
# glm(Gemfibrozil ~ Oxidation.Reduction.Potential.mV+TwoDay.MaxTemp.C+Alkalinity.mgCaCO3perL+
#   Water.pH+Ca.ppm+Heteroplate.CFUpermL+TwoDay.TotalPrecip.cm+Mg.ppm+Si.ppm, data=r)
