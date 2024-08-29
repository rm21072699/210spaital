######Correlation Matrix##################
install.packages('relaimpo')
install.packages('ggcorrplot')
library(relaimpo)
library(survey)
library(mitools)
library(ggplot2)
library(ggcorrplot)
library(stringr)

nasir=read.table("F:data.csv",header=TRUE,sep=",")

print(nasir)
names(nasir)
   
# Compute a correlation matrix
names(nasir)
corr <- round(cor(nasir), 2)
corr

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(nasir)
p.mat

# Visualize the correlation matrix
# --------------------------------
# method = "square" or "circle"
ggcorrplot(corr)
ggcorrplot(corr, method = "circle",lab=T)

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")

# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower",outline.col = "black",show.diag = T, show.legend = T,lab=T)
# Get the upeper triangle
ggcorrplot(corr, hc.order = TRUE, type = "upper",
     outline.col = "black")

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr, hc.order = TRUE, type = "lower",
   outline.col = "black",
   ggtheme = ggplot2::theme_gray,
   colors = c("#6D9EC1", "white", "#E46726"),lab = TRUE,method = "circle")

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, hc.order = TRUE, type = "lower",
   lab = TRUE)

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr, hc.order = TRUE,
    type = "lower", p.mat = p.mat)
# Leave blank on no significant coefficient
ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,


##########Prediction 
library(fields)
fat=read.table("F:/data.csv",header=TRUE,sep=",")

summary(fat)
#Hardness       
Calcium        
Magnesium      
TDS            
Chloride       
Sulfate        
Potassium      
Fluoride       

print(fat)
names(fat)
x<-fat[,2:1]
print(x)
tur<-fat[,3] # Turbidity
tds<-fat[,4] # TDS
cal<-fat[,5] # Calcium
mag<- fat[,6] # Magnesium 
Hard<-fat[,7] #Hardness
bic<- fat[,8] # Bicarbonate 
alk<- fat[,9] # Alkalinity 
chl<- fat[,10] # Chloride
pot<-fat[,11] # Potassium
sod<-fat[,12] # Sodium
sul<- fat[,13] # Sulfate
iron<-fat[,14] # Iron
Nit<-fat[,15] # Iron
flu<-fat[,16] # Flouride
#################################
fit1<-Tps(x,tur)
fit2<-Tps(x,tds)
fit3<-Tps(x,cal)
fit4<-Tps(x,mag)
fit5<-Tps(x,Hard)
fit6<-Tps(x,bic)
fit7<-Tps(x,alk)
fit8<-Tps(x,chl)
fit9<-Tps(x,pot)
fit10<-Tps(x,sod)
fit11<-Tps(x,sul)
fit12<-Tps(x,iron)
fit13<-Tps(x,Nit)
fit14<-Tps(x,flu)
##################################
par(mfrow = c(2,4),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))

surface(fit1) # tur
title("Turbidity(mg/L)")

surface(fit2) # tds
title("TDS(mg/L)")

surface(fit3) # cal
title("Calcium(mg/L)")

surface(fit4) # mag
title("Magnesium (mg/L)")

surface(fit5) # hard
title("Hardness (mg/L)")

surface(fit6) # sul
title("Bicarbonate(mg/L)")

surface(fit7) # alk
title("Alkalinity(mg/L)")

surface(fit8) # chl
title("Chloride(mg/L)")

surface(fit9) # pot
title("Potassium(mg/L)")

surface(fit10) # sod
title("Sodium(mg/L)")

surface(fit11) # sod
title("Sulfate(mg/L)")

surface(fit12) # sul
title("Iron(mg/L)")

surface(fit13) # Nit
title("Niterate(mg/L)")

surface(fit14) # flu
title("Flouride(mg/L)")





summary(fit) #diagnostic summary of the fit 

set.panel(2,2)
plot(fit2) # four diagnostic plots of  fit and residuals.


set.panel()
surface(fit) # contour/image plot of the fitted surface
#US( add=TRUE, col="magenta", lwd=2) # US map overlaid
title("Magnesium Concentration (mg/l)")


out.p<-predictSurface( fit6)
image( out.p)
fit2<- spatialProcess( x,y)
surface(fit2)
title("Chloride Concentration")



########################################
library(sp)
library(gstat)
data1=read.table("F:data.csv",header=TRUE,sep=",")
data(data1)
print(data1)
names(data1)
attach(data1)
coordinates(data1) = ~latitude+longitude

g <- gstat(id = "TDS", formula = log(TDS)~1, data = data1)
g <- gstat(g, id = "Bicarbonate", formula = log(Sulfate)~1, data = data1)
g <- gstat(g, id = "Alkalinity", formula = log(Sulfate)~1, data = data1)
g <- gstat(g, id = "Sodium", formula = log(Sodium)~1, data = data1)
g <- gstat(g, id = "Sulfate", formula = log(Sulfate)~1, data = data1)
g <- gstat(g, id = "Magnesium", formula = log(Magnesium)~1, data = data1)
g <- gstat(g, id = "Chloride", formula = log(Chloride)~1, data = data1)

# examine variograms and cross variogram:
plot(variogram(g))


################################################

library(maps)
library(geoR)
F:/Rashid Munir/Thesis/Thesis/
Rdata=read.table("F:data.csv",header=TRUE,sep=",")
attach(Rdata)
names(Rdata)

par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(TDS,main="",xlab="TDS (mg/l)")
boxplot(TDS,main="",xlab="TDS (mg/l)")


par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Calcium,main="",xlab="Calcium (mg/l)")
boxplot(Calcium,main="",xlab="Calcium (mg/l)")


par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Magnesium,main="",xlab="Magnesium (mg/l)")
boxplot(Magnesium,main="",xlab="Magnesium (mg/l)")


par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Hardness,main="",xlab="Hardness (mg/l)")
boxplot(Hardness,main="",xlab="Hardness (mg/l)")

par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Chloride,main="",xlab="Chloride (mg/l)")
boxplot(Chloride,main="",xlab="Chloride (mg/l)")


par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Potassium,main="",xlab="Potassium (mg/l)")
boxplot(Potassium,main="",xlab="Potassium (mg/l)")

par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Sodium,main="",xlab="Sodium (mg/l)")
boxplot(Sodium,main="",xlab="Sodium (mg/l)")


par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Sulfate,main="",xlab="Sulfate (mg/l)")
boxplot(Sulfate,main="",xlab="Sulfate (mg/l)")


par(mfrow = c(1,2),oma = c( 2.5, 2.5, 2.5, 2.5 ), mar=c(2.5,2.5,1.5,1),mgp=c(1.5,0,0))
hist(Fluoride,main="",xlab="Fluoride (mg/l)")
boxplot(Fluoride,main="",xlab="Fluoride (mg/l)")


TDS.geo=as.geodata(okara,coords.col = 1:2, data.col =4)
plot(TDS.geo$coord[,1] ~ TDS.geo$coord[,2], data = TDS.geo,xlab="longitude",ylab="latitude", main = "TDS Concentration")
with(TDS.geo, text(TDS.geo$coord[,2], TDS.geo$coord[,1], formatC(TDS.geo$data,dig=2), adj = 0.1))


###### Transformation using Box-Cox approach#########
library(car)
powerTransform(okara[,4]) #### Estimated value of lambda = 0.06524205

plot(TDS.geo,lambda= -0.2202624)
summary(TDS.geo,lambda= -0.2202624)
hist(TDS.geo,lambda= -0.2202624)

hist(TDSt,main="",xlab="Transformed TDS (mg/l)")


plot(TDS.geo)
