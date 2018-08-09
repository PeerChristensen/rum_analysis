#Rum ratings predicted by price and sugar content

setwd("/Users/peerchristensen/Desktop/rom data")

rum_data <- read.csv("romdata_full.csv", header=TRUE, sep=";")
#price between 200 and 1000 DKK
premium<-subset(rum_data, price >= 200 & price <= 1000)
rum_data<-subset(premium, howler >=1 )
#DV
Rating1 <- rum_data$romhat
Rating2 <- rum_data$howler
#IVs
Price <- rum_data$price
Sugar <- rum_data$sugar

par(mfrow=c(1,2))
#Plots Price
plot(Price, Rating1, ylim=c(50,100))
abline(lm(Rating1~Price), col = "red", lwd=2)
plot(Price, Rating2, ylim=c(50,100))
abline(lm(Rating2~Price), col = "red", lwd=2)

#Plots Sugar
plot(Sugar, Rating1, ylim=c(50,100))
abline(lm(Rating1~Sugar), col = "red", lwd=2)
plot(Sugar, Rating2, ylim=c(50,100))
abline(lm(Rating2~Sugar), col = "red", lwd=2)

#analyses
#lm
summary(lm(Rating~Price))
summary(lm(Rating1~Sugar))
summary(lm(Rating1~Price+Sugar))
#glm
summary(glm(Rating ~ Price + Sugar))
#arcsine transformed
Rating2 <- asin(sqrt(Rating/100)) 
summary(lm(Rating2~Sugar))

#price kept constant,check column n
rum1<-subset(premium, premium[,7]>=200 & premium[,7]<300)
rum2<-subset(premium, premium[,7]>=300 & premium[,7]<400)
rum3<-subset(premium, premium[,7]>=400 & premium[,7]<500)

#Romhatten
par(mfrow=c(1,3))
plot(rum1$Sugar.g.L, rum1$Romhatten, main="200-250 DKK", 
     xlab="Added sugar (g/L)", ylab="Rating",  pch=16, xlim=c(0,70), ylim=c(50,100))
abline(lm(rum1$Romhatten~rum1$Sugar.g.L), lwd=2, col = "red")
plot(rum2$Sugar.g.L, rum2$Romhatten, main="300-350 DKK", 
     xlab="Added sugar (g/L)", ylab="Rating",  pch=16, xlim=c(0,70), ylim=c(50,100))
abline(lm(rum2$Romhatten~rum2$Sugar.g.L), lwd=2, col = "red")
plot(rum3$Sugar.g.L, rum3$Romhatten, main="400-450 DKK", 
     xlab="Added sugar (g/L)", ylab="Rating",  pch=16, xlim=c(0,70), ylim=c(50,100))
abline(lm(rum3$Romhatten~rum3$Sugar.g.L), lwd=2, col = "red")

#howler
par(mfrow=c(1,3))
plot(rum1$Sugar.g.L, rum1$Howler, main="200-250 DKK", 
     xlab="Added sugar (g/L)", ylab="Rating",  pch=16, xlim=c(0,70), ylim=c(50,100))
abline(lm(rum1$Howler~rum1$Sugar.g.L), lwd=2, col = "red")
plot(rum2$Sugar.g.L, rum2$Howler, main="300-350 DKK", 
     xlab="Added sugar (g/L)", ylab="Rating",  pch=16, xlim=c(0,70), ylim=c(50,100))
abline(lm(rum2$Howler~rum2$Sugar.g.L), lwd=2, col = "red")
plot(rum3$Sugar.g.L, rum3$Howler, main="400-450 DKK", 
     xlab="Added sugar (g/L)", ylab="Rating",  pch=16, xlim=c(0,70), ylim=c(50,100))
abline(lm(rum3$Howler~rum3$Sugar.g.L), lwd=2, col = "red")


#analyses
with(rum1, summary(lm(Romhatten.score~Sugar.g.L)))
with(rum2, summary(lm(Romhatten.score~Sugar.g.L)))  
with(rum3, summary(lm(Romhatten.score~Sugar.g.L)))

#Using ggplot2
rum_cat<-rum_data[ order(Price), ]
rum_cat["Category"] <- NA
rum_cat["Category"][1:4,]=1
rum_cat["Category"][5:14,]=2
rum_cat["Category"][15:21,]=3
rum_cat<-rum_cat[1:20,]
rum_cat<- transform(rum_cat, Category = factor(Category))
#romhatten
qplot(Sugar.g.L, Romhatten,  data = rum_cat, 
      facets = .~Category) 
p<-qplot(Sugar.g.L, Romhatten,  data = rum_cat)
q<-p+ geom_point(aes(color = Category), size=4)
r<-q+ geom_smooth(method="lm", color = "black")
r
#howler
qplot(Sugar.g.L, Howler,  data = rum_cat, 
      facets = .~Category) 
p<-qplot(Sugar.g.L, Howler,  data = rum_cat)
q<-p+ geom_point(aes(color = Category), size=4)
r<-q+ geom_smooth(method="lm", color = "black")
r
#full data set
library(ggplot2)
fp<-qplot(Sugar, Rating2, data=rum_data)
fq<-fp+ geom_smooth(method="lm", color = "black")
fq