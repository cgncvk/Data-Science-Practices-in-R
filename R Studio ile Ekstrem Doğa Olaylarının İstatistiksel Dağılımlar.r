#####Gerekli paketlerin yüklenmesi#####
install.packages('xts')
install.packages('fExtremes')
install.packages('evd')
install.packages('extRemes')
install.packages('EnvStats')
install.packages('imputeTS')
install.packages('timeSeries')

library(imputeTS)
library(xts)
library(timeSeries)
library(fExtremes)
library(evd)
library(extRemes)
library(EnvStats)

#########Verinin okutulması#######
datt=read.csv(file.choose(),sep=";",header=T) ###pm10 verisini seç.
attach(datt)
head(datt)
##################################

##################################
###############Bloktaki En Büyük Gözlemlerin Elde Edilmesi######
pm10 <- xts(datt[,2], order.by=as.Date(datt[,1], "%d.%m.%Y"))
pm10new=na_kalman(pm10) ##kalman filter***
pm10new=as.timeSeries(pm10new)
head(pm10new)
y <- blockMaxima(pm10new, block = "monthly")
##################################
##################################

##################################
####GEV dağılımının parametre tahminlerinin elde edilmesi###
#### ve model uyumunun incelenmesi####
fit1=fevd(as.numeric(y))
summary(fit1)
plot(fit1)
##################################
##################################

##################################
##### K-S testi ile model uyumunun incelenmesi#####
gofTest(as.numeric(y), distribution = "gev",test = "ks")
######################################

##################################
######Getiri Periyodlarının hesaplanması####
return.level(fit1, do.ci = TRUE,return.period=c(2,20,100))
##################################

######################################################################
###########Maksimum Yağış Verisinin GEV dağılımı ile Modellenmesi#####
######################################################################
####Verinin okutulması#####
datt=read.csv(file.choose(),sep=";",header=T) ## yağış verisini seç.
attach(datt)
head(datt)
###########################
###########################

##################################
####GEV dağılımının parametre tahminlerinin elde edilmesi###
#### ve model uyumunun incelenmesi####

fit1=fevd(as.numeric(yagis))
summary(fit1)
plot(fit1)
###########################
###########################

##################################
##### K-S testi ile model uyumunun incelenmesi#####
###################################################
gofTest(as.numeric(yagis), distribution = "gev",test = "ks")
###########################
###########################

##################################
######Getiri Periyodlarının hesaplanması####
return.level(fit1, do.ci = TRUE,return.period=c(2,20,100))
###########################
###########################

##########################################################################	
######En Büyük Deprem Şiddetinin Uç Değer Dağılımlar ile Modellenmesi#####
##########################################################################

####Verinin okutulması########
datt=read.csv(file.choose(),sep=";",header=T) ###deprem verisini seç
attach(datt)
head(datt)

##############################
###Eşik Seviyesinin Belirlenmesi###
mePlot(magnitude)
threshrange.plot(magnitude, r = c(4, 5.9), nint=50)

####################################
###########GPD dağılımının parametre tahminleri#####
f1=fevd(magnitude,threshold=5,type=c("GP"))
summary(f1)

########################################################################
####GPD dağılımının model uyumu####
plot(f1)

###################################
####Getiri periyodlarının hesaplanması#####
return.level(f1, do.ci = TRUE,return.period=c(2,20,100))
###########################################




