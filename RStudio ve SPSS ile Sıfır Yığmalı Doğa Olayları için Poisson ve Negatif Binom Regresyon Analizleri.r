###Sedir Verisinin Okutulması######
data=read.csv(file.choose(),header=T,sep=";")
attach(data)
head(data)
### arm paketi ile bağımlı değişken için histogram grafiği###
install.packages('arm')
library(arm)
discrete.histogram(cedlib_bolluk)
#################

###Zero-inflation test (Sıfırda yığılma durumu için hipotez testi)###
###Score test (Van den Broek, 1995)###

install.packages('vcdExtra')
library(vcdExtra)

zero.test(cedlib_bolluk)
#################################
##Poisson regresyon modelinin R’da uygulanışı###
formula=cedlib_bolluk~alty+usty+kirectasi
fit1=glm(formula,family = poisson)
summary(fit1)
#################################
##Poisson regresyon modelinin R’da uygulanışı (Model Uyumu)###
par(mfrow=c(2,2))
plot(fit1)
#######################################
### Over-dispersion test (Aşırı yayılım durumu için hipotez testi) ###
install.packages('AER')
library(AER)
dispersiontest(fit1,trafo=1)
######################################
##NB regresyon modelinin R’da uygulanışı###
formula=cedlib_bolluk~alty+usty+kirectasi
fit2=glm.nb(formula)
summary(fit2)
#########################################
##NB regresyon modelinin R’da uygulanışı (Model Uyumu)###
par(mfrow=c(2,2))
plot(fit2)
#########################################

##ZIP regresyon modelinin R’da uygulanışı###
install.packages('countreg')
library(countreg)
zip <- zeroinfl(cedlib_bolluk~alty+usty+kirectasi| 1, dist = "poisson") 
summary(zip)
#########################################
##ZINB regresyon modelinin R’da uygulanışı###
zinb <- zeroinfl(cedlib_bolluk~alty+usty+kirectasi| 1, dist ="negbin") 
summary(zinb)

############################################
###########Modellerin Karşılaştırılması#####
AIC(fit1,fit2,zip,zinb)
BIC(fit1,fit2,zip,zinb)
library(pscl)
vuong(fit2, zinb) 
############################################
############################################

####################################################################
###########Tahmin edilen sıfır sıklıklarının karşılaştırılması #####

round(c("Obs" = sum(cedlib_bolluk < 1),
 "ML-Pois" = sum(dpois(0, fitted(fit1))),
"NB" = sum(dnbinom(0, mu = fitted(fit2), size = fit2$theta)),
 "ZIP" = sum(predict(zip, type = "prob")[,1]),
 "ZINB" = sum(predict(zinb, type = "prob")[,1])))
############################################
############################################

####################################################################
###########Modellerinin Rootogram Grafikleri ile Karşılaştırılması #####
par(mfrow=c(2,2))
rootogram(fit1)
rootogram(fit2)
rootogram(zip)
rootogram(zinb)
############################################
############################################

###############################################
###############################################
####NB ve ZINB modellerinin kantil artıkları###
par(mfrow=c(1,2))
qqrplot(fit2)
qqrplot(zinb)
###############################################
###############################################
