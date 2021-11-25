#####TD 1 DU BIG DATA ##############

# libraries
library(readxl)
library(tidyverse)
library(lgarch)		# Gets modelling
library(gets)
library(glmnet) 	# penalized regressions
library(rbridge)	# bridge regressions
library(foreach)
library(doParallel)
library(tseries)
library(tsoutliers)
library(e1071)
library(FactoMineR)
library(cluster)
library(NbClust) 

##################### IMPORTATION ET PREMIERS TRAITEMENTS ##################################
setwd("D:/Dossiers/Etudes/M2 EKAP/Big Data/Projet")
base <- read.csv("US.csv", sep=",")

str(base)
dim(base)
View(base)

base$sasdate  <- as.Date(base$sasdate,format = "%m/%d/%Y")
View(base)

#Graphiques pour choisir la var à expliquer
plot(base$INDPRO, type="l", main="Production industrielle") #Production industrielle
plot(base$UNRATE, type="l", main="Taux de chomage") #Taux de chomage
plot(base$CE16OV, type="l", main="Taux d'emploi") #Taux de d'emploi
plot(base$CPIAUCSL, type="l", main = "Inflation") #Inflation
plot(base$HOUST, type="l", main="Construction") #Construction
#On sélectionne la production industrielle

##Enlever les données d'avant 1970 en raison des NA
base2 <- base [-(1:133),]
#Dernière ligne contient des na
base2 <- base2[-622,]
#On retire les variables qui ont beaucoup de NA
summary(base2)
base2 <- base2[,!(names(base2) %in% c("UMCSENTx","TWEXAFEGSMTHx","ACOGNO"))]
#On retire les quelques na restants
base2 <- na.omit(base2)
##################################################################################################



############################# OUTLIERS #######################################################
#Analyse des outliers ###
boxplot(base2$INDPRO)
INDPRO<- ts(data=base2$INDPRO, start=c(1970,01), frequency =12)
plot(INDPRO)

tso(INDPRO)
fit <- tso(INDPRO)
plot(fit)
show(fit)

#corriger les outliers
adj<-fit$yadj 

##graphiques des séries en niveau de toutes les variables 

plot(adj) #d'après ce graphique on constate que la variable à expliquer 
#a une tendance à la hausse durant la période de l'étude et elle semble pas stationnaire 
#pour s'assurer on va effectuer le test ADF 

#Etude de la stationnarité de la variable à expliquer 
library(tseries)
adf.test(base3$INDPRO) #d'après ce test la p value est strictement supérieure à 5% 
#H0 est acceptée la série n'est pas stationnaire 

#Etude de la stationnarité de l'ensemble des variables explicatives 

base2$INDPRO <- adj
####################################################################################################

######################### STATIONNARISER ######################################################
#Boucle pour stationariser
n <- ncol(base2)
base3 = matrix(0,616,125)
base3[,1] <- base2[-1,1]
count = 0
nom_col = names(base2)
for (j in 2:n){
  result_test <- adf.test(base2[,j])
  if (result_test$p.value>0.05){
    base3[,j] <- diff(base2[,j], differences = 1)
    count = count + 1
    cat("Variable stationnarisée : ",j, "\n")
  }else{
      base3[,j] <- base2[-1,j]
    }
}
cat("Nombre de variables stationnarisées : ", count, "/", n)

base3 <- as.data.frame(base3)
colnames(base3) <- nom_col

#Export
write.csv(base3, "D:/Dossiers/Etudes/M2 EKAP/Big Data/Projet/base_us.csv")
#######################################################################################################"


####################### STAT DESC ##################################################"
summary(base3$INDPRO)
sd(base3$INDPRO)
#0.11 en moyenne avec un écart type de 0.46
hist(base3$INDPRO, col = "blue", freq=F, xlab="INDPRO", ylab="Densité", main="Production industrielle aux USA")
curve(dnorm(x, mean=mean(base3$INDPRO), sd=sd(base3$INDPRO)),col="red", lwd=2, add=TRUE, yaxt="n")
#Distribution proche d'une loi normale, voir les tests 
boxplot(base3$INDPRO, col = "blue", main="Production industrielle aux USA")
#Quelques points extrêmes à la fin de la distribution
#Stat de normalité :
#skweness - coef d'asymétrie
skewness(base3$INDPRO) 
# -0.7090, légèrement négatif donc distribution trop décalée à droite de la médiane
#kurtosis - coef d'applatissement
kurtosis(base3$INDPRO) 
# 2.3951 - La distribution est trop pointue
################################################################################################"




##################### CLASSIFICATION ##################################################"
acp <- PCA(base3[,c(2:125)])
#On oberseve des paquets de variables
barplot(acp$eig[,2], xlab="Dim", ylab ="Percentage of variance", col = 9)
base3cr<-scale(base3[,c(2:125)],center=T,scale=T) #Pour centrer réduire
pol.dist <- dist(base3cr,method="euc") #Pour calculer la distance
class0 <- hclust(pol.dist, method="ward.D2") #Pour faire les classes
plot(as.dendrogram(class0),main="Dendrogramme") #Pour obtenir le dendogramme avec la méthode Ward.
plot(class0, hang=-1, cex=0.8, main="Dendrogramme") #Même graphique, hang=-1 pour mettre les noms sur la même ligne. Cex pour la taille du text.

#Classification difficilement interprétable

#K means
res.kmeans <- kmeans(base3cr, 30, nstart =50, algorithm = "MacQueen") #On utilise la méthode des kmeans
res.kmeans$cluster #On visulalise les classes
res.kmeans$centers #Les centres de gravité
res.kmeans$betweenss #Between sum of square. Proportionel à l'inertie inter. C'est ce qu'on veut maximiser. Somme des carrés expliqués
res.kmeans$totss #Total sum of square. 
qual <-res.kmeans$betweenss/res.kmeans$totss
round(qual*100,2) #Qualité = 53 %
#######################################################################################################"



###################### CORRELATION ######################################################
corrplot(base3[,2:125], method="color")
#######################################################################################

#--------------------------------------------------------------------------------------------------------


dlbase <- read_excel("C:/Users/E20E816N/Downloads/dlbase.xlsx", sheet = "dlbase")
dlbase <- data.frame(dlbase) # sinon erreur pour la suite
summary(dlbase)
str(dlbase)
training_dlbase <- dlbase
data.frame(training_dlbase)

# Correlations
library(corrplot)
par(mfrow=c(1,1))
cor1 <- cor(dlbase[1:11],use="complete.obs",method=c("spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1, method="color", col=col(200), 
         type="upper",
         addCoef.col = "black")

par(mfrow=c(1,1))
cor2 <- cor(dlbase[,c(1,12:22)],use="complete.obs",method=c("spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor2, method="color", col=col(200), 
         type="upper",
         addCoef.col = "black")

cor3 <- cor(dlbase[1:11],use="complete.obs",method=c("spearman"))
corrplot(cor3)
cor(dlbase[1:10])

cor4 <- cor(dlbase[,c(1,12:22)],use="complete.obs",method=c("spearman"))
corrplot(cor4)


#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Penalized regressions

# standardized y and x (centered and standardized)
library(dplyr)
y <- data.frame(training_dlbase) %>%
  select(WTI) %>%
  scale(center = T, scale = T) %>%
  as.matrix()

x <- data.frame(training_dlbase) %>%
  select(-WTI) %>% # on retire la variable à expliquer y
  scale(center = T, scale = T) %>%
  as.matrix()

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Ridge regression
model_cv <- glmnet(x, y, alpha = 0, standardize = T) #quand on a mis alpha =0 donc c du ridge 
plot(model_cv)
#to ce qui est en bas jusqu'a plot (ridge cv ) c la cross validation en ridge 
# 10-folds CV to choose lambda : on fixe a 10 par défaut 
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5 : essayer une 100 ene de fois 
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)#on le stoque dans lamda to try 

# alpha = 0, implementation of ridge regression
# choix du meilleur lambda parmi 100
ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10)#cross validation avec 10 blocs  

# Figures of lambdas
plot(ridge_cv) 

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv <- ridge_cv$lambda.min #trouver le lamda minimum 
lambda_cv #comment pénaliser les données 0.265
# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
summary(model_cv)

# Ridge betas
model_cv$beta #???afficher tous les coefficients du ridge 

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# LASSO regression
model_cv <- glmnet(x, y, alpha = 1, standardize = T)
plot(model_cv)

# 10-folds CV to choose lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# alpha = 1, implementation of Lasso regression
lasso_cv <- cv.glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100

# Figures of lambdas
plot(lasso_cv)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- lasso_cv$lambda.1se

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv$beta

# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Elastic-Net regression with alpha = 0.5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
en_cv <- cv.glmnet(x, y, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
# Figures of lambdas
plot(en_cv)
lambda_cv <- en_cv$lambda.1se
model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cv, standardize = T)
# EN betas
model_cv$beta
# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Choose alpha sequencially with 0 < alpha < 1: a = {0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9}
en_min <- NULL
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
alphalist <- seq(0.1,by=0.1)
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(x, y, alpha=a, lambda = lambdas_to_try, standardize = T, nfolds = 10)
})
for (i in 1:9) {
  print(min(elasticnet[[i]]$cvm))
  en_min <- c(en_min, min(elasticnet[[i]]$cvm))
}
elasticnet_cvm <- min(en_min)
elasticnet_cvm #CA DONNE LA VALEUR QUI EST MINIMIS2E AIS C PAS ALPHA ON DOIT LA PRENDRE ET LA METTRE EN BAS 

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
en_cv <- cv.glmnet(x, y, alpha = 0.7, lambda = lambdas_to_try, standardize = T, nfolds = 10) #.LE ALPHA QUE J4AI MIS ICI C LE LA VALEUR MINIMISEE TROUVEE EN HAUT 
# Figures of lambdas
plot(en_cv)
lambda_cv <- en_cv$lambda.1se
model_cv <- glmnet(x, y, alpha = 0.5, lambda = lambda_cv, standardize = T)
# EN betas
model_cv$beta
# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)  

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Adaptive Lasso regression using Lasso to compute the weights in the first step
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)
coef_lasso <- predict(model_cv,type="coef",s=lambda_cv)
# Weighted with gamma = 0.5
gamma = 0.5
w0 <- 1/(abs(coef_lasso) + (1/length(y)))
poids.lasso <- w0^(gamma)

# Adaptive LASSO
fit_adalasso <- glmnet(x, y, penalty.factor =poids.lasso)
fit_cv_adalasso <- cv.glmnet(x, y,penalty.factor=poids.lasso)

# Figure of lambdas 
plot(fit_cv_adalasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- fit_cv_adalasso$lambda.1se

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv$beta

# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Adaptive Lasso regression using Ridge to compute the weights in the first step
model_cv <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = T)
coef_ridge <- predict(model_cv,type="coef",s=lambda_cv)
# Weighted with gamma = 0.5
gamma = 0.5
w0 <- 1/(abs(coef_ridge) + (1/length(y)))
poids.ridge <- w0^(gamma)

# Adaptive LASSO
fit_adalasso <- glmnet(x, y, penalty.factor =poids.ridge)
fit_cv_adalasso <- cv.glmnet(x, y,penalty.factor=poids.ridge)

# Figure of lambdas 
plot(fit_cv_adalasso)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_cv <- fit_cv_adalasso$lambda.1se

# Evaluation of the final model with the selected lambda
model_cv <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = T)

# Lasso betas
model_cv$beta

# Get the name of relevant variables
which(! coef(model_cv) == 0, arr.ind = TRUE)

#--------------------------------------------------------------------
#---------------------------------------------------------------------
# Bridge regression
# 10-folds CV to choose lambda
# seq(-3, 5, length.out = 100) : random sequence of 100 numbers betwwene -3 and 5
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

# choix du meilleur lambda parmi 100
bridge_cv <- cv.bridge(x, y, q=0.5, lambda = lambdas_to_try, nfolds = 10) 

# Figures of lambdas
plot(bridge_cv)

# Best lambda obtained from CV (lambda.min) - other possibility: lambda.1se
lambda_cv <- bridge_cv$lambda.min

# Evaluation of the final model with the selected lambda
model_cv <- bridge(x, y, q=0.5, lambda = lambda_cv)
summary(model_cv)

# Ridge betas
model_cv$beta

#----------------------------------------------------------------------
#---------------------------------------------------------------------
# Weighted fusion Lasso regressions

# Initialization of parameters
gamma=0.5
mu=0.1

# Compute pxQ matrix.
cor.mat <- cor(x)
abs.cor.mat <- abs(cor.mat)
sign.mat <- sign(cor.mat) - diag(2, nrow(cor.mat))
Wmat <- (abs.cor.mat^gamma - 1 * (abs.cor.mat == 1))/(1 -abs.cor.mat * (abs.cor.mat != 1))
weight.vec <- apply(Wmat, 1, sum)
fusion.penalty <- -sign.mat * (Wmat + diag(weight.vec))

# Compute Cholesky decomposition
R<-chol(fusion.penalty, pivot = TRUE)

# Transform Weighted Fusion in a Lasso issue sur les donn´ees augment´ees (1.40).
p<-dim(x)[2]
xstar<-rbind(x,sqrt(mu)*R)
ystar<-c(y,rep(0,p))

# Apply Lasso .
fit_wfusion<-glmnet(xstar,ystar)
fit_cv_wfusion<-cv.glmnet(xstar,ystar)

par(mfrow=c(1,2))
plot(fit_wfusion,xvar ="lambda",label = FALSE,col=T,xlab=expression(log(lambda)))
plot(fit_cv_wfusion,xlab=expression(log(lambda)))

min(fit_cv_wfusion$cvm)
lambda.opt<-fit_cv_wfusion$lambda.min

# We obtain the estimator of beta_weighted-fusion
fit_coef_wfusion<-predict(fit_wfusion,type="coef",s=lambda.opt)
show(fit_coef_wfusion)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# SCAD
library(ncvreg)
fit_SCAD=ncvreg(x, y, penalty = c("SCAD"))
plot(fit_SCAD)
summary(fit_SCAD, lambda=0.10)

# Validation croisé pour le meilleur lambda 
cvfit_SCAD=cv.ncvreg(x, y, penalty = c("SCAD"))
plot(cvfit_SCAD)

# On attribue le meilleur lambda 
lambda_SCAD <- cvfit_SCAD$lambda.min

#Modele finale 
SCAD_Final=ncvreg(x, y, lambda=lambda_SCAD, alpha = 1)
SCAD_Final$beta

which(! coef(SCAD_Final) == 0, arr.ind = TRUE)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# MCP
library(ncvreg)
fit_MCP=ncvreg(x, y, penalty = c("MCP"))
plot(fit_MCP)
summary(fit_MCP, lambda=0.10)

# Validation croisé pour le meilleur lambda 
cvfit_MCP=cv.ncvreg(x, y, penalty = c("MCP"))
plot(cvfit_MCP)

# On attribue le meilleur lambda 
lambda_MCP <- cvfit_MCP$lambda.min

#Modele finale 
MCP_Final=ncvreg(x, y, lambda=lambda_MCP, alpha = 1)
MCP_Final$beta

which(! coef(MCP_Final) == 0, arr.ind = TRUE)


#---------------------------------------------------------------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# GETS

# convert tibble in matrix for the function arx
class(dlbase[,2:22]) # tibble
mX = data.matrix(training_dlbase[,2:22]) # ça marche !

# ARX model with AR(1)
Mod02WTI <- arx(training_dlbase$WTI, mc = T, ar = 1, mxreg = mX[, 1:21], vcov.type = "white") # car seulement les 2 premieres variables sont signif
Mod02WTI 

# GETS modelling
getsm_WTI2 <- getsm(Mod02WTI) 
getsm_WTI2

# GETS betas
coef.arx(getsm_WTI2)

# Get the name of relevant variables
names_mX <- names(coef.arx(getsm_WTI2))
names_mX <- names_mX[-1] # on retire le ar1
names_mX


# GETS modelling without ARCH test
getsm_WTI2b <- getsm(Mod02WTI, arch.LjungB=NULL) 
getsm_WTI2b


# GETS without AR(1)
Mod02WTI <- arx(training_dlbase$WTI, mc = T, ar = NULL, mxreg = mX[, 1:21], vcov.type = "white") # car seulement les 2 premieres variables sont signif
Mod02WTI
getsm_WTI2 <- getsm(Mod02WTI) 
getsm_WTI2


# isat function
yy <- dlbase[,1]
isat(yy, sis=TRUE, iis=FALSE, plot=TRUE, t.pval=0.005)
isat(yy, sis=FALSE, iis=TRUE, plot=TRUE, t.pval=0.005)
isat(yy, sis=FALSE, iis=FALSE, tis=TRUE, plot=TRUE, t.pval=0.005)
isat(yy, sis=TRUE, iis=TRUE, tis=TRUE, plot=TRUE, t.pval=0.005)











