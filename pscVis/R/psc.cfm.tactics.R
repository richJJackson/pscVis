## PSC model for TACTICS data

setwd("Develop")
library(survival)
library(psc)
library(flexsurv)

### Get flexsurv### Get Data
data <- read.csv("tactics data final.csv")


### defining data
data$stime <- as.numeric(as.Date(data$Date,"%d/%m/%Y")-as.Date(data$Date.of.randomization,"%d/%m/%Y"))
data$cen <- data$Death.1.Alive.0.2020.7.31Cutoff

## Defining survival object
data$s.ob <- Surv(data$stime,data$cen)


### cleaning

data$Maximum.Tumor.size.cm. <- sqrt(as.numeric(as.character(data$Maximum.Tumor.size.cm.)))
data$AFP..ng.ml. <- log(as.numeric(as.character(data$AFP..ng.ml.)))
data$ALB..g.dl. <- as.numeric(as.character(data$ALB..g.dl.))
data$T.Bil..mg.dl. <- as.numeric(as.character(data$T.Bil..mg.dl.))
data$Vascular.invasion..Yes.No.<- as.factor(data$Vascular.invasion..Yes.No.)
data$HBｓ.Ag <- as.factor(data$HBｓ.Ag)
data$HCV.Ab <- as.factor(data$HCV.Ab)
data$ECOG.PS <- as.factor(data$ECOG.PS)
data$Extarhepatic.spread <- as.factor(data$Extarhepatic.spread)
data$Creatinine..mg.dl. <- as.numeric(as.character(data$Creatinine..mg.dl.))
data$AST..IU.L. <- as.numeric(as.character(data$AST..IU.L.))



## Getting covaraiate matrix
cov.id <- which(names(data)%in%c("Maximum.Tumor.size.cm.","Number.of.tumors",
  "AFP..ng.ml.","ALB..g.dl.","T.Bil..mg.dl.","HBｓ.Ag","HCV.Ab","ECOG.PS",
  "Creatinine..mg.dl.","AST..IU.L."))
cov <- data[,cov.id]

### removing missing.id
miss.id <- which(is.na(cov)|cov==""|is.na(data$stime),arr.ind=T);miss.id
miss.id <- unique(miss.id[,1]);miss.id
data <- data[-miss.id,]
miss.id

data

### Defining covariate dataset for modelling
cov <- data[,cov.id]

cov

## Splitting dataset by treatment group
tace.id <- which(data$Group=="TACE alone")
sor.id <- which(data$Group=="TACE plus Sorafenib")

### definign outcome
tace_s.ob <- data$s.ob[tace.id]
tace_cov <- cov[tace.id,]


## tace model
null.mod <- coxph(tace_s.ob~.,data=tace_cov)
anova(null.mod)

step.mod <- step(null.mod,k=3)
summary(step.mod)


fpm <- flexsurvspline(tace_s.ob~Maximum.Tumor.size.cm.+HCV.Ab+Creatinine..mg.dl.,data=tace_cov,k=4)
fpm$AIC

plot(fpm)
attributes((fpm))

tace_cov




