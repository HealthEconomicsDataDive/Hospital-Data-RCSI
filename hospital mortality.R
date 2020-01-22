###
# 
###

#libraries
library(survival)
library(Epi)        #contains the ccwc function
library(haven)
library(Zelig)
library(boot)


#Data
EAS_data <- read_dta("MEGA/Health Economics/Hospital-Data-RCSI-master/EAS_data_for_Rworkshop.dta")
head(EAS_data)

EAS_dat <- EAS_data
EAS_dat$sex <- as.character(EAS_dat$sex)
EAS_dat$cancerdiag <- as.character(EAS_dat$cancerdiag)
EAS_dat$hospvol <- as.character(EAS_dat$hospvol)
EAS_dat$teamvol <- as.character(EAS_dat$teamvol)
#set.seed(20140111) #seed

# analysis 
 
#logistic regression
resClogit <- glm(death ~ as.factor(cancerdiag) + as.factor(sex) + as.factor(teamvol) * as.factor(hospvol),data = EAS_data,family = binomial)
summary(resClogit) 





#rare event logistic regression
relogitres <- zelig(death ~ cancerdiag + sex + teamvol * hospvol,
                    data = EAS_dat,model='relogit', case.control = "weighting",
                    tau=length(dt)/length(EAS_dat$id))

summary(relogitres)

x.out1 <- setx(relogitres)

s.out1 <- sim(relogitres, x = x.out1)
summary(s.out1)
plot(s.out1)

predictor.fun <- function(x){
preind <- EAS_dat[2,]
preind$sex = x[1]
preind$cancerdiag = x[2]
preind$hospvol = x[3]
preind$teamvol = x[4]
predict(relogitres, preind,type="response" )
preds <- predict(relogitres, preind, type = "link", se.fit = TRUE)
preds <- preds[[1]]
critval <- 1.96 ## approx 95% CI
upr <- preds[1]$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

tmp <- c(inv.logit(lwr)[1],inv.logit(fit),inv.logit(upr)  )
return ( tmp )

}


predictor.fun(c('1','1','1','1'))
predictor.fun(c('2','1','1','1'))

predictor.fun(c('1','1','1','3'))
predictor.fun(c('1','1','3','1'))




#poisson regression

resClogit <- glm(death ~ as.factor(cancerdiag) + as.factor(sex) + as.factor(teamvol) * as.factor(hospvol),data = EAS_data,family = "poisson")
summary(resClogit) 




### Survival analysis 
head(title)
surv_ob=
  model=coxph(Surv(lostotal, death)~factor(hospvol)*factor(teamvol),data=EAS_data)
model
newdata=data.frame(hospvol=c(1,2,3),teamvol=c(1,1,1))
plot(survfit(model,newdata))




## Unfinish mathching 


plot(EAS_data$teamvol,EAS_data$death)


dt <- which(EAS_data$death==1)
lf <- which(EAS_data$death==0)

match.fun <-  function(vec,dat){
  sx <- which(dat$sex == vec$sex)
  age <- which(dat$ag == vec$ag)
  can <- which(dat$cancerdiag == vec$cancerdiag)  
  tmp <- intersect(intersect(sx,age),can)
  return(tmp)
}



tmp1 <- NULL
for (i in 1:length(dt)){
  lind <- EAS_data[dt[i],]
  dind <- sample(match.fun(lind,EAS_data[lf,]),size=1)
  tmp1 <- c(tmp1,c(EAS_data[lf[dind],]$teamvol,EAS_data[lf[dind],]$hospvol,lind$teamvol,lind$hospvol) )
}

dat=matrix(tmp1,ncol = 4,byrow = T)
colnames(dat)=c('teamv.d','hosv.d','teamv.l','hosv.l')
head(dat)
#team vol
chisq.test(dat[,1],dat[,3])
#hospital vol
chisq.test(dat[,2],dat[,4])





