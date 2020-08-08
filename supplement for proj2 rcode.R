data(nhanes, package="mice")
dat <- nhanes

#*** prepare imputation method
vars <- colnames(dat)
V <- length(vars)
impMethod <- rep("hotDeck", V)
method <- "cor"

#*** imputation in mice
imp <- mice::mice( data=as.matrix(dat), m=1, method=impMethod, method=method )
summary(imp)

library(sem)
install.packages("lavaan")
library(lavaan)

data("Bollen")

#Factor analysis to construct composite factor score for regression moel
```{r}
#EFA
correlations<-cor(data_imp_1[,c(6,15:43)])
fa.result<-fa(correlations,nfactors = 5, rotate  ="varimax",fm = "pa")
fa.weights = data.frame(fa.result$weights)
fa.weights

language.correlations<-cor(data_imp_1[,c(6,15)])
fa.language <- fa(language.correlations, nfactors = 1,fm = "pa", rotate  ="varimax")
fa.language
fa.language.weight = data.frame(fa.language$weights)
fa.language.weight

#add fa values to data_imp_1
data_imp_1$fa.language.value =
  -0.79*data_imp_1$bpvs_raw+0.79*data_imp_1$vocabprocess_processing_speed_target.1

social1.correlations<-cor(data_imp_1[,17:20])
fa.social1 <- fa(social1.correlations, nfactors = 1, fm = "pa")
fa.social1

corrplot(cor(data_imp_1[,22:26]))#find which group of variables are related

social2.correlations<-cor(data_imp_1[,24:26])
fa.social2 <- fa(social2.correlations, nfactors = 1, fm = "pa")
fa.social2$weights

social3.correlations<-cor(data_imp_1[,22:23])
fa.social3 <- fa(social3.correlations, nfactors = 1, fm = "pa")
fa.social3

mydata.sem <- sem(model.mydata, mydata.cov, nrow(mydata))
# print results (fit indices, paramters, hypothesis tests)
summary(mydata.sem)
# print standardized coefficients (loadings)
std.coef(mydata.sem)
```

```
all_search1_1 = summary(regsubsets(composite.language.value ~ I(bilec_home_input^2) + I(bilec_english_input^2) + I(bilec_total_input^2) + I(bilec_total_output^2) +I( bilec_home_output^2) + I(bilec_english_output^2) + I(wasi_sum_rawscores.1^2) + as.factor(where_english) + as.factor(diagnosis) + as.factor(age_acquisition) + I(age_m^2),data= data_imp_1))
all_search1_1$which[which.min(all_search1_1$adjr2),]
OutVals_imp_1 = boxplot(composite.language.value)$out
OutVals_imp_1

data_imp_1_wo_otl_box = data_imp_1[!(composite.language.value %in% OutVals_imp_1),]#delete outliers based on boxplot
```

#pair plot of variables with language
data_imp_1_forpairplot = cbind(composite.language.value,data_imp_1[,c(7:14,16,3,4)])
data_imp_1_forpairplot = data_imp_1_forpairplot[which(data_imp_1_forpairplot$diagnosis==1),]
par(mfrow=c(3,3))
plot(data_imp_1_forpairplot[,2],data_imp_1_forpairplot$composite.language.value,xlab = "bilec_home_input",ylab = "composite.language.scores")
plot(data_imp_1_forpairplot[,3],data_imp_1_forpairplot$composite.language.value,xlab = "bilec_english_input",ylab = "composite.language.scores")
plot(data_imp_1_forpairplot[,4],data_imp_1_forpairplot$composite.language.value,xlab = "bilec_home_output",ylab = "composite.language.scores")
plot(data_imp_1_forpairplot[,5],data_imp_1_forpairplot$composite.language.value,xlab = "bilec_english_output",ylab = "composite.language.scores")
plot(data_imp_1_forpairplot[,6],data_imp_1_forpairplot$composite.language.value,xlab = "bilec_total_input",ylab = "composite.language.scores")
plot(data_imp_1_forpairplot[,7],data_imp_1_forpairplot$composite.language.value,xlab = "bilec_total_output",ylab = "composite.language.scores")
plot(data_imp_1_forpairplot[,10],data_imp_1_forpairplot$composite.language.value,xlab = "wasi_sum_rawscores.1",ylab = "composite.language.scores")
plot(data_imp_1_forpairplot[,11],data_imp_1_forpairplot$composite.language.value,xlab = "age_m",ylab = "composite.language.scores")

data_imp_1_forpairplot.social = cbind(composite.social.value,data_imp_1[,c(7:14,16,3,4)])
data_imp_1_forpairplot.social = data_imp_1_forpairplot.social[which(data_imp_1_forpairplot.social$diagnosis==1),]
par(mfrow=c(3,3))
plot(data_imp_1_forpairplot.social[,2],data_imp_1_forpairplot.social$composite..social.value,xlab = "bilec_home_input",ylab = "composite.social.scores")
plot(data_imp_1_forpairplot.social[,3],data_imp_1_forpairplot.social$composite..social.value,xlab = "bilec_english_input",ylab = "composite.social.scores")
plot(data_imp_1_forpairplot.social[,4],data_imp_1_forpairplot.social$composite..social.value,xlab = "bilec_home_output",ylab = "composite.social.scores")
plot(data_imp_1_forpairplot.social[,5],data_imp_1_forpairplot.social$composite..social.value,xlab = "bilec_english_output",ylab = "composite.social.scores")
plot(data_imp_1_forpairplot.social[,6],data_imp_1_forpairplot.social$composite.social.value,xlab = "bilec_total_input",ylab = "composite.social.scores")
plot(data_imp_1_forpairplot.social[,7],data_imp_1_forpairplot.social$composite.social.value,xlab = "bilec_total_output",ylab = "composite.social.scores")
plot(data_imp_1_forpairplot.social[,10],data_imp_1_forpairplot.social$composite.social.value,xlab = "wasi_sum_rawscores.1",ylab = "composite.social.scores")
plot(data_imp_1_forpairplot.social[,11],data_imp_1_forpairplot.social$composite.social.value,xlab = "age_m",ylab = "composite.social.scores")

data_imp_1_forpairplot.exec1 = cbind(composite.exec1.value,data_imp_1[,c(7:14,16,3,4)])
data_imp_1_forpairplot.exec1= data_imp_1_forpairplot.exec1[which(data_imp_1_forpairplot.exec1$diagnosis==1),]
par(mfrow=c(3,3))
plot(data_imp_1_forpairplot.exec1[,2],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "bilec_home_input",ylab = "composite.exec1.scores")
plot(data_imp_1_forpairplot.exec1[,3],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "bilec_english_input",ylab = "composite.exec1.scores")
plot(data_imp_1_forpairplot.exec1[,4],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "bilec_home_output",ylab = "composite.exec1.scores")
plot(data_imp_1_forpairplot.exec1[,5],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "bilec_english_output",ylab = "composite.exec1.scores")
plot(data_imp_1_forpairplot.exec1[,6],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "bilec_total_input",ylab = "composite.exec1.scores")
plot(data_imp_1_forpairplot.exec1[,7],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "bilec_total_output",ylab = "composite.exec1.scores")
plot(data_imp_1_forpairplot.exec1[,10],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "wasi_sum_rawscores.1",ylab = "composite.exec1.scores")
plot(data_imp_1_forpairplot.exec1[,11],data_imp_1_forpairplot.exec1$composite.exec1.value,xlab = "age_m",ylab = "composite.exec1.scores")

data_imp_1_forpairplot.exec2 = cbind(composite.exec2.value,data_imp_1[,c(7:14,16,3,4)])
data_imp_1_forpairplot.exec2 = data_imp_1_forpairplot.exec2[which(data_imp_1_forpairplot.exec2$diagnosis==1),]
par(mfrow=c(3,3))
plot(data_imp_1_forpairplot.exec2[,2],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "bilec_home_input",ylab = "composite.exec2.scores")
plot(data_imp_1_forpairplot.exec2[,3],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "bilec_english_input",ylab = "composite.exec2.scores")
plot(data_imp_1_forpairplot.exec2[,4],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "bilec_home_output",ylab = "composite.exec2.scores")
plot(data_imp_1_forpairplot.exec2[,5],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "bilec_english_output",ylab = "composite.exec2.scores")
plot(data_imp_1_forpairplot.exec2[,6],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "bilec_total_input",ylab = "composite.exec2.scores")
plot(data_imp_1_forpairplot.exec2[,7],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "bilec_total_output",ylab = "composite.exec2.scores")
plot(data_imp_1_forpairplot.exec2[,10],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "wasi_sum_rawscores.1",ylab = "composite.exec2.scores")
plot(data_imp_1_forpairplot.exec2[,11],data_imp_1_forpairplot.exec2$composite.exec2.value,xlab = "age_m",ylab = "composite.exec2.scores")

par(mfrow=c(2,2))
plot(model3_2_b)
