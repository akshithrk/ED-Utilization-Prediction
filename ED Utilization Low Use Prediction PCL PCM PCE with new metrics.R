install.packages("gplots")
install.packages("ROCR")
install.packages("gmodels")
install.packages("logistf")

library(openxlsx) # how to easily open files
library(MASS) # modern applied statistics with S
library(dplyr) # to clean data
library(bitops)
library(gplots)
library(ROCR) # to create ROC 
library(MicroStrategyR)
library(gmodels)
library(logistf)#to find 0,1 error, not used

# set working directory
setwd("J:/Ashley/ED Utilization Prediction")

# 2016 ED Utilization for Training
EDLowusePC_cy2016.df <- read.xlsx("High ED Primary Care WC Low Resources 2015 & 2016 in 2016 for Training.xlsx")

# Validation data set
EDLowusePC_cy2017.df <- read.xlsx("High ED Primary Care WC Low Resources  2016 & 2017 in 2017 for Validation.xlsx")

# Fit model: ch4ecking for colnames to include important metrics in the select below
colnames(EDLowusePC_cy2016.df)
## Select from the full dataset just the variables to try fitting.
EDPredictorsLowusePC.2016.df <- select(EDLowusePC_cy2016.df, High.ED.Utilization.Indicator, Age.Group:Viral.Infection.of.Unspecified.Site.DX, Gender, Major.Region, Race.and.Ethnicity.Group, ED.Shift,Season)

## Fit a logistic regression model with all of the initial set of selected variables
EDPredictorsLowusePC.2016.logis <- glm(High.ED.Utilization.Indicator ~ ., family=binomial(logit), data=EDPredictorsLowusePC.2016.df)
summary(EDPredictorsLowusePC.2016.logis)

# manual test to find error, was season
# error: Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred, error with prior ED count

#EDPredictorsLowusePC.2016.logis <- glm(High.ED.Utilization.Indicator ~ Age.Group+CCC.Count+CCC.Congenital+CCC.CVD+CCC.GI+CCC.Hem.Imm+CCC.Malignancy+CCC.Metabolic+CCC.Misc+CCC.Neonatal+CCC.Neuromuscular+CCC.Renal
#                                       +CCC.Respiratory+Problem.Count+Public.or.Private.Metric+Holiday.Indicator+ED.Checkin.Hour.Metric+ED.Checkin.Day.of.the.Week
#                                       +Weekend.Indicator+ED.Triage.Acuity.Metric+ED.Triage.Acuity.Grouped+Med.Date.Time.Ind+Lab.Event.Date.Time.Ind+Order.Date.Time.Ind
#                                       +Abdominal.and.Pelvic.Pain.DX+Abnormalities.of.Breathing.DX+Acute.URI.DX+Asthma.DX+Cough.DX+Fever.DX
#                                       +Long.Term.Drug.Therapy.DX+Nausea.and.Vomiting.DX+Viral.Agents.DX+Viral.Infection.of.Unspecified.Site.DX
#                                       +Gender+Major.Region+Race.and.Ethnicity.Group+ED.Shift+Season, family=binomial(logit), data=EDPredictorsLowusePC.2016.df)
#summary(EDPredictorsLowusePC.2016.logis)

## Use step-wise elimination to automatically remove those predictor variables that do not signficantly contribute to the model

EDPredictorsLowusePC.2016.logis.steplogis <- stepAIC(EDPredictorsLowusePC.2016.logis, trace=0)
summary(EDPredictorsLowusePC.2016.logis.steplogis)
# save the new model
#save(EDPredictorsLowusePC.2016.logis.steplogis, file="EDPredictorsPC.2016.logis.steplogis.Rda")

EDPredictorsLowusePC.2017.df <- select(EDLowusePC_cy2017.df, High.ED.Utilization.Indicator, Age.Group:Prior.ED.Visit.Count, Gender, Major.Region, Race.and.Ethnicity.Group, ED.Shift)
# save the 2017 validation data set
# save(ICUPredictorsLowusePC.JanDec2017.df, file="ICUPredictorsPC.JanDec2017.df.Rda")

## Predict outcomes by feeding the validation predictor variables into the model
EDPredictorsLowusePC.t2016.v2017.steplogis.predict <- predict(EDPredictorsLowusePC.2016.logis.steplogis, newdata=EDPredictorsLowusePC.2017.df, type="response", se.fit=TRUE)

## Tally up the predicted outcomes vs. the actual outcomes included in the validation dataset (This is called scoring.)

evaluation.scores <- prediction(EDPredictorsLowusePC.t2016.v2017.steplogis.predict$fit, EDPredictorsLowusePC.2017.df$High.ED.Utilization.Indicator)


## Calculate statistical measures of the quality of the model from the evaluation scores

evaluation.roc <- performance(evaluation.scores, "tpr", "fpr")          # ROC curve
evaluation.rocarea <- performance(evaluation.scores, "auc")@y.values    # Area under ROC curve
evaluation.cutoffs <- evaluation.roc@alpha.values[[1]]                  # Prediction cutoff values
evaluation.fpr <- evaluation.roc@x.values[[1]]                          # False positive rate
evaluation.tpr <- evaluation.roc@y.values[[1]]                          # True positive rate
evaluation.tnr <- performance(evaluation.scores, "tnr")                 # True negative rate
evaluation.ppv <- performance(evaluation.scores, "ppv")                 # Positive predictive value
evaluation.npv <- performance(evaluation.scores, "npv")                 # Negative predictive value
evaluation.acc <- performance(evaluation.scores, "acc")                 # Accuracy
evaluation.err <- performance(evaluation.scores, "err")                 # Error rate


## Plot the evaluation measures in various ways to visually assess the quality of the predictions


### Create ROC plot and write to file

plot(evaluation.roc, col = "red", lwd=2, ylim=c(0,1),xlim=c(0,1))
abline(0,1, lty = 8, col = "grey")
title( main="ROC Curve for Predictions of High ED Use with Low Activity in Primary Care Well Child Patients",
       sub="Logistic model trained with CY2016 and validated with 2017 data")
legend("bottom",legend=paste("ROC Area:",format(evaluation.rocarea, digits=3)))


### Create TPR/TNR plot and write to file

plot(evaluation.tnr, ylim=c(0,1), col="black", lwd=2, ylab="Rates")
lines(evaluation.cutoffs, evaluation.tpr, ylim=c(0,1),col="red", lwd=2, lty=1)
legend("bottom",c("TPR   21+ days", "TNR   21+ days"), col=c("red","black"), lty=c(1,1), lwd=c(2,2))
title(main="True Positive and True Negative Rate Plots by Cutoff\nFor Predictions of High ED Use with Low Activity in Primary Care Well Child Patients",
      sub="Logistic model trained with CY2016 and validated with 2017 data")


### Create PPV/NPV plot and write to file

plot(evaluation.ppv, ylim=c(0,1), col="red", lwd=2, ylab="Rates")
lines(evaluation.npv@x.values[[1]], evaluation.npv@y.values[[1]],
      ylim=c(0,1),col="black", lwd=2, lty=1)
legend("bottom",c("PPV   21+ days", "NPV   21+ days"),
       col=c("red","black"), lty=c(1,1), lwd=c(2,2))
title(main="Positive Predictive Value and Negative Predictive Value Plots by Cutoff\nFor Predictions of High ED Use with Low Activity in Primary Care Well Child Patients",
      sub="Logistic model trained with CY2016 and validated with 2017 data")


### Create Accuracy/Error plot and write to file

plot(evaluation.acc@x.values[[1]], evaluation.acc@y.values[[1]], col="red",
     xlab=evaluation.acc@x.name, ylab="Accuracy", type="l",lwd=2,xlim=c(0,1),ylim=c(0,1))
lines(evaluation.err@x.values[[1]], evaluation.err@y.values[[1]],col="black", lwd=1, lty=2)
legend("bottomright",c("ACC   21+ days", "ERR   21+ days"),
       col=c("red","black"),lty=c(1,2), lwd=c(2,1))
title(main="Accuracy and Error Rates by Cutoff\nFor Predictions of High ED Use with Low Activity in Primary Care Well Child Patients",
      sub="Logistic model trained with CY2016 and validated with 2017 data")

