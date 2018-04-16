#MICROSTRATEGY_BEGIN
#
#RVAR Age.Group -input -string -vector
#RVAR Problem.Count -input -numeric -vector
#RVAR CCC.Congenital -input -numeric -vector
#RVAR CCC.Hem.Imm -input -numeric -vector
#RVAR CCC.Malignancy -input -numeric -vector
#RVAR CCC.Metabolic -input -numeric -vector
#RVAR CCC.Renal -input -numeric -vector
#RVAR Public.or.Private.Metric -input -string -vector
#RVAR Holiday.Indicator -input -numeric -vector
#RVAR Cerebral.Palsy.DX -input -numeric -vector
#RVAR Developmental.Disorders.Group.DX -input -numeric -vector
#RVAR Fractures.DX -input -numeric -vector
#RVAR Nausea.and.Vomiting.DX -input -numeric -vector
#RVAR Last.Primary.Care.Specialty.Seen -input -string -vector
#RVAR Prior.ED.Visit.Count.Orig -input -numeric -vector
#RVAR Gender -input -string -vector
#RVAR Major.Region -input -string -vector
#RVAR Race.and.Ethnicity.Group -input -string -vector

#
#RVAR predicted.fit -output -numeric -vector  #Metric Expression: RScript<_RScriptFile="J:\Ashley\ED Utilization Prediction\ED Utilization Low Use Prediction PC Clinics.R", _InputNames="Age.Group, Problem.Count, CCC.Congenital, CCC.Hem.Imm, CCC.Malignancy, CCC.Metabolic, CCC.Renal, Public.or.Private.Metric, Holiday.Indicator, Cerebral.Palsy.DX,  Developmental.Disorders.Group.DX, Fractures.DX, Nausea.and.Vomiting.DX, Last.Primary.Care.Specialty.Seen, Prior.ED.Visit.Count.Orig, Gender, Major.Region,  Race.and.Ethnicity.Group", _WorkingDir="\\BI_ISD\BI_Share\Ashley\ED Utilization Prediction">(Age.Group, Problem.Count, CCC.Congenital, CCC.Hem.Imm, CCC.Malignancy, CCC.Metabolic, CCC.Renal, Public.or.Private.Metric, Holiday.Indicator, Cerebral.Palsy.DX,  Developmental.Disorders.Group.DX, Fractures.DX, Nausea.and.Vomiting.DX, Last.Primary.Care.Specialty.Seen, Prior.ED.Visit.Count.Orig, Gender, Major.Region,  Race.and.Ethnicity.Group)
#RVAR predicted.se.fit -output -numeric -vector  #Metric Expression: RScript<_RScriptFile="J:\Ashley\ED Utilization Prediction\ED Utilization Low Use Prediction PC Clinics.R", _InputNames="Age.Group, Problem.Count, CCC.Congenital, CCC.Hem.Imm, CCC.Malignancy, CCC.Metabolic, CCC.Renal, Public.or.Private.Metric, Holiday.Indicator, Cerebral.Palsy.DX,  Developmental.Disorders.Group.DX, Fractures.DX, Nausea.and.Vomiting.DX, Last.Primary.Care.Specialty.Seen, Prior.ED.Visit.Count.Orig, Gender, Major.Region,  Race.and.Ethnicity.Group", _WorkingDir="\\BI_ISD\BI_Share\Ashley\ED Utilization Prediction">(Age.Group, Problem.Count, CCC.Congenital, CCC.Hem.Imm, CCC.Malignancy, CCC.Metabolic, CCC.Renal, Public.or.Private.Metric, Holiday.Indicator, Cerebral.Palsy.DX,  Developmental.Disorders.Group.DX, Fractures.DX, Nausea.and.Vomiting.DX, Last.Primary.Care.Specialty.Seen, Prior.ED.Visit.Count.Orig, Gender, Major.Region,  Race.and.Ethnicity.Group)
if(exists("mstr.WorkingDir")) setwd(mstr.WorkingDir)  #Working Directory if executed by MicroStrategy
#
#MICROSTRATEGY_END

if(exists("mstr.WorkingDir")) setwd(mstr.WorkingDir)  #Working Directory if executed by MicroStrategy
## Rscript for predicting High ED utilization with low resource used based on logistic model trained on 2016 data
mstr.ErrMsg <- tryCatch({                                      #tryCatch for Exception Handling
  if(exists("mstr.WorkingDir")) setwd(mstr.WorkingDir)         #Working Directory if executed by MicroStrategy
  #Get the prediction model
  load("EDPredictorsPC.2016.logis.steplogis' .Rda")
  #Get the data
  if(exists("mstr.ExFlag")) {
    #Create a data frame from the input variables
    predictors.df <- data.frame(Age.Group, Problem.Count, CCC.Congenital, CCC.Hem.Imm, CCC.Malignancy, CCC.Metabolic, CCC.Renal, Public.or.Private.Metric, Holiday.Indicator, Cerebral.Palsy.DX,  Developmental.Disorders.Group.DX, Fractures.DX, Nausea.and.Vomiting.DX, Last.Primary.Care.Specialty.Seen, Prior.ED.Visit.Count.Orig, Gender, Major.Region,  Race.and.Ethnicity.Group)
    #If InputNames is non-empty
    if(length(mstr.InputNames) > 0) {
      #Name these variables
      colnames(predictors.df) <- mstr.InputNames
    }
    #If this is NOT via a MicroStrategy Report Execution
  } else {
    load("EDPredictorsPC.2016.logis.steplogis' .Rda") 
    predictors.df <-EDPredictorsLowusePC2017.df
  }
  #Make Predictions
  ## Debug Printing Input
  # library(openxlsx)
  # write.xlsx(predictors.df,"debug_predictors.xlsx")
  ## End Debug Printing Input
  predicted.values <- predict(EDPredictorsLowusePC.2016.logis.steplogis, newdata=predictors.df, type="response", se.fit=TRUE)
  predicted.fit <- predicted.values$fit
  predicted.se.fit <- predicted.values$se.fit
  ## Deug Printing Output
  # write.xlsx(predicted.fit,"debug_predictedfit.xlsx")
  ## End Debug Printing Output
  #Finish
  try(print("Success!"))                                       #Print completion message when run from the console                                             #If we made it here, no errors were caught (using try to continue on any print error)
  mstr.ErrMsg <- ""                                            #If we made it here, no errors were caught
}, error = function(err) {                                     #Catch block to report an error
  try(print(err))                                              #  Print error message to console (using try to continue on any print error)
  return(err$message)                                          #  Return error Message
})