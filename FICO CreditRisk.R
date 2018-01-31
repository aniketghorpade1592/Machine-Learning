11.	Appendix B - R Codes
###Set Working Directory for FICO: either R code or GUI
##setwd("C:/Users/Simon/Desktop/MIS 749/FICO Project/Datasets")
getwd()

############################################################################
###Load neccessary packages
library(data.table)
library(ggplot2)
library(lattice)
library(caret)
library(doParallel)
library(corrplot)
library(psych)
library(utils)
library(tree)
library(GGally)
library(mice)
library(dplyr)
library(missForest)
library(VIM)
library(pROC)   #to use the Area Under Curve function: i.e. auc(predictions$survived, predictions$pred)

############################################################################
###Set the cores. REMEMBER TO release the laptop cores at the end of the file!!!
cl <- makePSOCKcluster(16)     #register 2 cores to split of training
clusterSetRNGStream(cl, 749)  #set seed for every member of cluster
registerDoParallel(cl)
getDoParWorkers()             #list number of workers/cores

############################################################################
###Set seed for all the analysis performed below
set.seed(749)

############################################################################
###Read the clean/processed RiskData as RiskData.rds
##Use this after the first time running all the necessary Pre-Processing codes
RiskData <- data.table(read.csv("~/RiskData.csv"))

str(RiskData)

table(is.na(RiskData$Debt_Ratio))       #127 NA values because Debt CAN'T DIVIDE BY 0
table(is.na(RiskData$CB_IL_Util))       #4,007 NA values MISSING NOT AT RANDOM

############################################################################
######DATA CLEAN UP

###Load data table: RiskData.csv using read.table FIRST
RiskData <- read.table("~/RiskData.csv", header=TRUE, sep=",")

###Some basic checks on RiskData
str(RiskData)
names(RiskData)
head(RiskData, 5)
levels(RiskData$Region)
table(is.na(RiskData$Region))     #No missing data for Region column

levels(RiskData$Bank_Relationship) #"M_Ever" "M_Open" "O_Ever" "O_Open" and we should have "NA" as New Customer/Applicant
is.na(RiskData$Bank_Relationship)

############################################################################
###Converting Application_Date from numerics to date 
RiskData$Application_Date <- as.Date(as.character(RiskData$Application_Date), "%Y%m%d")
RiskData$Application_Date
##Then, we try some arithmetics and conversions between Date and Numeric formats:
head(RiskData$Application_Date)

as.numeric(RiskData$Application_Date[2] - RiskData$Application_Date[1])

## smallest value for date is the following
min(RiskData$Application_Date)
##The idea is to create a point-based column which take into account how current applicant date is in term of days.
#The first/earliest/smallest date in the data set is "2010-03-01".
#The last/latest/largest date in the data set is "2012-02-01".
#Let's assign the baseline Date to be "2010-02-28" so that 1 point represents the least current Application Date: "2010-03-01" 
#Then, the bigger the point, more current the Application Date. This may help in our analysis later on.

RiskData$Application_Date.points <- rep(0, 10451)   #Add the "Application_Date.points" Column with all 10,459 "0" values initially

for(i in 1:nrow(RiskData)){
  RiskData$Application_Date.points[i] <- as.numeric(RiskData$Application_Date[i] - as.Date(as.character("2010-02-28", "%Y-%m-%d")))
}

str(RiskData)
RiskData$Application_Date.points
View(RiskData)

##Now we can SAFELY REMOVE the original "Application_Date" Column after obtaining and assigning points to the "Application_Date.points" Column 

RiskData$Application_Date <- NULL
str(RiskData)

############################################################################
###Converting numeric values in factor forms TO actual numeric data type
RiskData$Debt_Ratio <- as.numeric(as.character(RiskData$Debt_Ratio))
RiskData$FICO_Score <- as.numeric(as.character(RiskData$FICO_Score))
RiskData$CB_Age_Oldest_TL <- as.numeric(as.character(RiskData$CB_Age_Oldest_TL))
RiskData$CB_Age_Newest_TL <- as.numeric(as.character(RiskData$CB_Age_Newest_TL))
RiskData$CB_Avg_Mos_File <- as.numeric(as.character(RiskData$CB_Avg_Mos_File))
RiskData$CB_Nb_Sat_TL <- as.numeric(as.character(RiskData$CB_Nb_Sat_TL))
RiskData$CB_Nb_60._TL <- as.numeric(as.character(RiskData$CB_Nb_60._TL))
RiskData$CB_Nb_90._TL <- as.numeric(as.character(RiskData$CB_Nb_90._TL))
RiskData$CB_Pct_Sat_TL <- as.numeric(as.character(RiskData$CB_Pct_Sat_TL))
RiskData$CB_Mos_Since_Dlq <- as.numeric(as.character(RiskData$CB_Mos_Since_Dlq))
RiskData$CB_Max_Dlq_12_Mos <- as.numeric(as.character(RiskData$CB_Max_Dlq_12_Mos))
RiskData$CB_Max_Dlq_Ever <- as.numeric(as.character(RiskData$CB_Max_Dlq_Ever))
RiskData$CB_Nb_Total_TL <- as.numeric(as.character(RiskData$CB_Nb_Total_TL))
RiskData$CB_Nb_TL_Open_12 <-  as.numeric(as.character(RiskData$CB_Nb_TL_Open_12))
RiskData$CB_Pct_IL_TL <- as.numeric(as.character(RiskData$CB_Pct_IL_TL))
RiskData$CB_Nb_Inq_6_Mos <- as.numeric(as.character(RiskData$CB_Nb_Inq_6_Mos))
RiskData$CB_Nb_Inq_6_Mos_excl_7_Days<-as.numeric(as.character(RiskData$CB_Nb_Inq_6_Mos_excl_7_Days))
RiskData$CB_Rev_Util <- as.numeric(as.character(RiskData$CB_Rev_Util))
RiskData$CB_IL_Util <- as.numeric(as.character(RiskData$CB_IL_Util))
RiskData$CB_Nb_Rev_TL_w_Bal<-as.numeric(as.character(RiskData$CB_Nb_Rev_TL_w_Bal))
RiskData$CB_Nb_IL_TL_w_Bal <- as.numeric(as.character(RiskData$CB_Nb_IL_TL_w_Bal))
RiskData$CB_Nb_Rev_Tl_75_Pct_Limit<-as.numeric(as.character(RiskData$CB_Nb_Rev_Tl_75_Pct_Limit))
RiskData$CB_Pct_TL_w_Bal <- as.numeric(as.character(RiskData$CB_Pct_TL_w_Bal))

############################################################################
###Since RiskData$Bank_Relationship has NA values for New Customer. Let's add "NA" value as an extra level of the factor Bank_Relationship
#addNA() modifies a factor by turning NA into an extra level (so that NA values are counted in tables, for instance).
RiskData$Bank_Relationship <- addNA(RiskData$Bank_Relationship)
#Now the levels are: 1 - "M_Ever"; 2 - "M_Open"; 3 - "O_Ever"; 4 - "O_Open"; 5 - "NA"
levels(RiskData$Bank_Relationship)
str(RiskData)
table(is.na(RiskData$Bank_Relationship))    #double-check to make sure NA values are grouped as the 5th level

############################################################################


#Working on the CB_Mos_Since_Dlq column. Shifting the scale of the column to the right by 1, and thus eventually replacing 1000 by 0. Done to handle No_DLq == 999, in order to match the scale. 
###CB_Mos_Since_Dlq:
###1. Add 1 to the entire column
###2. Replace all "1000" values to "0" values

table(is.na(RiskData$CB_Mos_Since_Dlq))  #758 observations have missing values in CB_Mos_Since_Dlq.flag Column.
RiskData$CB_Mos_Since_Dlq <- RiskData$CB_Mos_Since_Dlq+1
RiskData$CB_Mos_Since_Dlq[RiskData$CB_Mos_Since_Dlq==1000] <- 0
str(RiskData)

######CHECKPOINT
###Save as RiskData.rds  
saveRDS(RiskData, "~/RiskData.rds")
###Read the clean/processed RiskData as RiskData.rds
RiskData <- data.table(readRDS("~/RiskData.rds"))

############################################################################
###View RiskData File using utils package
View(RiskData)

############################################################################
######ANALYZING AND IMPUTING MISSING DATA
##The mice package provides a nice function md.pattern() to get a better understanding of the pattern of missing data
md.pattern(RiskData)    #pattern of missing variables
md.pairs(RiskData)      #view frequency of missing value pairs

##A perhaps more helpful visual representation can be obtained using the VIM package as follows
#magenta is imputed data and green is observed
?aggr()

#Full Missing Data Visualizations of the whole RiskData
str(RiskData)
aggr_plot <- aggr(RiskData, col=c('Green','Red'), numbers=TRUE, sortVars=FALSE, labels=names(RiskData), cex.axis=0.45, gap=0, ylab=c("Proportion of Missing Data","Pattern"))

#With "Bank_Relationship" column removed for better visualization
aggr_plot <- aggr(RiskData[ , -9], col=c('Green','Red'), numbers=TRUE, sortVars=FALSE, labels=names(RiskData[ , -9]), cex.axis=0.5, gap=0, ylab=c("Proportion of Missing Data","Pattern"))

##If missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out (BUT WE HAVE THE MICE PACKAGE FOR IMPUTING MISSING DATA). We therefore check for features (columns) and samples (rows) where more than 5% of the data is missing using a simple function:
pMiss <- function(x)
{
  sum(is.na(x))/length(x)*100
}
apply(RiskData, 2, pMiss)     #check for % missing data in each feature (column)
apply(RiskData, 1, pMiss)     #check for % missing data in each observation (row)

############################################################################
#######NEED TO TEST OUT VARIOUS OPTIONS FOR MICE() IMPUTATION: 
###We have decided to use the "pmm" method of the mice()
###1. "Debt_Ratio" KEEP & "CB_IL_Util" KEEP BEFORE IMPUTING RiskData.Y.rm.Imputed
###2. "Debt_Ratio" DROP & "CB_IL_Util" KEEP BEFORE IMPUTING RiskData.Y.rm.Imputed
###3. "Debt_Ratio" DROP & "CB_IL_Util" DROP BEFORE IMPUTING RiskData.Y.rm.Imputed

############################################################################
##mice() function
#default is m=10 sets of imputations to be merged into one final dataset
#defaults to polytomous regression for categorical variables and predictive mean matching for numeric data
?mice()
methods(mice)       #a list of the available imputation methods.
str(RiskData)

############################################################################
######Approach 1:
##Assign "RiskData$Risk_Flag" to a stand alone Response Y, namely "Risk_Flag.Y"
Risk_Flag.Y <- RiskData$Risk_Flag

##ALL APPROACHES HAVE Response Y: "Risk_Flag" DROPPED
##Assign the new "RiskData" as "RiskData.Y.rm" and remove the "Risk_Flag" Column 
RiskData.Y.rm <- RiskData
RiskData.Y.rm$Risk_Flag <- NULL   #Remove Reponse Y: "Risk_Flag" Column
str(RiskData.Y.rm)

###"Debt_Ratio" KEEP & "CB_IL_Util" KEEP BEFORE IMPUTING RiskData.Y.rm.Imputed
str(RiskData.Y.rm)
set.seed(749)

RiskData.Y.rm.Imputed.Appr1.pmm <- mice(RiskData.Y.rm, m = 5, maxit = 20, method = 'pmm')     
#default method for numeric variables are 'pmm': predictive mean matching
#m = 5 means there will be 5 imputed sets
#maxit = 20 means there will be 20 iterations, each iteration has 5 imputed sets

#The following commands will help to check the names of imputed columns and various processing carried out on the data
names(RiskData.Y.rm.Imputed.Appr1.pmm)
names(RiskData.Y.rm.Imputed.Appr1.pmm$imp)

RiskData.Y.rm.Imputed.Appr1.pmm$m          #to check that m = 5 imputed sets
RiskData.Y.rm.Imputed.Appr1.pmm$imp        #to check the imputed data for all specified variables
RiskData.Y.rm.Imputed.Appr1.pmm$method     #to check the imputation method used for each variable: since all columns are numeric, 'pmm' method is used by default
RiskData.Y.rm.Imputed.Appr1.pmm$nmis       #to check the "number of missing values" for each column in the "RiskData.Y.rm.Imputed" imputed data set
RiskData.Y.rm.Imputed.Appr1.pmm$predictorMatrix #to check which variables predict missingness in which other variables: "0" = NO, "1" = YES

###Compare Distributions in "RiskData.Y.rm.Imputed.Appr1.pmm"
densityplot(RiskData.Y.rm.Imputed.Appr1.pmm)   #magenta is imputed dataset while blue is the observed data

###use complete() to combine the original values and the imputed values 
RiskData.Y.rm.Completed.1.Appr1.pmm <- complete(RiskData.Y.rm.Imputed.Appr1.pmm, 1, include = FALSE)     #... using the 1st imputed set of Approach 1 with pmm WITHOUT INCLUDING THE ORIGINAL PRE-IMPUTED DATA SET
write.csv(RiskData.Y.rm.Completed.1.Appr1.pmm, "~/RiskData.Y.rm.Completed.1.Appr1.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.1.Appr1.pmm, "~/RiskData.Y.rm.Completed.1.Appr1.pmm.rds")
str(RiskData.Y.rm.Completed.1.Appr1.pmm)

###
RiskData.Y.rm.Completed.2.Appr1.pmm <- complete(RiskData.Y.rm.Imputed.Appr1.pmm, 2, include = FALSE)
write.csv(RiskData.Y.rm.Completed.2.Appr1.pmm, "~/RiskData.Y.rm.Completed.2.Appr1.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.2.Appr1.pmm, "~/RiskData.Y.rm.Completed.2.Appr1.pmm.rds")
###
RiskData.Y.rm.Completed.3.Appr1.pmm <- complete(RiskData.Y.rm.Imputed.Appr1.pmm, 3, include = FALSE)
write.csv(RiskData.Y.rm.Completed.3.Appr1.pmm, "~/RiskData.Y.rm.Completed.3.Appr1.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.3.Appr1.pmm, "~/RiskData.Y.rm.Completed.3.Appr1.pmm.rds")
###
RiskData.Y.rm.Completed.4.Appr1.pmm <- complete(RiskData.Y.rm.Imputed.Appr1.pmm, 4, include = FALSE)
write.csv(RiskData.Y.rm.Completed.4.Appr1.pmm, "~/RiskData.Y.rm.Completed.4.Appr1.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.4.Appr1.pmm, "~/RiskData.Y.rm.Completed.4.Appr1.pmm.rds")
###
RiskData.Y.rm.Completed.5.Appr1.pmm <- complete(RiskData.Y.rm.Imputed.Appr1.pmm, 5, include = FALSE)
write.csv(RiskData.Y.rm.Completed.5.Appr1.pmm, "~/RiskData.Y.rm.Completed.5.Appr1.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.5.Appr1.pmm, "~/RiskData.Y.rm.Completed.5.Appr1.pmm.rds")

set.seed(749)
###### Approach 2 
###2. "Debt_Ratio" DROP & "CB_IL_Util" KEEP BEFORE IMPUTING RiskData.Y.rm.Imputed
##Assign the new "RiskData" as "RiskData.Y.rm" and remove the "Risk_Flag" Column 
RiskData.Y.rm <- RiskData
RiskData.Y.rm$Risk_Flag <- NULL   #Remove Reponse Y: "Risk_Flag" Column
str(RiskData.Y.rm)

### Now lets remove the "Debt_Ratio" column as per approach 2
RiskData.Y.rm.App2 <- RiskData.Y.rm
RiskData.Y.rm.App2$Debt_Ratio <-  NULL
str(RiskData.Y.rm.App2)

write.csv(RiskData.Y.rm.App2, "~/RiskData.Y.rm.App2.csv")
saveRDS(RiskData.Y.rm.App2, "~/RiskData.Y.rm.App2.rds")

######APPROACH 2 WITH 'PMM' IMPUTATION METHOD
RiskData.Y.rm.App2.Imputed.pmm <- mice(RiskData.Y.rm.App2, m = 5, maxit = 20, method = 'pmm')
densityplot(RiskData.Y.rm.App2.Imputed.pmm)   #magenta is imputed dataset while blue is the observed data
####Completing the data
RiskData.Y.rm.App2.Completed.1.pmm <- complete(RiskData.Y.rm.App2.Imputed.pmm,1,include = FALSE)
write.csv(RiskData.Y.rm.App2.Completed.1.pmm, "~/RiskData.Y.rm.App2.Completed.1.pmm.csv")
saveRDS(RiskData.Y.rm.App2.Completed.1.pmm, "~/RiskData.Y.rm.App2.Completed.1.pmm.rds")

RiskData.Y.rm.App2.Completed.2.pmm <- complete(RiskData.Y.rm.App2.Imputed.pmm,2,include = FALSE)
write.csv(RiskData.Y.rm.App2.Completed.2.pmm, "~/RiskData.Y.rm.App2.Completed.2.pmm.csv")
saveRDS(RiskData.Y.rm.App2.Completed.2.pmm, "~/RiskData.Y.rm.App2.Completed.2.pmm.rds")

RiskData.Y.rm.App2.Completed.3.pmm <- complete(RiskData.Y.rm.App2.Imputed.pmm,3,include = FALSE)
write.csv(RiskData.Y.rm.App2.Completed.3.pmm, "~/RiskData.Y.rm.App2.Completed.3.pmm.csv")
saveRDS(RiskData.Y.rm.App2.Completed.3.pmm, "~/RiskData.Y.rm.App2.Completed.3.pmm.rds")

RiskData.Y.rm.App2.Completed.4.pmm <- complete(RiskData.Y.rm.App2.Imputed.pmm,4,include = FALSE)
write.csv(RiskData.Y.rm.App2.Completed.4.pmm, "~/RiskData.Y.rm.App2.Completed.4.pmm.csv")
saveRDS(RiskData.Y.rm.App2.Completed.4.pmm, "~/RiskData.Y.rm.App2.Completed.4.pmm.rds")

RiskData.Y.rm.App2.Completed.5.pmm <- complete(RiskData.Y.rm.App2.Imputed.pmm,5,include = FALSE)
write.csv(RiskData.Y.rm.App2.Completed.5.pmm, "~/RiskData.Y.rm.App2.Completed.5.pmm.csv")
saveRDS(RiskData.Y.rm.App2.Completed.5.pmm, "~/RiskData.Y.rm.App2.Completed.5.pmm.rds")

#Better visualization for RiskData.Y.rm.Completed.Appr1. NOTE THAT "Bank_Relationship" "NA" values are ACTUALLY NEW APPLICANTS/CUSTOMERS
aggr_plot <- aggr(RiskData.Y.rm.Completed.Appr1.pmm, col=c('Green','Red'), numbers=TRUE, sortVars=FALSE, labels=names(RiskData.Y.rm.Completed.Appr1.pmm), cex.axis=0.45, gap=0, ylab=c("Proportion of Missing Data","Pattern"))

############################################################################
############APPROACH 3
###3. "Debt_Ratio" DROP & "CB_IL_Util" DROP BEFORE IMPUTING "RiskData.Y.rm.Imputed.Appr3.pmm" and "RiskData.Y.rm.Imputed.Appr3.rf"
set.seed(749)
### DROP Response Y, "Risk_Flag" AND "Debt_Ratio" AND "CB_IL_Util"
RiskData.Y.rm.Appr3 <- RiskData
RiskData.Y.rm.Appr3$Risk_Flag <- NULL   #Remove Reponse Y: "Risk_Flag" Column
RiskData.Y.rm.Appr3$Debt_Ratio <- NULL
RiskData.Y.rm.Appr3$CB_IL_Util <- NULL
str(RiskData.Y.rm.Appr3)
###Save as .csv and .rds
write.csv(RiskData.Y.rm.Appr3, "~/RiskData.Y.rm.Appr3.csv")
saveRDS(RiskData.Y.rm.Appr3, "~/RiskData.Y.rm.Appr3.rds")

set.seed(749)
######APPROACH 3 WITH 'PMM' IMPUTATION METHOD
RiskData.Y.rm.Imputed.Appr3.pmm <- mice(RiskData.Y.rm.Appr3, m = 5, maxit = 20, method = 'pmm')     
saveRDS(RiskData.Y.rm.Imputed.Appr3.pmm, "C:/Users/Simon/Desktop/MIS 749/FICO Project/Datasets/RiskData.Y.rm.Imputed.Appr3.pmm.rds")
densityplot(RiskData.Y.rm.Imputed.Appr3.pmm)   #magenta is imputed dataset while blue is the observed data

RiskData.Y.rm.Completed.1.Appr3.pmm <- complete(RiskData.Y.rm.Imputed.Appr3.pmm, 5, include = FALSE)
write.csv(RiskData.Y.rm.Completed.1.Appr3.pmm, "~/RiskData.Y.rm.Completed.1.Appr3.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.1.Appr3.pmm, "~/RiskData.Y.rm.Completed.1.Appr3.pmm.rds")

RiskData.Y.rm.Completed.2.Appr3.pmm <- complete(RiskData.Y.rm.Imputed.Appr3.pmm, 5, include = FALSE)
write.csv(RiskData.Y.rm.Completed.2.Appr3.pmm, "~/RiskData.Y.rm.Completed.2.Appr3.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.2.Appr3.pmm, "~/RiskData.Y.rm.Completed.2.Appr3.pmm.rds")

RiskData.Y.rm.Completed.3.Appr3.pmm <- complete(RiskData.Y.rm.Imputed.Appr3.pmm, 5, include = FALSE)
write.csv(RiskData.Y.rm.Completed.3.Appr3.pmm, "~/RiskData.Y.rm.Completed.3.Appr3.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.3.Appr3.pmm, "~/RiskData.Y.rm.Completed.3.Appr3.pmm.rds")

RiskData.Y.rm.Completed.4.Appr3.pmm <- complete(RiskData.Y.rm.Imputed.Appr3.pmm, 5, include = FALSE)
write.csv(RiskData.Y.rm.Completed.4.Appr3.pmm, "~/RiskData.Y.rm.Completed.4.Appr3.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.4.Appr3.pmm, "~/RiskData.Y.rm.Completed.4.Appr3.pmm.rds")

RiskData.Y.rm.Completed.5.Appr3.pmm <- complete(RiskData.Y.rm.Imputed.Appr3.pmm, 5, include = FALSE)
write.csv(RiskData.Y.rm.Completed.5.Appr3.pmm, "~/RiskData.Y.rm.Completed.5.Appr3.pmm.csv")
saveRDS(RiskData.Y.rm.Completed.5.Appr3.pmm, "~/RiskData.Y.rm.Completed.5.Appr3.pmm.rds")

########################################################################################## Data Modeling for all approaches and selection of the best approach based on results
set.seed(749)

#https://stats.stackexchange.com/questions/188955/is-it-necessary-to-split-dataset-for-cross-validation
trainIndex <- createDataPartition(Risk_Flag.Y, p = 0.8, list = FALSE)
trainIndex

### writing control for modeling and using caret package for the same.
control <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary,
                        classProbs = TRUE, savePredictions = TRUE) 

###### APPROACH 1 COMPLETED 1
### For modeling, we again join the RiskFlag to the dataset for model training
RiskData.Completed.1.Appr1.pmm <- RiskData.Y.rm.Completed.1.Appr1.pmm
RiskData.Completed.1.Appr1.pmm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.1.Appr1.pmm.d.model <- dummyVars(~ . , data = RiskData.Completed.1.Appr1.pmm, fullRank = TRUE)
RiskData.Completed.1.Appr1.pmm.d <- as.data.frame(predict(RiskData.Completed.1.Appr1.pmm.d.model, RiskData.Completed.1.Appr1.pmm))
for(i in 1:nrow(RiskData.Completed.1.Appr1.pmm.d)){
  RiskData.Completed.1.Appr1.pmm.d$Risk_Flag[i] <- if_else(RiskData.Completed.1.Appr1.pmm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.1.Appr1.pmm.d$Risk_Flag <- as.factor(RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
str(RiskData.Completed.1.Appr1.pmm.d)

train.RiskData.Completed.1.Appr1.pmm.d <- RiskData.Completed.1.Appr1.pmm.d[trainIndex,]
test.RiskData.Completed.1.Appr1.pmm.d <- RiskData.Completed.1.Appr1.pmm.d[-trainIndex,]

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.logit <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.1.Appr1.pmm.d[trainIndex,],
                                                 method = "glm", 
                                                 family = "binomial", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"))
RiskData.Completed.1.Appr1.pmm.d.logit

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.glmnet <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                  weights = Sampling_Weight, 
                                                  data = RiskData.Completed.1.Appr1.pmm.d[trainIndex,],
                                                  method = "glmnet",
                                                  metric = "ROC", 
                                                  trControl = control, 
                                                  preProcess = c("scale","center"),
                                                  tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.d.glmnet

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.lda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                               method = "lda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.1.Appr1.pmm.d.lda

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.stepLDA <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                                   method = "stepLDA", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"),
                                                   tuneLength = 10)											   
RiskData.Completed.1.Appr1.pmm.d.stepLDA

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.qda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                               method = "qda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.1.Appr1.pmm.d.qda

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.knn <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                               method = "knn", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.d.knn

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.pls <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                               method = "pls", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.d.pls

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                     weights = Sampling_Weight, 
                                                     data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                                     method = "gamSpline", 
                                                     metric = "ROC", 
                                                     trControl = control, 
                                                     preProcess = c("scale","center"),
                                                     tuneLength = 10)
varImp(RiskData.Completed.1.Appr1.pmm.d.gamSpline)
RiskData.Completed.1.Appr1.pmm.d.gamSpline

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.rpart <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                                 method = "rpart", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"),
                                                 tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.d.rpart

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.treebag <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                                   method = "treebag", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"))
RiskData.Completed.1.Appr1.pmm.d.treebag

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.rf <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                              weights = Sampling_Weight, 
                                              data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                              method = "rf", 
                                              metric = "ROC", 
                                              trControl = control, 
                                              preProcess = c("scale","center"),
                                              tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.d.rf

set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.gbm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                               method = "gbm", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.d.gbm


##svm
set.seed(749)
RiskData.Completed.1.Appr1.pmm.d.svm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.1.Appr1.pmm.d[trainIndex, ], 
                                               method = "svmRadial", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.d.svm

RiskData.Completed.1.Appr1.pmm.d.list.models <- list("logit" = RiskData.Completed.1.Appr1.pmm.d.logit,
                                                     "lasso/ridge" = RiskData.Completed.1.Appr1.pmm.d.glmnet, 
                                                     "lda" = RiskData.Completed.1.Appr1.pmm.d.lda,
                                                     "stepLDA" = RiskData.Completed.1.Appr1.pmm.d.stepLDA,
                                                     "qda" = RiskData.Completed.1.Appr1.pmm.d.qda,
                                                     "knn" = RiskData.Completed.1.Appr1.pmm.d.knn,
                                                     "pls" = RiskData.Completed.1.Appr1.pmm.d.pls,
                                                     "gamSpline" = RiskData.Completed.1.Appr1.pmm.d.gamSpline,
                                                     "tree" = RiskData.Completed.1.Appr1.pmm.d.rpart,
                                                     "bagging" = RiskData.Completed.1.Appr1.pmm.d.treebag,
                                                     "randomForest" = RiskData.Completed.1.Appr1.pmm.d.rf,
                                                     "boosting" = RiskData.Completed.1.Appr1.pmm.d.gbm,
                                                     "svm" = RiskData.Completed.1.Appr1.pmm.d.svm)

RiskData.Completed.1.Appr1.pmm.d.list.resamples = resamples(RiskData.Completed.1.Appr1.pmm.d.list.models)


#plot performance comparisons
bwplot(RiskData.Completed.1.Appr1.pmm.d.list.resamples, metric="ROC") 
bwplot(RiskData.Completed.1.Appr1.pmm.d.list.resamples, metric="Sens")
bwplot(RiskData.Completed.1.Appr1.pmm.d.list.resamples, metric="Spec") 

##calculate ROC curves on resampled data
RiskData.Completed.1.Appr1.pmm.d.logit.roc<- roc(response = RiskData.Completed.1.Appr1.pmm.d.logit$pred$obs, 
                                                 predictor = RiskData.Completed.1.Appr1.pmm.d.logit$pred$Default)

RiskData.Completed.1.Appr1.pmm.d.lda.roc<- roc(response = RiskData.Completed.1.Appr1.pmm.d.lda$pred$obs, 
                                               predictor = RiskData.Completed.1.Appr1.pmm.d.lda$pred$Default)

RiskData.Completed.1.Appr1.pmm.d.qda.roc<- roc(response = RiskData.Completed.1.Appr1.pmm.d.qda$pred$obs, 
                                               predictor = RiskData.Completed.1.Appr1.pmm.d.qda$pred$Default)

RiskData.Completed.1.Appr1.pmm.d.treebag.roc<- roc(response = RiskData.Completed.1.Appr1.pmm.d.treebag$pred$obs, 
                                                   predictor = RiskData.Completed.1.Appr1.pmm.d.treebag$pred$Default)

#WHEN MODEL HAS PARAMETERS, MAKE SURE TO SELECT FINAL/OPTIMAL PARAMETER VALUE. EXAMPLE:
RiskData.Completed.1.Appr1.pmm.d.glmnet.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.1.Appr1.pmm.d.glmnet$pred[RiskData.Completed.1.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.1.Appr1.pmm.d.glmnet$pred[RiskData.Completed.1.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$Default)

RiskData.Completed.1.Appr1.pmm.d.stepLDA.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.stepLDA$pred$obs, 
  predictor = RiskData.Completed.1.Appr1.pmm.d.stepLDA$pred$Default)

RiskData.Completed.1.Appr1.pmm.d.knn.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.knn$pred[RiskData.Completed.1.Appr1.pmm.d.knn$pred$k == 23,]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.d.knn$pred[RiskData.Completed.1.Appr1.pmm.d.knn$pred$k == 23,]$Default) 

RiskData.Completed.1.Appr1.pmm.d.pls.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.pls$pred[RiskData.Completed.1.Appr1.pmm.d.pls$pred$ncomp == 7,]$obs, 
  predictor = RiskData.Completed.1.Appr1.pmm.d.pls$pred[RiskData.Completed.1.Appr1.pmm.d.pls$pred$ncomp == 7,]$Default)

RiskData.Completed.1.Appr1.pmm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.1.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.1.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

RiskData.Completed.1.Appr1.pmm.d.rpart.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.1.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.1.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$Default)

RiskData.Completed.1.Appr1.pmm.d.rf.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.rf$pred[RiskData.Completed.1.Appr1.pmm.d.rf$pred$mtry == 10, ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.d.rf$pred[RiskData.Completed.1.Appr1.pmm.d.rf$pred$mtry == 10, ]$Default)

RiskData.Completed.1.Appr1.pmm.d.gbm.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.1.Appr1.pmm.d.gbm$pred[RiskData.Completed.1.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.1.Appr1.pmm.d.gbm$pred[RiskData.Completed.1.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$Default)


RiskData.Completed.1.Appr1.pmm.d.svm.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.d.svm$pred[RiskData.Completed.1.Appr1.pmm.d.svm$pred[round(RiskData.Completed.1.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.d.svm$pred[RiskData.Completed.1.Appr1.pmm.d.svm$pred[round(RiskData.Completed.1.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$Default)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.logit$pred$pred, RiskData.Completed.1.Appr1.pmm.d.logit$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.glmnet$pred$pred, RiskData.Completed.1.Appr1.pmm.d.glmnet$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.lda$pred$pred, RiskData.Completed.1.Appr1.pmm.d.lda$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.stepLDA$pred$pred, RiskData.Completed.1.Appr1.pmm.d.stepLDA$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.knn$pred$pred, RiskData.Completed.1.Appr1.pmm.d.knn$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.pls$pred$pred, RiskData.Completed.1.Appr1.pmm.d.pls$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.gamSpline$pred$pred, RiskData.Completed.1.Appr1.pmm.d.gamSpline$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.rpart$pred$pred, RiskData.Completed.1.Appr1.pmm.d.rpart$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.treebag$pred$pred, RiskData.Completed.1.Appr1.pmm.d.treebag$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.rf$pred$pred, RiskData.Completed.1.Appr1.pmm.d.rf$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.gbm$pred$pred, RiskData.Completed.1.Appr1.pmm.d.gbm$pred$obs)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.d.svm$pred$pred, RiskData.Completed.1.Appr1.pmm.d.svm$pred$obs)


#build to combined ROC plot with resampled ROC curves
plot(RiskData.Completed.1.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(RiskData.Completed.1.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(RiskData.Completed.1.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(RiskData.Completed.1.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(RiskData.Completed.1.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")
plot(RiskData.Completed.1.Appr1.pmm.d.knn.roc, add = TRUE, col = "mediumvioletred")
plot(RiskData.Completed.1.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(RiskData.Completed.1.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(RiskData.Completed.1.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(RiskData.Completed.1.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")
plot(RiskData.Completed.1.Appr1.pmm.d.rf.roc, add = TRUE, col = "lightpink")
plot(RiskData.Completed.1.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(RiskData.Completed.1.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")

###TEST DATA PERFORMANCE

#PREDICT PROBABILITIES ON TEST SET WITH ALL TRAINED MODELS
############################################################################
###EXAMPLE WITH SOME COMMENTS
test.RiskData.Completed.1.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.logit, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.logit.pred.prob
test.RiskData.Completed.1.Appr1.pmm.d.logit.pred.prob[1] #NOT A VECTOR
test.RiskData.Completed.1.Appr1.pmm.d.logit.pred.prob[[1]] #NOW A VECTOR
############################################################################
#pred.prob
##Postive class Default is reference level
test.RiskData.Completed.1.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.logit, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.glmnet.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.lda.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.lda, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.stepLDA.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.qda.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.qda, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.knn.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.knn, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.pls.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.pls, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.rpart.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.treebag.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.rf.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.rf, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.1.Appr1.pmm.d.gbm.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")


test.RiskData.Completed.1.Appr1.pmm.d.svm.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.d.svm, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")

#pred.class
test.Completed.1.Appr1.pmm.d.logit.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.logit, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.glmnet.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.lda.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.lda, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.stepLDA.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.qda.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.qda, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.knn.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.knn, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.pls.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.pls, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.rpart.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.treebag.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.rf.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.rf, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.gbm.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.1.Appr1.pmm.d)

test.Completed.1.Appr1.pmm.d.svm.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.d.svm, 
  test.RiskData.Completed.1.Appr1.pmm.d)

########################################################
## Test Confusion Matrix
confusionMatrix(test.Completed.1.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)  
confusionMatrix(test.Completed.1.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)    
confusionMatrix(test.Completed.1.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)      
confusionMatrix(test.Completed.1.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.1.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)




#calculate TEST Misclassification Error Rate with confusion matrix
RiskData.Completed.1.Appr1.pmm.d.Test.MisClass.Rate = c(
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.1.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.1.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.1.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.1.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( confusionMatrix(test.Completed.1.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.1.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.1.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)





############################################################################

#ROC Curve of Training and Test Performance of Logit Model: RiskData.Completed.1.Appr1.pmm.d.logit
#Postive class Default is reference level
test.RiskData.Completed.1.Appr1.pmm.d.logit.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.logit.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.glmnet.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.glmnet.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.lda.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.lda.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.stepLDA.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.stepLDA.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.qda.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.qda.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.knn.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.knn.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.pls.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.pls.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.gamSpline.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.rpart.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.rpart.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.treebag.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.treebag.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.rf.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.rf.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.gbm.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.gbm.pred.prob[[1]])

test.RiskData.Completed.1.Appr1.pmm.d.svm.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.d.svm.pred.prob[[1]])

#Test ROC Curve Vs. Training ROC Curve for Each Model
plot(test.RiskData.Completed.1.Appr1.pmm.d.logit.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.logit.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Logit", "Train Logit"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.glmnet.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.glmnet.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Lasso/Ridge", "Train Lasso/Ridge"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.lda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.lda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test LDA", "Train LDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.stepLDA.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.stepLDA.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test stepLDA", "Train stepLDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.qda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.qda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test QDA", "Train QDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.knn.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.knn.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test KNN", "Train KNN"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.pls.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.pls.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test PLS", "Train PLS"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.rpart.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.rpart.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Decision Tree Classification", "Train Decision Tree Classification"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.treebag.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.treebag.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Bagging", "Train Bagging"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.rf.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.rf.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test randomForest", "Train randomForest"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.1.Appr1.pmm.d.gbm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.gbm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Boosting", "Train Boosting"), 
       col = c("black", "blue"),
       lty = 1)	   


plot(test.RiskData.Completed.1.Appr1.pmm.d.svm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.d.svm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test svm", "Train svm"), 
       col = c("black", "blue"),
       lty = 1)	   

#test performance slightly lower than resample
test.RiskData.Completed.1.Appr1.pmm.d.logit.roc$auc
RiskData.Completed.1.Appr1.pmm.d.logit.roc$auc

plot(test.RiskData.Completed.1.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(test.RiskData.Completed.1.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(test.RiskData.Completed.1.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(test.RiskData.Completed.1.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(test.RiskData.Completed.1.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")

plot(test.RiskData.Completed.1.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(test.RiskData.Completed.1.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(test.RiskData.Completed.1.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(test.RiskData.Completed.1.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(test.RiskData.Completed.1.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")

plot(test.RiskData.Completed.1.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(test.RiskData.Completed.1.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(test.RiskData.Completed.1.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")





###### APPROACH 1 COMPLETED 2
### For modeling, we again join the RiskFlag to the dataset for model training
RiskData.Completed.2.Appr1.pmm <- RiskData.Y.rm.Completed.2.Appr1.pmm
RiskData.Completed.2.Appr1.pmm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.2.Appr1.pmm.d.model <- dummyVars(~ . , data = RiskData.Completed.2.Appr1.pmm, fullRank = TRUE)
RiskData.Completed.2.Appr1.pmm.d <- as.data.frame(predict(RiskData.Completed.2.Appr1.pmm.d.model, RiskData.Completed.2.Appr1.pmm))
for(i in 1:nrow(RiskData.Completed.2.Appr1.pmm.d)){
  RiskData.Completed.2.Appr1.pmm.d$Risk_Flag[i] <- if_else(RiskData.Completed.2.Appr1.pmm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.2.Appr1.pmm.d$Risk_Flag <- as.factor(RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
str(RiskData.Completed.2.Appr1.pmm.d)

train.RiskData.Completed.2.Appr1.pmm.d <- RiskData.Completed.2.Appr1.pmm.d[trainIndex,]
test.RiskData.Completed.2.Appr1.pmm.d <- RiskData.Completed.2.Appr1.pmm.d[-trainIndex,]

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.logit <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.2.Appr1.pmm.d[trainIndex,],
                                                 method = "glm", 
                                                 family = "binomial", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"))
RiskData.Completed.2.Appr1.pmm.d.logit

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.glmnet <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                  weights = Sampling_Weight, 
                                                  data = RiskData.Completed.2.Appr1.pmm.d[trainIndex,],
                                                  method = "glmnet",
                                                  metric = "ROC", 
                                                  trControl = control, 
                                                  preProcess = c("scale","center"),
                                                  tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.glmnet

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.lda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                               method = "lda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.2.Appr1.pmm.d.lda

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.stepLDA <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                                   method = "stepLDA", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"),
                                                   tuneLength = 10)											   
RiskData.Completed.2.Appr1.pmm.d.stepLDA

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.qda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                               method = "qda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.2.Appr1.pmm.d.qda

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.knn <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                               method = "knn", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.knn

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.pls <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                               method = "pls", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.pls

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                     weights = Sampling_Weight, 
                                                     data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                                     method = "gamSpline", 
                                                     metric = "ROC", 
                                                     trControl = control, 
                                                     preProcess = c("scale","center"),
                                                     tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.gamSpline

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.rpart <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                                 method = "rpart", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"),
                                                 tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.rpart

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.treebag <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                                   method = "treebag", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"))
RiskData.Completed.2.Appr1.pmm.d.treebag

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.rf <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                              weights = Sampling_Weight, 
                                              data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                              method = "rf", 
                                              metric = "ROC", 
                                              trControl = control, 
                                              preProcess = c("scale","center"),
                                              tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.rf

set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.gbm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                               method = "gbm", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.gbm


##svm
set.seed(749)
RiskData.Completed.2.Appr1.pmm.d.svm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.2.Appr1.pmm.d[trainIndex, ], 
                                               method = "svmRadial", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.d.svm


confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.logit$pred$pred, RiskData.Completed.2.Appr1.pmm.d.logit$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.glmnet$pred$pred, RiskData.Completed.2.Appr1.pmm.d.glmnet$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.lda$pred$pred, RiskData.Completed.2.Appr1.pmm.d.lda$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.stepLDA$pred$pred, RiskData.Completed.2.Appr1.pmm.d.stepLDA$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.knn$pred$pred, RiskData.Completed.2.Appr1.pmm.d.knn$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.pls$pred$pred, RiskData.Completed.2.Appr1.pmm.d.pls$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.gamSpline$pred$pred, RiskData.Completed.2.Appr1.pmm.d.gamSpline$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.rpart$pred$pred, RiskData.Completed.2.Appr1.pmm.d.rpart$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.treebag$pred$pred, RiskData.Completed.2.Appr1.pmm.d.treebag$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.rf$pred$pred, RiskData.Completed.2.Appr1.pmm.d.rf$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.gbm$pred$pred, RiskData.Completed.2.Appr1.pmm.d.gbm$pred$obs)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.d.svm$pred$pred, RiskData.Completed.2.Appr1.pmm.d.svm$pred$obs)


RiskData.Completed.2.Appr1.pmm.d.list.models <- list("logit" = RiskData.Completed.2.Appr1.pmm.d.logit,
                                                     "lasso/ridge" = RiskData.Completed.2.Appr1.pmm.d.glmnet, 
                                                     "lda" = RiskData.Completed.2.Appr1.pmm.d.lda,
                                                     "stepLDA" = RiskData.Completed.2.Appr1.pmm.d.stepLDA,
                                                     "qda" = RiskData.Completed.2.Appr1.pmm.d.qda,
                                                     "knn" = RiskData.Completed.2.Appr1.pmm.d.knn,
                                                     "pls" = RiskData.Completed.2.Appr1.pmm.d.pls,
                                                     "gamSpline" = RiskData.Completed.2.Appr1.pmm.d.gamSpline,
                                                     "tree" = RiskData.Completed.2.Appr1.pmm.d.rpart,
                                                     "bagging" = RiskData.Completed.2.Appr1.pmm.d.treebag,
                                                     "randomForest" = RiskData.Completed.2.Appr1.pmm.d.rf,
                                                     "boosting" = RiskData.Completed.2.Appr1.pmm.d.gbm,
                                                     "svm" = RiskData.Completed.2.Appr1.pmm.d.svm)

RiskData.Completed.2.Appr1.pmm.d.list.resamples = resamples(RiskData.Completed.2.Appr1.pmm.d.list.models)


#plot performance comparisons
bwplot(RiskData.Completed.2.Appr1.pmm.d.list.resamples, metric="ROC") 
bwplot(RiskData.Completed.2.Appr1.pmm.d.list.resamples, metric="Sens")
bwplot(RiskData.Completed.2.Appr1.pmm.d.list.resamples, metric="Spec") 

##calculate ROC curves on resampled data
RiskData.Completed.2.Appr1.pmm.d.logit.roc<- roc(response = RiskData.Completed.2.Appr1.pmm.d.logit$pred$obs, 
                                                 predictor = RiskData.Completed.2.Appr1.pmm.d.logit$pred$Default)

RiskData.Completed.2.Appr1.pmm.d.lda.roc<- roc(response = RiskData.Completed.2.Appr1.pmm.d.lda$pred$obs, 
                                               predictor = RiskData.Completed.2.Appr1.pmm.d.lda$pred$Default)

RiskData.Completed.2.Appr1.pmm.d.qda.roc<- roc(response = RiskData.Completed.2.Appr1.pmm.d.qda$pred$obs, 
                                               predictor = RiskData.Completed.2.Appr1.pmm.d.qda$pred$Default)

RiskData.Completed.2.Appr1.pmm.d.treebag.roc<- roc(response = RiskData.Completed.2.Appr1.pmm.d.treebag$pred$obs, 
                                                   predictor = RiskData.Completed.2.Appr1.pmm.d.treebag$pred$Default)

#WHEN MODEL HAS PARAMETERS, MAKE SURE TO SELECT FINAL/OPTIMAL PARAMETER VALUE. EXAMPLE:
RiskData.Completed.2.Appr1.pmm.d.glmnet.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.2.Appr1.pmm.d.glmnet$pred[RiskData.Completed.2.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.2.Appr1.pmm.d.glmnet$pred[RiskData.Completed.2.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$Default)

RiskData.Completed.2.Appr1.pmm.d.stepLDA.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.stepLDA$pred$obs, 
  predictor = RiskData.Completed.2.Appr1.pmm.d.stepLDA$pred$Default)

RiskData.Completed.2.Appr1.pmm.d.knn.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.knn$pred[RiskData.Completed.2.Appr1.pmm.d.knn$pred$k == 23,]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.d.knn$pred[RiskData.Completed.2.Appr1.pmm.d.knn$pred$k == 23,]$Default) 

RiskData.Completed.2.Appr1.pmm.d.pls.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.pls$pred[RiskData.Completed.2.Appr1.pmm.d.pls$pred$ncomp == 7,]$obs, 
  predictor = RiskData.Completed.2.Appr1.pmm.d.pls$pred[RiskData.Completed.2.Appr1.pmm.d.pls$pred$ncomp == 7,]$Default)

RiskData.Completed.2.Appr1.pmm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.2.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.2.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

RiskData.Completed.2.Appr1.pmm.d.rpart.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.2.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.2.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$Default)

RiskData.Completed.2.Appr1.pmm.d.rf.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.rf$pred[RiskData.Completed.2.Appr1.pmm.d.rf$pred$mtry == 10, ]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.d.rf$pred[RiskData.Completed.2.Appr1.pmm.d.rf$pred$mtry == 10, ]$Default)

RiskData.Completed.2.Appr1.pmm.d.gbm.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.2.Appr1.pmm.d.gbm$pred[RiskData.Completed.2.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.2.Appr1.pmm.d.gbm$pred[RiskData.Completed.2.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$Default)


RiskData.Completed.2.Appr1.pmm.d.svm.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.d.svm$pred[RiskData.Completed.2.Appr1.pmm.d.svm$pred[round(RiskData.Completed.2.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01608428, ]$C == 0.5, ]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.d.svm$pred[RiskData.Completed.2.Appr1.pmm.d.svm$pred[round(RiskData.Completed.2.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01608428, ]$C == 0.5, ]$Default)


#build to combined ROC plot with resampled ROC curves
plot(RiskData.Completed.2.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(RiskData.Completed.2.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(RiskData.Completed.2.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(RiskData.Completed.2.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(RiskData.Completed.2.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")
plot(RiskData.Completed.2.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(RiskData.Completed.2.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(RiskData.Completed.2.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(RiskData.Completed.2.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(RiskData.Completed.2.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")
plot(RiskData.Completed.2.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(RiskData.Completed.2.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(RiskData.Completed.2.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")

###TEST DATA PERFORMANCE

#PREDICT PROBABILITIES ON TEST SET WITH ALL TRAINED MODELS
############################################################################
###EXAMPLE WITH SOME COMMENTS
test.RiskData.Completed.2.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.logit, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.logit.pred.prob
test.RiskData.Completed.2.Appr1.pmm.d.logit.pred.prob[1] #NOT A VECTOR
test.RiskData.Completed.2.Appr1.pmm.d.logit.pred.prob[[1]] #NOW A VECTOR
############################################################################
#pred.prob
##Postive class Default is reference level
test.RiskData.Completed.2.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.logit, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.glmnet.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.lda.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.lda, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.stepLDA.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.qda.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.qda, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.knn.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.knn, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.pls.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.pls, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.rpart.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.treebag.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.rf.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.rf, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.2.Appr1.pmm.d.gbm.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")


test.RiskData.Completed.2.Appr1.pmm.d.svm.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.d.svm, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")

#pred.class
test.Completed.2.Appr1.pmm.d.logit.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.logit, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.glmnet.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.lda.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.lda, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.stepLDA.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.qda.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.qda, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.knn.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.knn, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.pls.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.pls, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.rpart.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.treebag.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.rf.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.rf, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.gbm.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.2.Appr1.pmm.d)

test.Completed.2.Appr1.pmm.d.svm.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.d.svm, 
  test.RiskData.Completed.2.Appr1.pmm.d)

#######################################################
## Test Confusion Matrix
confusionMatrix(test.Completed.2.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)  
confusionMatrix(test.Completed.2.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)    
confusionMatrix(test.Completed.2.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)      
confusionMatrix(test.Completed.2.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.2.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)


#calculate TEST Misclassification Error Rate with confusion matrix
RiskData.Completed.2.Appr1.pmm.d.Test.MisClass.Rate = c(
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.2.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.2.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.2.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.2.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( confusionMatrix(test.Completed.2.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.2.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.2.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.2.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.2.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.2.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)





############################################################################

#ROC Curve of Training and Test Performance of Logit Model: RiskData.Completed.2.Appr1.pmm.d.logit
#Postive class Default is reference level
test.RiskData.Completed.2.Appr1.pmm.d.logit.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.logit.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.glmnet.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.glmnet.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.lda.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.lda.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.stepLDA.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.stepLDA.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.qda.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.qda.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.knn.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.knn.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.pls.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.pls.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.gamSpline.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.rpart.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.rpart.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.treebag.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.treebag.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.rf.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.rf.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.gbm.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.gbm.pred.prob[[1]])

test.RiskData.Completed.2.Appr1.pmm.d.svm.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.d.svm.pred.prob[[1]])

#Test ROC Curve Vs. Training ROC Curve for Each Model
plot(test.RiskData.Completed.2.Appr1.pmm.d.logit.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.logit.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Logit", "Train Logit"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.glmnet.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.glmnet.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Lasso/Ridge", "Train Lasso/Ridge"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.lda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.lda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test LDA", "Train LDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.stepLDA.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.stepLDA.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test stepLDA", "Train stepLDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.qda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.qda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test QDA", "Train QDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.knn.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.knn.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test KNN", "Train KNN"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.pls.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.pls.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test PLS", "Train PLS"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.rpart.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.rpart.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Decision Tree Classification", "Train Decision Tree Classification"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.treebag.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.treebag.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Bagging", "Train Bagging"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.rf.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.rf.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test randomForest", "Train randomForest"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.2.Appr1.pmm.d.gbm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.gbm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Boosting", "Train Boosting"), 
       col = c("black", "blue"),
       lty = 1)	   


plot(test.RiskData.Completed.2.Appr1.pmm.d.svm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.d.svm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test svm", "Train svm"), 
       col = c("black", "blue"),
       lty = 1)	   

#test performance slightly lower than resample
test.RiskData.Completed.2.Appr1.pmm.d.logit.roc$auc
RiskData.Completed.2.Appr1.pmm.d.logit.roc$auc

plot(test.RiskData.Completed.2.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(test.RiskData.Completed.2.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(test.RiskData.Completed.2.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(test.RiskData.Completed.2.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(test.RiskData.Completed.2.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")

plot(test.RiskData.Completed.2.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(test.RiskData.Completed.2.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(test.RiskData.Completed.2.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(test.RiskData.Completed.2.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(test.RiskData.Completed.2.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")

plot(test.RiskData.Completed.2.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(test.RiskData.Completed.2.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(test.RiskData.Completed.2.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")




###### APPROACH 1 COMPLETED 3
### For modeling, we again join the RiskFlag to the dataset for model training
RiskData.Completed.3.Appr1.pmm <- RiskData.Y.rm.Completed.3.Appr1.pmm
RiskData.Completed.3.Appr1.pmm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.3.Appr1.pmm.d.model <- dummyVars(~ . , data = RiskData.Completed.3.Appr1.pmm, fullRank = TRUE)
RiskData.Completed.3.Appr1.pmm.d <- as.data.frame(predict(RiskData.Completed.3.Appr1.pmm.d.model, RiskData.Completed.3.Appr1.pmm))
for(i in 1:nrow(RiskData.Completed.3.Appr1.pmm.d)){
  RiskData.Completed.3.Appr1.pmm.d$Risk_Flag[i] <- if_else(RiskData.Completed.3.Appr1.pmm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.3.Appr1.pmm.d$Risk_Flag <- as.factor(RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
str(RiskData.Completed.3.Appr1.pmm.d)

train.RiskData.Completed.3.Appr1.pmm.d <- RiskData.Completed.3.Appr1.pmm.d[trainIndex,]
test.RiskData.Completed.3.Appr1.pmm.d <- RiskData.Completed.3.Appr1.pmm.d[-trainIndex,]

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.logit <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.3.Appr1.pmm.d[trainIndex,],
                                                 method = "glm", 
                                                 family = "binomial", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"))
RiskData.Completed.3.Appr1.pmm.d.logit

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.glmnet <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                  weights = Sampling_Weight, 
                                                  data = RiskData.Completed.3.Appr1.pmm.d[trainIndex,],
                                                  method = "glmnet",
                                                  metric = "ROC", 
                                                  trControl = control, 
                                                  preProcess = c("scale","center"),
                                                  tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.glmnet

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.lda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                               method = "lda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.3.Appr1.pmm.d.lda

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.stepLDA <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                                   method = "stepLDA", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"),
                                                   tuneLength = 10)											   
RiskData.Completed.3.Appr1.pmm.d.stepLDA

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.qda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                               method = "qda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.3.Appr1.pmm.d.qda

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.knn <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                               method = "knn", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.knn

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.pls <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                               method = "pls", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.pls

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                     weights = Sampling_Weight, 
                                                     data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                                     method = "gamSpline", 
                                                     metric = "ROC", 
                                                     trControl = control, 
                                                     preProcess = c("scale","center"),
                                                     tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.gamSpline

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.rpart <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                                 method = "rpart", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"),
                                                 tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.rpart

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.treebag <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                                   method = "treebag", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"))
RiskData.Completed.3.Appr1.pmm.d.treebag

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.rf <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                              weights = Sampling_Weight, 
                                              data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                              method = "rf", 
                                              metric = "ROC", 
                                              trControl = control, 
                                              preProcess = c("scale","center"),
                                              tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.rf

set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.gbm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                               method = "gbm", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.gbm


##svm
set.seed(749)
RiskData.Completed.3.Appr1.pmm.d.svm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.3.Appr1.pmm.d[trainIndex, ], 
                                               method = "svmRadial", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.d.svm

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.logit$pred$pred, RiskData.Completed.3.Appr1.pmm.d.logit$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.glmnet$pred$pred, RiskData.Completed.3.Appr1.pmm.d.glmnet$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.lda$pred$pred, RiskData.Completed.3.Appr1.pmm.d.lda$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.stepLDA$pred$pred, RiskData.Completed.3.Appr1.pmm.d.stepLDA$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.knn$pred$pred, RiskData.Completed.3.Appr1.pmm.d.knn$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.pls$pred$pred, RiskData.Completed.3.Appr1.pmm.d.pls$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.gamSpline$pred$pred, RiskData.Completed.3.Appr1.pmm.d.gamSpline$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.rpart$pred$pred, RiskData.Completed.3.Appr1.pmm.d.rpart$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.treebag$pred$pred, RiskData.Completed.3.Appr1.pmm.d.treebag$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.rf$pred$pred, RiskData.Completed.3.Appr1.pmm.d.rf$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.gbm$pred$pred, RiskData.Completed.3.Appr1.pmm.d.gbm$pred$obs)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.d.svm$pred$pred, RiskData.Completed.3.Appr1.pmm.d.svm$pred$obs)

RiskData.Completed.3.Appr1.pmm.d.list.models <- list("logit" = RiskData.Completed.3.Appr1.pmm.d.logit,
                                                     "lasso/ridge" = RiskData.Completed.3.Appr1.pmm.d.glmnet, 
                                                     "lda" = RiskData.Completed.3.Appr1.pmm.d.lda,
                                                     "stepLDA" = RiskData.Completed.3.Appr1.pmm.d.stepLDA,
                                                     "qda" = RiskData.Completed.3.Appr1.pmm.d.qda,
                                                     "knn" = RiskData.Completed.3.Appr1.pmm.d.knn,
                                                     "pls" = RiskData.Completed.3.Appr1.pmm.d.pls,
                                                     "gamSpline" = RiskData.Completed.3.Appr1.pmm.d.gamSpline,
                                                     "tree" = RiskData.Completed.3.Appr1.pmm.d.rpart,
                                                     "bagging" = RiskData.Completed.3.Appr1.pmm.d.treebag,
                                                     "randomForest" = RiskData.Completed.3.Appr1.pmm.d.rf,
                                                     "boosting" = RiskData.Completed.3.Appr1.pmm.d.gbm,
                                                     "svm" = RiskData.Completed.3.Appr1.pmm.d.svm)

RiskData.Completed.3.Appr1.pmm.d.list.resamples = resamples(RiskData.Completed.3.Appr1.pmm.d.list.models)


#plot performance comparisons
bwplot(RiskData.Completed.3.Appr1.pmm.d.list.resamples, metric="ROC") 
bwplot(RiskData.Completed.3.Appr1.pmm.d.list.resamples, metric="Sens")
bwplot(RiskData.Completed.3.Appr1.pmm.d.list.resamples, metric="Spec") 

##calculate ROC curves on resampled data
RiskData.Completed.3.Appr1.pmm.d.logit.roc<- roc(response = RiskData.Completed.3.Appr1.pmm.d.logit$pred$obs, 
                                                 predictor = RiskData.Completed.3.Appr1.pmm.d.logit$pred$Default)

RiskData.Completed.3.Appr1.pmm.d.lda.roc<- roc(response = RiskData.Completed.3.Appr1.pmm.d.lda$pred$obs, 
                                               predictor = RiskData.Completed.3.Appr1.pmm.d.lda$pred$Default)

RiskData.Completed.3.Appr1.pmm.d.qda.roc<- roc(response = RiskData.Completed.3.Appr1.pmm.d.qda$pred$obs, 
                                               predictor = RiskData.Completed.3.Appr1.pmm.d.qda$pred$Default)

RiskData.Completed.3.Appr1.pmm.d.treebag.roc<- roc(response = RiskData.Completed.3.Appr1.pmm.d.treebag$pred$obs, 
                                                   predictor = RiskData.Completed.3.Appr1.pmm.d.treebag$pred$Default)

#WHEN MODEL HAS PARAMETERS, MAKE SURE TO SELECT FINAL/OPTIMAL PARAMETER VALUE. EXAMPLE:
RiskData.Completed.3.Appr1.pmm.d.glmnet.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.3.Appr1.pmm.d.glmnet$pred[RiskData.Completed.3.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.3.Appr1.pmm.d.glmnet$pred[RiskData.Completed.3.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$Default)

RiskData.Completed.3.Appr1.pmm.d.stepLDA.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.stepLDA$pred$obs, 
  predictor = RiskData.Completed.3.Appr1.pmm.d.stepLDA$pred$Default)

RiskData.Completed.3.Appr1.pmm.d.knn.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.knn$pred[RiskData.Completed.3.Appr1.pmm.d.knn$pred$k == 23,]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.d.knn$pred[RiskData.Completed.3.Appr1.pmm.d.knn$pred$k == 23,]$Default) 

RiskData.Completed.3.Appr1.pmm.d.pls.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.pls$pred[RiskData.Completed.3.Appr1.pmm.d.pls$pred$ncomp == 7,]$obs, 
  predictor = RiskData.Completed.3.Appr1.pmm.d.pls$pred[RiskData.Completed.3.Appr1.pmm.d.pls$pred$ncomp == 7,]$Default)

RiskData.Completed.3.Appr1.pmm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.3.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.3.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

RiskData.Completed.3.Appr1.pmm.d.rpart.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.3.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.3.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$Default)

RiskData.Completed.3.Appr1.pmm.d.rf.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.rf$pred[RiskData.Completed.3.Appr1.pmm.d.rf$pred$mtry == 10, ]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.d.rf$pred[RiskData.Completed.3.Appr1.pmm.d.rf$pred$mtry == 10, ]$Default)

RiskData.Completed.3.Appr1.pmm.d.gbm.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.3.Appr1.pmm.d.gbm$pred[RiskData.Completed.3.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.3.Appr1.pmm.d.gbm$pred[RiskData.Completed.3.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$Default)


RiskData.Completed.3.Appr1.pmm.d.svm.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.d.svm$pred[RiskData.Completed.3.Appr1.pmm.d.svm$pred[round(RiskData.Completed.3.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.d.svm$pred[RiskData.Completed.3.Appr1.pmm.d.svm$pred[round(RiskData.Completed.3.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$Default)


#build to combined ROC plot with resampled ROC curves
plot(RiskData.Completed.3.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(RiskData.Completed.3.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(RiskData.Completed.3.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(RiskData.Completed.3.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(RiskData.Completed.3.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")
plot(RiskData.Completed.3.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(RiskData.Completed.3.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(RiskData.Completed.3.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(RiskData.Completed.3.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(RiskData.Completed.3.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")
plot(RiskData.Completed.3.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(RiskData.Completed.3.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(RiskData.Completed.3.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")

###TEST DATA PERFORMANCE

#PREDICT PROBABILITIES ON TEST SET WITH ALL TRAINED MODELS
############################################################################
###EXAMPLE WITH SOME COMMENTS
test.RiskData.Completed.3.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.logit, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.logit.pred.prob
test.RiskData.Completed.3.Appr1.pmm.d.logit.pred.prob[1] #NOT A VECTOR
test.RiskData.Completed.3.Appr1.pmm.d.logit.pred.prob[[1]] #NOW A VECTOR
############################################################################
#pred.prob
##Postive class Default is reference level
test.RiskData.Completed.3.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.logit, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.glmnet.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.lda.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.lda, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.stepLDA.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.qda.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.qda, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.knn.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.knn, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.pls.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.pls, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.rpart.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.treebag.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.rf.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.rf, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.3.Appr1.pmm.d.gbm.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")


test.RiskData.Completed.3.Appr1.pmm.d.svm.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.d.svm, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")

#pred.class
test.Completed.3.Appr1.pmm.d.logit.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.logit, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.glmnet.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.lda.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.lda, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.stepLDA.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.qda.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.qda, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.knn.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.knn, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.pls.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.pls, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.rpart.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.treebag.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.rf.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.rf, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.gbm.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.3.Appr1.pmm.d)

test.Completed.3.Appr1.pmm.d.svm.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.d.svm, 
  test.RiskData.Completed.3.Appr1.pmm.d)


#######################################################
## Test Confusion Matrix
confusionMatrix(test.Completed.3.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)  
confusionMatrix(test.Completed.3.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)    
confusionMatrix(test.Completed.3.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)      
confusionMatrix(test.Completed.3.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.3.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)


#calculate TEST Misclassification Error Rate with confusion matrix
RiskData.Completed.3.Appr1.pmm.d.Test.MisClass.Rate = c(
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.3.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.3.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.3.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.3.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( confusionMatrix(test.Completed.3.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.3.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.3.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.3.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.3.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.3.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)





############################################################################

#ROC Curve of Training and Test Performance of Logit Model: RiskData.Completed.3.Appr1.pmm.d.logit
#Postive class Default is reference level
test.RiskData.Completed.3.Appr1.pmm.d.logit.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.logit.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.glmnet.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.glmnet.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.lda.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.lda.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.stepLDA.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.stepLDA.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.qda.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.qda.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.knn.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.knn.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.pls.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.pls.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.gamSpline.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.rpart.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.rpart.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.treebag.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.treebag.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.rf.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.rf.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.gbm.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.gbm.pred.prob[[1]])

test.RiskData.Completed.3.Appr1.pmm.d.svm.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.d.svm.pred.prob[[1]])

#Test ROC Curve Vs. Training ROC Curve for Each Model
plot(test.RiskData.Completed.3.Appr1.pmm.d.logit.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.logit.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Logit", "Train Logit"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.glmnet.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.glmnet.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Lasso/Ridge", "Train Lasso/Ridge"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.lda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.lda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test LDA", "Train LDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.stepLDA.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.stepLDA.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test stepLDA", "Train stepLDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.qda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.qda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test QDA", "Train QDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.knn.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.knn.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test KNN", "Train KNN"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.pls.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.pls.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test PLS", "Train PLS"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.rpart.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.rpart.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Decision Tree Classification", "Train Decision Tree Classification"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.treebag.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.treebag.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Bagging", "Train Bagging"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.rf.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.rf.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test randomForest", "Train randomForest"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.3.Appr1.pmm.d.gbm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.gbm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Boosting", "Train Boosting"), 
       col = c("black", "blue"),
       lty = 1)	   


plot(test.RiskData.Completed.3.Appr1.pmm.d.svm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.d.svm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test svm", "Train svm"), 
       col = c("black", "blue"),
       lty = 1)	   

#test performance slightly lower than resample
test.RiskData.Completed.3.Appr1.pmm.d.logit.roc$auc
RiskData.Completed.3.Appr1.pmm.d.logit.roc$auc

plot(test.RiskData.Completed.3.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(test.RiskData.Completed.3.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(test.RiskData.Completed.3.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(test.RiskData.Completed.3.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(test.RiskData.Completed.3.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")

plot(test.RiskData.Completed.3.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(test.RiskData.Completed.3.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(test.RiskData.Completed.3.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(test.RiskData.Completed.3.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(test.RiskData.Completed.3.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")

plot(test.RiskData.Completed.3.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(test.RiskData.Completed.3.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(test.RiskData.Completed.3.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")





###### APPROACH 1 COMPLETED 4
### For modeling, we again join the RiskFlag to the dataset for model training
RiskData.Completed.4.Appr1.pmm <- RiskData.Y.rm.Completed.4.Appr1.pmm
RiskData.Completed.4.Appr1.pmm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.4.Appr1.pmm.d.model <- dummyVars(~ . , data = RiskData.Completed.4.Appr1.pmm, fullRank = TRUE)
RiskData.Completed.4.Appr1.pmm.d <- as.data.frame(predict(RiskData.Completed.4.Appr1.pmm.d.model, RiskData.Completed.4.Appr1.pmm))
for(i in 1:nrow(RiskData.Completed.4.Appr1.pmm.d)){
  RiskData.Completed.4.Appr1.pmm.d$Risk_Flag[i] <- if_else(RiskData.Completed.4.Appr1.pmm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.4.Appr1.pmm.d$Risk_Flag <- as.factor(RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
str(RiskData.Completed.4.Appr1.pmm.d)

train.RiskData.Completed.4.Appr1.pmm.d <- RiskData.Completed.4.Appr1.pmm.d[trainIndex,]
test.RiskData.Completed.4.Appr1.pmm.d <- RiskData.Completed.4.Appr1.pmm.d[-trainIndex,]

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.logit <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.4.Appr1.pmm.d[trainIndex,],
                                                 method = "glm", 
                                                 family = "binomial", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"))
RiskData.Completed.4.Appr1.pmm.d.logit

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.glmnet <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                  weights = Sampling_Weight, 
                                                  data = RiskData.Completed.4.Appr1.pmm.d[trainIndex,],
                                                  method = "glmnet",
                                                  metric = "ROC", 
                                                  trControl = control, 
                                                  preProcess = c("scale","center"),
                                                  tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.glmnet

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.lda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                               method = "lda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.4.Appr1.pmm.d.lda

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.stepLDA <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                                   method = "stepLDA", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"),
                                                   tuneLength = 10)											   
RiskData.Completed.4.Appr1.pmm.d.stepLDA

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.qda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                               method = "qda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.4.Appr1.pmm.d.qda

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.knn <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                               method = "knn", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.knn

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.pls <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                               method = "pls", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.pls

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                     weights = Sampling_Weight, 
                                                     data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                                     method = "gamSpline", 
                                                     metric = "ROC", 
                                                     trControl = control, 
                                                     preProcess = c("scale","center"),
                                                     tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.gamSpline

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.rpart <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                                 method = "rpart", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"),
                                                 tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.rpart

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.treebag <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                                   method = "treebag", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"))
RiskData.Completed.4.Appr1.pmm.d.treebag

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.rf <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                              weights = Sampling_Weight, 
                                              data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                              method = "rf", 
                                              metric = "ROC", 
                                              trControl = control, 
                                              preProcess = c("scale","center"),
                                              tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.rf

set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.gbm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                               method = "gbm", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.gbm


##svm
set.seed(749)
RiskData.Completed.4.Appr1.pmm.d.svm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.4.Appr1.pmm.d[trainIndex, ], 
                                               method = "svmRadial", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.d.svm


confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.logit$pred$pred, RiskData.Completed.4.Appr1.pmm.d.logit$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.glmnet$pred$pred, RiskData.Completed.4.Appr1.pmm.d.glmnet$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.lda$pred$pred, RiskData.Completed.4.Appr1.pmm.d.lda$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.stepLDA$pred$pred, RiskData.Completed.4.Appr1.pmm.d.stepLDA$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.knn$pred$pred, RiskData.Completed.4.Appr1.pmm.d.knn$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.pls$pred$pred, RiskData.Completed.4.Appr1.pmm.d.pls$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.gamSpline$pred$pred, RiskData.Completed.4.Appr1.pmm.d.gamSpline$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.rpart$pred$pred, RiskData.Completed.4.Appr1.pmm.d.rpart$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.treebag$pred$pred, RiskData.Completed.4.Appr1.pmm.d.treebag$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.rf$pred$pred, RiskData.Completed.4.Appr1.pmm.d.rf$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.gbm$pred$pred, RiskData.Completed.4.Appr1.pmm.d.gbm$pred$obs)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.d.svm$pred$pred, RiskData.Completed.4.Appr1.pmm.d.svm$pred$obs)


RiskData.Completed.4.Appr1.pmm.d.list.models <- list("logit" = RiskData.Completed.4.Appr1.pmm.d.logit,
                                                     "lasso/ridge" = RiskData.Completed.4.Appr1.pmm.d.glmnet, 
                                                     "lda" = RiskData.Completed.4.Appr1.pmm.d.lda,
                                                     "stepLDA" = RiskData.Completed.4.Appr1.pmm.d.stepLDA,
                                                     "qda" = RiskData.Completed.4.Appr1.pmm.d.qda,
                                                     "knn" = RiskData.Completed.4.Appr1.pmm.d.knn,
                                                     "pls" = RiskData.Completed.4.Appr1.pmm.d.pls,
                                                     "gamSpline" = RiskData.Completed.4.Appr1.pmm.d.gamSpline,
                                                     "tree" = RiskData.Completed.4.Appr1.pmm.d.rpart,
                                                     "bagging" = RiskData.Completed.4.Appr1.pmm.d.treebag,
                                                     "randomForest" = RiskData.Completed.4.Appr1.pmm.d.rf,
                                                     "boosting" = RiskData.Completed.4.Appr1.pmm.d.gbm,
                                                     "svm" = RiskData.Completed.4.Appr1.pmm.d.svm)

RiskData.Completed.4.Appr1.pmm.d.list.resamples = resamples(RiskData.Completed.4.Appr1.pmm.d.list.models)


#plot performance comparisons
bwplot(RiskData.Completed.4.Appr1.pmm.d.list.resamples, metric="ROC") 
bwplot(RiskData.Completed.4.Appr1.pmm.d.list.resamples, metric="Sens")
bwplot(RiskData.Completed.4.Appr1.pmm.d.list.resamples, metric="Spec") 

##calculate ROC curves on resampled data
RiskData.Completed.4.Appr1.pmm.d.logit.roc<- roc(response = RiskData.Completed.4.Appr1.pmm.d.logit$pred$obs, 
                                                 predictor = RiskData.Completed.4.Appr1.pmm.d.logit$pred$Default)

RiskData.Completed.4.Appr1.pmm.d.lda.roc<- roc(response = RiskData.Completed.4.Appr1.pmm.d.lda$pred$obs, 
                                               predictor = RiskData.Completed.4.Appr1.pmm.d.lda$pred$Default)

RiskData.Completed.4.Appr1.pmm.d.qda.roc<- roc(response = RiskData.Completed.4.Appr1.pmm.d.qda$pred$obs, 
                                               predictor = RiskData.Completed.4.Appr1.pmm.d.qda$pred$Default)

RiskData.Completed.4.Appr1.pmm.d.treebag.roc<- roc(response = RiskData.Completed.4.Appr1.pmm.d.treebag$pred$obs, 
                                                   predictor = RiskData.Completed.4.Appr1.pmm.d.treebag$pred$Default)

#WHEN MODEL HAS PARAMETERS, MAKE SURE TO SELECT FINAL/OPTIMAL PARAMETER VALUE. EXAMPLE:
RiskData.Completed.4.Appr1.pmm.d.glmnet.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.4.Appr1.pmm.d.glmnet$pred[RiskData.Completed.4.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.4.Appr1.pmm.d.glmnet$pred[RiskData.Completed.4.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$Default)

RiskData.Completed.4.Appr1.pmm.d.stepLDA.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.stepLDA$pred$obs, 
  predictor = RiskData.Completed.4.Appr1.pmm.d.stepLDA$pred$Default)

RiskData.Completed.4.Appr1.pmm.d.knn.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.knn$pred[RiskData.Completed.4.Appr1.pmm.d.knn$pred$k == 23,]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.d.knn$pred[RiskData.Completed.4.Appr1.pmm.d.knn$pred$k == 23,]$Default) 

RiskData.Completed.4.Appr1.pmm.d.pls.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.pls$pred[RiskData.Completed.4.Appr1.pmm.d.pls$pred$ncomp == 7,]$obs, 
  predictor = RiskData.Completed.4.Appr1.pmm.d.pls$pred[RiskData.Completed.4.Appr1.pmm.d.pls$pred$ncomp == 7,]$Default)

RiskData.Completed.4.Appr1.pmm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.4.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.4.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

RiskData.Completed.4.Appr1.pmm.d.rpart.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.4.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.4.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$Default)

RiskData.Completed.4.Appr1.pmm.d.rf.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.rf$pred[RiskData.Completed.4.Appr1.pmm.d.rf$pred$mtry == 10, ]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.d.rf$pred[RiskData.Completed.4.Appr1.pmm.d.rf$pred$mtry == 10, ]$Default)

RiskData.Completed.4.Appr1.pmm.d.gbm.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.4.Appr1.pmm.d.gbm$pred[RiskData.Completed.4.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.4.Appr1.pmm.d.gbm$pred[RiskData.Completed.4.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$Default)


RiskData.Completed.4.Appr1.pmm.d.svm.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.d.svm$pred[RiskData.Completed.4.Appr1.pmm.d.svm$pred[round(RiskData.Completed.4.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.d.svm$pred[RiskData.Completed.4.Appr1.pmm.d.svm$pred[round(RiskData.Completed.4.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$Default)


#build to combined ROC plot with resampled ROC curves
plot(RiskData.Completed.4.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(RiskData.Completed.4.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(RiskData.Completed.4.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(RiskData.Completed.4.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(RiskData.Completed.4.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")
plot(RiskData.Completed.4.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(RiskData.Completed.4.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(RiskData.Completed.4.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(RiskData.Completed.4.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(RiskData.Completed.4.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")
plot(RiskData.Completed.4.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(RiskData.Completed.4.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(RiskData.Completed.4.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")

###TEST DATA PERFORMANCE

#PREDICT PROBABILITIES ON TEST SET WITH ALL TRAINED MODELS
############################################################################
###EXAMPLE WITH SOME COMMENTS
test.RiskData.Completed.4.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.logit, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.logit.pred.prob
test.RiskData.Completed.4.Appr1.pmm.d.logit.pred.prob[1] #NOT A VECTOR
test.RiskData.Completed.4.Appr1.pmm.d.logit.pred.prob[[1]] #NOW A VECTOR
############################################################################
#pred.prob
##Postive class Default is reference level
test.RiskData.Completed.4.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.logit, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.glmnet.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.lda.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.lda, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.stepLDA.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.qda.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.qda, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.knn.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.knn, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.pls.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.pls, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.rpart.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.treebag.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.rf.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.rf, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.4.Appr1.pmm.d.gbm.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")


test.RiskData.Completed.4.Appr1.pmm.d.svm.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.d.svm, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")

#pred.class
test.Completed.4.Appr1.pmm.d.logit.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.logit, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.glmnet.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.lda.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.lda, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.stepLDA.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.qda.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.qda, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.knn.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.knn, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.pls.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.pls, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.rpart.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.treebag.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.rf.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.rf, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.gbm.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.4.Appr1.pmm.d)

test.Completed.4.Appr1.pmm.d.svm.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.d.svm, 
  test.RiskData.Completed.4.Appr1.pmm.d)

#######################################################
## Test Confusion Matrix
confusionMatrix(test.Completed.4.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)  
confusionMatrix(test.Completed.4.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)    
confusionMatrix(test.Completed.4.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)      
confusionMatrix(test.Completed.4.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.4.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)



#calculate TEST Misclassification Error Rate with confusion matrix
RiskData.Completed.4.Appr1.pmm.d.Test.MisClass.Rate = c(
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.4.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.4.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.4.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.4.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( confusionMatrix(test.Completed.4.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.4.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.4.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.4.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.4.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.4.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)





############################################################################

#ROC Curve of Training and Test Performance of Logit Model: RiskData.Completed.4.Appr1.pmm.d.logit
#Postive class Default is reference level
test.RiskData.Completed.4.Appr1.pmm.d.logit.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.logit.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.glmnet.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.glmnet.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.lda.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.lda.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.stepLDA.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.stepLDA.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.qda.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.qda.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.knn.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.knn.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.pls.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.pls.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.gamSpline.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.rpart.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.rpart.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.treebag.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.treebag.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.rf.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.rf.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.gbm.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.gbm.pred.prob[[1]])

test.RiskData.Completed.4.Appr1.pmm.d.svm.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.d.svm.pred.prob[[1]])

#Test ROC Curve Vs. Training ROC Curve for Each Model
plot(test.RiskData.Completed.4.Appr1.pmm.d.logit.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.logit.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Logit", "Train Logit"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.glmnet.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.glmnet.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Lasso/Ridge", "Train Lasso/Ridge"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.lda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.lda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test LDA", "Train LDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.stepLDA.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.stepLDA.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test stepLDA", "Train stepLDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.qda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.qda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test QDA", "Train QDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.knn.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.knn.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test KNN", "Train KNN"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.pls.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.pls.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test PLS", "Train PLS"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.rpart.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.rpart.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Decision Tree Classification", "Train Decision Tree Classification"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.treebag.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.treebag.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Bagging", "Train Bagging"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.rf.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.rf.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test randomForest", "Train randomForest"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.4.Appr1.pmm.d.gbm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.gbm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Boosting", "Train Boosting"), 
       col = c("black", "blue"),
       lty = 1)	   


plot(test.RiskData.Completed.4.Appr1.pmm.d.svm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.d.svm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test svm", "Train svm"), 
       col = c("black", "blue"),
       lty = 1)	   

#test performance slightly lower than resample
test.RiskData.Completed.4.Appr1.pmm.d.logit.roc$auc
RiskData.Completed.4.Appr1.pmm.d.logit.roc$auc

plot(test.RiskData.Completed.4.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(test.RiskData.Completed.4.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(test.RiskData.Completed.4.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(test.RiskData.Completed.4.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(test.RiskData.Completed.4.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")

plot(test.RiskData.Completed.4.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(test.RiskData.Completed.4.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(test.RiskData.Completed.4.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(test.RiskData.Completed.4.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(test.RiskData.Completed.4.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")

plot(test.RiskData.Completed.4.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(test.RiskData.Completed.4.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(test.RiskData.Completed.4.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")




###### APPROACH 1 COMPLETED 5
### For modeling, we again join the RiskFlag to the dataset for model training
RiskData.Completed.5.Appr1.pmm <- RiskData.Y.rm.Completed.5.Appr1.pmm
RiskData.Completed.5.Appr1.pmm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.5.Appr1.pmm.d.model <- dummyVars(~ . , data = RiskData.Completed.5.Appr1.pmm, fullRank = TRUE)
RiskData.Completed.5.Appr1.pmm.d <- as.data.frame(predict(RiskData.Completed.5.Appr1.pmm.d.model, RiskData.Completed.5.Appr1.pmm))
for(i in 1:nrow(RiskData.Completed.5.Appr1.pmm.d)){
  RiskData.Completed.5.Appr1.pmm.d$Risk_Flag[i] <- if_else(RiskData.Completed.5.Appr1.pmm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.5.Appr1.pmm.d$Risk_Flag <- as.factor(RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
str(RiskData.Completed.5.Appr1.pmm.d)

train.RiskData.Completed.5.Appr1.pmm.d <- RiskData.Completed.5.Appr1.pmm.d[trainIndex,]
test.RiskData.Completed.5.Appr1.pmm.d <- RiskData.Completed.5.Appr1.pmm.d[-trainIndex,]

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.logit <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.5.Appr1.pmm.d[trainIndex,],
                                                 method = "glm", 
                                                 family = "binomial", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"))
RiskData.Completed.5.Appr1.pmm.d.logit

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.glmnet <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                  weights = Sampling_Weight, 
                                                  data = RiskData.Completed.5.Appr1.pmm.d[trainIndex,],
                                                  method = "glmnet",
                                                  metric = "ROC", 
                                                  trControl = control, 
                                                  preProcess = c("scale","center"),
                                                  tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.glmnet

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.lda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                               method = "lda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.5.Appr1.pmm.d.lda

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.stepLDA <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                                   method = "stepLDA", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"),
                                                   tuneLength = 10)											   
RiskData.Completed.5.Appr1.pmm.d.stepLDA

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.qda <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                               method = "qda", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"))
RiskData.Completed.5.Appr1.pmm.d.qda

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.knn <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                               method = "knn", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.knn

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.pls <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                               method = "pls", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.pls

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                     weights = Sampling_Weight, 
                                                     data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                                     method = "gamSpline", 
                                                     metric = "ROC", 
                                                     trControl = control, 
                                                     preProcess = c("scale","center"),
                                                     tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.gamSpline

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.rpart <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                 weights = Sampling_Weight, 
                                                 data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                                 method = "rpart", 
                                                 metric = "ROC", 
                                                 trControl = control, 
                                                 preProcess = c("scale","center"),
                                                 tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.rpart

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.treebag <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                   weights = Sampling_Weight, 
                                                   data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                                   method = "treebag", 
                                                   metric = "ROC", 
                                                   trControl = control, 
                                                   preProcess = c("scale","center"))
RiskData.Completed.5.Appr1.pmm.d.treebag

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.rf <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                              weights = Sampling_Weight, 
                                              data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                              method = "rf", 
                                              metric = "ROC", 
                                              trControl = control, 
                                              preProcess = c("scale","center"),
                                              tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.rf

set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.gbm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                               method = "gbm", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.gbm


##svm
set.seed(749)
RiskData.Completed.5.Appr1.pmm.d.svm <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                               weights = Sampling_Weight, 
                                               data = RiskData.Completed.5.Appr1.pmm.d[trainIndex, ], 
                                               method = "svmRadial", 
                                               metric = "ROC", 
                                               trControl = control, 
                                               preProcess = c("scale","center"),
                                               tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.d.svm


confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.logit$pred$pred, RiskData.Completed.5.Appr1.pmm.d.logit$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.glmnet$pred$pred, RiskData.Completed.5.Appr1.pmm.d.glmnet$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.lda$pred$pred, RiskData.Completed.5.Appr1.pmm.d.lda$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.stepLDA$pred$pred, RiskData.Completed.5.Appr1.pmm.d.stepLDA$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.knn$pred$pred, RiskData.Completed.5.Appr1.pmm.d.knn$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.pls$pred$pred, RiskData.Completed.5.Appr1.pmm.d.pls$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.gamSpline$pred$pred, RiskData.Completed.5.Appr1.pmm.d.gamSpline$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.rpart$pred$pred, RiskData.Completed.5.Appr1.pmm.d.rpart$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.treebag$pred$pred, RiskData.Completed.5.Appr1.pmm.d.treebag$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.rf$pred$pred, RiskData.Completed.5.Appr1.pmm.d.rf$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.gbm$pred$pred, RiskData.Completed.5.Appr1.pmm.d.gbm$pred$obs)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.d.svm$pred$pred, RiskData.Completed.5.Appr1.pmm.d.svm$pred$obs)



RiskData.Completed.5.Appr1.pmm.d.list.models <- list("logit" = RiskData.Completed.5.Appr1.pmm.d.logit,
                                                     "lasso/ridge" = RiskData.Completed.5.Appr1.pmm.d.glmnet, 
                                                     "lda" = RiskData.Completed.5.Appr1.pmm.d.lda,
                                                     "stepLDA" = RiskData.Completed.5.Appr1.pmm.d.stepLDA,
                                                     "qda" = RiskData.Completed.5.Appr1.pmm.d.qda,
                                                     "knn" = RiskData.Completed.5.Appr1.pmm.d.knn,
                                                     "pls" = RiskData.Completed.5.Appr1.pmm.d.pls,
                                                     "gamSpline" = RiskData.Completed.5.Appr1.pmm.d.gamSpline,
                                                     "tree" = RiskData.Completed.5.Appr1.pmm.d.rpart,
                                                     "bagging" = RiskData.Completed.5.Appr1.pmm.d.treebag,
                                                     "randomForest" = RiskData.Completed.5.Appr1.pmm.d.rf,
                                                     "boosting" = RiskData.Completed.5.Appr1.pmm.d.gbm,
                                                     "svm" = RiskData.Completed.5.Appr1.pmm.d.svm)

RiskData.Completed.5.Appr1.pmm.d.list.resamples = resamples(RiskData.Completed.5.Appr1.pmm.d.list.models)


#plot performance comparisons
bwplot(RiskData.Completed.5.Appr1.pmm.d.list.resamples, metric="ROC") 
bwplot(RiskData.Completed.5.Appr1.pmm.d.list.resamples, metric="Sens")
bwplot(RiskData.Completed.5.Appr1.pmm.d.list.resamples, metric="Spec") 

##calculate ROC curves on resampled data
RiskData.Completed.5.Appr1.pmm.d.logit.roc<- roc(response = RiskData.Completed.5.Appr1.pmm.d.logit$pred$obs, 
                                                 predictor = RiskData.Completed.5.Appr1.pmm.d.logit$pred$Default)

RiskData.Completed.5.Appr1.pmm.d.lda.roc<- roc(response = RiskData.Completed.5.Appr1.pmm.d.lda$pred$obs, 
                                               predictor = RiskData.Completed.5.Appr1.pmm.d.lda$pred$Default)

RiskData.Completed.5.Appr1.pmm.d.qda.roc<- roc(response = RiskData.Completed.5.Appr1.pmm.d.qda$pred$obs, 
                                               predictor = RiskData.Completed.5.Appr1.pmm.d.qda$pred$Default)

RiskData.Completed.5.Appr1.pmm.d.treebag.roc<- roc(response = RiskData.Completed.5.Appr1.pmm.d.treebag$pred$obs, 
                                                   predictor = RiskData.Completed.5.Appr1.pmm.d.treebag$pred$Default)

#WHEN MODEL HAS PARAMETERS, MAKE SURE TO SELECT FINAL/OPTIMAL PARAMETER VALUE. EXAMPLE:
RiskData.Completed.5.Appr1.pmm.d.glmnet.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.5.Appr1.pmm.d.glmnet$pred[RiskData.Completed.5.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.d.glmnet$pred[round(RiskData.Completed.5.Appr1.pmm.d.glmnet$pred[RiskData.Completed.5.Appr1.pmm.d.glmnet$pred$alpha == 0.1, ]$lambda, 9) == 0.001291826, ]$Default)

RiskData.Completed.5.Appr1.pmm.d.stepLDA.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.stepLDA$pred$obs, 
  predictor = RiskData.Completed.5.Appr1.pmm.d.stepLDA$pred$Default)

RiskData.Completed.5.Appr1.pmm.d.knn.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.knn$pred[RiskData.Completed.5.Appr1.pmm.d.knn$pred$k == 23,]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.d.knn$pred[RiskData.Completed.5.Appr1.pmm.d.knn$pred$k == 23,]$Default) 

RiskData.Completed.5.Appr1.pmm.d.pls.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.pls$pred[RiskData.Completed.5.Appr1.pmm.d.pls$pred$ncomp == 7,]$obs, 
  predictor = RiskData.Completed.5.Appr1.pmm.d.pls$pred[RiskData.Completed.5.Appr1.pmm.d.pls$pred$ncomp == 7,]$Default)

RiskData.Completed.5.Appr1.pmm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.5.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.d.gamSpline$pred[round(RiskData.Completed.5.Appr1.pmm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

RiskData.Completed.5.Appr1.pmm.d.rpart.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.5.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.d.rpart$pred[round(RiskData.Completed.5.Appr1.pmm.d.rpart$pred$cp, 9) == 0.003003755, ]$Default)

RiskData.Completed.5.Appr1.pmm.d.rf.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.rf$pred[RiskData.Completed.5.Appr1.pmm.d.rf$pred$mtry == 10, ]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.d.rf$pred[RiskData.Completed.5.Appr1.pmm.d.rf$pred$mtry == 10, ]$Default)

RiskData.Completed.5.Appr1.pmm.d.gbm.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.5.Appr1.pmm.d.gbm$pred[RiskData.Completed.5.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.d.gbm$pred[round(RiskData.Completed.5.Appr1.pmm.d.gbm$pred[RiskData.Completed.5.Appr1.pmm.d.gbm$pred$n.trees == 500, ]$interaction.depth, 0) == 2, ]$Default)


RiskData.Completed.5.Appr1.pmm.d.svm.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.d.svm$pred[RiskData.Completed.5.Appr1.pmm.d.svm$pred[round(RiskData.Completed.5.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.d.svm$pred[RiskData.Completed.5.Appr1.pmm.d.svm$pred[round(RiskData.Completed.5.Appr1.pmm.d.svm$pred$sigma, 8) == 0.01609618, ]$C == 0.5, ]$Default)


#build to combined ROC plot with resampled ROC curves
plot(RiskData.Completed.5.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(RiskData.Completed.5.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(RiskData.Completed.5.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(RiskData.Completed.5.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(RiskData.Completed.5.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")
plot(RiskData.Completed.5.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(RiskData.Completed.5.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(RiskData.Completed.5.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(RiskData.Completed.5.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(RiskData.Completed.5.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")
plot(RiskData.Completed.5.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(RiskData.Completed.5.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(RiskData.Completed.5.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")

###TEST DATA PERFORMANCE

#PREDICT PROBABILITIES ON TEST SET WITH ALL TRAINED MODELS
############################################################################
###EXAMPLE WITH SOME COMMENTS
test.RiskData.Completed.5.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.logit, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.logit.pred.prob
test.RiskData.Completed.5.Appr1.pmm.d.logit.pred.prob[1] #NOT A VECTOR
test.RiskData.Completed.5.Appr1.pmm.d.logit.pred.prob[[1]] #NOW A VECTOR
############################################################################
#pred.prob
##Postive class Default is reference level
test.RiskData.Completed.5.Appr1.pmm.d.logit.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.logit, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.glmnet.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.lda.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.lda, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.stepLDA.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.qda.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.qda, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.knn.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.knn, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.pls.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.pls, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.rpart.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.treebag.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.rf.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.rf, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

test.RiskData.Completed.5.Appr1.pmm.d.gbm.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")


test.RiskData.Completed.5.Appr1.pmm.d.svm.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.d.svm, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")

#pred.class
test.Completed.5.Appr1.pmm.d.logit.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.logit, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.glmnet.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.glmnet, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.lda.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.lda, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.stepLDA.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.stepLDA, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.qda.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.qda, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.knn.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.knn, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.pls.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.pls, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.gamSpline, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.rpart.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.rpart, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.treebag.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.treebag, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.rf.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.rf, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.gbm.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.gbm, 
  test.RiskData.Completed.5.Appr1.pmm.d)

test.Completed.5.Appr1.pmm.d.svm.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.d.svm, 
  test.RiskData.Completed.5.Appr1.pmm.d)


#######################################################
## Test Confusion Matrix
confusionMatrix(test.Completed.5.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)  
confusionMatrix(test.Completed.5.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)    
confusionMatrix(test.Completed.5.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)      
confusionMatrix(test.Completed.5.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
confusionMatrix(test.Completed.5.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)


#calculate TEST Misclassification Error Rate with confusion matrix
RiskData.Completed.5.Appr1.pmm.d.Test.MisClass.Rate = c(
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.logit.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.glmnet.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.lda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.stepLDA.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.qda.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.knn.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.pls.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.rpart.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.treebag.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.rf.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( 
    ( confusionMatrix(test.Completed.5.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
        confusionMatrix(test.Completed.5.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
    /
      (confusionMatrix(test.Completed.5.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
         confusionMatrix(test.Completed.5.Appr1.pmm.d.gbm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
  )
  ,
  ( confusionMatrix(test.Completed.5.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.5.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.5.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.5.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.5.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.5.Appr1.pmm.d.svm.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)





############################################################################

#ROC Curve of Training and Test Performance of Logit Model: RiskData.Completed.5.Appr1.pmm.d.logit
#Postive class Default is reference level
test.RiskData.Completed.5.Appr1.pmm.d.logit.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.logit.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.glmnet.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.glmnet.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.lda.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.lda.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.stepLDA.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.stepLDA.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.qda.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.qda.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.knn.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.knn.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.pls.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.pls.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.gamSpline.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.rpart.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.rpart.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.treebag.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.treebag.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.rf.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.rf.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.gbm.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.gbm.pred.prob[[1]])

test.RiskData.Completed.5.Appr1.pmm.d.svm.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.d.svm.pred.prob[[1]])

#Test ROC Curve Vs. Training ROC Curve for Each Model
plot(test.RiskData.Completed.5.Appr1.pmm.d.logit.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.logit.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Logit", "Train Logit"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.glmnet.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.glmnet.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Lasso/Ridge", "Train Lasso/Ridge"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.lda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.lda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test LDA", "Train LDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.stepLDA.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.stepLDA.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test stepLDA", "Train stepLDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.qda.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.qda.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test QDA", "Train QDA"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.knn.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.knn.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test KNN", "Train KNN"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.pls.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.pls.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test PLS", "Train PLS"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.rpart.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.rpart.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Decision Tree Classification", "Train Decision Tree Classification"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.treebag.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.treebag.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Bagging", "Train Bagging"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.rf.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.rf.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test randomForest", "Train randomForest"), 
       col = c("black", "blue"),
       lty = 1)

plot(test.RiskData.Completed.5.Appr1.pmm.d.gbm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.gbm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test Boosting", "Train Boosting"), 
       col = c("black", "blue"),
       lty = 1)	   


plot(test.RiskData.Completed.5.Appr1.pmm.d.svm.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.d.svm.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test svm", "Train svm"), 
       col = c("black", "blue"),
       lty = 1)	   

#test performance slightly lower than resample
test.RiskData.Completed.5.Appr1.pmm.d.logit.roc$auc
RiskData.Completed.5.Appr1.pmm.d.logit.roc$auc

plot(test.RiskData.Completed.5.Appr1.pmm.d.logit.roc, legacy.axes = TRUE, col = "Black")
plot(test.RiskData.Completed.5.Appr1.pmm.d.glmnet.roc, add = TRUE, col = "Blue")
plot(test.RiskData.Completed.5.Appr1.pmm.d.lda.roc, add = TRUE, col = "Green")
plot(test.RiskData.Completed.5.Appr1.pmm.d.stepLDA.roc, add = TRUE, col = "Red")
plot(test.RiskData.Completed.5.Appr1.pmm.d.qda.roc, add = TRUE, col = "Yellow")

plot(test.RiskData.Completed.5.Appr1.pmm.d.knn.roc, legacy.axes = TRUE, col = "mediumvioletred")
plot(test.RiskData.Completed.5.Appr1.pmm.d.pls.roc, add = TRUE, col = "Orange")
plot(test.RiskData.Completed.5.Appr1.pmm.d.gamSpline.roc, add = TRUE, col = "darkgreen")
plot(test.RiskData.Completed.5.Appr1.pmm.d.rpart.roc, add = TRUE, col = "goldenrod3")
plot(test.RiskData.Completed.5.Appr1.pmm.d.treebag.roc, add = TRUE, col = "darkturquoise")

plot(test.RiskData.Completed.5.Appr1.pmm.d.rf.roc, legacy.axes = TRUE, col = "lightpink")
plot(test.RiskData.Completed.5.Appr1.pmm.d.gbm.roc, add = TRUE, col = "slategray3")
plot(test.RiskData.Completed.5.Appr1.pmm.d.svm.roc, add = TRUE, col = "magenta")



############### Finding the Highly correlated columns in the dataset with a correlation greater than 0.65

str(RiskData.Y.rm.Completed.1.Appr1.pmm[ , -c(7,8)])

cor(RiskData.Y.rm.Completed.1.Appr1.pmm[ , -c(7,8)])

#corrplot(cor(RiskData.Y.rm.Completed.1.Appr1.pmm[ , -c(7,8)]), tl.cex = 0.35, type = "lower")

findCorrelation(cor(RiskData.Y.rm.Completed.1.Appr1.pmm[ , -c(7,8)]), cutoff = 0.65, verbose = TRUE)

findCorrelation(cor(RiskData.Y.rm.Completed.2.Appr1.pmm[ , -c(7,8)]), cutoff = 0.65, verbose = TRUE)

findCorrelation(cor(RiskData.Y.rm.Completed.3.Appr1.pmm[ , -c(7,8)]), cutoff = 0.65, verbose = TRUE)

findCorrelation(cor(RiskData.Y.rm.Completed.4.Appr1.pmm[ , -c(7,8)]), cutoff = 0.65, verbose = TRUE)

findCorrelation(cor(RiskData.Y.rm.Completed.5.Appr1.pmm[ , -c(7,8)]), cutoff = 0.65, verbose = TRUE)


############## Modeling the dataset for the best model and at the same time removing the high correlation variables
### We are trying to observe if the modelling results after removing the high correlation variables
##### APPROACH 1 COMPLETED 1

RiskData.Completed.1.Appr1.pmm.corr.rm <- RiskData.Y.rm.Completed.1.Appr1.pmm[, -c(12, 15, 16, 17, 27)]
str(RiskData.Completed.1.Appr1.pmm.corr.rm)
RiskData.Completed.1.Appr1.pmm.corr.rm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.1.Appr1.pmm.corr.rm.d.model <- dummyVars(~ . , data = RiskData.Completed.1.Appr1.pmm.corr.rm, fullRank = TRUE)
RiskData.Completed.1.Appr1.pmm.corr.rm.d <- as.data.frame(predict(RiskData.Completed.1.Appr1.pmm.corr.rm.d.model, RiskData.Completed.1.Appr1.pmm.corr.rm))
for(i in 1:nrow(RiskData.Completed.1.Appr1.pmm.corr.rm.d)){
  RiskData.Completed.1.Appr1.pmm.corr.rm.d$Risk_Flag[i] <- if_else(RiskData.Completed.1.Appr1.pmm.corr.rm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.1.Appr1.pmm.corr.rm.d$Risk_Flag <- as.factor(RiskData.Completed.1.Appr1.pmm.corr.rm.d$Risk_Flag)
str(RiskData.Completed.1.Appr1.pmm.corr.rm.d)

train.RiskData.Completed.1.Appr1.pmm.corr.rm.d <- RiskData.Completed.1.Appr1.pmm.corr.rm.d[trainIndex,]
test.RiskData.Completed.1.Appr1.pmm.corr.rm.d <- RiskData.Completed.1.Appr1.pmm.corr.rm.d[-trainIndex,]


set.seed(749)
RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                             weights = Sampling_Weight, 
                                                             data = train.RiskData.Completed.1.Appr1.pmm.corr.rm.d, 
                                                             method = "gamSpline", 
                                                             metric = "ROC", 
                                                             trControl = control, 
                                                             preProcess = c("scale","center"),
                                                             tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline

RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline$pred$pred, RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline$pred$obs)

plot(RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")

test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.1.Appr1.pmm.d, 
  type = "prob")
test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.1.Appr1.pmm.d)

confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)

( 
  ( confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)


test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.prob[[1]])


plot(test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)


plot(test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")



##### APPROACH 1 COMPLETED 2

RiskData.Completed.2.Appr1.pmm.corr.rm <- RiskData.Y.rm.Completed.2.Appr1.pmm[, -c(12, 15, 16, 17, 27)]
str(RiskData.Completed.2.Appr1.pmm.corr.rm)
RiskData.Completed.2.Appr1.pmm.corr.rm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.2.Appr1.pmm.corr.rm.d.model <- dummyVars(~ . , data = RiskData.Completed.2.Appr1.pmm.corr.rm, fullRank = TRUE)
RiskData.Completed.2.Appr1.pmm.corr.rm.d <- as.data.frame(predict(RiskData.Completed.2.Appr1.pmm.corr.rm.d.model, RiskData.Completed.2.Appr1.pmm.corr.rm))
for(i in 1:nrow(RiskData.Completed.2.Appr1.pmm.corr.rm.d)){
  RiskData.Completed.2.Appr1.pmm.corr.rm.d$Risk_Flag[i] <- if_else(RiskData.Completed.2.Appr1.pmm.corr.rm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.2.Appr1.pmm.corr.rm.d$Risk_Flag <- as.factor(RiskData.Completed.2.Appr1.pmm.corr.rm.d$Risk_Flag)
str(RiskData.Completed.2.Appr1.pmm.corr.rm.d)

train.RiskData.Completed.2.Appr1.pmm.corr.rm.d <- RiskData.Completed.2.Appr1.pmm.corr.rm.d[trainIndex,]
test.RiskData.Completed.2.Appr1.pmm.corr.rm.d <- RiskData.Completed.2.Appr1.pmm.corr.rm.d[-trainIndex,]


set.seed(749)
RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                             weights = Sampling_Weight, 
                                                             data = train.RiskData.Completed.2.Appr1.pmm.corr.rm.d, 
                                                             method = "gamSpline", 
                                                             metric = "ROC", 
                                                             trControl = control, 
                                                             preProcess = c("scale","center"),
                                                             tuneLength = 10)
RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline

RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

confusionMatrix(RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline$pred$pred, RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline$pred$obs)

plot(RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")

test.RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.2.Appr1.pmm.d, 
  type = "prob")
test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.2.Appr1.pmm.d)

confusionMatrix(test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)

( 
  ( confusionMatrix(test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)


test.RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.2.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.pred.prob[[1]])


plot(test.RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)


plot(test.RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")


##### APPROACH 1 COMPLETED 3

RiskData.Completed.3.Appr1.pmm.corr.rm <- RiskData.Y.rm.Completed.3.Appr1.pmm[, -c(12, 15, 16, 17, 27)]
str(RiskData.Completed.3.Appr1.pmm.corr.rm)
RiskData.Completed.3.Appr1.pmm.corr.rm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.3.Appr1.pmm.corr.rm.d.model <- dummyVars(~ . , data = RiskData.Completed.3.Appr1.pmm.corr.rm, fullRank = TRUE)
RiskData.Completed.3.Appr1.pmm.corr.rm.d <- as.data.frame(predict(RiskData.Completed.3.Appr1.pmm.corr.rm.d.model, RiskData.Completed.3.Appr1.pmm.corr.rm))
for(i in 1:nrow(RiskData.Completed.3.Appr1.pmm.corr.rm.d)){
  RiskData.Completed.3.Appr1.pmm.corr.rm.d$Risk_Flag[i] <- if_else(RiskData.Completed.3.Appr1.pmm.corr.rm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.3.Appr1.pmm.corr.rm.d$Risk_Flag <- as.factor(RiskData.Completed.3.Appr1.pmm.corr.rm.d$Risk_Flag)
str(RiskData.Completed.3.Appr1.pmm.corr.rm.d)

train.RiskData.Completed.3.Appr1.pmm.corr.rm.d <- RiskData.Completed.3.Appr1.pmm.corr.rm.d[trainIndex,]
test.RiskData.Completed.3.Appr1.pmm.corr.rm.d <- RiskData.Completed.3.Appr1.pmm.corr.rm.d[-trainIndex,]


set.seed(749)
RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                             weights = Sampling_Weight, 
                                                             data = train.RiskData.Completed.3.Appr1.pmm.corr.rm.d, 
                                                             method = "gamSpline", 
                                                             metric = "ROC", 
                                                             trControl = control, 
                                                             preProcess = c("scale","center"),
                                                             tuneLength = 10)
RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline

RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

confusionMatrix(RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline$pred$pred, RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline$pred$obs)

plot(RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")

test.RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.3.Appr1.pmm.d, 
  type = "prob")
test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.3.Appr1.pmm.d)

confusionMatrix(test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)

( 
  ( confusionMatrix(test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)


test.RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.3.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.pred.prob[[1]])


plot(test.RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)


plot(test.RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")



##### APPROACH 1 COMPLETED 4

RiskData.Completed.4.Appr1.pmm.corr.rm <- RiskData.Y.rm.Completed.4.Appr1.pmm[, -c(12, 15, 16, 17, 27)]
str(RiskData.Completed.4.Appr1.pmm.corr.rm)
RiskData.Completed.4.Appr1.pmm.corr.rm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.4.Appr1.pmm.corr.rm.d.model <- dummyVars(~ . , data = RiskData.Completed.4.Appr1.pmm.corr.rm, fullRank = TRUE)
RiskData.Completed.4.Appr1.pmm.corr.rm.d <- as.data.frame(predict(RiskData.Completed.4.Appr1.pmm.corr.rm.d.model, RiskData.Completed.4.Appr1.pmm.corr.rm))
for(i in 1:nrow(RiskData.Completed.4.Appr1.pmm.corr.rm.d)){
  RiskData.Completed.4.Appr1.pmm.corr.rm.d$Risk_Flag[i] <- if_else(RiskData.Completed.4.Appr1.pmm.corr.rm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.4.Appr1.pmm.corr.rm.d$Risk_Flag <- as.factor(RiskData.Completed.4.Appr1.pmm.corr.rm.d$Risk_Flag)
str(RiskData.Completed.4.Appr1.pmm.corr.rm.d)

train.RiskData.Completed.4.Appr1.pmm.corr.rm.d <- RiskData.Completed.4.Appr1.pmm.corr.rm.d[trainIndex,]
test.RiskData.Completed.4.Appr1.pmm.corr.rm.d <- RiskData.Completed.4.Appr1.pmm.corr.rm.d[-trainIndex,]


set.seed(749)
RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                             weights = Sampling_Weight, 
                                                             data = train.RiskData.Completed.4.Appr1.pmm.corr.rm.d, 
                                                             method = "gamSpline", 
                                                             metric = "ROC", 
                                                             trControl = control, 
                                                             preProcess = c("scale","center"),
                                                             tuneLength = 10)
RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline

RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

confusionMatrix(RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline$pred$pred, RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline$pred$obs)

plot(RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")

test.RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.4.Appr1.pmm.d, 
  type = "prob")
test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.4.Appr1.pmm.d)

confusionMatrix(test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)

( 
  ( confusionMatrix(test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)


test.RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.4.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.pred.prob[[1]])


plot(test.RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)


plot(test.RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")


##### APPROACH 1 COMPLETED 5

RiskData.Completed.5.Appr1.pmm.corr.rm <- RiskData.Y.rm.Completed.5.Appr1.pmm[, -c(12, 15, 16, 17, 27)]
str(RiskData.Completed.5.Appr1.pmm.corr.rm)
RiskData.Completed.5.Appr1.pmm.corr.rm$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.5.Appr1.pmm.corr.rm.d.model <- dummyVars(~ . , data = RiskData.Completed.5.Appr1.pmm.corr.rm, fullRank = TRUE)
RiskData.Completed.5.Appr1.pmm.corr.rm.d <- as.data.frame(predict(RiskData.Completed.5.Appr1.pmm.corr.rm.d.model, RiskData.Completed.5.Appr1.pmm.corr.rm))
for(i in 1:nrow(RiskData.Completed.5.Appr1.pmm.corr.rm.d)){
  RiskData.Completed.5.Appr1.pmm.corr.rm.d$Risk_Flag[i] <- if_else(RiskData.Completed.5.Appr1.pmm.corr.rm.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.5.Appr1.pmm.corr.rm.d$Risk_Flag <- as.factor(RiskData.Completed.5.Appr1.pmm.corr.rm.d$Risk_Flag)
str(RiskData.Completed.5.Appr1.pmm.corr.rm.d)

train.RiskData.Completed.5.Appr1.pmm.corr.rm.d <- RiskData.Completed.5.Appr1.pmm.corr.rm.d[trainIndex,]
test.RiskData.Completed.5.Appr1.pmm.corr.rm.d <- RiskData.Completed.5.Appr1.pmm.corr.rm.d[-trainIndex,]


set.seed(749)
RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline <-  train(Risk_Flag ~ . -Sampling_Weight, 
                                                             weights = Sampling_Weight, 
                                                             data = train.RiskData.Completed.5.Appr1.pmm.corr.rm.d, 
                                                             method = "gamSpline", 
                                                             metric = "ROC", 
                                                             trControl = control, 
                                                             preProcess = c("scale","center"),
                                                             tuneLength = 10)
RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline

RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$obs,
  predictor = RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline$pred[round(RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline$pred$df, 6) == 2.777778 , ]$Default)

confusionMatrix(RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline$pred$pred, RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline$pred$obs)

plot(RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")

test.RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.5.Appr1.pmm.d, 
  type = "prob")
test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class <- predict(
  RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline, 
  test.RiskData.Completed.5.Appr1.pmm.d)

confusionMatrix(test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)
( 
  ( confusionMatrix(test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)

test.RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.5.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.pred.prob[[1]])

plot(test.RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)
plot(test.RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")



###################################################################
### Now we try to work on the modeling by only taking the first 20 most important variables in our models
varImp(RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline)
varImp(RiskData.Completed.2.Appr1.pmm.corr.rm.d.gamSpline)
varImp(RiskData.Completed.3.Appr1.pmm.corr.rm.d.gamSpline)
varImp(RiskData.Completed.4.Appr1.pmm.corr.rm.d.gamSpline)
varImp(RiskData.Completed.5.Appr1.pmm.corr.rm.d.gamSpline)

ncol(RiskData.Y.rm.Completed.1.Appr1.pmm)
#####################################################################
RiskData.Completed.1.Appr1.pmm.varimp <- RiskData.Completed.1.Appr1.pmm.d[,c(2, 30, 13, 6, 29, 14, 11, 42, 17, 19, 27, 20, 33, 8, 5, 16, 23, 12, 34, 18, 3)]
str(RiskData.Completed.1.Appr1.pmm.varimp)
RiskData.Completed.1.Appr1.pmm.varimp$Risk_Flag <- Risk_Flag.Y
RiskData.Completed.1.Appr1.pmm.varimp.d.model <- dummyVars(~ . , data = RiskData.Completed.1.Appr1.pmm.varimp, fullRank = TRUE)
RiskData.Completed.1.Appr1.pmm.varimp.d <- as.data.frame(predict(RiskData.Completed.1.Appr1.pmm.varimp.d.model, RiskData.Completed.1.Appr1.pmm.varimp))
for(i in 1:nrow(RiskData.Completed.1.Appr1.pmm.varimp.d)){
  RiskData.Completed.1.Appr1.pmm.varimp.d$Risk_Flag[i] <- if_else(RiskData.Completed.1.Appr1.pmm.varimp.d$Risk_Flag[i] == 0, "Default", "NonDefault")
}
RiskData.Completed.1.Appr1.pmm.varimp.d$Risk_Flag <- as.factor(RiskData.Completed.1.Appr1.pmm.varimp.d$Risk_Flag)
str(RiskData.Completed.1.Appr1.pmm.varimp.d)

train.RiskData.Completed.1.Appr1.pmm.varimp.d <- RiskData.Completed.1.Appr1.pmm.varimp.d[trainIndex,]
test.RiskData.Completed.1.Appr1.pmm.varimp.d <- RiskData.Completed.1.Appr1.pmm.varimp.d[-trainIndex,]
RiskData.Completed.1.Appr1.pmm.varimp.d

set.seed(749)
RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline <-  train(Risk_Flag ~ .-Sampling_Weight, 
                                                            weights = Sampling_Weight,
                                                            data = train.RiskData.Completed.1.Appr1.pmm.varimp.d, 
                                                            method = "gamSpline", 
                                                            metric = "ROC", 
                                                            trControl = control, 
                                                            preProcess = c("scale","center"),
                                                            tuneLength = 10)
RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline

RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline.roc<- roc(
  response = RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline$pred[round(RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline$pred$df, 6) == 3 , ]$obs,
  predictor = RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline$pred[round(RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline$pred$df, 6) == 3 , ]$Default)

confusionMatrix(RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline$pred$pred, RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline$pred$obs)

plot(RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline.roc, add = TRUE, col = "darkgreen")

test.RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline.pred.prob <- predict(
  RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline, 
  test.RiskData.Completed.1.Appr1.pmm.varimp.d, 
  type = "prob")
test.RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline.pred.class <- predict(
  RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline, 
  test.RiskData.Completed.1.Appr1.pmm.varimp.d)

confusionMatrix(test.RiskData.Completed.1.Appr1.pmm.varimp.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.varimp.d$Risk_Flag)

( 
  ( confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] +
      confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1]) 
  /
    (confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,1] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[1,2] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,1] + 
       confusionMatrix(test.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.class, test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag)$table[2,2]) 
)


test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc<- roc(
  response = test.RiskData.Completed.1.Appr1.pmm.d$Risk_Flag,
  predictor = test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.pred.prob[[1]])


plot(test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc, legacy.axes = TRUE) #testing data set
plot(RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col="blue")  #training data set
legend(x = 0.2, y = 0.8, 
       legend = c("Test gamSpline", "Train gamSpline"), 
       col = c("black", "blue"),
       lty = 1)
plot(test.RiskData.Completed.1.Appr1.pmm.corr.rm.d.gamSpline.roc, add = TRUE, col = "darkgreen")

#####################################################################
stopCluster(cl)
