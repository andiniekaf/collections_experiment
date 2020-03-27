setwd("xx")

## LOADING THE NECESSARY PACKAGES ##

library(readxl)
library(dplyr)
library(rlang)
library(MASS)
library(boot)
library(margins)
library(e1071)
library(caret)

## LOADING THE DATA ##

Field_Experiment_Analysis <- read_excel("Field Experiment Analysis.xlsx")

## VARIABLE TRANSFORMATION ##

# 1. Payment Number Dummies #
pnumber.f = factor(Field_Experiment_Analysis$payment_number)
dummies <- model.matrix(~pnumber.f)
Field_Experiment_Analysis$pnumber.f2 <- dummies$pnumber.f2
Field_Experiment_Analysis$pnumber.f3 <- dummies$pnumber.f3
Field_Experiment_Analysis$pnumber.f4 <- dummies$pnumber.f4
Field_Experiment_Analysis$pnumber.f5 <- dummies$pnumber.f5
Field_Experiment_Analysis$pnumber.f6 <- dummies$pnumber.f6

# 2. Due Date Dummies #
Field_Experiment_Analysis$dd10 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-10",1,0)
Field_Experiment_Analysis$dd11 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-11",1,0)
Field_Experiment_Analysis$dd12 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-12",1,0)
Field_Experiment_Analysis$dd13 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-13",1,0)
Field_Experiment_Analysis$dd14 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-14",1,0)
Field_Experiment_Analysis$dd15 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-15",1,0)
Field_Experiment_Analysis$dd16 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-16",1,0)
Field_Experiment_Analysis$dd17 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-17",1,0)
Field_Experiment_Analysis$dd18 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-18",1,0)
Field_Experiment_Analysis$dd19 <- ifelse(Field_Experiment_Analysis$due_date == "2020-03-19",1,0)

## DESCRIPTIVE STATISTICS ##

summary(Field_Experiment_Analysis)

#    loan_id        payment_id        payment_number    due_date          groupings         treatment_indicator
# Min.   :3e+09   Min.   :4.001e+09   Min.   :1.000   Length:3163        Length:3163        Min.   :0.0000     
# 1st Qu.:3e+09   1st Qu.:4.001e+09   1st Qu.:1.000   Class :character   Class :character   1st Qu.:0.0000     
# Median :3e+09   Median :4.001e+09   Median :2.000   Mode  :character   Mode  :character   Median :1.0000     
# Mean   :3e+09   Mean   :4.001e+09   Mean   :2.084                                         Mean   :0.6668     
# 3rd Qu.:3e+09   3rd Qu.:4.001e+09   3rd Qu.:3.000                                         3rd Qu.:1.0000     
# Max.   :3e+09   Max.   :4.001e+09   Max.   :6.000                                         Max.   :1.0000     
 
# repayment_indicator hp1_rpc_indicator hp2_rpc_indicator company_rpc_indicator   username        
# Min.   :0.0000      Min.   :0.000     Min.   :0.00000   Min.   :0             Length:3163       
# 1st Qu.:0.0000      1st Qu.:0.000     1st Qu.:0.00000   1st Qu.:0             Class :character  
# Median :1.0000      Median :0.000     Median :0.00000   Median :0             Mode  :character  
# Mean   :0.6633      Mean   :0.257     Mean   :0.01676   Mean   :0                               
# 3rd Qu.:1.0000      3rd Qu.:1.000     3rd Qu.:0.00000   3rd Qu.:0                               
# Max.   :1.0000      Max.   :1.000     Max.   :1.00000   Max.   :0  

## CHI SQUARE ##

tbl <- table(Field_Experiment_Analysis$treatment_indicator, Field_Experiment_Analysis$repayment_indicator)

tbl
   
#       0    1
#  0  357  697
#  1  708 1401

chisq.test(tbl) 

# Pearson's Chi-squared test with Yates' continuity correction

# data:  tbl
# X-squared = 0.016561, df = 1, p-value = 0.8976

## LOGISTICS REGRESSION ## 

# 1. MODEL 1 #

glm.office_1 <- glm(repayment_indicator ~ treatment_indicator + pnumber.f2 + pnumber.f3 + pnumber.f4 + pnumber.f5 + pnumber.f6 + hp1_rpc_indicator + hp2_rpc_indicator, data = Field_Experiment_Analysis, family ="binomial")
summary(glm.office_1)

# Call:
# glm(formula = repayment_indicator ~ treatment_indicator + pnumber.f2 + 
#    pnumber.f3 + pnumber.f4 + pnumber.f5 + pnumber.f6 + hp1_rpc_indicator + 
#    hp2_rpc_indicator, family = "binomial", data = Field_Experiment_Analysis)

# Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
# -2.7574  -1.1879   0.2851   1.0377   1.2160  

# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -0.09035    0.08364  -1.080    0.280    
# treatment_indicator  0.11495    0.08818   1.304    0.192    
# pnumber.f2           0.41489    0.10622   3.906 9.38e-05 ***
# pnumber.f3           0.59660    0.12992   4.592 4.39e-06 ***
# pnumber.f4           0.17415    0.15527   1.122    0.262    
# pnumber.f5           0.31335    0.15704   1.995    0.046 *  
# pnumber.f6           0.16563    0.48443   0.342    0.732    
# hp1_rpc_indicator    3.15795    0.19737  16.000  < 2e-16 ***
# hp2_rpc_indicator    2.67634    0.59687   4.484 7.33e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 4041.2  on 3162  degrees of freedom
# Residual deviance: 3395.5  on 3154  degrees of freedom
# AIC: 3413.5

# Number of Fisher Scoring iterations: 6

margins.office1 <- summary(margins(glm.office_1))
margins(glm.office_1)

# Average marginal effects
# glm(formula = repayment_indicator ~ treatment_indicator + pnumber.f2 +     pnumber.f3 + pnumber.f4 + pnumber.f5 + pnumber.f6 + hp1_rpc_indicator +     hp2_rpc_indicator, family = "binomial", data = Field_Experiment_Analysis)

#  treatment_indicator pnumber.f2 pnumber.f3 pnumber.f4 pnumber.f5 pnumber.f6 hp1_rpc_indicator
             0.02147    0.07748     0.1114    0.03252    0.05852    0.03093            0.5898
#  hp2_rpc_indicator
            0.4998
            
Field_Experiment_Analysis$prob_repay1 <- predict(glm.office_1, type="response")
Field_Experiment_Analysis$pred_repay1 <- ifelse(Field_Experiment_Analysis$prob_repay1 > 0.6,1,0)

confusionMatrix(table(Field_Experiment_Analysis$pred_repay1,Field_Experiment_Analysis$repayment_indicator))

# Confusion Matrix and Statistics

   
#        0    1
#  0  784  843
#  1  281 1255
                                          
#               Accuracy : 0.6446          
#                 95% CI : (0.6277, 0.6613)
#    No Information Rate : 0.6633          
#    P-Value [Acc > NIR] : 0.9872          
                                          
#                  Kappa : 0.2959          
                                          
# Mcnemar's Test P-Value : <2e-16          
                                          
#            Sensitivity : 0.7362          
#            Specificity : 0.5982          
#         Pos Pred Value : 0.4819          
#         Neg Pred Value : 0.8171          
#             Prevalence : 0.3367          
#         Detection Rate : 0.2479          
#   Detection Prevalence : 0.5144          
#      Balanced Accuracy : 0.6672          
                                          
#       'Positive' Class : 0    

# 2. MODEL 2 #

glm.office_2 <- glm(repayment_indicator ~ treatment_indicator + pnumber.f2 + pnumber.f3 + pnumber.f4 + pnumber.f5 + pnumber.f6 + hp1_rpc_indicator + hp2_rpc_indicator + dd10 + dd11 + dd12 + dd13 + dd14 + dd15 + dd16 + dd17 + dd18, data = Field_Experiment_Analysis, family ="binomial")

margins.office2 <- summary(margins(glm.office_2))
margins(glm.office_2)


Field_Experiment_Analysis$prob_repay2 <- predict(glm.office_2, type="response")
Field_Experiment_Analysis$pred_repay2 <- ifelse(Field_Experiment_Analysis$prob_repay2 > 0.5,1,0)
confusionMatrix(table(Field_Experiment_Analysis$pred_repay2,Field_Experiment_Analysis$repayment_indicator))

# Confusion Matrix and Statistics

   
#        0    1
#  0  362  239
#  1  703 1859
                                          
#               Accuracy : 0.7022          
#                 95% CI : (0.6859, 0.7181)
#    No Information Rate : 0.6633          
#    P-Value [Acc > NIR] : 1.627e-06       
                                          
#                  Kappa : 0.2531          
                                          
# Mcnemar's Test P-Value : < 2.2e-16       
                                          
#            Sensitivity : 0.3399          
#            Specificity : 0.8861          
#         Pos Pred Value : 0.6023          
#         Neg Pred Value : 0.7256          
#             Prevalence : 0.3367          
#         Detection Rate : 0.1144          
#   Detection Prevalence : 0.1900          
#      Balanced Accuracy : 0.6130          
                                          
#       'Positive' Class : 0               


