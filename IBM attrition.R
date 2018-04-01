# Reading the data file:


ibmdata <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv", header = T)

# Removing unnecessary colomns and data cleaning
#ibmdata$BusinessTravel <- NULL
ibmdata$DailyRate <- NULL
#ibmdata$DistanceFromHome <-  NULL
ibmdata$EmployeeCount <- NULL
ibmdata$EmployeeNumber <- NULL
#ibmdata$EnvironmentSatisfaction <- NULL
ibmdata$HourlyRate <- NULL
ibmdata$MonthlyRate <- NULL
ibmdata$StandardHours <- NULL
#ibmdata$StockOptionLevel <- NULL
ibmdata$Over18 <- NULL

colnames(ibmdata)[which(names(ibmdata) == "ï..Age")] <- "Age"

set.seed(9)
#Dividing the data into train(70) and test(30)
index= sample(1:nrow(ibmdata), size=0.7*nrow(ibmdata))
#Train data
train_ibm <- ibmdata[index,]
#Test data
test_ibm <- ibmdata[-index,]

#OLS 
attach(ibmdata)
#linearmod <- glm(ibmdata$MonthlyIncome~ ., data = ibmdata)
lmmod <- lm(train_ibm$MonthlyIncome~ ., data = train_ibm)

# #Call:
# lm(formula = train_ibm$MonthlyIncome ~ ., data = train_ibm)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3687.8  -672.4   -14.7   657.5  4238.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       749.9635   646.9483   1.159 0.246640    
# Age                                -3.9123     5.4471  -0.718 0.472778    
# AttritionYes                       10.6256   108.1689   0.098 0.921768    
# DepartmentResearch & Development  197.7460   426.3883   0.464 0.642915    
# DepartmentSales                   177.0986   456.6418   0.388 0.698226    
# Education                         -48.7727    34.0300  -1.433 0.152108    
# EducationFieldLife Sciences      -333.1478   354.7870  -0.939 0.347955    
# EducationFieldMarketing          -256.7895   376.4880  -0.682 0.495357    
# EducationFieldMedical            -331.6265   356.2303  -0.931 0.352115    
# EducationFieldOther              -385.2046   377.9075  -1.019 0.308305    
# EducationFieldTechnical Degree   -262.6057   368.3691  -0.713 0.476083    
# GenderMale                         49.2526    70.4266   0.699 0.484500    
# JobInvolvement                    -44.5659    48.9762  -0.910 0.363071    
# JobLevel                         2826.8467    79.9594  35.354  < 2e-16 ***
#   JobRoleHuman Resources           -311.0440   461.8355  -0.673 0.500789    
# JobRoleLaboratory Technician     -664.7646   165.5617  -4.015 6.39e-05 ***
#   JobRoleManager                   3970.1534   233.4027  17.010  < 2e-16 ***
#   JobRoleManufacturing Director    -294.5989   157.6895  -1.868 0.062026 .  
# JobRoleResearch Director         3706.3160   211.8334  17.496  < 2e-16 ***
#   JobRoleResearch Scientist        -599.5405   164.5438  -3.644 0.000283 ***
#   JobRoleSales Executive           -354.3975   321.6878  -1.102 0.270867    
# JobRoleSales Representative      -711.9773   358.0885  -1.988 0.047056 *  
#   JobSatisfaction                     3.1528    31.1676   0.101 0.919448    
# MaritalStatusMarried               28.7748    89.1897   0.323 0.747047    
# MaritalStatusSingle                35.3538    97.5949   0.362 0.717242    
# NumCompaniesWorked                 19.9793    15.6725   1.275 0.202679    
# OverTimeYes                        16.2832    81.6623   0.199 0.841993    
# PercentSalaryHike                  11.4473    14.9062   0.768 0.442697    
# PerformanceRating                 -38.7303   152.8795  -0.253 0.800059    
# RelationshipSatisfaction            6.5586    31.7548   0.207 0.836414    
# TotalWorkingYears                  42.8773     9.7816   4.383 1.29e-05 ***
#   TrainingTimesLastYear             -32.8754    27.0813  -1.214 0.225055    
# WorkLifeBalance                   -58.3869    47.7358  -1.223 0.221573    
# YearsAtCompany                     -8.6273    12.0281  -0.717 0.473382    
# YearsInCurrentRole                  0.5237    15.1159   0.035 0.972368    
# YearsSinceLastPromotion            43.2117    13.8834   3.112 0.001908 ** 
#   YearsWithCurrManager              -30.7176    15.8628  -1.936 0.053095 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1082 on 992 degrees of freedom
# Multiple R-squared:  0.9481,	Adjusted R-squared:  0.9462 
# F-statistic: 503.5 on 36 and 992 DF,  p-value: < 2.2e-16


plot(lmmod$residuals) # No sign of non-linearity from the plot


#ibmdata_cor <- ibmdata
# 
# ibmdata_cor$Attrition <- NULL
# ibmdata_cor$Department <- NULL
# ibmdata_cor$EducationField <- NULL
# ibmdata_cor$Gender <- NULL
# ibmdata_cor$JobRole <- NULL
# ibmdata_cor$MaritalStatus <- NULL
# ibmdata_cor$OverTime <- NULL
# 
# round(cor(ibmdata_cor),2)

#Ridge and lasso regression
library(glmnet)
X_train = model.matrix(train_ibm$MonthlyIncome~ ., data = train_ibm)
Y_train = train_ibm$MonthlyIncome

X_test = model.matrix(test_ibm$MonthlyIncome~ ., data = test_ibm)


x = X_train[,-1]

set.seed(1)
#cv_ridge = cv.glmnet(x,Y_train,alpha=0,nlambda = 100,lambda.min.ratio = 0.0001)
cv_ridge = cv.glmnet(x,Y_train,alpha=0)

Cv_lasso = cv.glmnet(x,Y_train,alpha=1)

plot(cv_ridge)
plot(Cv_lasso)

best_lamda_ridge <- cv_ridge$lambda.min
best_lamda_lasso <- Cv_lasso$lambda.min
#Coefficients
ridge_Coefficients <- predict(cv_ridge, type = "coefficients", s = cv_ridge$lambda.min)
lasso_Coefficients <- predict(Cv_lasso, type = "coefficients", s = Cv_lasso$lambda.min)

#Predicting sales with OLS, ridge and Lasso to see which is best 
test_monthly_income <- test_ibm$MonthlyIncome
test_ibm$MonthlyIncome <- NULL

predict_lmmod <- predict(lmmod, newdata = test_ibm)
predict_ridge <- predict(cv_ridge, s = cv_ridge$lambda.min, newx = X_test[,-1])
predict_lasso <- predict(Cv_lasso,s = Cv_lasso$lambda.min, newx = X_test[,-1])
#Calculating the MSE for OLS and Ridge
MSE_Ridge <- mean((predict_ridge- test_monthly_income )^2)
MSE_OLS <- mean((predict_lmmod - test_monthly_income )^2)
MSE_Lasso <- mean((predict_lasso - test_monthly_income )^2)

sprintf("MSE for ridge is %f", MSE_Ridge)
sprintf("MSE for Lasso is %f", MSE_Lasso)
sprintf("MSE for OLS is %f", MSE_OLS)

plt <- c(MSE_Lasso,MSE_OLS,MSE_Ridge)

barplot(plt, main="Mean Square Error", xlab="Lasso || OLS || Ridge")
#---------------------------------------------------------------------------------------#

#SURVIVAL ANALYSIS#

#'Attrition' is a categorical variable, so changing the variable to binary 1/0

ibmdata$Attrition_event <- with(ibmdata, ifelse(Attrition=="Yes",1,0))
time <- ibmdata$YearsAtCompany
event <- ibmdata$Attrition_event
group <- ibmdata$OverTime

library(survival)
survival <- Surv(time, event)
head(survival)
