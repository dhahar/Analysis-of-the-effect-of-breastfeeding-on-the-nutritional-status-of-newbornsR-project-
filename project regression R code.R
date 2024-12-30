

 mydata<- read.csv("C:\\R\\project2023.csv")
 mydata

 
 #Day1 nutrition variables
 # Glucose_d1
 
 model1 <- lm(log(Glucose_d1) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model1)
 
 # Residuals of model1
 residuals_model1 <- residuals(model1)
 
 # Histogram of residuals
 hist(residuals_model1, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model1)
 qqline(residuals_model1)
 
 
 
 #Day1 nutrition variables
 # Fructose_d1
 
 model2 <- lm(log(Fructose_d1) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model2)
 
 # Residuals of model2
 residuals_model2 <- residuals(model1)
 
 # Histogram of residuals
 hist(residuals_model2, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model2)
 qqline(residuals_model2)
 
 

 #Suga_alchol_d1 
 
 model3 <- lm(log(Suga_alchol_d1) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model3)
 
 # Residuals of model3
 residuals_model3 <- residuals(model3)
 
 # Histogram of residuals
 hist(residuals_model3, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model3)
 qqline(residuals_model3)
 
 
 
 model4 <- lm(log(Saturated_fatty.acid_d1) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model4)
 
 # Residuals of model4
 residuals_model4 <- residuals(model4)
 
 # Histogram of residuals
 hist(residuals_model4, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model4)
 qqline(residuals_model4)
 
 
 
 
 
 
 
 model5 <- lm(log(Unsturated_fatty.acid_d1) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model5)
 
 # Residuals of model5
 residuals_model5 <- residuals(model5)
 
 # Histogram of residuals
 hist(residuals_model5, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model5)
 qqline(residuals_model5)
 
 
 
 
 
 
 #Day14 nutrition variables
 # Glucose_d14
 
 model6 <- lm(log(Glucose_d14) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model6)
 
 # Residuals of model6
 residuals_model6 <- residuals(model6)
 
 # Histogram of residuals
 hist(residuals_model6, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model6)
 qqline(residuals_model6)
 
 
 
 #Day1 nutrition variables
 # Fructose_d14
 
 model7 <- lm(log(Fructose_d14) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model7)
 
 # Residuals of model7
 residuals_model7 <- residuals(model1)
 
 # Histogram of residuals
 hist(residuals_model7, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model7)
 qqline(residuals_model7)
 
 
 
 #Suga_alchol_d14
 
 model8 <- lm(log(Sugar_alchol._d14) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model8)
 
 # Residuals of model8
 residuals_model8 <- residuals(model8)
 
 # Histogram of residuals
 hist(residuals_model8, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model8)
 qqline(residuals_model8)
 
 # Saturated_fatty.acid_d14 
 
 model9 <- lm(log(Saturated_fatty.acid_d14) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model9)
 
 # Residuals of model9
 residuals_model9 <- residuals(model9)
 
 # Histogram of residuals
 hist(residuals_model9, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model9)
 qqline(residuals_model9)
 
 
 #Unsturated_fatty.acid_d14
 #model 10
 
 
 
 model10 <- lm(log( Unsturated_fatty.acid_d14) ~ milk_type+BMI+WBC+gestational_hypertension+gestational_diabetes+sex+ Head.cm_d1
              +WBC_d1,data=mydata)
 
 summary(model10)
 
 # Residuals of model10
 residuals_model10 <- residuals(model10)
 
 # Histogram of residuals
 hist(residuals_model10, breaks = 10, main = "Histogram of Residuals")
 
 # Q-Q plot of residuals
 qqnorm(residuals_model10)
 qqline(residuals_model10)
 
 
 
 
 
 
 
 
 
 
 
 