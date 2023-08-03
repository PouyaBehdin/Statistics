## FinalExam

##Problem1

SolvedEx <- read_excel("Training1.xlsx")
View(SolvedEx)
str(SolvedEx) ## View the structure
End_of_Training <- SolvedEx$End_of_Training
Proficiency <- SolvedEx$Proficiency
Method <- SolvedEx$Method
Classroom <- ifelse(SolvedEx$Method=='classroom',1,0)
Online <- ifelse(SolvedEx$Method=='online',1,0)
App <- ifelse(SolvedEx$Method=='courseware app',1,0)
SolvedEx$Classroom <- Classroom
SolvedEx$Online <- Online
SolvedEx$App <- App
View(SolvedEx)
Training_Model <- lm(End_of_Training~Proficiency+Classroom+Online,data=SolvedEx)
options(scipen=999)
anova(Training_Model)
summary(Training_Model)
options(scipen=0)
Training_Model_Summary <- summary(Training_Model)
sprintf("End_of_Training=%.3f+%.3f*Proficiency+%.3f*Classroom+%.3f*Online",
        Training_Model_Summary$coefficients[1,1],
        Training_Model_Summary$coefficients[2,1],
        Training_Model_Summary$coefficients[3,1],
        Training_Model_Summary$coefficients[4,1])
sprintf("Taking into account the effect of the training method, for each additional point scored on the proficiency exam, the predicted end-of-training exam score is estimated to increase by %.3f points. Holding constant the score on the proficiency exam, the estimated effect of an underwriter being trained by the classroom method rather than the courseware app method or the online method is to increase the end-of-training exam score by %.3f points. Holding constant the score on the proficiency exam, the estimated effect of an underwriter being trained by the online method rather than the courseware app method or the classroom method is to increase the end-of-training exam score by %.3f points.",
        Training_Model_Summary$coefficients[2,1],
        Training_Model_Summary$coefficients[3,1],
        Training_Model_Summary$coefficients[4,1])
format(round(predict(Training_Model,data.frame(Proficiency=102,Classroom=1,Online=0)),1),nsmall=1)
residuals <- resid(Training_Model)
plot(x=End_of_Training,y=residuals,xlab="End of Training",ylab="Residuals",main="Plot of Residuals and End of Training",xlim=c(0,100),ylim=c(-20,20))
round(Training_Model_Summary$fstatistic[1],2)
sprintf("p-value is %1.3f.",pf(Training_Model_Summary$fstatistic[1],
                               Training_Model_Summary$fstatistic[2],
                               Training_Model_Summary$fstatistic[3],lower.tail=F))
Training_Model_Summary
round(Training_Model_Summary$coefficients[2,3],2)
options(scipen=999)
format(round(Training_Model_Summary$coefficients[2,4],3),nsmall=3)
options(scipen=0)
format(round(Training_Model_Summary$coefficients[3,3],2),nsmall=2)
options(scipen=999)
format(round(Training_Model_Summary$coefficients[3,4],3),nsmall=3)
options(scipen=0)
format(round(Training_Model_Summary$coefficients[4,3],2),nsmall=2)
options(scipen=999)
format(round(Training_Model_Summary$coefficients[4,4],3),nsmall=3)
options(scipen=0)
lcl <- confint(Training_Model,"Proficiency",level=0.95)[1]
lcl <- round(lcl,3)
ucl <- confint(Training_Model,"Proficiency",level=0.95)[2]
ucl <- round(ucl,3)
sprintf("The 95%% CI for the Proficiency is [%4.3f points, %4.3f points].",lcl,ucl)
sprintf("Taking into account the effect of the training method, for each additional point scored on the proficiency exam, the predicted end-of-training exam score is estimated to increase by approximately %4.3f and %4.3f points.",lcl,ucl)
lcl <- confint(Training_Model,"Classroom",level=0.95)[1]
lcl <- round(lcl,3)
ucl <- confint(Training_Model,"Classroom",level=0.95)[2]
ucl <- round(ucl,3)
sprintf("The 95%% CI for the Classroom is [%4.3f points, %4.3f points].",lcl,ucl)
sprintf("Taking into account the other variables, for classroom training, the predicted end-of-training exam score is estimated to decrease by approximately %4.3f and %4.3f points.",lcl,ucl)
lcl <- confint(Training_Model,"Online",level=0.95)[1]
lcl <- round(lcl,3)
ucl <- confint(Training_Model,"Online",level=0.95)[2]
ucl <- round(ucl,3)
sprintf("The 95%% CI for the Online training is [%4.3f points, %4.3f points].",lcl,ucl)
sprintf("Taking into account the other variables, for online training, the predicted end-of-training exam score is estimated to change by approximately %4.3f and %4.3f points.",lcl,ucl)
round(Training_Model_Summary$adj.r.squared,3)
fullmodel <- lm(End_of_Training~Proficiency+Classroom+Online,data=SolvedEx)
reducedmodel <- lm(End_of_Training~Classroom+Online,data=SolvedEx)
f <- anova(fullmodel)
r <- anova(reducedmodel)
SSE_F <- f$`Sum Sq`[4]
SSE_R <- r$`Sum Sq`[3]
round(((SSE_R-SSE_F)/SSE_R),3)
sprintf("For given value(s) of X2 (Classroom) and X3 (Online), %.1f%% of the variation in Y (End of training) is explained by the variation in X1 (Proficiency).",round(((SSE_R-SSE_F)/SSE_R)*100,1))
fullmodel <- lm(End_of_Training~Proficiency+Classroom+Online,data=SolvedEx)
reducedmodel <- lm(End_of_Training~Proficiency+Online,data=SolvedEx)
f <- anova(fullmodel)
r <- anova(reducedmodel)
SSE_F <- f$`Sum Sq`[4]
SSE_R <- r$`Sum Sq`[3]
round(((SSE_R-SSE_F)/SSE_R),3)
sprintf("For given value(s) of X1 (Proficiency) and X3 (Online), %.1f%% of the variation in Y (End of training) is explained by the variation in X2 (Classroom).",round(((SSE_R-SSE_F)/SSE_R)*100,1))
fullmodel <- lm(End_of_Training~Proficiency+Classroom+Online,data=SolvedEx)
reducedmodel <- lm(End_of_Training~Proficiency+Classroom,data=SolvedEx)
f <- anova(fullmodel)
r <- anova(reducedmodel)
SSE_F <- f$`Sum Sq`[4]
SSE_R <- r$`Sum Sq`[3]
round(((SSE_R-SSE_F)/SSE_R),3)
sprintf("For given value(s) of X1 (Proficiency) and X2 (Classroom), %.1f%% of the variation in Y (End of training) is explained by the variation in X3 (Online).",round(((SSE_R-SSE_F)/SSE_R)*100,1))
Training_Model_With_Interaction <- lm(End_of_Training~Proficiency+Classroom+Online+Proficiency:Classroom+Proficiency:Online,data=SolvedEx)
options(scipen=999)
anova(Training_Model_With_Interaction)
summary(Training_Model_With_Interaction)
options(scipen=0)
Training_Model_With_Interaction_Summary <- summary(Training_Model_With_Interaction)
sprintf("End_of_Training=%.3f+%.3f*Proficiency+%.3f*Classroom+%.3f*Online+%.3f*Proficiency*Classroom+%.3f*Proficiency*Online",
        Training_Model_With_Interaction_Summary$coefficients[1,1],
        Training_Model_With_Interaction_Summary$coefficients[2,1],
        Training_Model_With_Interaction_Summary$coefficients[3,1],
        Training_Model_With_Interaction_Summary$coefficients[4,1],
        Training_Model_With_Interaction_Summary$coefficients[5,1],
        Training_Model_With_Interaction_Summary$coefficients[6,1])
fullmodel <- lm(End_of_Training~Proficiency+Classroom+Online+Proficiency*Classroom+Proficiency*Online,data=SolvedEx)
reducedmodel <- lm(End_of_Training~Proficiency+Classroom+Online,data=SolvedEx)
f <- anova(fullmodel)
r <- anova(reducedmodel)
SSE_F <- f$`Sum Sq`[6]
SSE_R <- r$`Sum Sq`[4]
MSE_F <- f$`Mean Sq`[6]
Num <- (SSE_R-SSE_F)/2
Den <- MSE_F
Partial_F_stat <- Num/Den
round(Partial_F_stat,2)
round(pf(Partial_F_stat,2,nrow(SolvedEx)-5-1,lower.tail=FALSE),3)

## Problem2

SolvedEx2 <- read_excel("Used_Cars.xlsx")
View(SolvedEx2)
Age <- SolvedEx2$`Age (years)`
AskingPrice <- SolvedEx2$`Asking Price ($000)`
plot(x=Age,y=AskingPrice,xlab="Age (years)",ylab="Asking Price ($000)",xlim=c(0,20),ylim=c(0,25),main="Plot of Asking Price and Age")
abline(lm(AskingPrice~Age)) # Optional. Only if you want to see the line.
model <- lm(AskingPrice~Age)
summary(model)
residuals <- resid(model)
plot(x=Age,y=residuals,xlab="Age (years)",ylab="Residuals",xlim=c(0,20),ylim=c(-10,10),main="Plot of Residuals by Age (years)")
abline(h=0) ## Optional: add a horizontal line at 0.
logAge <- log(Age)
plot(x=logAge,y=AskingPrice,xlab="Log(Age)",ylab="Asking Price ($000)",xlim=c(0,3),ylim=c(0,25),main="Plot of LogAge and AskingPrice")
abline(lm(AskingPrice~logAge)) # Optional. Only if you want to see the line.
transf_model <- lm(AskingPrice~logAge)
summary(transf_model)
transf_residuals <- resid(transf_model)
plot(x=logAge,y=transf_residuals,xlab="logAge",ylab="Residuals",xlim=c(0,3),ylim=c(-3,3),main="Plot of Residuals by logAge")
abline(h=0) ## Optional: add a horizontal line at 0.
par(mfrow=c(2,2))
plot(x=Age,y=AskingPrice,xlab="Age (years)",ylab="Asking Price ($000)",xlim=c(0,20),ylim=c(0,25),main="Plot of Asking Price and Age")
abline(lm(AskingPrice~Age)) # Optional. Only if you want to see the line.
plot(x=logAge,y=AskingPrice,xlab="Log(Age)",ylab="Asking Price ($000)",xlim=c(0,3),ylim=c(0,25),main="Plot of LogAge and AskingPrice")
abline(lm(AskingPrice~logAge)) # Optional. Only if you want to see the line.
plot(x=Age,y=residuals,xlab="Age (years)",ylab="Residuals",xlim=c(0,20),ylim=c(-10,10),main="Plot of Residuals by Age (years)")
abline(h=0) ## Optional: add a horizontal line at 0.
plot(x=logAge,y=transf_residuals,xlab="logAge",ylab="Transformed Residuals",xlim=c(0,3),ylim=c(-3,3),main="Plot of Transformed Residuals by logAge")
abline(h=0) ## Optional: add a horizontal line at 0.
