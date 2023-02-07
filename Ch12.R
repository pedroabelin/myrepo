library(ggplot2)
library(tidyverse)
library(crosstable)
library(readxl)
library(gmodels)
library(dplyr)
library(stargazer)
library(hrbrthemes)
library(poliscidata)
library(jtools)
library(ggeffects)
library(ggiraphExtra)
library(plyr)
library(plotrix)
library(readxl)
library(psych)
library(haven)
library(lmtest)
library(htmlTable)
library(lm.beta)
library(stringr)
library(car)
library(mfx)
library(aod)
library(xtable)

tab<-read.csv("BushIraq.csv")
tab

#1a

model1<-glm(bushvote04~proiraqwar02, family =binomial(link = "probit"), data = tab)
model1
summary(model1)


stargazer(model1, model2,logit1, title = "Question 1 Models ", allign = TRUE )
xtable(pro1)

model2<-glm(bushvote04~proiraqwar02+party02+bushvote00+ cutrichtaxes02+abortion00, family =binomial(link = "probit"), data = tab)
summary(model2)
stargazer(model2)

#1b

summary(model2)$coefficients

probit<- pnorm(model2$coef[1] + model2$coef[2]*tab$proiraqwar02+model2$coef[3]*tab$party02+model2$coef[4]*tab$bushvote00+model2$coef[5]*tab$cutrichtaxes02+model2$coef[6]*tab$abortion00)
probit
iraqsd<- sd(tab$proiraqwar02, na.rm = TRUE)
probit2 <- pnorm(coef(model2)[1] +coef(model2)[2]*(tab$proiraqwar02 + iraqsd) +coef(model2)[3]*tab$party02 +coef(model2)[4]*tab$bushvote00 +coef(model2)[5]*tab$cutrichtaxes02+
                   coef(model2)[6]*tab$abortion00)
probit2

mean(probit2 - probit, na.rm = T)

#the effect of one standard deviation increase in proiraqwar02 is a 0.22727 decrease in support for Bush.

#1c

probit3<- pnorm(model2$coef[1] + model2$coef[2]*tab$proiraqwar02+model2$coef[3]*tab$party02+model2$coef[4]*tab$bushvote00+model2$coef[5]*tab$cutrichtaxes02+model2$coef[6]*tab$abortion00)
probit3

partsd<-sd(tab$party02, na.rm = TRUE)

probit4 <- pnorm(coef(model2)[1] +
                   coef(model2) [2]*tab$proiraqwar02 +
                   coef(model2)[3]*(tab$party02+partsd) +
                   coef(model2)[4]*tab$bushvote00 +
                   coef(model2)[5]*tab$cutrichtaxes02+
                   coef(model2)[6]*tab$abortion00)
probit4
mean(probit4 - probit, na.rm = T)

#In the party02 model, an increase in a standad deviation in party02 is a 0.0769 increase in support for Bush. Party 02 has a postive effect on Bush support 
#while proiraq war does not.

#1d
pro1<-probitmfx(bushvote04~proiraqwar02+party02+bushvote00+ cutrichtaxes02+abortion00, data = tab, atmean = F)
pro1
#The result of the marginal effect shows smaller values in the effects of the variables. Proiraqwar became positive, while abortion remainded negative, 
#and boshvote00 still has the biggest value.

#1e
logit1<- glm(bushvote04~proiraqwar02+party02+bushvote00+ cutrichtaxes02+abortion00, family =binomial(link = "logit"), data = tab)
summary(logit1)
#of all the variables in this model, only two are statistically insignificant: cutrichtaxes02 and abortion00.
#When comparing the results of the logt model to the probit model, it is possible to observe that both have similar results.
#The magnitude of the intercept is bigger, and the absolute values of cutrichtaxes02 and abortion00 increased in the logit models, while the others decreased.

pro2<-logitmfx(bushvote04~proiraqwar02+party02+bushvote00+ cutrichtaxes02+abortion00, data = tab, atmean = F)
pro2
#The results are very similar 

#1f
stargazer(cor(cbind(model2$fitted.values, logit1$fitted.values)))

#1g
lr =  glm(bushvote04 ~ party02 + bushvote00, data = tab[is.na(tab$proiraqwar02)==0 & is.na(tab$cutrichtaxes02)==0 & is.na(tab$abortion00)==0,], family = binomial(link = "probit"))
summary(lr)

lr2<-2*(logLik(model2)[1]-logLik(lr)[1])
qchisq(0.95,1)
1-pchisq(lr2,1)

lrtest (lr, model2)


#2a

tab2<-read.csv("EnvSurvey.csv")
tab2

stargazer(model3, model4, model5,model6, title = "Question 2 Models PT1 ", allign = TRUE )
#stargazer(model6, model7,  title = "Question 2 Models PT2 ", allign = TRUE )
model3<-lm(tab2$humancause~tab2$male+tab2$white+tab2$educ+tab2$incomecat+tab2$age+tab2$party7)
summary(model3)
#Party identification is the most influential variable on opinion since it has the greatest absolute value.

max(model3$fitted.values)
min(model3$fitted.values)

#The implication is that the fitted values are continuous, while the actual values of humancause are binary (1 or 0).

ggplot(data=tab2, mapping=aes(x=educ, y=humancause)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)

modelsd <- lm(scale(humancause) ~ scale(male) + scale(white) + scale(educ) 
                      + scale(incomecat) + scale(age) + scale(party7), data = tab2)
summary(modelsd)
stargazer(modelsd)

#2b
model4<-glm(humancause~tab2$male+tab2$white+tab2$educ+tab2$incomecat+tab2$age+tab2$party7, family =binomial(link = "probit"), data = tab2)
summary(model4)
#when comparing both models, it is possible to observe that the variables that are significant/insignicant in a model,
#are significant/insignificant in the other. For instance, the gender variable is insignificant in both models.

max(model4$fitted.values)
min(model4$fitted.values)

model5<-glm(humancause~tab2$male+tab2$white+tab2$educ+tab2$incomecat+tab2$age+tab2$agesq+tab2$party7, family =binomial(link = "probit"), data = tab2)
summary(model5)

model6<-lm(tab2$humancause~tab2$male+tab2$white+tab2$educ+tab2$incomecat+tab2$age+tab2$agesq+tab2$party7)
summary(model6)

p4 = pnorm(coef(model5)[1] +
             coef(model5) [2]*tab2$male +
             coef(model5)[3]*tab2$white +
             coef(model5)[4]*tab2$educ +
             coef(model5)[5]*tab2$incomecat+
             coef(model5)[6]*tab2$age +
             coef(model5)[7]*tab2$agesq +
             coef(model5)[8]*tab2$party7)

p5 = pnorm(coef(model5)[1] +
             coef(model5) [2]*tab2$male +
             coef(model5)[3]*tab2$white +
             coef(model5)[4]*tab2$educ +
             coef(model5)[5]*tab2$incomecat+
             coef(model5)[6]*tab2$age +
             coef(model5)[7]*tab2$agesq +
             coef(model5)[8]*(tab2$party7 + 1))

mean(p5 - p4, na.rm = TRUE) 

probitmfx(model5, data = tab2, atmean = FALSE)

#The results are the same for discrete differences and marginal effects.
#the results across the interpretations is very similar considering the magnitude of the effect, even when considering the signs.

male = pnorm(coef(model5)[1]+
               coef(model5) [2]* 1 +
               coef(model5)[3]*tab2$white +
               coef(model5)[4]*tab2$educ +
               coef(model5)[5]*tab2$incomecat+
               coef(model5)[6]*tab2$age +
               coef(model5)[7]*tab2$agesq +
               coef(model5)[8]*tab2$party7)

female = pnorm(coef(model5)[1]+
                 coef(model5) [2]*  +
                 coef(model5)[3]*tab2$white +
                 coef(model5)[4]*tab2$educ +
                 coef(model5)[5]*tab2$incomecat+
                 coef(model5)[6]*tab2$age +
                 coef(model5)[7]*tab2$agesq +
                 coef(model5)[8]*tab2$party7)

mean(male - female, na.rm = TRUE)
#the marginal effect result, the discrete differences and LPM results on the effect of gender on human case is very similar.

#2c
#tab2$t1=tab2$treatment=1
#tab2$t2=tab2$treatment=2
#tab2$t3=tab2$treatment=3
#tab2$t4=tab2$treatment=4

model7<-glm(warmagree~tab2$treatment, family =binomial(link = "probit"), data = tab2)
summary(model7)

model7<-glm(warmagree~tab2$t1+tab2$t2+tab2$t3+tab2$t4, family =binomial(link = "probit"), data = tab2)

tab2$treat1 <- 0
tab2$treat1[tab2$treatment == 1] <- 1

tab2$treat2 <- 0
tab2$treat2[tab2$treatment == 2] <- 1

tab2$treat3 <- 0
tab2$treat3[tab2$treatment == 3] <- 1

tab2$treat4 <- 0
tab2$treat4[tab2$treatment == 4] <- 1


model19 <- glm(warmagree ~ treat1 + treat2 + treat3, family = binomial(link = "probit"),data = tab2)
stargazer(summary(model19)$coefficients)
model19$coefficients


tab2$t1 = (tab2$treatment == 1)
tab2$t2 = (tab2$treatment == 2)
tab2$t3 = (tab2$treatment == 3)

# probit model
modl <- glm(warmagree ~ t1 + t2 + t3, family = binomial(link = "probit"), data = tab2)
summary(modl)
#The effect of the predictors on the independent variable is that an increase in the predictor, leads to a decrease of 0.04672 in the depend variable. 
#This means that the higher the number of the identification of the treatment, it leads to a decrease in agreeing to global warming.

#3a
stargazer(model8, model9,  title = "Question 2 Models PT1 ", allign = TRUE )
stargazer(model10, model11,  title = "Question 2 Models PT1 ", allign = TRUE )

tab3<-read.csv("Football coaches.csv")
tab3

model8<-glm(FiredCoach~tab3$WinPct, family =binomial(link = "probit"), data = tab3)
summary(model8)

model9<-lm(tab3$FiredCoach~tab3$WinPct)
summary(model9)

 
prob.lpm <- predict(model9)
prob <- pnorm(model8$coef[1] + model8$coef[2]*tab3$WinPct)

prob.lpm
prob 
plot1 <- ggplot(tab3, aes(WinPct, FiredCoach)) +
  geom_line(aes(y = prob)) +
  geom_line(aes(y = prob.lpm)) + 
  ylab("Probability that coach is fired") + 
  xlab("Team's winning percentage")

ggsave("ploto2.pdf", plot1, path = "/Users/suava/Desktop/R")
"C:\Users\suava\Desktop\R"

#3b
model9<-glm(tab3$FiredCoach~tab3$WinPct+tab3$LagWinPct+tab3$NewCoach+tab3$ScheduleStrength+tab3$Tenure, family =binomial(link = "probit"), data = tab3)
summary(model9)

model10<-glm(tab3$FiredCoach~tab3$WinPct+tab3$LagWinPct+tab3$NewCoach+tab3$ScheduleStrength+tab3$Tenure, family =binomial(link = "logit"), data = tab3)
summary(model10)

model11<-lm(tab3$FiredCoach~tab3$WinPct+tab3$LagWinPct+tab3$NewCoach+tab3$ScheduleStrength+tab3$Tenure)
summary(model11)

#Across models, the only variables that are not statistically significant are ScheduleStrength and Tenure. The coefficients are not hat substantially different, with the 
#Exception of WinPct. The z values in the probit and logit models are similar.

#3c

min(model9$fitted.values)
max(model9$fitted.values)
min(model10$fitted.values)
max(model10$fitted.values)
min(model11$fitted.values)
max(model11$fitted.values)

#Across models, model9 (probit) has the lowest min at 7.76994e-05, logit has the min that is in the middle 7.76994e-05. and LPM has the highest min, in absolute value
#at -0.3132691. The max of logit and probit and logit are very similar with each having 0.0009644091 and 0.8907037 respectively, and LPM has the lowest max at 0.663271.

#3d
stargazer(cor(cbind(model9$fitted.values, model10$fitted.values, model11$fitted.values)))
#The values in the correlation model are very close across probit, logit, and LPM. All of them are close to 0.

#3e
tab3$coachd<-tab3$LagWinPct*tab3$NewCoach

#model12<-glm(tab3$FiredCoach~tab3$LagWinPct+tab3$coachd, family =binomial(link = "probit"), data = tab3)
#stargazer(model12)
#summary(model12)
#wald.test(Sigma = vcov(model12), b = coef(model12), Terms = 2:3)
#we fail to reject the null hypothesis since the p-value is 4.1e-07. Lagged winning percentage has no effect on new coaches.


tab3$Minb <-tab3$LagWinPct-tab3$coachd

#restricted
model17res=  glm(FiredCoach ~ WinPct + Minb+ NewCoach + ScheduleStrength + Tenure, data = tab3, family = binomial(link = "probit"))
summary(model17res)

model18un = glm(FiredCoach ~ WinPct + LagWinPct + coachd + NewCoach + ScheduleStrength + Tenure, data = tab3, family = binomial(link = "probit"))
summary(model18un)

stargazer(model17res,model18un,  title = "Question 3E Models ", allign = TRUE )


#wald test

linearHypothesis(model18un, "LagWinPct + coachd = 0")
#fail to reject the null 

#lr test

lrtest(model17res, model18un)
#fail to reject the null

#4a
stargazer(model13, model14,  title = "Question 2 Models PT1 ", allign = TRUE )
stargazer(model15, model16,  title = "Question 2 Models PT1 ", allign = TRUE )
tab3<-read.csv("kalla-broockman-donor-access-2013-data.csv")
tab3

#Yes, there is a risk of endogeneity because people who met with a member of congress and also donated money could be friends,
#or there is also the possibility that the member of congress met with powerful individual who did not donate money to them.

#4b

tab3$con <- ifelse(tab3$staffrank == '5', 1, 0)
tab3$con

model13<-glm(tab3$con~tab3$treat_donor, family =binomial(link = "probit"), data = tab3)
summary(model13)
#A point increase in donor means a 0.5663 increase in the probability of meeting a congressman

#4c
#The factors missing from the model are the amount of money donated, a dummy variable on political influence of the activists, among others. The omission of this variables creates a bias.

#4d
model13<-glm(tab3$con~tab3$treat_donor, family =binomial(link = "probit"), data = tab3)
summary(model13)

model14<-lm(tab3$con~tab3$treat_donor)
summary(model14)

#The LPM model shows that a increase in treat donor, means a 0.05450  unit increase in seeing a congress member.

stargazer(cor(cbind(model13$fitted.values, model14$fitted.values)))


#The correlation is 1 across variables and models.This means that the fitted values across models are identical.

#4e
model15<-lm(tab3$staffrank>2~tab3$treat_donor)
summary(model15)
#The probability of meeting a senior staffer increases by 0.13238 for every unit increase in donor. This means that it is difficult to 
#for a person to meet them.

#4f
model16<-lm(tab3$staffrank==1~tab3$treat_donor)
summary(model16)
#The probability of meeting a non policy staff decreases by 0.01661 for every unit increase in donor. This means that a donor will meet with someone higher in the ranks.



