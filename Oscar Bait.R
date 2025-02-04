library("ggplot2")

#RESET PLOT WINDOW
par(mfrow=c(1,1))

#Read in Data Set
oscar_bait <- read.csv("Oscar Bait_NEW.csv")

################################################################################

#Create Suite of Histograms for Winners
par(mfrow=c(3,3))
hist(oscar_bait$MOSTNOMS, main="Total Nominations")
hist(oscar_bait$DGAWINS, main="DGA Wins")
hist(oscar_bait$GGWIN, main="GG Best Picture")
hist(oscar_bait$GGDIRWINS, main="GG Best Directing")
hist(oscar_bait$PREVACTWINS, main="Previous Acting Win")
hist(oscar_bait$PREVDIRNOMS, main="Previous Directing Nom")
hist(oscar_bait$MOSTPREVDIRNOMS, main="Most Directing Noms")
hist(oscar_bait$GGACTWINS, main="GG Best Actor/Actress")
hist(oscar_bait$SAGWINS, main="SAG Awards")

#Create Suite of Histograms for Demographic Information
par(mfrow=c(3,3))
hist(oscar_bait$BIOP, main="Biopic")
hist(oscar_bait$EPICBIOP, main="Epic Biopic")
hist(oscar_bait$FOREIGNLANG, main="Foreign Language")
hist(oscar_bait$BECHDEL, main="Bechdel Test")
hist(oscar_bait$TWOLEAD, main="Multiple Leads")
hist(oscar_bait$GENDER, main="Cast Gender")
hist(oscar_bait$LEADGENDER, main="Lead Gender(s)")
hist(oscar_bait$RACE, main="Cast Race")

#Create Suite of Histograms for Themeing Information
par(mfrow=c(2,4))
hist(oscar_bait$TRAUMA, main="Trauma")
hist(oscar_bait$RACISM, main="Racism")
hist(oscar_bait$SEXISM, main="Sexism")
hist(oscar_bait$CLASSISM, main="Classism")
hist(oscar_bait$LGBTQ, main="LGBTQIA+")
hist(oscar_bait$LOVE, main="Romantic Love")
hist(oscar_bait$DISABILITY, main="Disability")

################################################################################

#Calculate Betahat
Y_oscar <- oscar_bait$WIN

X_oscar <- cbind(1, oscar_bait$MOSTNOMS, oscar_bait$DGAWINS, oscar_bait$GGWIN, 
                 oscar_bait$GGDIRWINS, oscar_bait$PREVACTWINS, 
                 oscar_bait$PREVDIRNOMS, oscar_bait$MOSTPREVDIRNOMS, 
                 oscar_bait$BIOP, oscar_bait$EPICBIOP, 
                 oscar_bait$SAGWINS, oscar_bait$GGACTWINS, 
                 oscar_bait$FOREIGNLANG, oscar_bait$BECHDEL, oscar_bait$TWOLEAD,
                 oscar_bait$GENDER, oscar_bait$LEADGENDER, oscar_bait$RACE,
                 oscar_bait$TRAUMA, oscar_bait$RACISM, oscar_bait$SEXISM, 
                 oscar_bait$CLASSISM, oscar_bait$LGBTQ, oscar_bait$LOVE, 
                 oscar_bait$DISABILITY)

betahat_allvar = solve(t(X_oscar) %*% X_oscar, t(X_oscar) %*% Y_oscar)
betahat_allvar

################################################################################

#Conversion of all Kaplan dummy variables to categorical for logit & regression
oscar_bait$MOSTNOMS <- factor(oscar_bait$MOSTNOMS)
oscar_bait$DGAWINS <- factor(oscar_bait$DGAWINS)
oscar_bait$GGWIN <- factor(oscar_bait$GGWIN)
oscar_bait$GGDIRWINS <- factor(oscar_bait$GGDIRWINS)
oscar_bait$MOSTPREVDIRNOMS <- factor(oscar_bait$MOSTPREVDIRNOMS)
oscar_bait$BIOP <- factor(oscar_bait$BIOP)
oscar_bait$EPICBIOP <- factor(oscar_bait$EPICBIOP)

kaplan_reg <- lm(WIN ~ MOSTNOMS + DGAWINS + GGWIN + GGDIRWINS + 
                      MOSTPREVDIRNOMS + BIOP + EPICBIOP + PREVACTWINS, 
                    data=oscar_bait)

kaplan_logit <- glm(WIN ~ MOSTNOMS + DGAWINS + GGWIN + GGDIRWINS + 
                   MOSTPREVDIRNOMS + BIOP + EPICBIOP + PREVACTWINS, 
                 data=oscar_bait, family="binomial")


summary(kaplan_reg)

summary(kaplan_logit)

################################################################################

#Conversion of all demographic dummy variables to categorical for logit & 
#regression
oscar_bait$FOREIGNLANG <- factor(oscar_bait$FOREIGNLANG)
oscar_bait$BECHDEL <- factor(oscar_bait$BECHDEL)
oscar_bait$TWOLEAD <- factor(oscar_bait$TWOLEAD)
oscar_bait$GENDER <- factor(oscar_bait$GENDER)
oscar_bait$LEADGENDER <- factor(oscar_bait$LEADGENDER)
oscar_bait$RACE <- factor(oscar_bait$RACE)

demo_reg <- lm(WIN ~ FOREIGNLANG + BECHDEL + TWOLEAD + GENDER + LEADGENDER +
                    RACE, data=oscar_bait)

demo_logit <- glm(WIN ~ FOREIGNLANG + BECHDEL + TWOLEAD + GENDER + LEADGENDER +
                    RACE, data=oscar_bait, family="binomial")

summary(demo_reg)

summary(demo_logit)

################################################################################

#Conversion of all thematic dummy variables to categorical for logit & 
#regression
oscar_bait$TRAUMA <- factor(oscar_bait$TRAUMA)
oscar_bait$RACISM <- factor(oscar_bait$RACISM)
oscar_bait$SEXISM <- factor(oscar_bait$SEXISM)
oscar_bait$CLASSISM <- factor(oscar_bait$CLASSISM)
oscar_bait$LGBTQ <- factor(oscar_bait$LGBTQ)
oscar_bait$LOVE <- factor(oscar_bait$LOVE)
oscar_bait$DISABILITY <- factor(oscar_bait$DISABILITY)

theme_reg <- lm(WIN ~ TRAUMA + RACISM + SEXISM + CLASSISM + LGBTQ + LOVE + 
                     DISABILITY, data=oscar_bait)

theme_logit <- glm(WIN ~ TRAUMA + RACISM + SEXISM + CLASSISM + LGBTQ + LOVE + 
                     DISABILITY, data=oscar_bait, family="binomial")

summary(theme_logit)
summary(theme_reg)

################################################################################

#Conversion of remaining dummy variables to categorical for logit & regression
oscar_bait$GGACTWINS <- factor(oscar_bait$GGACTWINS)

oscar_reg <- lm(WIN ~ MOSTNOMS + DGAWINS + GGWIN + GGDIRWINS + PREVACTWINS + 
                      PREVDIRNOMS + MOSTPREVDIRNOMS + BIOP + EPICBIOP +
                      SAGWINS + GGACTWINS + FOREIGNLANG + BECHDEL + TWOLEAD + 
                      GENDER + LEADGENDER + RACE + TRAUMA + RACISM + SEXISM + 
                      CLASSISM + LGBTQ + LOVE + DISABILITY, data=oscar_bait)

oscar_logit <- glm(WIN ~ MOSTNOMS + DGAWINS + GGWIN + GGDIRWINS + PREVACTWINS + 
                     PREVDIRNOMS + MOSTPREVDIRNOMS + BIOP + EPICBIOP +
                     SAGWINS + GGACTWINS + FOREIGNLANG + BECHDEL + TWOLEAD + 
                     GENDER + LEADGENDER + RACE + TRAUMA + RACISM + SEXISM + 
                     CLASSISM + LGBTQ + LOVE + DISABILITY, data=oscar_bait, 
                     family="binomial")

oscar_logit$fitted.values

summary(oscar_reg)
summary(oscar_logit)

################################################################################

#Error Assumption Checking
par(mfrow=c(1, 2))
qqnorm(oscar_reg$residuals)
qqnorm(oscar_logit$residuals)
plot(oscar_reg$fitted.values, oscar_reg$residuals)
plot(oscar_logit$fitted.values, oscar_logit$residuals)

qqnorm(theme_reg$residuals)
qqnorm(theme_logit$residuals)
plot(theme_reg$fitted.values, theme_reg$residuals)
plot(theme_logit$fitted.values, theme_logit$residuals)

qqnorm(demo_reg$residuals)
qqnorm(demo_logit$residuals)
plot(demo_reg$fitted.values, demo_reg$residuals)
plot(demo_logit$fitted.values, demo_logit$residuals)

qqnorm(kaplan_reg$residuals)
qqnorm(kaplan_logit$residuals)
plot(kaplan_reg$fitted.values, kaplan_reg$residuals)
plot(kaplan_logit$fitted.values, kaplan_logit$residuals)

#All residuals plots show linear relationship(s); linear regression is not good
#for prediction. All logistic plots for the reduced models show a linear
#association in the upper quadrant and a negative quadratic relationship as well

################################################################################

#Calculate VIF for Multicollinearity
library(car)
vif_full <- vif(oscar_reg)
vif_kaplan <- vif(kaplan_reg)
vif_demo <- vif(demo_reg)
vif_theme <- vif(theme_reg)

par(mfrow=c(1,1))
barplot(t(vif_full[1:24]), main = "VIF Values for the Full Model", 
        horiz = TRUE, xlim=c(0, 6))
abline(v = 5, lwd = 3, lty = 2)

barplot(t(vif_kaplan[1:8]), main = "VIF Values for the Kaplan Model", 
        horiz = TRUE, xlim=c(0, 6))
abline(v = 5, lwd = 3, lty = 2)

barplot(t(vif_demo[1:6]), main = "VIF Values for the Demographic Model", 
        horiz = TRUE, xlim=c(0, 6))
abline(v = 5, lwd = 3, lty = 2)

barplot(t(vif_theme[1:7]), main = "VIF Values for the Themeing Model", 
        horiz = TRUE, xlim=c(0, 6))
abline(v = 5, lwd = 3, lty = 2)

#No concerning Multicollinearity in any models! Thus, we can conclude the
#associations between models once cherry-picked does not translate to the full
#model and skews the data. They are independent observations that can be 
#commented on. 

################################################################################
library("glmnet")
library("caret")

lasso_xmat <- makeX(oscar_bait)

lasso_y <- oscar_bait$WIN
lasso_x <- data.matrix(lasso_xmat)

lasso_model <- cv.glmnet(lasso_x, lasso_y, alpha=1)
best_lambda <- lasso_model$lambda.min

best_model <- glmnet(lasso_x, lasso_y, alpha = 1, lambda = best_lambda)
coef(best_model)

#None of these variables are significant under LASSO - this confirms a poor
#predictive model. This does not necessarily mean the regressors don't have an
#association with winning the Oscars, just that they are not significant 
#enough to predict the outcome

################################################################################

#calculate msres
par(mfrow=c(1,1))
oscar_res <- oscar_reg$residuals

oscar_MSres <- (sum(oscar_res^2))/(length(oscar_res))

oscar_stand_res <- oscar_res/(sqrt(oscar_MSres))

plot(oscar_reg$fitted.values, oscar_stand_res, 
     ylab="Standardized Residuals, d", xlab="Fitted Values, y_hat")

################################################################################

#Assuming every variable is equally weighted and all beneficial
aggregate_oscar <- aggregate(WIN ~ MOSTNOMS + DGAWINS + GGWIN + GGDIRWINS + 
                               MOSTPREVDIRNOMS + BIOP + EPICBIOP + PREVACTWINS + 
                               SAGWINS + GGACTWINS + FOREIGNLANG + BECHDEL + 
                               TWOLEAD + GENDER + LEADGENDER + RACE + TRAUMA + 
                               RACISM + SEXISM + CLASSISM + LGBTQ + LOVE + 
                               DISABILITY, data=oscar_bait, FUN=mean)

ggplot(oscar_bait, aes(x=YEAR, y=PROB)) + geom_line() + 
  ggtitle("Proability of each Winning Film Securing its Oscar")

ggplot(oscar_bait, aes(x=YEAR, y=ADJPROB)) + geom_line() + 
  ggtitle("Adjusted Proability of each Winning Film Securing its Oscar")

ggplot(oscar_bait, aes(x=YEAR, y=THEMEONLY)) + geom_line() +
  ggtitle("Pecentage of Themes for Each Movie (X/7)")

################################################################################

#AIC for Forward Stepwise Selection
library(AICcmodavg)

om1 <- lm(WIN~MOSTNOMS, data=oscar_bait)
om2 <- lm(WIN~MOSTNOMS + DGAWINS, data=oscar_bait)
om3 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN, data=oscar_bait)
om4 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS, data=oscar_bait)
om5 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS, data=oscar_bait)
om6 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP, 
          data=oscar_bait)
om7 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP, 
          data=oscar_bait)
om8 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
            PREVACTWINS, data=oscar_bait)
om9 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
            PREVACTWINS+SAGWINS, data=oscar_bait)
om10 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
            PREVACTWINS+SAGWINS+GGACTWINS, data=oscar_bait)
om11 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
            PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG, data=oscar_bait)
om12 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
            PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL, data=oscar_bait)
om13 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
            PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD, 
           data=oscar_bait)
om14 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER, 
           data=oscar_bait)
om15 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER, data=oscar_bait)
om16 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE, data=oscar_bait)
om17 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE+TRAUMA, data=oscar_bait)
om18 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE+TRAUMA+RACISM, data=oscar_bait)
om19 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE+TRAUMA+RACISM+SEXISM, data=oscar_bait)
om20 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE+TRAUMA+RACISM+SEXISM+CLASSISM, data=oscar_bait)
om21 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE+TRAUMA+RACISM+SEXISM+CLASSISM+LGBTQ, 
           data=oscar_bait)
om22 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE+TRAUMA+RACISM+SEXISM+CLASSISM+LGBTQ+LOVE, 
           data=oscar_bait)
om23 <- lm(WIN~MOSTNOMS+DGAWINS+GGWIN+GGDIRWINS+MOSTPREVDIRNOMS+BIOP+EPICBIOP+
             PREVACTWINS+SAGWINS+GGACTWINS+FOREIGNLANG+BECHDEL+TWOLEAD+GENDER+
             LEADGENDER+RACE+TRAUMA+RACISM+SEXISM+CLASSISM+LGBTQ+LOVE+
             DISABILITY, data=oscar_bait)

aic1 <- extractAIC(om1, k=3)
aic2 <- extractAIC(om2, k=4)
aic3 <- extractAIC(om3, k=5)
aic4 <- extractAIC(om4, k=6)
aic5 <- extractAIC(om5, k=7)
aic6 <- extractAIC(om6, k=8)
aic7 <- extractAIC(om7, k=9)
aic8 <- extractAIC(om8, k=10)
aic9 <- extractAIC(om9, k=11)
aic10 <- extractAIC(om10, k=12)
aic11 <- extractAIC(om11, k=13)
aic12 <- extractAIC(om12, k=14)
aic13 <- extractAIC(om13, k=15)
aic14 <- extractAIC(om14, k=16)
aic15 <- extractAIC(om15, k=17)
aic16 <- extractAIC(om16, k=18)
aic17 <- extractAIC(om17, k=19)
aic18 <- extractAIC(om18, k=20)
aic19 <- extractAIC(om19, k=21)
aic20 <- extractAIC(om20, k=22)
aic21 <- extractAIC(om21, k=23)
aic22 <- extractAIC(om22, k=24)
aic23 <- extractAIC(om23, k=25)

aicx <- c(aic1[1], aic2[1], aic3[1], aic4[1], aic5[1], aic6[1], aic7[1], 
          aic8[1], aic9[1], aic10[1], aic11[1], aic12[1], aic13[1], aic14[1], 
          aic15[1], aic16[1], aic17[1], aic18[1], aic19[1], aic20[1], aic21[1],
          aic22[1], aic23[1])

aicy <- c(aic1[2], aic2[2], aic3[2], aic4[2], aic5[2], aic6[2], aic7[2], 
          aic8[2], aic9[2], aic10[2], aic11[2], aic12[2], aic13[2], aic14[2], 
          aic15[2], aic16[2], aic17[2], aic18[2], aic19[2], aic20[2], aic21[2],
          aic22[2], aic23[2])

plot(aicx, aicy, ylab="AIC", xlab="k")

################################################################################

seq.models <- list(om1, om2, om3, om4, om5, om6, om7, om8, om9, om10, om11, om12,
               om13, om14, om15, om16, om17, om18, om19, om20, om21, om22, om23)
seq.names <- c('om1', 'om2', 'om3', 'om4', 'om5', 'om6', 'om7', 'om8', 'om9', 
              'om10', 'om11', 'om12', 'om13', 'om14', 'om15', 'om16', 'om17', 
              'om18', 'om19', 'om20', 'om21', 'om22', 'om23')

aictab(cand.set = seq.models, modnames = seq.names)

AICclist <- c(89.51, 91.43, 92.35, 94.08, 95.98, 98.29, 100.65, 101.90, 103.91,
              104.65, 106.74, 109.07, 110.78, 113.32, 117.47, 120.20, 120.44, 
              121.85, 124.80, 127.92, 131.12, 134.26, 138.98)

################################################################################

rsquaredlist <- c(summary(om1)$r.squared,summary(om2)$r.squared,
                     summary(om3)$r.squared,summary(om4)$r.squared,
                     summary(om5)$r.squared,summary(om6)$r.squared,
                     summary(om7)$r.squared,summary(om8)$r.squared,
                     summary(om9)$r.squared,summary(om10)$r.squared,
                     summary(om11)$r.squared,summary(om12)$r.squared,
                     summary(om13)$r.squared,summary(om14)$r.squared,
                     summary(om15)$r.squared,summary(om16)$r.squared,
                     summary(om17)$r.squared,summary(om18)$r.squared,
                     summary(om19)$r.squared,summary(om20)$r.squared,
                     summary(om21)$r.squared,summary(om22)$r.squared,
                     summary(om23)$r.squared)

adjrsquaredlist <- c(summary(om1)$adj.r.squared,summary(om2)$adj.r.squared,
                  summary(om3)$adj.r.squared,summary(om4)$adj.r.squared,
                  summary(om5)$adj.r.squared,summary(om6)$adj.r.squared,
                  summary(om7)$adj.r.squared,summary(om8)$adj.r.squared,
                  summary(om9)$adj.r.squared,summary(om10)$adj.r.squared,
                  summary(om11)$adj.r.squared,summary(om12)$adj.r.squared,
                  summary(om13)$adj.r.squared,summary(om14)$adj.r.squared,
                  summary(om15)$adj.r.squared,summary(om16)$adj.r.squared,
                  summary(om17)$adj.r.squared,summary(om18)$adj.r.squared,
                  summary(om19)$adj.r.squared,summary(om20)$adj.r.squared,
                  summary(om21)$adj.r.squared,summary(om22)$adj.r.squared,
                  summary(om23)$adj.r.squared)

plot(aicx, AICclist, ylab="AICc", xlab="k")

plot(aicx, adjrsquaredlist, ylab="Adjusted R^2", xlab="Number of Parameters")

plot(aicx, rsquaredlist, ylab="R^2", xlab="Number of Parameters")

################################################################################

omt <- lm(WIN~MOSTNOMS+TRAUMA, data=oscar_bait)
omr <- lm(WIN~MOSTNOMS+RACISM, data=oscar_bait)
oms <- lm(WIN~MOSTNOMS+SEXISM, data=oscar_bait)
omc <- lm(WIN~MOSTNOMS+CLASSISM, data=oscar_bait)
omlgbt <- lm(WIN~MOSTNOMS+LGBTQ, data=oscar_bait)
oml <- lm(WIN~MOSTNOMS+LOVE, data=oscar_bait)
omd <- lm(WIN~MOSTNOMS+DISABILITY, data=oscar_bait)

theme.models <- list(om1, omt, omr, oms, omc, omlgbt, oml, omd)
theme.names <- c('om1', 'omt', 'omr', 'oms', 'omc', 'omlgbt', 'oml', 'omd')

aictab(cand.set = theme.models, modnames = theme.names)

#Each theme has a minimal effect on the AIC

################################################################################

summary(om1)$adj.r.squared
summary(omt)$adj.r.squared
summary(omr)$adj.r.squared
summary(oms)$adj.r.squared
summary(omlgbt)$adj.r.squared
summary(oml)$adj.r.squared
summary(omd)$adj.r.squared
#OMT maximizes @ 0.08

################################################################################

omtr <- lm(WIN~MOSTNOMS+TRAUMA+RACISM, data=oscar_bait)
omts <- lm(WIN~MOSTNOMS+TRAUMA+SEXISM, data=oscar_bait)
omtc <- lm(WIN~MOSTNOMS+TRAUMA+CLASSISM, data=oscar_bait)
omtlgbt <- lm(WIN~MOSTNOMS+TRAUMA+LGBTQ, data=oscar_bait)
omtl <- lm(WIN~MOSTNOMS+TRAUMA+LOVE, data=oscar_bait)
omtd <- lm(WIN~MOSTNOMS+TRAUMA+DISABILITY, data=oscar_bait)

twotheme.models <- list(omtr, omts, omtc, omtlgbt, omtl, omtd)
twotheme.names <- c('omtr', 'omts', 'omtc', 'omtlgbt', 'omctl', 'omtd')

aictab(cand.set = twotheme.models, modnames = twotheme.names)

summary(omtc)$adj.r.squared
summary(omtr)$adj.r.squared
summary(omts)$adj.r.squared
summary(omtlgbt)$adj.r.squared
summary(omtl)$adj.r.squared

#R^2 maximized @0.078 with omtlgbt

################################################################################

meant <- mean(as.numeric(oscar_bait$TRAUMA))
meanr <- mean(as.numeric(oscar_bait$RACISM))
meanc <- mean(as.numeric(oscar_bait$CLASSISM))
means <- mean(as.numeric(oscar_bait$SEXISM))
meanlgbt <- mean(as.numeric(oscar_bait$LGBTQ))
meanl <- mean(as.numeric(oscar_bait$LOVE))
meand <- mean(as.numeric(oscar_bait$DISABILITY))

c(meant, meanr, meanc, means, meanlgbt, meanl, meand)

#Most movies contain themes of love, then trauma, disability, racism, classism
#sexism, and lastly LGBTQIA+ representation

################################################################################

#Calculate AIC for individual models

omnom <- lm(WIN~MOSTNOMS, data=oscar_bait)
omdga <- lm(WIN~DGAWINS, data=oscar_bait)
omggwin <- lm(WIN~GGWIN, data=oscar_bait)
omggdir <- lm(WIN~GGDIRWINS, data=oscar_bait)
omprevdir <- lm(WIN~MOSTPREVDIRNOMS, data=oscar_bait)
ombiop <- lm(WIN~BIOP, data=oscar_bait)
omepic <- lm(WIN~EPICBIOP, data=oscar_bait)
omprevact <- lm(WIN~PREVACTWINS, data=oscar_bait)
omsag <- lm(WIN~SAGWINS, data=oscar_bait)
omggact <- lm(WIN~GGACTWINS, data=oscar_bait)
omfl <- lm(WIN~FOREIGNLANG, data=oscar_bait)
ombech <- lm(WIN~BECHDEL, data=oscar_bait)
omtwo <- lm(WIN~TWOLEAD, data=oscar_bait)
omgender <- lm(WIN~GENDER, data=oscar_bait)
omlead <- lm(WIN~LEADGENDER, data=oscar_bait)
ompoc <- lm(WIN~RACE, data=oscar_bait)
omtrauma <- lm(WIN~TRAUMA, data=oscar_bait)
omracism <- lm(WIN~RACISM, data=oscar_bait)
omsexism <- lm(WIN~SEXISM, data=oscar_bait)
omclassism <- lm(WIN~CLASSISM, data=oscar_bait)
omlgbtq <- lm(WIN~LGBTQ, data=oscar_bait)
omlove <- lm(WIN~LOVE, data=oscar_bait)
omdisability <- lm(WIN~DISABILITY, data=oscar_bait)

indvi.models <- list(omnom, omdga, omggwin, omggdir, omprevdir, ombiop, omepic,
                     omprevact, omsag, omggact, omfl, ombech, omtwo, omgender,
                     omlead, ompoc, omtrauma, omracism, omsexism, omclassism,
                     omlgbtq, omlove, omdisability)

indvi.names <- c('omnom', 'omdga', 'omggwin', 'omggdir', 'omprevdir', 'ombiop',
                 'omepic', 'omprevact', 'omsag', 'omggact', 'omfl', 'ombech', 
                 'omtwo', 'omgender', 'omlead', 'ompoc', 'omtrauma', 'omracism', 
                 'omsexism', 'omclassism', 'omlgbtq', 'omlove', 'omdisability')

aictab(cand.set = indvi.models, modnames = indvi.names)

#Individual variables minimize AIC with nominations, maximize with the lead
#gender

################################################################################

#Calculate R^2 and adj R^2 for each individual model

summary(omnom)$r.squared
summary(omdga)$r.squared
summary(omggwin)$r.squared
summary(omggdir)$r.squared
summary(omprevdir)$r.squared
summary(ombiop)$r.squared
summary(omepic)$r.squared
summary(omprevact)$r.squared
summary(omsag)$r.squared
summary(omggact)$r.squared
summary(omfl)$r.squared
summary(ombech)$r.squared
summary(omtwo)$r.squared
summary(omgender)$r.squared
summary(omlead)$r.squared
summary(ompoc)$r.squared
summary(omtrauma)$r.squared
summary(omracism)$r.squared
summary(omsexism)$r.squared
summary(omclassism)$r.squared
summary(omlgbtq)$r.squared
summary(omlove)$r.squared
summary(omdisability)$r.squared

summary(omnom)$adj.r.squared
summary(omdga)$adj.r.squared
summary(omggwin)$adj.r.squared
summary(omggdir)$adj.r.squared
summary(omprevdir)$adj.r.squared
summary(ombiop)$adj.r.squared
summary(omepic)$adj.r.squared
summary(omprevact)$adj.r.squared
summary(omsag)$adj.r.squared
summary(omggact)$adj.r.squared
summary(omfl)$adj.r.squared
summary(ombech)$adj.r.squared
summary(omtwo)$adj.r.squared
summary(omgender)$adj.r.squared
summary(omlead)$adj.r.squared
summary(ompoc)$adj.r.squared
summary(omtrauma)$adj.r.squared
summary(omracism)$adj.r.squared
summary(omsexism)$adj.r.squared
summary(omclassism)$adj.r.squared
summary(omlgbtq)$adj.r.squared
summary(omlove)$adj.r.squared
summary(omdisability)$adj.r.squared
#OMNOM and OMGGACT maximize adj R^2  and R^2 @ .078 and .0212 (.086 and .0299)

################################################################################

summary(lm(as.numeric(oscar_bait$RACISM) ~ as.numeric(oscar_bait$FOREIGNLANG)))
#0.184, Significant at 90%
summary(lm(as.numeric(oscar_bait$RACISM) ~ as.numeric(oscar_bait$RACE)))
#0.62, Significant 100%
summary(lm(as.numeric(oscar_bait$RACISM) ~ as.numeric(oscar_bait$CLASSISM)))
#0.22, Significant at 95%

summary(lm(as.numeric(oscar_bait$CLASSISM) ~ as.numeric(oscar_bait$RACE)))
#0.3058, Significant at 95%
summary(lm(as.numeric(oscar_bait$CLASSISM) ~ as.numeric(oscar_bait$RACISM)))
#0.21, Significant at 95%
summary(lm(as.numeric(oscar_bait$CLASSISM) ~ 
             as.numeric(oscar_bait$FOREIGNLANG)))
#0.195, Significant at 95%

summary(lm(as.numeric(oscar_bait$SEXISM) ~ as.numeric(oscar_bait$GENDER)))
#0.34, Significant at 99%
summary(lm(as.numeric(oscar_bait$SEXISM) ~ as.numeric(oscar_bait$LEADGENDER)))
#0.062, Significant at 90%

summary(lm(as.numeric(oscar_bait$TRAUMA) ~ as.numeric(oscar_bait$LEADGENDER)))
#-0.11, Significant at 95%
summary(lm(as.numeric(oscar_bait$TRAUMA) ~ as.numeric(oscar_bait$RACE)))
#0.29, Significant at 90%
summary(lm(as.numeric(oscar_bait$TRAUMA) ~ as.numeric(oscar_bait$GENDER)))
#-0.165, Not Significant

summary(lm(as.numeric(oscar_bait$LOVE) ~ as.numeric(oscar_bait$LEADGENDER)))
#0.19, Significant at 100%
summary(lm(as.numeric(oscar_bait$LOVE) ~ as.numeric(oscar_bait$GENDER)))
#0.21, Not significant
summary(lm(as.numeric(oscar_bait$LOVE) ~ as.numeric(oscar_bait$LGBTQ)))
#0.16, Not significant
summary(lm(as.numeric(oscar_bait$LOVE) ~ as.numeric(oscar_bait$DISABILITY)))
#0.00041, Not Significant
summary(lm(as.numeric(oscar_bait$LOVE) ~ as.numeric(oscar_bait$RACE)))
#0.1592, Not Significant 


summary(lm(as.numeric(oscar_bait$LEADGENDER) ~ as.numeric(oscar_bait$LGBTQ)))
#-0.138, Not Significant
summary(lm(as.numeric(oscar_bait$LEADGENDER) ~ as.numeric(oscar_bait$GENDER)))
#0.9185, Significant at 100%
summary(lm(as.numeric(oscar_bait$LEADGENDER) ~ as.numeric(oscar_bait$RACE)))
#-0.1379, Not Significant

summary(lm(as.numeric(oscar_bait$BECHDEL) ~ as.numeric(oscar_bait$GENDER)))
#0.3592, Significant at 95%
summary(lm(as.numeric(oscar_bait$BECHDEL) ~ as.numeric(oscar_bait$LEADGENDER)))
#0.275, Significant at 100%

summary(lm(as.numeric(oscar_bait$TWOLEAD) ~ as.numeric(oscar_bait$LGBTQ)))
#-0.1243, Not Significant

summary(lm(as.numeric(oscar_bait$FOREIGNLANG) ~ as.numeric(oscar_bait$RACE)))
#0.3155, Significant at 95%

################################################################################

summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$TRAUMA)))
#-0.016, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$RACISM)))
#0.1251, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$SEXISM)))
#0.1298, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$CLASSISM)))
#-0.04818, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$LOVE)))
#0.06826, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$LGBTQ)))
#-0.063, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$DISABILITY)))
#-0.0078, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$GENDER)))
#0.5, Significant at 100%
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$RACE)))
#-0.06311, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$LEADGENDER)))
#0.0029997, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$TWOLEAD)))
#0.05893, Not Significant
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ 
             as.numeric(oscar_bait$FOREIGNLANG)))
#0.2973, Significant 99%
summary(lm(as.numeric(oscar_bait$MOSTNOMS) ~ as.numeric(oscar_bait$BECHDEL)))
#0.19892, Significant at 95%

################################################################################

summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$TRAUMA)))
#0.074, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$RACISM)))
#-0.2630, Significant at 95%
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$SEXISM)))
#-0.05789, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$CLASSISM)))
#-0.03682, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$LOVE)))
#-0.10187, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$LGBTQ)))
#-0.05631, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$DISABILITY)))
#0.1433, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$GENDER)))
#0.1623, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$RACE)))
#0.05631, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$LEADGENDER)))
#-0.04224, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$TWOLEAD)))
#-0.1, Not Significant
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ 
             as.numeric(oscar_bait$FOREIGNLANG)))
#0.2556, Significant 95%
summary(lm(as.numeric(oscar_bait$DGAWINS) ~ as.numeric(oscar_bait$BECHDEL)))
#0.10784, Not Significant

################################################################################