library(dplyr)
dat = read.csv("Gallup.csv")
head(dat)

str(dat)

# converting qualitative variable to factor
dat$Group = as.factor(dat$Group)

summary(dat$TshirtPrice)

# Remove anyone who said they would pay negative amounts
library(tidyverse)
dat2 = dat %>% filter(TshirtPrice >= 0)
table(dat2$Group)

summary(dat2$TshirtPrice)

quantile(dat2$TshirtPrice,c(0.9,0.99))

# Remove anyone who said they'd pay more than $50 for the shirt
dat3 = dat2 %>% filter(TshirtPrice <= 50)
summary(dat3$TshirtPrice)
summary(dat3$Excess)

# Now remove the following:
# 1) those who answered No to the initial question
# 2) those who answered yes, but gave an initial price of $0 for the shirt
# 3) those who answered Yes but gave an Excess of 0 (should've answered no)
# 4) those who answered Yes and said they'd pay $100 or more in excess (outliers)
dat4= dat3 %>% filter(Answer == 1, TshirtPrice>0, Excess>0, Excess < 100)

# Create a new variable to measure percentage increase in amount willing to pay
dat4 = dat4 %>% mutate(pct.inc = (Excess/TshirtPrice))
summary(dat4)

boxplot(pct.inc ~ Group, data=dat4) 
# Boxplot revealing outlier in group 4

which.max(dat4$pct.inc) #remove this outlier
dat4 = dat4[-c(2612),]

summary(dat4)
boxplot(pct.inc ~ Group, data=dat4) 

mod2 = aov(sqrt(Excess) ~Group, data=dat4)
anova(mod2)


interaction.plot(log(dat4$TshirtPrice),dat4$Group , log(dat4$Excess))


mod3.1 = aov(log(Excess) ~Group, data=dat4)
anova(mod3.1)

# significant differences between groups using log excess 
mod3 = aov(log(pct.inc) ~Group, data=dat4)
anova(mod3)

# check for normality of residuals
hist(resid(mod3))
plot(mod3,which=1)

library(emmeans)

emm = emmeans(mod3, ~ Group)
emm
pairs(emm, options=list(infer=T))


## Check for differences in percent increase, controlling for initial price
cor(dat4$TshirtPrice,dat4$Excess)
cor(log(dat4$Excess),dat4$DEMO_AGE)
plot(dat4$TshirtPrice,dat4$Excess, col=as.factor(dat4$Group))
plot(log(dat4$TshirtPrice),log(dat4$Excess), col=as.factor(dat4$Group)) # log-log transform looks most linear



plot(sqrt(dat4$TshirtPrice[dat4$Group == "1"]), sqrt(dat4$Excess[dat4$Group == "1"]), 
     col='blue3', xlab="T-shirt Price", ylab="Excess", 
     main="T-shirt Price vs Excess for Group 1")

plot(sqrt(dat4$TshirtPrice[dat4$Group == "2"]), sqrt(dat4$Excess[dat4$Group == "2"]), 
     col='red3', xlab="T-shirt Price", ylab="Excess", 
     main="T-shirt Price vs Excess for Group 2")

plot(sqrt(dat4$TshirtPrice[dat4$Group == "3"]), sqrt(dat4$Excess[dat4$Group == "3"]), 
     col='green4', xlab="T-shirt Price", ylab="Excess", 
     main="T-shirt Price vs Excess for Group 3")

plot(sqrt(dat4$TshirtPrice[dat4$Group == "4"]), sqrt(dat4$Excess[dat4$Group == "4"]), 
     col='yellow3', xlab="T-shirt Price", ylab="Excess", 
     main="T-shirt Price vs Excess for Group 4")





mod4 = lm(log(Excess) ~ log(TshirtPrice)*Group, data=dat4 )
anova(mod4)

# update model with significant terms
mod4 = update(mod4, ~ . - log(TshirtPrice):Group)
summary(mod4)




mod5 = lm(log(Excess) ~ log(TshirtPrice)+DEMO_AGE+Group, data=dat4 )
anova(mod5)


table(dat4$DEMO_GENDER) # let's remove the 5 "3"'s since these are unusual in the population
table(dat4$DEMO_EDUCATION_RECODED)

dat5 = dat4 %>% filter(DEMO_GENDER != 3) %>% mutate(educ=as.factor(DEMO_EDUCATION_RECODED),
                                                    age.cent=DEMO_AGE-mean(DEMO_AGE), 
                                                    sex=ifelse(DEMO_GENDER==1,1,0))


mod6 = lm(log(Excess) ~ log(TshirtPrice)+age.cent+sex+educ+Group, data=dat5 )
anova(mod6)

# no significant interaction between groups and factors
mod7 = lm(log(Excess) ~ log(TshirtPrice)*age.cent*sex*educ*Group, data=dat5 )
anova(mod7)


emm = emmeans(mod6, ~ Group, type="response")
emm

pairs(emm, options=list(infer=T))



interaction.plot(dat5$Group, dat5$sex ,log(dat5$Excess))

