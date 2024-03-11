#get packages if missing
install.packages("lavaan")
install.packages("haven")
install.packages("psych")

#load packages
library(haven)
library(lavaan)
library(psych)

#pretest Study 1
pretest_study1_raw <- read_sav("20231005 Study 1 PRETEST.sav")
pretest_study1 <- subset(pretest_study1_raw, filter_all > 0)

t.test(Fairness ~ FairnessVSconventional,data=pretest_study1)
sd_group0 <- sd(pretest_study1$Fairness[pretest_study1$FairnessVSconventional == 0])
sd_group1 <- sd(pretest_study1$Fairness[pretest_study1$FairnessVSconventional == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study1$Fairness, pretest_study1$FairnessVSconventional)

t.test(Accuracy ~ ConventionalVSfairness,data=pretest_study1)
sd_group0 <- sd(pretest_study1$Accuracy[pretest_study1$ConventionalVSfairness == 0])
sd_group1 <- sd(pretest_study1$Accuracy[pretest_study1$ConventionalVSfairness == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study1$Accuracy, pretest_study1$ConventionalVSfairness)


#pretest Study 2
pretest_study2_raw <- read_sav("20231005 Study 2 PRETEST.sav")
pretest_study2 <- subset(pretest_study2_raw, filter_all > 0)

t.test(Fairness ~ FairnessVSconventional,data=pretest_study2)
sd_group0 <- sd(pretest_study2$Fairness[pretest_study2$FairnessVSconventional == 0])
sd_group1 <- sd(pretest_study2$Fairness[pretest_study2$FairnessVSconventional == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study2$Fairness, pretest_study2$FairnessVSconventional)

t.test(Accuracy ~ ConventionalVSfairness,data=pretest_study2)
sd_group0 <- sd(pretest_study2$Accuracy[pretest_study2$ConventionalVSfairness == 0])
sd_group1 <- sd(pretest_study2$Accuracy[pretest_study2$ConventionalVSfairness == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study2$Accuracy, pretest_study2$ConventionalVSfairness)

#pretest Study 2
pretest_study3_raw <- read_sav("20240212 Study 3 PRETEST V4.sav")
pretest_study3 <- subset(pretest_study3_raw, filter_all > 0)

t.test(Fairness ~ Fairness90VSconventional,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Fairness[pretest_study3$Fairness90VSconventional == 0])
sd_group1 <- sd(pretest_study3$Fairness[pretest_study3$Fairness90VSconventional == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Fairness, pretest_study3$Fairness90VSconventional)

t.test(Accuracy ~ ConventionalVSfairness90,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Accuracy[pretest_study3$ConventionalVSfairness90 == 0])
sd_group1 <- sd(pretest_study3$Accuracy[pretest_study3$ConventionalVSfairness90 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Accuracy, pretest_study3$ConventionalVSfairness90)

t.test(Fairness ~ Fairness60VSconventional,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Fairness[pretest_study3$Fairness60VSconventional == 0])
sd_group1 <- sd(pretest_study3$Fairness[pretest_study3$Fairness60VSconventional == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Fairness, pretest_study3$Fairness60VSconventional)

t.test(Accuracy ~ ConventionalVSfairness60,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Accuracy[pretest_study3$ConventionalVSfairness60 == 0])
sd_group1 <- sd(pretest_study3$Accuracy[pretest_study3$ConventionalVSfairness60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Accuracy, pretest_study3$ConventionalVSfairness60)

t.test(Fairness ~ Fairness90VSfairness60,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Fairness[pretest_study3$Fairness90VSfairness60 == 0])
sd_group1 <- sd(pretest_study3$Fairness[pretest_study3$Fairness90VSfairness60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Fairness, pretest_study3$Fairness90VSfairness60)

t.test(Accuracy ~ Fairness90VSfairness60,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Accuracy[pretest_study3$Fairness90VSfairness60 == 0])
sd_group1 <- sd(pretest_study3$Accuracy[pretest_study3$Fairness90VSfairness60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Accuracy, pretest_study3$Fairness90VSfairness60)

t.test(Accuracy ~ Conventional90VSconventional60,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Accuracy[pretest_study3$Conventional90VSconventional60 == 0])
sd_group1 <- sd(pretest_study3$Accuracy[pretest_study3$Conventional90VSconventional60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Accuracy, pretest_study3$Conventional90VSconventional60)

t.test(Fairness ~ Conventional90VSconventional60,data=pretest_study3)
sd_group0 <- sd(pretest_study3$Fairness[pretest_study3$Conventional90VSconventional60 == 0])
sd_group1 <- sd(pretest_study3$Fairness[pretest_study3$Conventional90VSconventional60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(pretest_study3$Fairness, pretest_study3$Conventional90VSconventional60)

