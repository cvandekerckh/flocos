#get packages if missing
install.packages("lavaan")
install.packages("haven")
install.packages("psych")

#load packages
library(haven)
library(lavaan)
library(psych)

# input/output path
# Modify the right part of 'path <-' with your path containing a raw fold with data and a results fold where results will be stocked
path <- "C:/your_path/Data/"
input_path <- paste0(path, "raw/")
output_path <- paste0(path,"results/")

#load database and filter responses
study3_raw <- read_sav(paste0(input_path, "study-3.sav"))
study3 <- subset(study3_raw, filter_all > 0)
#note0: filtering is obtained with two variables: not recalling correctly the name of the fictitious company, spending too little (1s) or too much (15s) time per item on average
#note1: #MORALLID4 and MORALID5 are recoded. Original items are Reverse_MoralID3 and Reverse_MoralID5
#note2: m_Praising, m_Condemning, m_PWoM, m_NWoM, and m_MoralID are the unweighted means of the items measuring each construct
#note3: Fairness90VSconventional is a dummy variable, with 90% fairness-aware algorithm = 1 and conventional algorithm = 0
#note4: Fairness60VSconventional is a dummy variable, with 60% fairness-aware algorithm = 1 and conventional algorithm = 0
#note5: ConventionalVSfairness90 is a dummy variable, with 90% fairness-aware algorithm = 0 and conventional algorithm = 1
#note6: ConventionalVSfairness60 is a dummy variable, with 60% fairness-aware algorithm = 0 and conventional algorithm = 1

#correlation matrix with means of items: to obtain means and standard deviations
variables <- study3[, c("m_Praising", "m_Condemning", "m_PWoM", "m_NWoM", "m_MoralID")]
correlation_matrix <- cor(variables)
description <- describe(variables)
p_values <- corr.test(variables)$p
print(correlation_matrix)
print(description)
print(p_values)

# Cronbach's alphas
alpha_Praising <- alpha(study3[, c("Elevation1","Elevation2","Elevation3","Gratitude1","Gratitude2","Gratitude3")])
alpha_Condemning <- alpha(study3[, c("Anger1","Anger2","Anger3","Contempt1","Contempt2","Contempt3","Disgust1","Disgust2","Disgust3")])
alpha_NWoM <- alpha(study3[, c("NWoM1","NWoM2","NWoM3","NWoM4")])
alpha_PWoM <- alpha(study3[, c("PWoM1","PWoM2","PWoM3","PWoM4")])
alpha_MoralID <- alpha(study3[, c("MoralID1","MoralID2","MoralID3","MoralID4","MoralID5")])

print(alpha_Praising)
print(alpha_Condemning)
print(alpha_NWoM)
print(alpha_PWoM)
print(alpha_MoralID)

#t-tests 
t.test(m_Praising ~ Fairness90VSconventional,data=study3)
sd_group0 <- sd(study3$m_Praising[study3$Fairness90VSconventional == 0])
sd_group1 <- sd(study3$m_Praising[study3$Fairness90VSconventional == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study3$m_Praising, study3$Fairness90VSconventional)

t.test(m_Condemning ~ ConventionalVSfairness90,data=study3)
sd_group0 <- sd(study3$m_Condemning[study3$ConventionalVSfairness90 == 0])
sd_group1 <- sd(study3$m_Condemning[study3$ConventionalVSfairness90 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study3$m_Condemning, study3$ConventionalVSfairness90)

t.test(m_Praising ~ Fairness60VSconventional,data=study3)
sd_group0 <- sd(study3$m_Praising[study3$Fairness60VSconventional == 0])
sd_group1 <- sd(study3$m_Praising[study3$Fairness60VSconventional == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study3$m_Praising, study3$Fairness60VSconventional)

t.test(m_Condemning ~ ConventionalVSfairness60,data=study3)
sd_group0 <- sd(study3$m_Condemning[study3$ConventionalVSfairness60 == 0])
sd_group1 <- sd(study3$m_Condemning[study3$ConventionalVSfairness60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study3$m_Condemning, study3$ConventionalVSfairness60)

t.test(m_Praising ~ Fairness90VSfairness60,data=study3)
sd_group0 <- sd(study3$m_Praising[study3$Fairness90VSfairness60 == 0])
sd_group1 <- sd(study3$m_Praising[study3$Fairness90VSfairness60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study3$m_Praising, study3$Fairness90VSfairness60)

t.test(m_Condemning ~ Fairness90VSfairness60,data=study3)
sd_group0 <- sd(study3$m_Condemning[study3$Fairness90VSfairness60 == 0])
sd_group1 <- sd(study3$m_Condemning[study3$Fairness90VSfairness60 == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study3$m_Condemning, study3$Fairness90VSfairness60)

#fit of the hypothesized five-factor model
Study3_Model5F <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5"
fit5F <- cfa(Study3_Model5F, data=study3)
summary(fit5F, fit.measures=TRUE, standardized = TRUE)

#fit of the one-factor model
Study3_Model1F <- "
FACTOR=~
Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3+
Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3+
NWoM1+NWoM2+NWoM3+NWoM4+
PWoM1+PWoM2+PWoM3+PWoM4+
MoralID1+MoralID2+MoralID3+MoralID4+MoralID5"
fit1F <- cfa(Study3_Model1F, data=study3)
summary(fit1F, fit.measures=TRUE, standardized = TRUE)

modelconventionalvsfairness90 <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
#MORALLID4 and MORALID5 were reverse coded prior to analyses
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5

PRAISING ~ a1*ConventionalVSfairness90 
CONDEMNING ~ a2*ConventionalVSfairness90

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*ConventionalVSfairness90 + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*ConventionalVSfairness90 + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
"
fit1 <- sem(modelconventionalvsfairness90, data=study3, se = "bootstrap",bootstrap = 100)
summary(fit1, fit.measures=TRUE, standardized = TRUE)

estimates_modelconventionalvsfairness90 <- parameterEstimates(fit1,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                        standardized = TRUE, fmi = FALSE, level = 0.95, 
                        boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)

View(estimates_modelconventionalvsfairness90)
write.csv(estimates_modelconventionalvsfairness90, paste0(output_path, "ResultsStudy3modelconventionalvsfairness90.csv"))

modelfairness90vsconventional <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
#MORALLID4 and MORALID5 were reverse coded prior to analyses
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5

PRAISING ~ a1*Fairness90VSconventional 
CONDEMNING ~ a2*Fairness90VSconventional

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*Fairness90VSconventional + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*Fairness90VSconventional + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
"
fit2 <- sem(modelfairness90vsconventional, data=study3, se = "bootstrap",bootstrap = 100)
summary(fit2, fit.measures=TRUE, standardized = TRUE)

estimates_modelfairness90vsconventional <- parameterEstimates(fit2,
                                                            se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                            standardized = TRUE,
                                                            fmi = FALSE, level = 0.95, boot.ci.type = "bca.simple",
                                                            cov.std = TRUE, output = "data.frame", header = TRUE)
View(estimates_modelfairness90vsconventional)
write.csv(estimates_modelfairness90vsconventional, paste0(output_path, "ResultsStudy3modelfairness90vsconventional.csv"))

modelconventionalvsfairness60 <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
#MORALLID4 and MORALID5 were reverse coded prior to analyses
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5

PRAISING ~ a1*ConventionalVSfairness60 
CONDEMNING ~ a2*ConventionalVSfairness60

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*ConventionalVSfairness60 + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*ConventionalVSfairness60 + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
"
fit3 <- sem(modelconventionalvsfairness60, data=study3, se = "bootstrap",bootstrap = 100)
summary(fit3, fit.measures=TRUE, standardized = TRUE)

estimates_modelconventionalvsfairness60 <- parameterEstimates(fit3,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                         standardized = TRUE, fmi = FALSE, level = 0.95, 
                         boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)

View(estimates_modelconventionalvsfairness60)
write.csv(estimates_modelconventionalvsfairness60, paste0(output_path, "ResultsStudy3modelconventionalvsfairness60.csv"))

modelfairness60vsconventional <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
#MORALLID4 and MORALID5 were reverse coded prior to analyses
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5

PRAISING ~ a1*Fairness60VSconventional 
CONDEMNING ~ a2*Fairness60VSconventional

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*Fairness60VSconventional + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*Fairness60VSconventional + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
"
fit4 <- sem(modelfairness60vsconventional, data=study3, se = "bootstrap",bootstrap = 100)
summary(fit4, fit.measures=TRUE, standardized = TRUE)

estimates_modelfairness60vsconventional <- parameterEstimates(fit4,
                                                              se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                              standardized = TRUE,
                                                              fmi = FALSE, level = 0.95, boot.ci.type = "bca.simple",
                                                              cov.std = TRUE, output = "data.frame", header = TRUE)
View(estimates_modelfairness60vsconventional)
write.csv(estimates_modelfairness60vsconventional, paste0(output_path, "ResultsStudy3modelfairness60vsconventional.csv"))

modelfairness90vsfairness60 <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
#MORALLID4 and MORALID5 were reverse coded prior to analyses
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5

PRAISING ~ a1*Fairness90VSfairness60 
CONDEMNING ~ a2*Fairness90VSfairness60 

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*Fairness90VSfairness60 + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*Fairness90VSfairness60 + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
"
fit5 <- sem(modelfairness90vsfairness60, data=study3, se = "bootstrap",bootstrap = 100)
summary(fit5, fit.measures=TRUE, standardized = TRUE)

estimates_modelfairness90vsfairness60 <- parameterEstimates(fit5,
                                                              se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                              standardized = TRUE,
                                                              fmi = FALSE, level = 0.95, boot.ci.type = "bca.simple",
                                                              cov.std = TRUE, output = "data.frame", header = TRUE)
View(estimates_modelfairness90vsfairness60)
write.csv(estimates_modelfairness90vsfairness60, paste0(output_path, "ResultsStudy3modelfairness90vsfairness60.csv"))