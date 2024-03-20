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

study1_raw <- read_sav(paste0(input_path, "study-1.sav"))
study1 <- subset(study1_raw, filter_all > 0)
#note0: filtering is obtained with two variables: not recalling correctly the name of the fictitious company, spending too little (1s) or too much (15s) time per item on average
#note1: #MORALLID4 and MORALID5 are recoded. Original items are Reverse_MoralID3 and Reverse_MoralID5
#note2: m_Praising, m_Condemning, m_PWoM, m_NWoM, and m_MoralID are the unweighted means of the items measuring each construct
#note3: FairnessVSconventional is a dummy variable, with fairness-aware algorithm = 1 and conventional algorithm = 0
#note4: ConventionalVSfairness is a dummy variable, with fairness-aware algorithm = 0 and conventional algorithm = 1

#correlation matrix
variables <- study1[, c("m_Praising", "m_Condemning", "m_PWoM", "m_NWoM", "m_MoralID")]
correlation_matrix <- cor(variables)
description <- describe(variables)
p_values <- corr.test(variables)$p
print(correlation_matrix)
print(description)
print(p_values)

# Cronbach's alphas
alpha_Praising <- alpha(study1[, c("Elevation1","Elevation2","Elevation3","Gratitude1","Gratitude2","Gratitude3")])
alpha_Condemning <- alpha(study1[, c("Anger1","Anger2","Anger3","Contempt1","Contempt2","Contempt3","Disgust1","Disgust2","Disgust3")])
alpha_NWoM <- alpha(study1[, c("NWoM1","NWoM2","NWoM3","NWoM4")])
alpha_PWoM <- alpha(study1[, c("PWoM1","PWoM2","PWoM3","PWoM4")])
alpha_MoralID <- alpha(study1[, c("MoralID1","MoralID2","MoralID3","MoralID4","MoralID5")])

print(alpha_Praising)
print(alpha_Condemning)
print(alpha_NWoM)
print(alpha_PWoM)
print(alpha_MoralID)

#t-tests 
t.test(m_Praising ~ FairnessVSconventional,data=study1)
sd_group0 <- sd(study1$m_Praising[study1$FairnessVSconventional == 0])
sd_group1 <- sd(study1$m_Praising[study1$FairnessVSconventional == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study1$m_Praising, study1$FairnessVSconventional)

t.test(m_Condemning ~ ConventionalVSfairness,data=study1)
sd_group0 <- sd(study1$m_Condemning[study1$ConventionalVSfairness == 0])
sd_group1 <- sd(study1$m_Condemning[study1$ConventionalVSfairness == 1])
print(sd_group0)
print(sd_group1)
cohen.d(study1$m_Condemning, study1$ConventionalVSfairness)

#fit of the hypothesized five-factor model
Study1_Model5F <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5"
fit5F <- cfa(Study1_Model5F, data=study1)
summary(fit5F, fit.measures=TRUE, standardized = TRUE)

#fit of the one-factor model
Study1_Model1F <- "
FACTOR=~
Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3+
Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3+
NWoM1+NWoM2+NWoM3+NWoM4+
PWoM1+PWoM2+PWoM3+PWoM4+
MoralID1+MoralID2+MoralID3+MoralID4+MoralID5" 
fit1F <- cfa(Study1_Model1F, data=study1)
summary(fit1F, fit.measures=TRUE, standardized = TRUE)

modelconventionalvsfairness <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5

PRAISING ~ a1*ConventionalVSfairness 
CONDEMNING ~ a2*ConventionalVSfairness

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*ConventionalVSfairness + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*ConventionalVSfairness + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
"
fit1 <- sem(modelconventionalvsfairness, data=study1, se = "bootstrap",bootstrap = 100)
summary(fit1, fit.measures=TRUE, standardized = TRUE)

estimates_modelconventionalvsfairness <- parameterEstimates(fit1,
                                       se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                       standardized = TRUE,
                                       fmi = FALSE, level = 0.95, boot.ci.type = "bca.simple",
                                       cov.std = TRUE, output = "data.frame", header = TRUE)
View(estimates_modelconventionalvsfairness)
write.csv(estimates_modelconventionalvsfairness, paste0(output_path, "ResultsStudy1modelconventionalvsfairness.csv"))

modelfairnessvsconventional <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
#MORALLID4 and MORALID5 were reverse coded prior to analyses
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5

PRAISING ~ a1*FairnessVSconventional 
CONDEMNING ~ a2*FairnessVSconventional

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*FairnessVSconventional + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*FairnessVSconventional + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
"
fit2 <- sem(modelfairnessvsconventional, data=study1, se = "bootstrap",bootstrap = 100)
summary(fit2, fit.measures=TRUE, standardized = TRUE)

estimates_modelfairnessvsconventional <- parameterEstimates(fit2,
                                                            se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                            standardized = TRUE,
                                                            fmi = FALSE, level = 0.95, boot.ci.type = "bca.simple",
                                                            cov.std = TRUE, output = "data.frame", header = TRUE)
View(estimates_modelfairnessvsconventional)
write.csv(estimates_modelfairnessvsconventional, paste0(output_path, "ResultsStudy1modelfairnessvsconventional.csv"))
