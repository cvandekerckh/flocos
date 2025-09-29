#################### Loading ###################

#get packages if missing
install.packages("lavaan")
install.packages("haven")
install.packages("psych")

#load packages
library(haven)
library(lavaan)
library(psych)
library(dotenv)

# Load the environment variable PATH pointing towards the data location
# Needs to be configured by the user in the .env file (see Readme.md)
load_dot_env(file = ".env")

# input/output path
path <- Sys.getenv("PATH")
input_path <- paste0(path, "raw/")
output_path <- paste0(path,"results/")

study1_raw <- read_sav(paste0(input_path, "study-1.sav"))
study1 <- subset(study1_raw, filter_all > 0)
#note0: filtering is obtained with two variables: not recalling correctly the name of the fictitious company, spending too little (1s) or too much (15s) time per item on average
#note1: #MORALLID4 and MORALID5 are recoded. Original items are Reverse_MoralID3 and Reverse_MoralID5
#note2: m_Praising, m_Condemning, m_PWoM, m_NWoM, and m_MoralID are the unweighted means of the items measuring each construct
#note3: FairnessVSconventional is a dummy variable, with fairness-aware algorithm = 1 and conventional algorithm = 0
#note4: ConventionalVSfairness is a dummy variable, with fairness-aware algorithm = 0 and conventional algorithm = 1

#################### Correlation ###################

#correlation matrix
variables <- study1[, c("m_Praising", "m_Condemning", "m_PWoM", "m_NWoM", "m_MoralID")]
correlation_matrix <- cor(variables)
description <- describe(variables)
p_values <- corr.test(variables)$p
print(correlation_matrix)
print(description)
print(p_values)

#################### Cronbach's alphas ###################

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

#################### t-tests  ###################

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

#################### Factors ###################
praising_factors <- "
PRAISING=~Elevation1+Elevation2+Elevation3+Gratitude1+Gratitude2+Gratitude3
"

condemning_factors <- "
CONDEMNING=~Anger1+Anger2+Anger3+Contempt1+Contempt2+Contempt3+Disgust1+Disgust2+Disgust3
"

pwom_factors <- "
PWOM=~PWoM1+PWoM2+PWoM3+PWoM4
"

nwom_factors <- "
NWOM=~NWoM1+NWoM2+NWoM3+NWoM4
"

moralid_factors <- "
MORALID=~MoralID1+MoralID2+MoralID3+MoralID4+MoralID5
"

#################### Fitting models ###################

#fit of the hypothesized five-factor model
Study1_Model5F <- paste0(
  praising_factors,
  condemning_factors,
  nwom_factors,
  pwom_factors,
  moralid_factors
)
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

#################### Conventional VS Fairness ###################

modelconventionalvsfairness <- paste0(Study1_Model5F, "

PRAISING ~ a1*ConventionalVSfairness 
CONDEMNING ~ a2*ConventionalVSfairness

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*ConventionalVSfairness + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*ConventionalVSfairness + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
")
fit1 <- sem(modelconventionalvsfairness, data=study1, se = "bootstrap",bootstrap = 100)
summary(fit1, fit.measures=TRUE, standardized = TRUE)

estimates_modelconventionalvsfairness <- parameterEstimates(fit1,
                                       se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                       standardized = TRUE,
                                       fmi = FALSE, level = 0.95, boot.ci.type = "bca.simple",
                                       cov.std = TRUE, output = "data.frame", header = TRUE)
View(estimates_modelconventionalvsfairness)
write.csv(estimates_modelconventionalvsfairness, paste0(output_path, "ResultsStudy1modelconventionalvsfairness.csv"))

#################### Fairness VS Conventional ###################

modelfairnessvsconventional <- paste0(Study1_Model5F, "

PRAISING ~ a1*FairnessVSconventional 
CONDEMNING ~ a2*FairnessVSconventional

NWOM ~ b11*PRAISING + b12*CONDEMNING + c1*FairnessVSconventional + MORALID
PWOM ~ b21*PRAISING + b22*CONDEMNING + c2*FairnessVSconventional + MORALID

NWOMviaPRAISING := a1*b11
NWOMviaCONDEMNING := a2*b12
PWOMviaPRAISING := a1*b21
PWOMviaCONDEMNING := a2*b22
")
fit2 <- sem(modelfairnessvsconventional, data=study1, se = "bootstrap",bootstrap = 100)
summary(fit2, fit.measures=TRUE, standardized = TRUE)

estimates_modelfairnessvsconventional <- parameterEstimates(fit2,
                                                            se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                            standardized = TRUE,
                                                            fmi = FALSE, level = 0.95, boot.ci.type = "bca.simple",
                                                            cov.std = TRUE, output = "data.frame", header = TRUE)
View(estimates_modelfairnessvsconventional)
write.csv(estimates_modelfairnessvsconventional, paste0(output_path, "ResultsStudy1modelfairnessvsconventional.csv"))

#################### Hierarchical models ###################

# Praising model 1
Study1_praising_model1 <- paste0(
  praising_factors,
  moralid_factors,
  "PRAISING ~ MORALID"
)
fit_Study1_praising_model1 <- sem(Study1_praising_model1, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_praising_model1, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_praising_model1_output <- parameterEstimates(fit_Study1_praising_model1,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                    standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                    boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_praising_model1_output)
write.csv(Study1_praising_model1_output, "Study1_praising_model1_output.csv")

summary_output_Study1_praising_model1 <- capture.output(summary(fit_Study1_praising_model1, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_praising_model1, sep = "\n")
sink("summary_output_Study1_praising_model1.txt")
cat(summary_output_Study1_praising_model1, sep = "\n")
sink()

# Praising model 2
Study1_praising_model2 <- paste0(
  praising_factors,
  moralid_factors,
  "PRAISING ~ MORALID + FairnessVSconventional"
) 
fit_Study1_praising_model2 <- sem(Study1_praising_model2, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_praising_model2, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_praising_model2_output <- parameterEstimates(fit_Study1_praising_model2,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                    standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                    boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_praising_model2_output)
write.csv(Study1_praising_model2_output, "Study1_praising_model2_output.csv")

summary_output_Study1_praising_model2 <- capture.output(summary(fit_Study1_praising_model2, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_praising_model2, sep = "\n")
sink("summary_output_Study1_praising_model2.txt")
cat(summary_output_Study1_praising_model2, sep = "\n")
sink()

# Condemning model 1
Study1_condemning_model1 <- paste0(
  condemning_factors,
  moralid_factors,
  "CONDEMNING ~ MORALID"
)
fit_Study1_condemning_model1 <- sem(Study1_condemning_model1, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_condemning_model1, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_condemning_model1_output <- parameterEstimates(fit_Study1_condemning_model1,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                      standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                      boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_condemning_model1_output)
write.csv(Study1_condemning_model1_output, "Study1_condemning_model1_output.csv")

summary_output_Study1_condemning_model1 <- capture.output(summary(fit_Study1_condemning_model1, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_condemning_model1, sep = "\n")
sink("summary_output_Study1_condemning_model1.txt")
cat(summary_output_Study1_condemning_model1, sep = "\n")
sink()

# Condemning model 2
Study1_condemning_model2 <- paste0(
  condemning_factors,
  moralid_factors,
  "CONDEMNING ~ MORALID + FairnessVSconventional"
) 
fit_Study1_condemning_model2 <- sem(Study1_condemning_model2, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_condemning_model2, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_condemning_model2_output <- parameterEstimates(fit_Study1_condemning_model2,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                      standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                      boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_condemning_model2_output)
write.csv(Study1_condemning_model2_output, "Study1_condemning_model2_output.csv")

summary_output_Study1_condemning_model2 <- capture.output(summary(fit_Study1_condemning_model2, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_condemning_model2, sep = "\n")
sink("summary_output_Study1_condemning_model2.txt")
cat(summary_output_Study1_condemning_model2, sep = "\n")
sink()

# PWOM model 3
Study1_pwom_model3 <- paste0(
  pwom_factors,
  moralid_factors,
  "PWOM ~ MORALID"
)
fit_Study1_pwom_model3 <- sem(Study1_pwom_model3, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_pwom_model3, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_pwom_model3_output <- parameterEstimates(fit_Study1_pwom_model3,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_pwom_model3_output)
write.csv(Study1_pwom_model3_output, "Study1_pwom_model3_output.csv")

summary_output_Study1_pwom_model3 <- capture.output(summary(fit_Study1_pwom_model3, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_pwom_model3, sep = "\n")
sink("summary_output_Study1_pwom_model3.txt")
cat(summary_output_Study1_pwom_model3, sep = "\n")
sink()

# PWOM model 4
Study1_pwom_model4 <- paste0(
  pwom_factors,
  moralid_factors,
  "PWOM ~ MORALID + FairnessVSconventional"
)
fit_Study1_pwom_model4 <- sem(Study1_pwom_model4, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_pwom_model4, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_pwom_model4_output <- parameterEstimates(fit_Study1_pwom_model4,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_pwom_model4_output)
write.csv(Study1_pwom_model4_output, "Study1_pwom_model4_output.csv")

summary_output_Study1_pwom_model4 <- capture.output(summary(fit_Study1_pwom_model4, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_pwom_model4, sep = "\n")
sink("summary_output_Study1_pwom_model4.txt")
cat(summary_output_Study1_pwom_model4, sep = "\n")
sink()

# PWOM model 5
Study1_pwom_model5 <- paste0(
  pwom_factors,
  moralid_factors,
  praising_factors,
  condemning_factors,
  "PWOM ~ MORALID + PRAISING + CONDEMNING + FairnessVSconventional"
)
fit_Study1_pwom_model5 <- sem(Study1_pwom_model5, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_pwom_model5, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_pwom_model5_output <- parameterEstimates(fit_Study1_pwom_model5,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_pwom_model5_output)
write.csv(Study1_pwom_model5_output, "Study1_pwom_model5_output.csv")

summary_output_Study1_pwom_model5 <- capture.output(summary(fit_Study1_pwom_model5, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_pwom_model5, sep = "\n")
sink("summary_output_Study1_pwom_model5.txt")
cat(summary_output_Study1_pwom_model5, sep = "\n")
sink()

# NWOM model 3
Study1_nwom_model3 <- paste0(
  nwom_factors,
  moralid_factors,
  "NWOM ~ MORALID"
)
fit_Study1_nwom_model3 <- sem(Study1_nwom_model3, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_nwom_model3, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_nwom_model3_output <- parameterEstimates(fit_Study1_nwom_model3,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_nwom_model3_output)
write.csv(Study1_nwom_model3_output, "Study1_nwom_model3_output.csv")

summary_output_Study1_nwom_model3 <- capture.output(summary(fit_Study1_nwom_model3, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_nwom_model3, sep = "\n")
sink("summary_output_Study1_nwom_model3.txt")
cat(summary_output_Study1_nwom_model3, sep = "\n")
sink()

# NWOM model 4
Study1_nwom_model4 <- paste0(
  nwom_factors,
  moralid_factors,
  "NWOM ~ MORALID + ConventionalVSfairness"
)
fit_Study1_nwom_model4 <- sem(Study1_nwom_model4, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_nwom_model4, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_nwom_model4_output <- parameterEstimates(fit_Study1_nwom_model4,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_nwom_model4_output)
write.csv(Study1_nwom_model4_output, "Study1_nwom_model4_output.csv")

summary_output_Study1_nwom_model4 <- capture.output(summary(fit_Study1_nwom_model4, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_nwom_model4, sep = "\n")
sink("summary_output_Study1_nwom_model4.txt")
cat(summary_output_Study1_nwom_model4, sep = "\n")
sink()

# NWOM model 5
Study1_nwom_model5 <- paste0(
  nwom_factors,
  moralid_factors,
  praising_factors,
  condemning_factors,
  "NWOM ~ MORALID + PRAISING + CONDEMNING + ConventionalVSfairness"
)
fit_Study1_nwom_model5 <- sem(Study1_nwom_model5, data=study1, se = "bootstrap",bootstrap = 10000)
summary(fit_Study1_nwom_model5, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE)

Study1_nwom_model5_output <- parameterEstimates(fit_Study1_nwom_model5,se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE,
                                                standardized = TRUE, fmi = FALSE, level = 0.95, 
                                                boot.ci.type = "bca.simple", cov.std = TRUE, output = "data.frame", header = TRUE)
View(Study1_nwom_model5_output)
write.csv(Study1_nwom_model5_output, "Study1_nwom_model5_output.csv")

summary_output_Study1_nwom_model5 <- capture.output(summary(fit_Study1_nwom_model5, fit.measures=TRUE, standardized = TRUE, rsquare=TRUE))
cat(summary_output_Study1_nwom_model5, sep = "\n")
sink("summary_output_Study1_nwom_model5.txt")
cat(summary_output_Study1_nwom_model5, sep = "\n")
sink()