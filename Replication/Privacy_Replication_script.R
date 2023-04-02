#Replication script
#Nardis Y., & Panek, E. (2019). Explaining Privacy Control on Instagram and Twitter: The Roles of Narcissism and Self-esteem. 
#Communication Research Reports, 36, 1, 24-34.
#https://doi.org/10.1080/08824096.2018.1555522

#Original packages and libraries 
install.packages("tidyr")
install.packages("dplyr")
install.packages("lme4")
install.packages("ggplot2")

library(tidyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(stargazer)

#Additional packages and libraries for my twist

install.packages("lmtest")
install.packages("sandwich")

library(lmtest)
library(sandwich)

setwd("/Users/Caitchi/Desktop/ASDS/StatsII_Spring2023/Replication")
getwd()

load("/Users/Caitchi/Desktop/ASDS/StatsII_Spring2023/Replication/Nardis.Panek.Explaining_privacy_control_SNS.RData")

####Variables####
#factor variables: TW_Public IG_Public Female Q2_2 (Use Twitter) Q2_3 (Use Instagram)
#numeric variables: Exhibitionism Superiority Self_Esteem, Age
names(privacy_control_SNS)


####Data management####

#Converting Female to factor variable
privacy_control_SNS$Female <-as.factor(privacy_control_SNS$Female)
levels(privacy_control_SNS$Female) <- c("Male", "Female")
mytable <- table(privacy_control_SNS$IG_Public,privacy_control_SNS$Female)
prop.table(mytable, 2)

#Descriptive statistics
table(privacy_control_SNS$Q2_2)
table(privacy_control_SNS$Q2_3)
prop.table(table(privacy_control_SNS$Q2_2))
prop.table(table(privacy_control_SNS$Q2_3))


privacy_control_SNS <- privacy_control_SNS[ which(privacy_control_SNS$Q2_2=='Yes' | privacy_control_SNS$Q2_3=='Yes'), ] #Subsetting users of at least one SNS

table(privacy_control_SNS$TW_Public)
table(privacy_control_SNS$IG_Public)
table(privacy_control_SNS$Female)
prop.table(table(privacy_control_SNS$TW_Public))
prop.table(table(privacy_control_SNS$IG_Public))
prop.table(table(privacy_control_SNS$Female))




#Reshaping observations from the individual level to the individual-SNS level
privacy_control_SNS <- mutate(privacy_control_SNS, id = rownames(privacy_control_SNS))
analysis <- subset(privacy_control_SNS, select = c(id, TW_Public, IG_Public, Exhibitionism, Superiority, Self_Esteem, Age, Female))
summary(analysis)
analysis_reshaped <- analysis %>% gather(analysis, privacy, TW_Public, IG_Public, -id, -Exhibitionism, -Superiority, -Self_Esteem, -Age, -Female)
summary(analysis_reshaped)

names(analysis_reshaped)[names(analysis_reshaped) == "analysis"] <- "SNS"
analysis_reshaped$SNS <- as.factor(analysis_reshaped$SNS)
levels(analysis_reshaped$SNS)[levels(analysis_reshaped$SNS)=="TW_Public"] <- "Twitter"
levels(analysis_reshaped$SNS)[levels(analysis_reshaped$SNS)=="IG_Public"] <- "Instagram"
analysis_reshaped$SNS <- relevel(analysis_reshaped$SNS, ref = "Twitter")

str(analysis_reshaped$SNS)
summary(analysis_reshaped$SNS)
head(analysis_reshaped)

#Converting privacy to factor variable
table(analysis_reshaped$privacy)
str(analysis_reshaped$privacy)
analysis_reshaped$privacy <- as.factor(analysis_reshaped$privacy)
str(analysis_reshaped$privacy)
analysis_reshaped$privacy <- as.numeric(analysis_reshaped$privacy)
str(analysis_reshaped$privacy)
analysis_reshaped$privacy[analysis_reshaped$privacy==1] <- 0 
analysis_reshaped$privacy[analysis_reshaped$privacy==2] <- 1
analysis_reshaped$privacy <- factor(analysis_reshaped$privacy, levels = c(0, 1), labels = c("Private", "Public"))
str(analysis_reshaped$privacy)
table(analysis_reshaped$privacy)

analysis_reshaped <- subset(analysis_reshaped, privacy!="NA")
analysis_reshaped <- subset(analysis_reshaped, Age!="NA")
analysis_reshaped <- subset(analysis_reshaped, Female!="NA")



####Mixed models predicting privacy setting####

#Model without interaction terms
mixed_priv <- glmer(privacy ~ Exhibitionism + Superiority + Self_Esteem + Age + 
                      SNS + Female + (1|id), 
                    data = analysis_reshaped, 
                    family = "binomial", 
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(mixed_priv)

#Calculating odds ratios
se <- sqrt(diag(vcov(mixed_priv)))
(tab <- cbind(Est = fixef(mixed_priv), 
              LL = fixef(mixed_priv) - 1.96 * se, 
              UL = fixef(mixed_priv) + 1.96 *se))
exp(tab)

# Please note as the code to create the final table in th study was not included, 
# I have written the necessary code here: 


# Print coefficients, z-values, p-values, and odds ratios table
coefs <- fixef(mixed_priv)
std_err <- sqrt(diag(vcov(mixed_priv)))
z_scores <- coefs / std_err
odds_ratios <- exp(coefs)
results <- cbind(coefs, std_err, z_scores, odds_ratios, p_values)
colnames(results) <- c("Coefficients", "Standard Error", "Z-value", "Odds Ratio", "p-values")
rownames(results) <- names(coefs)
results


#Model with Exhibitionism*SNS interaction
mixed_priv_inter1 <- glmer(privacy ~ Superiority + Self_Esteem + Age + Female + Exhibitionism*SNS + (1|id), data = analysis_reshaped, family = "binomial", glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mixed_priv_inter1)

#Calculating odds ratios
se <- sqrt(diag(vcov(mixed_priv_inter1)))
(tab <- cbind(Est = fixef(mixed_priv_inter1), LL = fixef(mixed_priv_inter1) - 1.96 * se, UL = fixef(mixed_priv_inter1) + 1.96 *se))
exp(tab)


#Model with Superiority*SNS interaction
mixed_priv_inter2 <- glmer(privacy ~ Exhibitionism + Self_Esteem + Age + Female + Superiority*SNS + (1|id), data = analysis_reshaped, family = "binomial", glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mixed_priv_inter2)

#Calculating odds ratios
se <- sqrt(diag(vcov(mixed_priv_inter2)))
(tab <- cbind(Est = fixef(mixed_priv_inter2), LL = fixef(mixed_priv_inter2) - 1.96 * se, UL = fixef(mixed_priv_inter2) + 1.96 *se))
exp(tab)


#Model with Self_Esteem*SNS interaction
mixed_priv_inter3 <- glmer(privacy ~ Exhibitionism + Superiority + Age + Female + Self_Esteem*SNS + (1|id), data = analysis_reshaped, family = "binomial", glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mixed_priv_inter3)

#Calculating odds ratios
se <- sqrt(diag(vcov(mixed_priv_inter3)))
(tab <- cbind(Est = fixef(mixed_priv_inter3), LL = fixef(mixed_priv_inter3) - 1.96 * se, UL = fixef(mixed_priv_inter3) + 1.96 *se))
exp(tab)



#Plotting Self_Esteem*SNS interaction
pred_mixed_priv_inter3 <- predict(mixed_priv_inter3, type="response")
summary(pred_mixed_priv_inter3)


# Please note that this code did not run when I was replicating the data
# Below I have included some edited code that allowed me to plot this interaction
self_esteem_df <- data.frame(Self_Esteem = analysis_reshaped$Self_Esteem, 
                             pred_mixed_priv_inter3 = pred_mixed_priv_inter3, SNS = analysis_reshaped$SNS)

ggplot(data = self_esteem_df, aes(x = Self_Esteem, y = pred_mixed_priv_inter3, linetype = SNS)) +
  geom_smooth(method = "lm", col="black") +
  geom_blank() +
  labs(x = "Self Esteem", y = "Predicted Probability (Public Setting)") + 
  xlim(1,4) + 
  ylim(0,1)

######

# My twist including p-values in code

# Model without interaction terms
glm_priv <- glm(privacy ~ Exhibitionism + Superiority + Self_Esteem + Age + SNS + Female, 
                data = analysis_reshaped, family = "binomial")


# Compute coefficient estimates, standard errors, p-values, and odds ratios
coeffs_cl <- coeftest(glm_priv, vcov = vcovCL, cluster = analysis_reshaped$id)
odds_ratios <- exp(coeffs_cl[, "Estimate"])
p_values <- coeffs_cl[, "Pr(>|z|)"]

# Combine coefficients, odds ratios, and p-values into final table
coeffs_cl <- cbind(coeffs_cl[, c("Estimate", "Std. Error", "z value")], 
                   Odds_Ratio = odds_ratios,
                   p_value = p_values)

coeffs_cl


####### Running a glm on the interactions to make sure there is no significance 
# as in the original paper

#Model with Exhibitionism*SNS interaction
glm_priv_inter1 <- glm(privacy ~ Superiority + Self_Esteem + Age + Female + Exhibitionism*SNS, data = analysis_reshaped, family = "binomial")
summary(glm_priv_inter1)

# Compute coefficient estimates, standard errors, p-values, and odds ratios
coeffs_inter1 <- coeftest(glm_priv_inter1, vcov = vcovCL, cluster = analysis_reshaped$id)
odds_ratios_inter1 <- exp(coeffs_inter1[, "Estimate"])
p_values_inter1 <- coeffs_inter1[, "Pr(>|z|)"]

# Combine coefficients, odds ratios, and p-values into final table
coeffs_inter1 <- cbind(coeffs_inter1[, c("Estimate", "Std. Error", "z value")], 
                   Odds_Ratio = odds_ratios_inter1,
                   p_value = p_values_inter1)

coeffs_inter1

#Model with Superiority*SNS interaction
glm_priv_inter2 <- glm(privacy ~ Exhibitionism + Self_Esteem + Age + Female + Superiority*SNS, data = analysis_reshaped, family = "binomial")
summary(glm_priv_inter2)

# Compute coefficient estimates, standard errors, p-values, and odds ratios
coeffs_inter2 <- coeftest(glm_priv_inter2, vcov = vcovCL, cluster = analysis_reshaped$id)
odds_ratios_inter2 <- exp(coeffs_inter2[, "Estimate"])
p_values_inter2 <- coeffs_inter2[, "Pr(>|z|)"]

# Combine coefficients, odds ratios, and p-values into final table
coeffs_inter2 <- cbind(coeffs_inter2[, c("Estimate", "Std. Error", "z value")], 
                       Odds_Ratio = odds_ratios_inter2,
                       p_value = p_values_inter2)

coeffs_inter2

#Model with Self_Esteem*SNS interaction
glm_priv_inter3 <- glm(privacy ~ Exhibitionism + Superiority + Age + Female + Self_Esteem*SNS, data = analysis_reshaped, family = "binomial")
summary(glm_priv_inter3)

# Compute coefficient estimates, standard errors, p-values, and odds ratios
coeffs_inter3 <- coeftest(glm_priv_inter3, vcov = vcovCL, cluster = analysis_reshaped$id)
odds_ratios_inter3 <- exp(coeffs_inter3[, "Estimate"])
p_values_inter3 <- coeffs_inter3[, "Pr(>|z|)"]

# Combine coefficients, odds ratios, and p-values into final table
coeffs_inter3 <- cbind(coeffs_inter3[, c("Estimate", "Std. Error", "z value")], 
                       Odds_Ratio = odds_ratios_inter3,
                       p_value = p_values_inter3)

coeffs_inter3

