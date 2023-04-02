#Replication script
#Nardis Y., & Panek, E. (2019). Explaining Privacy Control on Instagram and Twitter: The Roles of Narcissism and Self-esteem. 
#Communication Research Reports, 36, 1, 24-34.
#https://doi.org/10.1080/08824096.2018.1555522


install.packages("tidyr")
install.packages("dplyr")
install.packages("lme4")
install.packages("ggplot2")

library(tidyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(stargazer)


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

# Cluster standard errors by ID
cluster_se <- vcovCL(glm_priv, cluster = analysis_reshaped$id)

# Calculate coefficients and standard errors
coefs <- coef(glm_priv)
std_err <- sqrt(diag(cluster_se))

# Calculate z scores and p-values
z_scores <- coefs / std_err
p_values <- 2 * (1 - pnorm(abs(z_scores)))

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(coefs)
conf_int <- exp(cbind(coefs - 1.96 * std_err, coefs + 1.96 * std_err))

# Combine coefficients, z scores, p-values, odds ratios, and confidence intervals into a table
results <- cbind(coefs, z_scores, p_values, odds_ratios, conf_int)
colnames(results) <- c("Coefficients", "Z Scores", "p-values", "Odds Ratios", 
                       "95% CI Lower", "95% CI Upper")
rownames(results) <- names(coefs)

# Print results table
results


# Stargazer: 
library(stargazer)

# Model without interaction terms
glm_priv <- glm(privacy ~ Exhibitionism + Superiority + Self_Esteem + Age + SNS + Female, 
                data = analysis_reshaped, family = "binomial")

# Cluster standard errors by ID
cluster_se <- vcovCL(glm_priv, cluster = analysis_reshaped$id)

# Calculate coefficients and standard errors
coefs <- coef(glm_priv)
std_err <- sqrt(diag(cluster_se))

# Calculate z-scores and p-values
z_scores <- coefs / std_err
p_values <- 2 * (1 - pnorm(abs(z_scores)))

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(coefs)
conf_int <- exp(cbind(coefs - 1.96 * std_err, coefs + 1.96 * std_err))

# Combine coefficients, z-scores, p-values, odds ratios, and confidence intervals into a table
results <- cbind(coefs, std_err, z_scores, p_values, odds_ratios, conf_int)
colnames(results) <- c("Coefficients", "Standard Error", "Z-value", "p-value", "Odds Ratio", "95% CI Lower", "95% CI Upper")
rownames(results) <- names(coefs)


# Print results table using stargazer
stargazer(glm_priv, type = "text", coef = list(results[,1]), se = list(results[,2]), 
          title = "Logistic Regression Results", align = TRUE, 
          ci = TRUE, ci.custom = list(results[,6:7]), 
          omit.table.layout = "n")
