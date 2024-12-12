#packages
library(readxl)
library(lme4)
library(pglm)
library(lme4)
library(modelsummary)
library(speedglm)
library(biglm)
library(modelsummary)
library(clubSandwich)
library(pscl)



# Load the data
data <- read_excel("C:/Users/creat/OneDrive - Högskolan Dalarna/thesis/Updated_annual_data_with_merge-2.xlsx")

summary(data)

data$totalwordcount<-scale(data$totalwordcount)
data$difficultwords<-scale(data$difficultwords)
data$sentencecount<-scale(data$sentencecount)
data$syllablecount<-scale(data$syllablecount)
data$boilerplatewordcount<-scale(data$boilerplatewordcount)
data$redundant_word_count<-scale(data$redundant_word_count)

data$per_difficult<-data$difficultwords/data$totalwordcount
data$avg_sentence<-data$totalwordcount/data$sentencecount
data$per_redundant<-data$redundant_word_count/data$totalwordcount
data$per_boiler<-data$boilerplatewordcount/data$totalwordcount

data$complexitymetric<-(data$per_boiler+data$per_difficult+data$per_redundant)/3
View(data)
pooled_effects<-pglm(is_material_weakness ~ avg_sentence +complexitymetric + 
                       syllablecount,data=data,family = binomial(link = "logit"),model="pooling",start=NULL)
custom_stars <- c('*' = 0.1, '**' = 0.05, '***' = 0.01)
modelsummary(pooled_effects,stars = custom_stars, exponentiate = TRUE)
modelsummary(pooled_effects,stars = TRUE, exponentiate = TRUE)
summary(pooled_effects)
pooled_effects <- glm(is_material_weakness ~ avg_sentence + complexitymetric + 
                        syllablecount, data = data, family = binomial(link = "logit"))

logLik_fitted <- logLik(pooled_effects)
logLik_fitted
# Fit a null model
null_model <- pglm(is_material_weakness ~ 1, data = data, family = binomial(link = "logit"), model = "pooling")
logLik_null <- logLik(null_model)

# Calculate McFadden's R^2
mcfadden_r2 <- 1 - (logLik_fitted/logLik_null)
mcfadden_r2
mixedeffects_industry <- glmer(is_material_weakness ~ avg_sentence +complexitymetric + 
                                 `syllable count` + 
                                 factor(year_of_disclosure) +(1|industry), 
                               data = data,family = binomial()
)
mixedeffects_year<-glmer(is_material_weakness ~ avg_sentence +complexitymetric + 
                           syllablecount + 
                           factor(industry) +(1|year_of_disclosure), 
                         data = data,family = binomial())
summary(mixedeffects_year)
custom_stars <- c('*' = 0.1, '**' = 0.05, '***' = 0.01)
modelsummary(mixedeffects_year, stars =custom_stars, exponentiate = TRUE)
modelsummary(mixedeffects_year,stars = TRUE, exponentiate = TRUE)
modelsummary(mixedeffects_industry,stars = TRUE, exponentiate = TRUE)
logLik_value <- logLik(mixedeffects_industry)


fixed_effects<-glm(is_material_weakness ~ avg_sentence +complexitymetric + 
                     syllablecount+ + factor(industry) +factor(year_of_disclosure),
                   data=data,family = 'binomial')



# First, compute McFadden's R-squared manually if not already calculated
logLik_fitted <- logLik(fixed_effects)  # log-likelihood of the model
null_model <- glm(is_material_weakness ~ 1, data=data, family=binomial(link="logit"))
logLik_null <- logLik(null_model)  # log-likelihood of the null model

mcfadden_r2 <- 1 - (logLik_fitted/logLik_null)  # McFadden's R-squared

# Model summary setup
modelsummary(list("Robust SE" = fixed_effects, "Cluster SE" = fixed_effects),
             stars = c('*' = .05, '**' = .01, '***' = .001),
             exponentiate = TRUE,
             vcov = list("Robust" = "HC3", "Firm-Cluster" = ~company_fkey),
             coef_omit = "factor", 
             notes = list('Standard errors in parenthesis',
                          paste('Log Likelihood: ', format(logLik_fitted, digits=4)),
                          paste('McFadden’s R-squared: ', format(mcfadden_r2, digits=4)))
)
summary(fixed_effects)

modelsummary(list("Robust SE" = mixedeffects_industry, "Cluster SE" = mixedeffects_industry
                  
),
stars = c('*' = .1, '**' = .05, '***' = .01 ),
exponentiate = T,
vcov = list("Robust" = "HC3",
            "Firm-Cluster" = ~company_fkey
            
),
coef_omit = "factor", 
gof_omit = "Log.Lik",
notes = list('Standard errors in parenthesis'))

# Extracting coefficients and their variances
coef_fixed <- coef(fixed_effects)
coef_random <- fixef(mixedeffects_year)
vcov_fixed <- vcov(fixed_effects)
vcov_random <- as.matrix(vcov(mixedeffects_year))

# Align the coefficients and variance-covariance matrices
common_coef_names <- intersect(names(coef_fixed), names(coef_random))
coef_fixed_aligned <- coef_fixed[common_coef_names]
coef_random_aligned <- coef_random[common_coef_names]

vcov_fixed_aligned <- vcov_fixed[common_coef_names, common_coef_names]
vcov_random_aligned <- vcov_random[common_coef_names, common_coef_names]

# Calculate the difference in coefficients
difference <- coef_fixed_aligned - coef_random_aligned

# Compute the covariance matrix of the difference
vcov_diff <- vcov_fixed_aligned - vcov_random_aligned

# Compute the Hausman test statistic
hausman_statistic <- t(difference) %*% solve(vcov_diff) %*% difference

# Chi-square test
p_value <- pchisq(hausman_statistic, df = length(difference), lower.tail = FALSE)

# Display results
hausman_statistic
p_value

# Degrees of freedom
degrees_of_freedom <- length(common_coef_names)
degrees_of_freedom


phtest_glmer <- function (glmerMod, glmMod, ...)  {  ## changed function call
  coef.wi <- coef(glmMod)
  coef.re <- fixef(glmerMod)  ## changed coef() to fixef() for glmer
  vcov.wi <- vcov(glmMod)
  vcov.re <- vcov(glmerMod)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  coef.h <- names.re[names.re %in% names.wi]
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.re[coef.h, coef.h] - vcov.wi[coef.h, coef.h]
  stat <- abs(t(dbeta) %*% as.matrix(solve(dvcov)) %*% dbeta)  ## added as.matrix()
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  names(stat) <- "chisq"
  parameter <- df
  names(parameter) <- "df"
  alternative <- "one model is inconsistent"
  res <- list(statistic = stat, p.value = pval, parameter = parameter, 
              method = "Hausman Test",
              data.name=deparse(getCall(glmerMod)$data))  ## changed
  class(res) <- "htest"
  return(res)
}

phtest_glmer(mixedeffects_year,fixed_effects)





#annova test
# Fit the full model with industry factor
full_model <- glmer(is_material_weakness ~ avg_sentence + complexitymetric + syllablecount +
                      factor(industry) + (1 | year_of_disclosure),
                    data = data, family = binomial(link = "logit"))

# Fit the reduced model without industry factor
reduced_model <- glmer(is_material_weakness ~ avg_sentence + complexitymetric + syllablecount +
                         (1 | year_of_disclosure),
                       data = data, family = binomial(link = "logit"))

# Perform the likelihood ratio test
anova(full_model, reduced_model)


#anova test for Year of disclosure

library(lme4)

# Fit the full model with random effect
full_model <- glmer(is_material_weakness ~ avg_sentence + complexitymetric + syllablecount +
                      factor(industry) + (1 | year_of_disclosure),
                    data = data, family = binomial(link = "logit"))

# Fit the reduced model without random effect
reduced_model_year <- glm(is_material_weakness ~ avg_sentence + complexitymetric + syllablecount +
                       factor(industry),
                     data = data, family = binomial(link = "logit"))

# Perform the likelihood ratio test
anova(full_model, reduced_model_year)


#Anova for Fixed effect
# Load necessary libraries
library(lme4)

# Fit the full model including both industry and year of disclosure
full_model_fixed <- glm(is_material_weakness ~ avg_sentence + complexitymetric + 
                    syllablecount + factor(industry) + factor(year_of_disclosure),
                  data = data, family = 'binomial')

# Fit reduced model excluding industry
reduced_model_no_industry <- glm(is_material_weakness ~ avg_sentence + complexitymetric + 
                                   syllablecount + factor(year_of_disclosure),
                                 data = data, family = 'binomial')

# Fit reduced model excluding year of disclosure
reduced_model_no_year <- glm(is_material_weakness ~ avg_sentence + complexitymetric + 
                               syllablecount + factor(industry),
                             data = data, family = 'binomial')

# Perform likelihood ratio tests
anova(full_model_fixed, reduced_model_no_industry, test="Chisq")
anova(full_model_fixed, reduced_model_no_year, test="Chisq")

