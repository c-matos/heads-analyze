library(rio)
library(janitor)
library(tidyverse)
library(gmodels)
library(table1)

df <- rio::import(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/00383/risk_factors_cervical_cancer.csv",
                  setclass = "tibble") %>%
  janitor::clean_names()

df <- df %>%
  mutate(across(everything(), as.numeric)) %>% #forces "?" to NA
  mutate(across(c(10, 12, 14:25, 29:36), ~ factor(., levels = c(0,1), labels = c("No","Yes")))) %>% #Convert the appropriate columns to factor
  #mutate(across(c(6, 7, 9, 11), as.numeric)) %>% #Convert the appropriate columns to numeric
  mutate(across(c(1:5, 8, 13, 26:28), as.integer)) %>% #Convert the appropriate columns to integer
  rename_all(.funs = ~ str_replace_all(names(df),"st_ds","stds"))


#summary(df)


# Pretende-se que cada grupo submeta um relatório que deve incluir:
#
# a.     Análise descritiva do conjunto de dados.
#
# b.     Identificação das covariáveis significativamente associadas com o outcome "Dx:Cancer", apresentando o efeito dessa associação (por exemplo, OR) e a significância correspondente (por exemplo, valor de p ou IC a 95%).
#
# c.     Modelo de regressão logística multivariável, usando qualquer método de seleção de covariáveis apropriado.
#
# d.     Avaliação do modelo derivado, usando qualquer estratégia de avaliação de modelo apropriada, apresentando estimativas da generalização no uso do modelo como preditor diagnóstico para novos pacientes.
#
# e.     Comentário sobre como sua abordagem e solução final precisariam de ser diferentes se aplicadas ao conjunto de dados drasticamente maior, discutindo sobre as implicações de ter conjuntos de dados “muito grandes”.
## questão de tudo ser significativo, mesmo que não seja particularmente relevante

# Alínea A - Análise descritiva
# descriptives for all variables, singularly
# table1(as.formula(paste("~",
#                         str_c(names(df),
#                               collapse = " + "))),
#        data = df)


# Mean age was


#Descriptives over variable "dx_cancer"
# table1(as.formula(paste("~",
#                         str_c(names(df)[names(df) != "dx_cancer"],
#                               collapse = " + "),
#                         "|",
#                         "dx_cancer")),
#       data = df)

view(dfSummary(df))

## TODO: comentar

# ALÍNEA B - Covariáveis significativamente associadas a dx_cancer -----> OR + p ou confidence level

## Testes de hipóteses
## contínua + dicotómica

## check for normality
## check for homogeneity of variances


#Confidence level
alpha <- 0.05


#Comparing dichotomous variables with dx_cancer (compare proportions)

factor_variables <- df %>% select(is.factor) %>% names

CrossTable(x = df$dx_cancer, y = df$smokes, format = 'SPSS', expected = T, chisq = T, fisher = T)

## infer - Two categorical (2 level) variables
d_hat <- df %>%
  drop_na(dx_cancer, smokes) %>%
  specify(dx_cancer ~ smokes, success = "Yes") %>%
  calculate(stat = "diff in props", order = c("No", "Yes"))

sampling_dist <- df %>%
  drop_na(dx_cancer, smokes) %>%
  specify(dx_cancer ~ smokes, success = "Yes") %>%
  assume(distribution = "z")

sampling_dist %>%
  get_p_value(obs_stat = d_hat, direction = "two-sided")

theor_ci <- get_ci(sampling_dist, point_estimate = d_hat)


## infer - Two categorical (2 level) variables (Z)
z_hat <- df %>%
  drop_na(dx_cancer, smokes) %>%
  specify(dx_cancer ~ smokes, success = "Yes") %>%
  hypothesise(null = "independence") %>%
  calculate(stat = "z", order = c("No", "Yes"))

#alternative to z_hat
df %>%
  drop_na(dx_cancer, smokes) %>%
  observe(dx_cancer ~ smokes, success = "Yes", stat = "z", order = c("No", "Yes"))

null_dist <- df %>%
  drop_na(dx_cancer, smokes) %>%
  specify(dx_cancer ~ smokes, success = "Yes") %>%
  assume("z")

null_dist %>%
  get_p_value(obs_stat = z_hat, direction = "two-sided")

prop_test(df, dx_cancer ~ smokes, order = c("No", "Yes"))

chisq_test(df, formula = dx_cancer ~ smokes)

## with prop.test
prop.test(x = c(18,123), n = c(858, 845), alternative = "two.sided", correct = F)
