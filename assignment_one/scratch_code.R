# NOT RUN {
# First, make a pdata.frame
data("Grunfeld", package = "plm")
pGrunfeld <- pdata.frame(Grunfeld)

# then make a model frame from a pFormula and a pdata.frame


form <- inv ~ value + capital
mf <- model.frame(pGrunfeld, form)
# construct (transformed) response of the within model
resp <- pmodel.response(form, data = mf, model = "within", effect = "individual")
# retrieve (transformed) response directly from model frame
resp_mf <- pmodel.response(mf, model = "within", effect = "individual")

# retrieve (transformed) response from a plm object, i.e. an estimated model
fe_model <- plm(form, data = pGrunfeld, model = "within")
pmodel.response(fe_model)

# same as constructed before
all.equal(resp, pmodel.response(fe_model), check.attributes = FALSE) # TRUE


# Question 2
df_2b <- df_light

# Using set.seed to may results reproducible
set.seed(123)

# Selecting the 150 countries
df_150_countries <- df_2b %>% 
  select(country) %>% 
  distinct() %>% 
  sample_n(150) %>% 
  mutate(training_group = TRUE)

df_2b <- left_join(df_2b, df_150_countries, by = "country") %>% 
  distinct() %>% 
  mutate(
    training_group = 
      if_else(training_group == TRUE, TRUE, FALSE, missing = FALSE),
    date = as_factor(date)) %>% 
  filter(
    !is.na(log_gdp) &
      !is.na(log_population) & 
      !is.na(log_dmsp))

# Dataframe for countries in training group
df_2b_150 <- df_2b %>% filter(training_group==TRUE)

# Dataframe for 30 countries in prediction group
df_2b_30 <- df_2b %>% filter(training_group==FALSE)




gdp_mod <- plm(log_gdp ~ log_population + log_dmsp,
               data = df_2b_150,
               index = c("date"),
               model = "within")

summary(gdp_mod)

model_coefficients <- coef(gdp_mod)
pop_coef <- model_coefficients [1]
dmsp_coef <- model_coefficients[2]

df_2b_30 <- df_2b_30 %>% 
  mutate(
    pop_coef_col = pop_coef,
    dmsp_doef_col = dmsp_coef) 

df_2b_30 

b <- a %>% tibble() %>% pivot_wider()

#predict(gdp_mod, df_2b_30)
#pmodel.response(gdp_mod, df_2b_30)
#predict.plm


p_2b_150 <- pdata.frame(df_2b_150, index  = c("date"))
new_mod <- plm(log_gdp ~ log_population + log_dmsp,
               data = p_2b_150,
               model = "within")


new_data <- df_2b_30 %>% select(log_population, log_dmsp, date)

summary(new_mod)
mod_resp <- pmodel.response(new_mod)

predict(gdp_mod, p_2b_150)
predict(new_mod, data.frame(log_population = 15, log_dmsp = 5, date=2005))

###
lm_mod <- lm(log_gdp ~ log_population + log_dmsp + date -1, data = df_2b_150)
#summary(lm_mod)
temp2 <- predict(lm_mod, df_2b_30)
temp3 <- temp2 %>% tibble() %>% rename(lm_pred_value = ".")

###



p_2b <- pdata.frame(df_2b, index  = c("date"))
p_2b2 <- p_2b %>% filter(!is.na(log_gdp) & !is.na(log_population) & !is.na(log_dmsp)) %>% select(date, log_gdp, log_population, log_dmsp) 

p_2b_150 <- p_2b %>% 
  filter(training_group==TRUE) %>% 
  filter(
    !is.na(log_gdp) & 
      !is.na(log_population) & 
      !is.na(log_dmsp)) %>% 
  select(date, log_gdp, log_population, log_dmsp) 

p_2b_30 <- p_2b %>% 
  filter(training_group==FALSE) %>% 
  filter(
    !is.na(log_gdp) & 
      !is.na(log_population) &
      !is.na(log_dmsp)) %>% 
  select(date, log_gdp, log_population, log_dmsp) 

###
new_mod2 <- plm(log_gdp ~ 0 + log_population + log_dmsp,
                data = p_2b_150,
                model = "within")

temp6 <- predict(new_mod2, newdata = p_2b_30) %>% tibble() %>% rename(plm_pred_value = ".")
temp7 <- bind_cols(p_2b_30, temp6)
temp8 <- bind_cols(temp7, temp3)


temp8 %>%
  ggplot(aes(x=plm_pred_value, y =log_gdp)) +
  geom_point()

temp8 %>%
  ggplot(aes(x=lm_pred_value, y = log_gdp)) +
  geom_point()


###

fitted <- pmodel.response(plm.model)-residuals(plm.model)

temp4 <- p_2b2 %>% filter(!is.na(log_gdp) & !is.na(log_population) & !is.na(log_dmsp)) %>% mutate(y_pred = predict(new_mod2))
temp5 <- temp4 %>% select(date, log_gdp, log_population, log_dmsp)


temp4 %>%
  ggplot(aes(x=log_gdp, y = y_pred)) +
  geom_point()

# r - Predict out of sample on fixed effects model - Stack Overflow
# https://stackoverflow.com/questions/65702581/predict-out-of-sample-on-fixed-effects-model
# 
# Is there a predict function for PLM in R? - Stack Overflow
# https://stackoverflow.com/questions/7123060/is-there-a-predict-function-for-plm-in-r
# 
# evantrowbridge/energy_dev_world
# https://github.com/evantrowbridge/energy_dev_world
# 
# 10.4 Regression with Time Fixed Effects | Introduction to Econometrics with R
# https://www.econometrics-with-r.org/10-4-regression-with-time-fixed-effects.html
# 
# pmodel.response out of sample - Google Search
# https://www.google.com/search?sxsrf=ALeKk03G443ZnOJNmtDDK9IQikvlBWDlVw:1619615571251&q=pmodel.response+out+of+sample&nfpr=1&sa=X&ved=2ahUKEwjb2q-WgqHwAhXLDDQIHY5bBCoQvgUoAXoECAEQMg&biw=1292&bih=1603
# 
# [R] Manual two-way demeaning of unbalanced panel data (Wansbeek/Kapteyn transformation)
# https://stat.ethz.ch/pipermail/r-help/2013-January/344997.html
# 
# plm function - RDocumentation
# https://www.rdocumentation.org/packages/plm/versions/2.4-1/topics/plm
# 
# plm predict r - Bing
# https://www.bing.com/search?q=plm+predict+r&qs=n&form=QBRE&sp=-1&ghc=1&pq=plm+predict+r&sc=1-13&sk=&cvid=71FDB6BB9109434788496F54AA1778FF
# 
# predict - Predicting with plm function in R - Stack Overflow
# https://stackoverflow.com/questions/41260493/predicting-with-plm-function-in-r
# 
# Is there a predict function for PLM in R? - Stack Overflow
# https://stackoverflow.com/questions/7123060/is-there-a-predict-function-for-plm-in-r?answertab=active#tab-top
# 
# r - Prediction with plm method - Stack Overflow
# https://stackoverflow.com/questions/28547614/prediction-with-plm-method
# 
# r - plm: using fixef() to manually calculate fitted values for a fixed effects twoways model - Stack Overflow
# https://stackoverflow.com/questions/19209689/plm-using-fixef-to-manually-calculate-fitted-values-for-a-fixed-effects-twowa
# 
# how to predict out of sample fixed effects r - Bing
# https://www.bing.com/search?q=how+to+predict+out+of+sample+fixed+effects+r&cvid=683d4abbe6294a4a85e4c9bd18c42c2c&aqs=edge..69i57.9968j0j4&FORM=ANAB01&PC=U531
# 
# Predicting Outcome Variable for out-of-sample Panel data R - Cross Validated
# https://stats.stackexchange.com/questions/331567/predicting-outcome-variable-for-out-of-sample-panel-data-r
# 
# +pmodel.response r - Bing
# https://www.bing.com/search?q=%2bpmodel.response+r&filters=rcrse%3a%221%22&FORM=RCRE
# 
# pmodel.response function - RDocumentation
# https://www.rdocumentation.org/packages/plm/versions/2.2-3/topics/pmodel.response
# 
# plm: vignettes/A_plmPackage.Rmd
# https://rdrr.io/rforge/plm/f/vignettes/A_plmPackage.Rmd
# 
# https://cran.r-project.org/web/packages/plm/plm.pdf
# https://cran.r-project.org/web/packages/plm/plm.pdf
# 
# pmodel.response r - Bing
# https://www.bing.com/search?q=pmodel.response+r&cvid=9cfe279a8fcc4041af9a9f926a03c7d5&aqs=edge.0.69i59j69i60l2.7962j0j4&FORM=ANAB01&PC=U531
# 
# r predict plm - Google Search
# https://www.google.com/search?q=r+predict+plm&oq=r+predict+plm&aqs=edge..69i64j69i57.6794j0j9&sourceid=chrome&ie=UTF-8
# 
# Is there a predict function for PLM in R? - Stack Overflow
# https://stackoverflow.com/questions/7123060/is-there-a-predict-function-for-plm-in-r
# 
# predict - Predicting with plm function in R - Stack Overflow
# https://stackoverflow.com/questions/41260493/predicting-with-plm-function-in-r
# 
# Quick-R: Correlations
# https://www.statmethods.net/stats/correlations.html
# 
# Predict in R: Model Predictions and Confidence Intervals - Articles - STHDA
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
#   
#   Linear Regression for Predictive Modeling in R –
# https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/
#   
#   r - Prediction using Fixed Effects - Stack Overflow
# https://stackoverflow.com/questions/45286538/prediction-using-fixed-effects
# 
# R: Predicting future panel data - Cross Validated
# https://stats.stackexchange.com/questions/359306/r-predicting-future-panel-data
# 
# predict.plm - Google Search
# https://www.google.com/search?q=predict.plm&oq=predict.plm&aqs=edge..69i64j0i30l2j0i8i30j69i57j69i60.2993j0j1&sourceid=chrome&ie=UTF-8
# 
# R: Predicting future panel data - Cross Validated
# https://stats.stackexchange.com/questions/359306/r-predicting-future-panel-data
# 
# Predicting forecasting a regression - General - RStudio Community
# https://community.rstudio.com/t/predicting-forecasting-a-regression/55582/5
# 
# r predict fixed effects - Google Search
# https://www.google.com/search?q=r+predict+fixed+effects&sxsrf=ALeKk00MBHO51onSjZOiEnWNtsTNYTA4Ag%3A1619573652186&source=hp&ei=lLuIYOrYB8T0tAbBxqHwCw&iflsig=AINFCbYAAAAAYIjJpEIuhxbkRXqIPbrngODXjth2b40c&oq=r+predict+with+fixed+&gs_lcp=Cgdnd3Mtd2l6EAMYADIGCAAQFhAeMgYIABAWEB46BAgjECc6BQgAEJECOgIIADoLCC4QsQMQxwEQowI6BQgAELEDOggILhCxAxCDAToECAAQQzoKCC4QhwIQsQMQFDoHCAAQsQMQQzoLCAAQsQMQgwEQkQI6CAguEMcBEK8BOgoIABCHAhCxAxAUOgcIABCHAhAUUM4MWJw5YNVBaABwAHgAgAFkiAHPDJIBBDE5LjKYAQCgAQGqAQdnd3Mtd2l6&sclient=gws-wiz
# 
# Computing the predicted value from a panel data model with the plm R package - Cross Validated
# https://stats.stackexchange.com/questions/147350/computing-the-predicted-value-from-a-panel-data-model-with-the-plm-r-package
# 
# r - Prediction using Fixed Effects - Stack Overflow
# https://stackoverflow.com/questions/45286538/prediction-using-fixed-effects
# 
# named number to tibble - Bing
# https://www.bing.com/search?q=named+number+to+tibble&cvid=06be6f08037c4f4bb693007cf54cf299&aqs=edge..69i57.2633j0j1&pglt=417&FORM=ANNTA1&PC=U531
# 
# r - How to convert list of list into a tibble (dataframe) - Stack Overflow
# https://stackoverflow.com/questions/45452015/how-to-convert-list-of-list-into-a-tibble-dataframe
# 
# How to compute correlations between all columns in R and detect highly correlated variables - Stack Overflow
# https://stackoverflow.com/questions/22282531/how-to-compute-correlations-between-all-columns-in-r-and-detect-highly-correlate
# 
# Build a data frame — tibble • tibble
# https://tibble.tidyverse.org/reference/tibble.html
# 
# Associate Program - Manila — IDinsight
# https://www.idinsight.org/clientfacing-asia/associate-program-manila?utm_sq=gpv65ia43g
# 
# name column r - Bing
# https://www.bing.com/search?q=name+column+r&cvid=9f7bd5cf70f0409994f25d290294ea97&aqs=edge.0.0j69i60.1529j0j1&pglt=417&FORM=ANNTA1&PC=U531
# 
# Rename Column Name in R | 3 Examples to Change Data Frame Colnames
# https://statisticsglobe.com/rename-column-name-in-r-data-frame/
#   
#   correlation between columns r - Bing
# https://www.bing.com/search?q=correlation+between+columns+r&cvid=b29cdfb287044e95ab721fd239910819&aqs=edge..69i57j0l6.4745j0j1&pglt=417&FORM=ANNTA1&PC=U531
# 
