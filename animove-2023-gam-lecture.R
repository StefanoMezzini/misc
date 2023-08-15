#' if needed install the `faraway` package: `install.packages('faraway')`
library('mgcv')    # for Generalized Additive Models (GAMs) and more families
library('faraway') # for datasets
library('ggplot2') # for fancy plots
library('dplyr')   # for data wrangling
theme_set(theme_bw())

?mgcv::family.mgcv

# when terms are nonlinear, use a GAM ----
# simulated Normalized Difference Vegetation Index ("greenness") ----
source('https://github.com/StefanoMezzini/misc/raw/main/simulated-ndvi-data.R')
head(ndvi_data)

ggplot(ndvi_data, aes(date, ndvi)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

ggplot(ndvi_data, aes(doy, ndvi)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

ggplot(ndvi_data, aes(year, ndvi)) +
  geom_point(alpha = 0.3) +
  stat_summary(fun = 'mean', color = 'blue')

# starting with a LM
lm_ndvi <- gam(ndvi ~ year + doy,
               family = gaussian(),
               data = ndvi_data)

# predict from the model
preds <- mutate(ndvi_data, year = lubridate::decimal_date(date))
ndvi_data$lm <- predict(lm_ndvi, newdata = preds)

# LM fits the data badly 
ggplot(ndvi_data) +
  geom_point(aes(date, ndvi)) +
  geom_line(aes(date, lm), color = 'red', linewidth = 1)

# fit a GLM instead ----
# convert NDVI from (-1, 1) to (0, 1)
ndvi_data <- mutate(ndvi_data,
                    ndvi_01 = (ndvi + 1) / 2,
                    dec_date = lubridate::decimal_date(date))

glm_ndvi <- gam(ndvi_01 ~ year + doy,
                family = betar(link = 'logit'),
                data = ndvi_data)

# predict from the GLM
inv_logit <- glm_ndvi$family$linkinv
ndvi_data <- mutate(ndvi_data,
                    eta = predict(glm_ndvi, newdata = preds),
                    mu = inv_logit(eta), # mu = g^-1(eta)
                    glm = mu * 2 - 1) # estimated NDVI from the GLM

# GLM also fits the data badly 
ggplot(ndvi_data) +
  geom_point(aes(date, ndvi)) +
  geom_line(aes(date, lm), color = 'red', linewidth = 1) +
  geom_line(aes(date, glm), color = 'darkorange', linewidth = 1)

# fit a GAM ----
# make the doy smooth continuous over the cycles
range(ndvi_data$doy) # place knots a bit before 1 and a bit after 366
gam_ndvi <-
  gam(ndvi_01 ~ year + s(doy, bs = 'cc'),
      family = betar(link = 'logit'),
      data = ndvi_data,
      method = 'REML', # REsidual Maximum Likelihood
      knots = list(doy = c(0.5, 366.5)))

plot(gam_ndvi, scheme = 1, all.terms = TRUE, pages = 1)
layout(1)

# predict using the GAM
ndvi_data <- mutate(ndvi_data,
                    gam = inv_logit(predict(gam_ndvi, newdata = preds))*2-1)

# GAM fits the data much better 
ggplot(ndvi_data) +
  geom_point(aes(date, ndvi)) +
  geom_line(aes(date, lm), color = 'red', linewidth = 1) +
  geom_line(aes(date, glm), color = 'darkorange', linewidth = 1) +
  geom_line(aes(date, gam), color = 'darkblue', linewidth = 1)

# variance isn't constant
plot(ndvi_data$date, residuals(gam_ndvi), pch = 19, cex = 0.5,
     col = '#00000040') # black with some alpha

# using chol, age, gender, and weight as predictors and... ----
?diabetes # see "Details" section
range(diabetes$glyhb, na.rm = TRUE)

## ... glyhb as the response ----
m_diab_1 <- gam(glyhb ~ s(chol) + s(age) + gender + s(weight),
                family = Gamma(link = 'log'),
                data = diabetes,
                method = 'REML') # REstricted Maximum Likelihood

# plotting the GAM with base plot()
plot(m_diab_1,
     all.terms = TRUE, # include parametric gender term
     pages = 1,        # all plots in one page
     scheme = 1)       # use shaded CIs instead of dashed lines

plot(m_diab_1,
     all.terms = TRUE, # include parametric gender term
     pages = 1,        # all plots in one page
     scheme = 1,       # use shaded CIs instead of dashed lines
     trans = exp)      # return to response scale, i.e. (0, Inf)

## ... a binary variable of diabetic or not as the response ----
diabetes <- mutate(diabetes, diabetic = glyhb > 7)

ggplot(data = diabetes) +
  geom_histogram(aes(glyhb, fill = diabetic), bins = 12, color = 1) +
  geom_vline(xintercept = 7, color = 2) +
  scale_fill_brewer('Diabetic', type = 'qual', palette = 6,
                    labels = c('No', 'Yes'), direction = -1)

m_diab_2 <- gam(diabetic ~ s(chol) + s(age) + gender + s(weight),
                family = binomial(link = 'logit'),
                data = diabetes,
                method = 'REML')

plot(m_diab_2, all.terms = TRUE, pages = 1, scheme = 1,
     trans = m_diab_2$family$linkinv)

# location-scale GAMs ----
# estimated yearly temperature during the last millennium ----
?globwarm
head(globwarm)

m_temp <- gam(jasper ~ s(year), # using default k
              family = gaussian(),
              data = globwarm,
              method = 'REML')
plot(m_temp, scheme = 1, residuals = TRUE, cex = 2)

# using a higher k to allow the term to be more wiggly
m_temp <- gam(jasper ~ s(year, k = 30),
              family = gaussian(),
              data = globwarm,
              method = 'REML')
plot(m_temp, scheme = 1, residuals = TRUE, cex = 2)

# super wiggly!
gam(jasper ~ s(year, k = 300),
    family = gaussian(),
    data = globwarm,
    method = 'REML') %>%
  plot()

# variance isn't constant
plot(globwarm$year, residuals(m_temp), pch = 19, cex = 0.5,
     col = '#00000040') # black with some alpha

# when variance isn't constant, use a location-scale model ----
# simulated NDVI ----
m_ndvi_ls <-
  gam(list(ndvi ~ factor(year) + s(doy, bs = 'cc'),
           ~ s(doy, bs = 'cc')),
      family = gaulss(),
      data = ndvi_data,
      method = 'REML',
      knots = list(doy = c(0, 366))) # make 0 and 366 be the same day

plot(m_ndvi_ls, scheme = 1, all.terms = FALSE, pages = 1)

# temperature data ----
m_temp_ls <-
  gam(list(jasper ~ s(year, k = 30),
           ~ s(year, k = 10)),
      family = gaulss(),
      data = globwarm,
      method = 'REML',
      knots = list(doy = c(0, 366))) # make 0 and 366 be the same day

# can't convert to response scale easily because there are 2 link functions
plot(m_temp_ls, scheme = 1, all.terms = FALSE, pages = 1, n = 400)

# choose k carefully!
gam(list(jasper ~ s(year, k = 100),
         ~ s(year, k = 10)),
    family = gaulss(),
    data = globwarm,
    method = 'REML',
    knots = list(doy = c(0, 366))) %>%
  plot(scheme = 1, all.terms = FALSE, pages = 1, n = 400)

gam(list(jasper ~ s(year, k = 5),
         ~ s(year, k = 10)),
    family = gaulss(),
    data = globwarm,
    method = 'REML',
    knots = list(doy = c(0, 366))) %>%
  plot(scheme = 1, all.terms = FALSE, pages = 1)

