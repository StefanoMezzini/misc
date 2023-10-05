#' if you need to install packges, use `install.packages('my_package')`
# install.packages(c('dyplr', 'tidyr', 'ggplot2', 'brms'))

library('dplyr')   #' for data wrangling: `mutate()`, `%>%`
library('tidyr')   #' for data wrangling: `expand_grid()`, `pivot_longer()`
library('ggplot2') #' for fancy plots
library('brms')    #' for `logit_scaled()` and `inv_logit_scaled()`

#' *NOTE:* here we use the `brms` functions for scaled logit because the
#'         original code was written for analyses using a score of
#'         satisfaction with life with 5 questions with scores from 1 to 5.
#'         Consequently, the min score was 5 * 1 = 5, while the max score
#'         was 5 * 7 = 35. The same code can be applied to any (rescaled)
#'         variable assumed to follow a beta family of distributions.

#' Assuming:
#' 1. that the null hypothesis (H_0) is beta = 0
#' 2. that the alternative hypothesis is H_a: beta =/= 0 (one-sided test)
#' 3. that we choose alpha 0.05,

#' We need the following information to do a power analysis:
#' 1. the model,
#' 2. an estimate of the variance of the model coefficients,
#' 3. a range of possible sample sizes
#' see the figure at https://www.statlect.com/glossary/power-function
#' without an estimate of the variance, we can decide what ranges of values
#' make sense using prior predictive simulations.

# function to calculate change in mu for a change of 1 in a given
# coefficient
delta_mu <- function(mu_0, beta) {
  mu_1 <- inv_logit_scaled(logit_scaled(mu_0, lb = 5, ub = 35) + beta,
                           lb = 5, ub = 35)
  return(mu_1 - mu_0)
}

#' assuming a linear effect on the logit scale, people with a low SwL
#' improve more than those with an already high SwL, but those with a
#' *very* low SwL do not improve as much
#' *using a beta GLM rather than a (Gaussian) LM allows us to understand*
#' *these nonlinear effects better*
#' try changing `beta` to see how changing the effect size affects the
#' change in `mu`
tibble(mu_0 = seq(5.001, 34.999, length.out = 400),
       d_mu = delta_mu(mu_0 = mu_0, beta = 1)) %>%
  plot(d_mu ~ mu_0, ., ylim = c(-30, 30), type = 'l', col = 'red', lwd = 2,
       xlab = 'Initial satisfaction with life',
       ylab = 'Change in satisfaction with life')

# visualize the effect sizes ----
d_1 <- expand_grid(beta = seq(-10, 10, length.out = 100),
                   mu_0 = seq(5.001, 34.999, length.out = 400)) %>%
  mutate(delta_mu = delta_mu(mu_0 = mu_0, beta = beta))

ggplot() +
  geom_raster(aes(mu_0, beta, fill = delta_mu), d_1) +
  geom_hline(yintercept = 0, color = 'grey') +
  scale_x_continuous('Initial satisfaction with life', expand = c(0, 0)) +
  scale_y_continuous('Effect size (on logit scale)', expand = c(0, 0)) +
  scale_fill_distiller('Change in satisfaction with life', type = 'div',
                       palette = 5, limits = c(-30, 30), direction = 1) +
  theme(legend.position = 'top')

#' regions that are reasonable (i.e., where `delta_mu < 10`)
ggplot() +
  geom_raster(aes(mu_0, beta, fill = abs(delta_mu) < 10), d_1) +
  geom_hline(yintercept = 0, color = 'grey') +
  scale_x_continuous('Initial satisfaction with life', expand = c(0, 0)) +
  scale_y_continuous('Effect size (on logit scale)', expand = c(0, 0)) +
  scale_fill_brewer('Change in satisfaction with life', type = 'qual',
                    palette = 6, direction = 1) +
  theme(legend.position = 'top')
# SD(beta) = 1 is quite conservative

# add power estimates
d_1_p <-
  expand_grid(beta_a = seq(-5, 5, length.out = 400),
              n = 2^seq(log2(2), log2(500), length.out = 400)) %>%
  mutate(sd_beta = 0.5, #' *need to change this based on figures above*
         se_beta = sd_beta / sqrt(n), # standard error in beta
         RR_cutoff = qnorm(p = 0.05, # alpha = 0.05, one-sided test
                           mean = 0, # based on H_0
                           sd = se_beta, # SE(beta_hat)
                           lower.tail = FALSE), # right-side area
         # power is P(Z > z* | H_a)
         power = pnorm(q = RR_cutoff,
                       mean = beta_a, # based on H_a
                       sd = se_beta, # SE(beta_hat)
                       lower.tail = FALSE)) # to look at right-side tail

#' sanity checks ----
# rejection region always decreases with sample size
filter(d_1_p, beta_a > 0) %>%
  filter(beta_a == min(beta_a)) %>%
  plot(RR_cutoff ~ n, .)

filter(d_1_p, beta_a < 0) %>%
  filter(beta_a == max(beta_a)) %>%
  plot(RR_cutoff ~ n, .)

# power increases with n if the true beta is > 0...
filter(d_1_p, beta_a > 0) %>%
  filter(beta_a == min(beta_a)) %>%
  plot(power ~ n, .)

# ...and it decreases with n if the true beta is negative
filter(d_1_p, beta_a < 0) %>%
  filter(beta_a == max(beta_a)) %>%
  plot(power ~ n, .)

# simplify based on a cutoff
CUTOFF <- 0.8

ggplot() +
  geom_raster(aes(beta_a, n, fill = power > CUTOFF), d_1_p) +
  scale_x_continuous(expression(Unknown~effect~size~(beta)),
                     expand = c(0, 0)) +
  scale_y_continuous(expression(Sample~size~(log[2]~scale)),
                     expand = c(0, 0), trans = 'log2') +
  scale_fill_brewer(paste('Statistical power >', CUTOFF),
                    type = 'qual', palette = 6)+
  theme(legend.position = 'top')

# choose a sample size from the figure above
