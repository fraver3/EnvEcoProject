
################################################################################
################ PARAMETRIC BOOTSTRAP FOR GPD RETURN LEVELS ####################
################################################################################

#Now, we can do this with the GPD model, model_avg, defining a new self-contained 
#function to estimate return levels:
gpd_rl <- function(u, sigma, xi, T, m = 52, zeta_u) {  # m IS HOW MANY OBSERVATIONS WE HAVE IN A YEAR, SO IN THIS CASE 12*NUMBER OF SITES (?)
  
  lambda_T <- T * m * zeta_u
  
  if (abs(xi) > 1e-6) {
    z <- u + (sigma / xi) * (lambda_T^xi - 1)
  } else {
    z <- u + sigma * log(lambda_T)
  }
  
  return(z)
}

#We see that we need the threshold, the parameters, the return period and zeta_u 
#(the exceedance probability) as arguments. Let's start with the last one.
#We can estimate it with its MLE, as defined in the report:
zeta_u <- mean(weekly_data$avg_flux_weekly > threshold, na.rm = TRUE)

#Now, we can extract the parameter estimates, conditional on some chosen covariate
#value. We only have to set a value for the average background level estimate and
#we will use its overall mean:
newdat_gpd <- data.frame(
  avg_background_multiplied = mean(weekly_data$avg_background_multiplied, na.rm = TRUE)  #SET COVARIATE VALUES CONDITIONAL ON WHICH WE WANT TO OBTAIN RETURN LEVELS,
                                                                                         #NOW YOU NEED TO SET SITE, WIND AND YEAR.
                                                                                         # YEAR = LAST YEAR (TO ACCOUNT FOR THE DECREASING TREND IN SOME WAY);
                                                                                         # WIND = OVERALL MEAN FOR WIND;
                                                                                         # SITE = DO WE HAVE ANY INTERESTING SITES? I WOULD SET IT TO THE SITE WITH MOST
                                                                                         #        EXCEEDANCES, BECAUSE IT WILL BE THE MOST LIKELY TO HAVE HIGH VALUES. 
                                                                                         # (ALTERNATIVE FOR SITE: USE EVERY SITE AND COMPUTE RETURN LEVELS SEPARATELY, THEN PLOT TOGETHER, DK)
)
#Then, we can obtain the parameter estimates:
pars_fixed_gpd <- predict(model_avg,
                          newdata = newdat_gpd,
                          type = "response")

sigma_hat_gpd <- pars_fixed_gpd[1, "scale"]
xi_hat_gpd    <- pars_fixed_gpd[1, "shape"]

z_hat_gpd <- gpd_rl(
  u       = threshold,
  sigma   = sigma_hat_gpd,
  xi      = xi_hat_gpd,
  T       = T_seq,
  m       = 52,
  zeta_u  = zeta_u
)

#At the same time, we can also retrieve the return period for a week of solar flare activity
#as intense as the peak week from the Great Halloween solar storms, that is taken as our 
#reference week in terms of fixing covariate values.
gpd_return_period <- function(z, u, sigma, xi, m = 52, zeta_u) {
  
  if (abs(xi) > 1e-6) {
    
    term <- 1 + xi * (z - u) / sigma
    
    if (term <= 0) return(NA_real_)
    
    T <- (term)^(1/xi) / (m * zeta_u)
    
  } else {
    
    T <- exp((z - u)/sigma) / (m * zeta_u)
    
  }
  
  return(T)
}
z_max <- max(weekly_data$avg_flux_weekly, na.rm = TRUE) #VALORE ESATTO PER CUI VOGLIAMO CALCOLARE IL RETURN PERIOD (180?)

T_hat_gpd <- gpd_return_period(
  z       = z_max,
  u       = threshold,
  sigma   = sigma_hat_gpd,
  xi      = xi_hat_gpd,
  m       = 52,
  zeta_u  = zeta_u
)

#Then, we can build our bootstrap loop.
#First, we extract fitted parameters for all observations:
pars_hat <- predict(model_avg, type = "parameter")
sigma_hat_all <- exp(pars_hat[, "logscale"])
xi_hat_all    <- pars_hat[, "shape"]

#Then, we build a function to simulate from the GPD model:
rgpd_boot <- function(n, sigma, xi) {
  u <- runif(n)
  z <- numeric(n)
  
  idx <- abs(xi) > 1e-6
  z[idx] <- (sigma[idx] / xi[idx]) * (u[idx]^(-xi[idx]) - 1)
  z[!idx] <- -sigma[!idx] * log(u[!idx])
  
  return(z)
}

#Finally, we can initialise the matrices of results and start the loop:
z_boot_gpd <- matrix(NA_real_, nrow = B, ncol = length(T_seq))
T_boot_gpd <- rep(NA_real_, B)

for (b in seq_len(B)) {
  
  # 1. We simulate excesses from fitted GPD
  y_sim <- rgpd_boot(
    n     = length(sigma_hat_all),
    sigma = sigma_hat_all,
    xi    = xi_hat_all
  )
  
  # 2. We need to reconstruct the full variable, so we add to the simulated excesses 
  #also the threshold value:
  data_b <- model_avg$data
  data_b$flux_excess <- y_sim
  data_b$avg_flux_weekly <- threshold + y_sim
  
  # 3. We refit the model:
  fit_b <- try(
    evgam(formula = formula5,
          data    = data_b,
          family  = "gpd"),
    silent = TRUE
  )
  
  if (inherits(fit_b, "try-error")) next
  
  # 4. We predict parameter estimates at the usual fixed covariate value:
  pars_b <- predict(fit_b,
                    newdata = newdat_gpd,
                    type = "response")
  
  if (any(is.na(pars_b))) next
  
  sigma_b <- pars_b[1, "scale"]
  xi_b    <- pars_b[1, "shape"]
  
  #Recall that we must recompute exceedance probability:
  zeta_b <- mean(data_b$avg_flux_weekly > threshold, na.rm = TRUE)
  if (zeta_b == 0) next
  
  # 5. Finally, we compute return levels and the return period to the largest event
  #in the dataset:
  z_row <- gpd_rl(
    u       = threshold - harmonic_value_gpd,
    sigma   = sigma_b,
    xi      = xi_b,
    T       = T_seq,
    m       = 52,
    zeta_u  = zeta_b
  )
  
  if (all(is.finite(z_row))) {
    z_boot_gpd[b, ] <- z_row
  }
  T_b <- gpd_return_period(
    z       = z_max - harmonic_value_gpd,
    u       = threshold,
    sigma   = sigma_b,
    xi      = xi_b,
    m       = 52,
    zeta_u  = zeta_b
  )
  
  if (is.finite(T_b)) {
    T_boot_gpd[b] <- T_b
  }
}

#Now, we can compute confidence intervals:
z_lower_gpd <- apply(z_boot_gpd, 2, quantile, probs = 0.025, na.rm = TRUE)
z_upper_gpd <- apply(z_boot_gpd, 2, quantile, probs = 0.975, na.rm = TRUE)

rl_ci_gpd <- data.frame(
  Return_Period = T_seq,
  RL_estimate   = z_hat_gpd,
  RL_lower      = z_lower_gpd,
  RL_upper      = z_upper_gpd)

T_lower_gpd <- quantile(T_boot_gpd, 0.025, na.rm = TRUE)
T_upper_gpd <- quantile(T_boot_gpd, 0.975, na.rm = TRUE)

rp_largestflare_gpd <- c(
  Estimate = T_hat_gpd,
  Lower    = T_lower_gpd,
  Upper    = T_upper_gpd)

rp_largestflare_gpd

#We can save the datasets, as we have done before:
saveRDS(rl_ci_gpd, "return_levels_gpd.rds")
write.csv(rp_largestflare_gpd, "Halloween_week_return_period.csv", row.names = TRUE)
#In this way, now we can access this any time we want simply by running:
rl_ci_gpd <- readRDS("return_levels_gpd.rds")
rp_largestflare_gpd <- read.csv("Halloween_week_return_period.csv", 
                                header = TRUE, 
                                row.names = 1, 
                                stringsAsFactors = FALSE)
#This return period is extremely large. However, this is not so surprising if we 
#look at the data, given that there is a really large difference between the largest
#value for the average weekly flare flux and the second largest one. 
#Moreover, we notice that there is an extremely high variability in this estimate.
#The lower bound is around 4 years, while the upper bound is larger than a billion years,
#which is something effectively impossible on geological time scales. This is due
#to the fact that we have a large positive value for the shape, meaning that return
#periods explode rapidly, especially for the upper bound, due to extreme extrapolation.
#So, we can say that the estimated return period is approximately 593 years, but the
#uncertainty is extremely large, ranging from a few decades to billions of years.
#The data do not pin down the tail sufficiently to estimate such a high return period
#with acceptable precision.

#Can we combine the two ggplots, in order to compare the two return levels on the 
#same scale?
rl_ci_gpd$Model <- "GPD"
rl_ci_gev$Model <- "GEV"
rl_all <- bind_rows(rl_ci_gpd, rl_ci_gev)

ggplot() +
  geom_ribbon(
    data = rl_all,
    aes(x = Return_Period, ymin = RL_lower, ymax = RL_upper, fill = Model),
    alpha = 0.25
  ) +
  geom_line(
    data = rl_all,
    aes(x = Return_Period, y = RL_estimate, colour = Model),
    linewidth = 1.2
  ) +
  scale_x_log10() +
  labs(
    x = "Return period (years, log scale)",
    y = "Return level (W/m²)",
    title = "Return Level Plot: GPD vs GEV"
  ) +
  scale_colour_manual(
    name = "Model",
    values = c("GPD" = "darkblue", "GEV" = "red"),
    labels = c("GPD" = "GPD (averages)", "GEV" = "GEV (maxima)")) +
  scale_fill_manual(
    name = "Model",
    values = c("GPD" = "steelblue", "GEV" = "orangered"),
    labels = c("GPD" = "GPD (averages)", "GEV" = "GEV (maxima)")) +
  theme_minimal(base_size = 13)

#In this way, it is easy to see how much more variability is present in the GEV model
#compared to the GPD one. We may get less relevant result (we are more interested 
#in single extremes rather than in extreme weeks), but they are much more stable.
