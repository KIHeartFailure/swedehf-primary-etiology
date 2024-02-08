source(here::here("setup/setup.R"))

# load data
load(here("data/clean-data/data.RData"))

dataass <- mice::complete(imprsdata, 3)
dataass <- mice::complete(imprsdata, 6)

# check assumptions for cox models ----------------------------------------

i <- 1
mod <- coxph(formula(paste0(
  "Surv(", outvars$time[i], ",", outvars$var[i], " == 'Yes') ~ shf_primaryetiology_cat + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[4], resid = T, col = "red")
plot(testpat[5], resid = T, col = "red")

ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)

i <- 2
mod <- coxph(formula(paste0(
  "Surv(", outvars$time[i], ",", outvars$var[i], " == 'Yes') ~ shf_primaryetiology_cat + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[4], resid = T, col = "red")
plot(testpat[5], resid = T, col = "red")

ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


i <- 3
mod <- coxph(formula(paste0(
  "Surv(", outvars$time[i], ",", outvars$var[i], " == 'Yes') ~ shf_primaryetiology_cat + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[4], resid = T, col = "red")
plot(testpat[5], resid = T, col = "red")

ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


# For log reg models
# pef vs mref

ormod <- glm(formula(paste0("shf_ef_cat ~ shf_primaryetiology_cat + ", paste(setdiff(modvars, "shf_ef_cat"), collapse = " + "))),
  family = binomial(link = "logit"), data = dataass %>% filter(shf_ef_cat != "HFrEF")
)

# vif
print(car::vif(ormod))

# outliers
cooks <- cooks.distance(ormod)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line

# pef vs ref

ormod <- glm(formula(paste0("shf_ef_cat ~ shf_primaryetiology_cat + ", paste(setdiff(modvars, "shf_ef_cat"), collapse = " + "))),
  family = binomial(link = "logit"), data = dataass %>% filter(shf_ef_cat != "HFmrEF")
)

# vif
print(car::vif(ormod))

# outliers
cooks <- cooks.distance(ormod)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line
