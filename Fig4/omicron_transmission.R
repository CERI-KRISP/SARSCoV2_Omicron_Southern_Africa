# Transmissibility and immune evasion of the SARS-CoV-2 Omicron variant in southern Africa
# Christian L. Althaus, 15 December 2021

# Load libraries
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(nnet)
library(splines)
library(emmeans)

# Random number generator
set.seed(421367)

# Define colors
cols <- c("#D93485", "#5FB175", "#E9D5BA", "#E4E4E4")
cols <- cols[c(2, 3, 1, 4)]
t.cols <- cols
for(i in 1:length(cols)) {
  x <- col2rgb(cols[i])
  t.cols[i] <- rgb(x[1, ], x[2, ], x[3, ], alpha = 125, maxColorValue = 255)
}

cols.seq <- brewer.pal(9, "YlGnBu")
t.cols.seq <- cols.seq
for(i in 1:length(cols.seq)) {
  x <- col2rgb(cols.seq[i])
  t.cols.seq[i] <- rgb(x[1, ], x[2, ], x[3, ], alpha = 125, maxColorValue = 255)
}

# Sample size for parameter sets and bootstrapping
n_sim <- 1e4

# Define functions
tau_func <- function(rho, kappa, epsilon, omega, D, R_w) {
  beta <- R_w/((1 - omega)*D)
  S <- 1 - omega
  gamma <- 1/D
  return(((kappa + 1)*(rho - beta*omega*epsilon) - gamma*kappa)/(beta*(kappa + 1)*(S + omega*epsilon)))
}

kappa_func <- function(rho, tau, epsilon, omega, D, R_w) {
  beta <- R_w/((1 - omega)*D)
  S <- 1 - omega
  gamma <- 1/D
  return((rho - beta*(S*tau + (tau + 1)*omega*epsilon))/(gamma - rho + beta*(S*tau + tau*omega*epsilon + omega*epsilon)))
}

epsilon_func <- function(rho, tau, kappa, omega, D, R_w) {
  beta <- R_w/((1 - omega)*D)
  S <- 1 - omega
  gamma <- 1/D
  return(((kappa + 1)*(rho - beta*S*tau) - gamma*kappa)/(beta*(kappa + 1)*(tau + 1)*omega))
}

# Read data from GISAID (downloaded on 15 December 2021)
gisaid <- readRDS("SA_20211215.rds")
gisaid <- subset(gisaid, grepl("Gauteng", Location, ignore.case = TRUE))

# Set variants and start date
variant_names <- c("Alpha", "Beta", "Delta", "Omicron")
begin <- ymd(20210901)

# Clean data
gisaid$date <- ymd(gisaid$`Collection date`)
gisaid <- subset(gisaid, !is.na(date) & date >= begin)
gisaid$variant <- gisaid$`Pango lineage`
gisaid$variant = case_when(
  grepl("B.1.1.7", gisaid$`Pango lineage`, fixed = TRUE) ~ "Alpha",
  grepl("B.1.351", gisaid$`Pango lineage`) ~ "Beta",
  grepl("B.1.617.2", gisaid$`Pango lineage`, fixed = TRUE) | grepl("AY", gisaid$`Pango lineage`)  ~ "Delta",
  grepl("BA", gisaid$`Pango lineage`) ~ "Omicron",
TRUE ~ "Other"
)
gisaid <- subset(gisaid, select = c("date", "variant"))
gisaid$variant <- as.factor(gisaid$variant)
gisaid$variant2 <- relevel(gisaid$variant, ref = "Delta")

# Convert to weekly data
gisaid <- cbind(gisaid, week = round_date(gisaid$date, unit = "week", week_start = 4))
week1 <- min(gisaid$week)
week2 <- max(gisaid$week)
week_y <- seq(week1, week2, 7)
var_prop <- as.data.frame(matrix(NA, nrow = length(week_y), ncol = 3 + length(variant_names)))
names(var_prop) <- c("date", "samples", variant_names, "Other")
var_prop$date <- week_y

for(i in week_y) {
  x <- subset(gisaid, week == i)
  var_prop[var_prop$date == i, 2] <- length(x$variant2)
  var_prop[var_prop$date == i, 3] <- length(x$variant2[x$variant2 == "Alpha"])/length(x$variant2)
  var_prop[var_prop$date == i, 4] <- length(x$variant2[x$variant2 == "Beta"])/length(x$variant2)
  var_prop[var_prop$date == i, 5] <- length(x$variant2[x$variant2 == "Delta"])/length(x$variant2)
  var_prop[var_prop$date == i, 6] <- length(x$variant2[x$variant2 == "Omicron"])/length(x$variant2)
  var_prop[var_prop$date == i, 7] <- length(x$variant2[x$variant2 == "Other"])/length(x$variant2)
}

# Multinomial logistic regression
gisaid_fit <- gisaid
gisaid_fit$date <- as.numeric(gisaid_fit$date) - as.numeric(min(gisaid_fit$date))
fit <- multinom(variant2 ~ ns(date, 1), data = gisaid_fit)

# Estimating the proportion of variants over time
t1 <- min(gisaid_fit$date)
t2 <- ymd(20211201) - min(gisaid$date)
prediction_date <- as_date(min(gisaid$date):max(gisaid$date))
prediction <- data.frame(emmeans(fit, ~ variant2, mode = "prob", by = "date", at = list(date = t1:t2)))

# Estimating the growth difference between Omicron and Delta
t3 <- max(prediction$date[prediction$variant2 == "Omicron" & prediction$prob < 0.5])
fit_emtrends <- emtrends(fit, trt.vs.ctrl ~ variant2, var = "date", mode = "latent", at = list(date = t3))

# Generation time
gen_mean <- 5.2
gen_sd <- (6.78 - 3.78)/2/qnorm(0.975)
generation <- rnorm(n_sim, gen_mean, gen_sd)
generation <- ifelse(generation > 0, generation, rnorm(1, gen_mean, gen_sd))

# Reproduction number
R_w <- read.csv("https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/ZAF-estimates.csv")
R_w$date <- ymd(R_w$date)
R_w <- subset(R_w,
               region == "ZAF"
               & data_type == "Confirmed cases"
               & estimate_type == "Cori_slidingWindow"
               & date >= ymd(20211001) # October 2021
               & date <= ymd(20211031))
R_w <- R_w$median_R_mean
R_w <- sample(R_w, n_sim, replace = TRUE)

rho_est <- confint(fit_emtrends)$contrasts[2, c(2, 5, 6)]
rho_mean <- rho_est[[1]]
rho_sd <- (rho_est[[3]] - rho_est[[2]])/2/qnorm(0.975)
rho <- rnorm(n_sim, rho_mean, rho_sd)
rho <- ifelse(rho > 0, rho, rnorm(1, rho_mean, rho_sd))

tau <- tau_func(rho, kappa = 0, epsilon = 0, omega = 0, generation, R_w)
tau_est <- quantile(tau, probs = c(0.5, 0.025, 0.975))

omega_range <- seq(0.1, 0.9, 0.1)
tau_range <- seq(-1, 3, 0.01)
y <- array(NA, dim = c(length(omega_range), length(tau_range), 3))

for(i in 1:length(omega_range)) {
  for(j in 1:length(tau_range)) {
    y[i, j, ] <- quantile(epsilon_func(rho, tau = tau_range[j], kappa = 0, omega = omega_range[i], generation, R_w), probs = c(0.5, 0.025, 0.975))
  }
}

y[, , 2][y[, , 2] > 1] <- 1
y[, , 3][y[, , 3] > 1] <- 1
y[, , 2][y[, , 2] < 0] <- 0
y[, , 3][y[, , 3] < 0] <- 0

# Show results
dim(gisaid)[1] # Number of sequences
range(gisaid$date) # Range of sample collection dates
round(rho_est, 2) # Estimated growth advantage

# Plot figure
layout(rbind(c(1, 1, 2), c(3, 4, 5)))
plot(var_prop$date, var_prop$sample, ty = "n",
     xlim = c(ymd(20211001), ymd(20211201)), ylim = c(0, 1),
     xlab = NA, ylab = "Proportion variant", axes = FALSE, frame = FALSE)
abline(h = seq(0, 1, 0.2), col = "lightgray", lty = 3)
abline(v = ymd(20211001) + 0:2*months(1), col = "lightgray", lty = 3)
axis(1, ymd(20211001) + 0:2*months(1), c("Oct 2021", "Nov 2021", "Dec 2021"), padj = 0.5)
axis(2)
title(main = "A", adj = 0)
for(i in 1:length(levels(prediction$variant2))) {
  x <- subset(prediction, variant2 == levels(prediction$variant2)[i])
  x$upper.CL[x$upper.CL > 1] <- 1
  x$lower.CL[x$lower.CL < 0] <- 1e-10
  x$date <- min(gisaid$date) + x$date
  polygon(c(x$date, rev(x$date)),
          c(x$lower.CL, rev(x$upper.CL)),
          col = t.cols[i], border = NA)
  lines(x$date, x$prob, col = cols[i])
}
for(i in 1:length(levels(prediction$variant2))) {
  points(var_prop$date, var_prop[, levels(prediction$variant2)[i]], pch = 19, cex = 0.5, col = cols[i])
}
legend("right", inset = 0.05, levels(prediction$variant2), col = cols, pch = 19, bty = "n")

label_all <- c(expression(paste(Omega, " = 10%")),
            expression(paste(Omega, " = 20%")),
            expression(paste(Omega, " = 30%")),
            expression(paste(Omega, " = 40%")),
            expression(paste(Omega, " = 50%")),
            expression(paste(Omega, " = 60%")),
            expression(paste(Omega, " = 70%")),
            expression(paste(Omega, " = 80%")),
            expression(paste(Omega, " = 90%")))
plot(NA,
     xlim = c(-1, 3), ylim = c(0, 1),
     xlab = "Increase in transmissibility", ylab = "Immune evasion ",
     axes = FALSE, frame = FALSE)
axis(1, seq(-1, 3, 0.5), paste0(seq(-1, 3, 0.5)*1e2, "%"))
axis(2, seq(-1, 3, 0.25), paste0(seq(-1, 3, 0.25)*1e2, "%"))
abline(h = seq(-1, 3, 0.25), col = "lightgray", lty = 3)
abline(v = seq(-1, 3, 0.5), col = "lightgray", lty = 3)
abline(v = 0, lty = 3)
title(main = "B", adj = 0)
for(i in 1:length(omega_range)) {
  lines(tau_range[y[i, , 1] > 0 & y[i, , 1] < 1], y[i, , 1][y[i, , 1] > 0 & y[i, , 1] < 1], col = cols.seq[i])
}
legend("topright", inset = 0, label_all, col = cols.seq, pch = 19, cex = 0.75, bty = "n")

omega_index <- c(4, 6, 8)
label1 <- c("C", "D", "E")
label2 <- c(expression(paste(Omega, " = 40%")),
            expression(paste(Omega, " = 60%")),
            expression(paste(Omega, " = 80%")))
for(i in 1:length(omega_index)) {
  ii <- omega_index[i]
  plot(NA,
       xlim = c(-1, 3), ylim = c(0, 1),
       xlab = "Increase in transmissibility", ylab = "Immune evasion ",
       axes = FALSE, frame = FALSE)
  axis(1, seq(-1, 3, 0.5), paste0(seq(-1, 3, 0.5)*1e2, "%"))
  axis(2, seq(-1, 3, 0.25), paste0(seq(-1, 3, 0.25)*1e2, "%"))
  abline(h = seq(-1, 3, 0.25), col = "lightgray", lty = 3)
  abline(v = seq(-1, 3, 0.5), col = "lightgray", lty = 3)
  abline(v = 0, lty = 3)
  title(main = label1[i], adj = 0)
  polygon(c(tau_range, rev(tau_range)),
          c(y[ii, , 2], rev(y[ii, , 3])),
          col = t.cols.seq[ii], border = NA)
  lines(tau_range[y[ii, , 1] > 0 & y[ii, , 1] < 1], y[ii, , 1][y[ii, , 1] > 0 & y[ii, , 1] < 1], col = cols.seq[ii])
  legend("topright", inset = 0, label2[i], col = cols.seq[ii], pch = 19, cex = 1, bty = "n")
}
