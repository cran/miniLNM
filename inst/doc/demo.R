## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::knit_hooks$set(output = miniLNM::ansi_aware_handler)
options(crayon.enabled = TRUE)

## ----setup, message = FALSE---------------------------------------------------
library(miniLNM)
library(dplyr)
set.seed(20240904)

## -----------------------------------------------------------------------------
example_data <- lnm_data(N = 200, K = 20)
xy <- bind_cols(example_data[c("X", "y")])

## -----------------------------------------------------------------------------
fit <- lnm(starts_with("y") ~ starts_with("x"), xy, refresh = 0)
fit

## -----------------------------------------------------------------------------
newx <- lnm_data()$X
new_p_hat <- predict(fit, newx, depth = 300)

## -----------------------------------------------------------------------------
p_hat <- predict(fit, example_data$X)
y_star <- sample(fit, newdata = example_data$X, depth = 1e4)

## -----------------------------------------------------------------------------
true <- colMeans(example_data$y / rowSums(example_data$y))
fitted <- colMeans(y_star / rowSums(y_star))
plot(true, colMeans(p_hat), asp = 1)
points(true, fitted, col = "red")
abline(a = 0, b = 1)

## -----------------------------------------------------------------------------
plot(example_data$B, beta_mean(fit))
abline(a = 0, b = 1)

## -----------------------------------------------------------------------------
sessionInfo()

