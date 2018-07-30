generate_data <- function(N, beta, sd) {
    x <- rnorm(N, 0, 1)
    eps <- rnorm(N, 0, sd)
    y <- beta * x + eps
    list(x = x, y = y)
}

calc_se <- function(y, x, coef) {
    n <- length(y)
    eps <- y - x*coef
    e_sd <- mean(eps^2)
    se <- sqrt(e_sd / (n*var(x)))
    se
}

calc_coef <- function(y,x) cov(x,y) / var(x)

# Note: var(x) gives unbiased, but okay for our purposes
run_regression <- function(y, x) {
    coef <- calc_coef(y, x)
    se <- calc_se(y, x, coef)
    list(coef=coef, se=se)
}

eval_model <- function(coef, se, beta, conf = 1.96) {
    up <- coef + se*conf
    down <- coef - se*conf
    beta > down & beta < up
}

simulate <- function(N, beta, sd) {
    d <- generate_data(N, beta, sd)
    m <- run_regression(d$y, d$x)
    eval_model(m$coef, m$se, beta)
}

avg_simulations <- function(M, N, beta, sd) {
    inside <- sapply(1:M, function (x) simulate(N, beta, sd))
    sum(inside) / M
}

check_N <- function(M, beta, sd) {
    x <- seq(4, 50, 2)
    y <- sapply(x, function(N) avg_simulations(M, N, beta, sd))
    qplot(x, y)
}


### S3 Classes
#############################

run_regression <- function(y, x) {
    coef <- cov(x,y) / var(x)
    se <- calc_se(y, x, coef)
    model <- list(coef=coef, se=se)
    class(model) <- "simple_lm"
    model
}

eval_model <- function(model, beta, conf) {
    UseMethod("eval_model")
}

eval_model.simple_lm <- function(model, beta, conf = 1.96) {
    up <- model$coef + model$se*conf
    down <- model$coef - model$se*conf
    beta > down & beta < up
}
