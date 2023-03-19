##-----------------------------------------------------##
##  An R Companion to Applied Regression, 3rd Edition  ##
##      J. Fox and S. Weisberg, Sage Publications      ##
##   Script for Appendix on Bayesian Estimation        ##
##                 of Regression Models                ##
##-----------------------------------------------------##

phi <- seq(0, 1, length=500)
L <- phi^4 * (1 - phi)^6
plot(phi, L, type="l", lwd=2, xlab=expression(phi), ylab=expression(L(phi)))
abline(h=0, col="darkgray")
abline(v=0.4, h=max(L), lty=2)
text(0.4, -0.00025, expression(hat(phi)), xpd=TRUE)

.par <- par(mfrow=c(1, 2))
plot(0:1, c(0, 5), type="n", xlab=expression(phi), 
     ylab=expression(p(phi)), main="(a)")
lines(phi, dbeta(phi, 1, 1), lwd=2)
lines(phi, dbeta(phi, 4, 4), lwd=2, lty=2)
lines(phi, dbeta(phi, 8, 8), lwd=2, lty=3)
legend("top", inset=0.02, legend=c(expression(alpha == 1 ~~ beta == 1),
                                   expression(alpha == 4 ~~ beta == 4),
                                   expression(alpha == 8 ~~ beta == 8)),
       lty=1:3, lwd=2, title="Symmetric Beta Priors", bty="n")

plot(0:1, c(0, 5), type="n", xlab=expression(phi), 
     ylab=expression(p(phi)), main="(b)")
lines(phi, dbeta(phi, 1, 4), lwd=2)
lines(phi, dbeta(phi, 4, 1), lwd=2, lty=2)
lines(phi, dbeta(phi, 4, 8), lwd=2, lty=3)
lines(phi, dbeta(phi, 8, 4), lwd=2, lty=4)
legend("top", inset=0.02, legend=c(expression(alpha == 1 ~~ beta == 4),
                                   expression(alpha == 4 ~~ beta == 1),
                                   expression(alpha == 4 ~~ beta == 8),
                                   expression(alpha == 8 ~~ beta == 4)),
       lty=1:4, lwd=2, title="Asymmetric Beta Priors", bty="n")
par(.par)

plot(phi, dbeta(phi, 12, 13), type="l", lwd=2, xlab=expression(phi), 
     ylab=expression(p(phi)))
lines(phi, dbeta(phi, 5, 7), lty=2, lwd=2)
legend("topright", legend=c("for Beta(8, 8) prior", "for Beta(1, 1) prior"),
       lty=1:2, lwd=2, title="Posterior Distributions", bty="n", inset=0.02)

library(rstan)
(ncores <- parallel::detectCores())
options(mc.cores=ncores)
rstan_options(auto_write=TRUE)

bern.model <- "
data {
    int<lower=0> n; // number of observations
    int<lower=0, upper=1> y[n]; // observations, 0=failure; 1=success
    int<lower=1> a; // shape parameter 1 for Beta prior
    int<lower=1> b; // shape parameter 2 for Beta prior
}
parameters {
    real<lower=0, upper=1> phi;  // per observation probability of success
}
model {
    phi ~ beta(a, b); // Beta(a, b) prior for phi
    y ~ bernoulli(phi); // likelihood 
}
"



coin.exper <- list(n=10, y=c(1, 1, 0, 1, 0, 0, 0, 0, 1, 0), a=1, b=1) # flat prior

system.time(
    bern.1 <- stan(
        model_code=bern.model,
        model_name="Bernoulli model, flat prior",
        seed=277360,
        data=coin.exper,
        iter=10000,
        chains=4
    )
)

print(bern.1, digits=4)

library("car")
densityPlot(extract(bern.1, "phi")$phi, from=0, to=1, normalize=TRUE, 
            xlab=expression(phi))
lines(phi, dbeta(phi, 5, 7), col="magenta", lwd=2, lty=2)
abline(v=0.4, lty=2)

traceplot(bern.1)

duncan.model.1 <- "
    data {
        int<lower=0> n;       // number of cases
        int<lower=0> k;       // number of regressors (less intercept)
        vector[n] prestige;   // response
        vector[n] income;     // predictor
        vector[n] education;  // predictor
    }
    parameters {
        vector[k + 1] beta;   // regression coefficients
        real<lower=0> sigma;  // error standard deviation
    }
    transformed parameters {
        vector[n] mu = beta[1] + beta[2]*income + beta[3]*education; 
                    // conditional expection of the response
    }
    model {
        prestige ~ normal(mu, sigma); // likelihood
    }
"

data.duncan <- with(Duncan, list(n=nrow(Duncan), k=2, 
                    prestige=prestige, income=income, education=education))

system.time(fit.duncan.1 <- stan(model_code=duncan.model.1, 
                      model_name="Duncan regression, flat priors",
                      seed=693089, data=data.duncan, iter=10000))

print(fit.duncan.1, pars=c("beta", "sigma"), digits=4)
brief(lm(prestige ~ income + education, data=Duncan))

duncan.model.2 <- "
    data {
        int<lower=0> n;       // number of cases
        int<lower=0> k;       // number of regressors (less intercept)
        vector[n] prestige;   // response
        vector[n] income;     // predictor
        vector[n] education;  // predictor
    }
    transformed data{
        vector[n] income_d = income - mean(income);
        vector[n] education_d = education - mean(education);
    }
    parameters {
        vector[k + 1] beta;   // regression coefficients
        real<lower=0> sigma;  // error standard deviation
    }
    transformed parameters {
        vector[n] mu = beta[1] + beta[2]*income_d + beta[3]*education_d; 
                    // conditional expection of the response
    }
    model {
        prestige ~ normal(mu, sigma); // likelihood
        // priors:
        beta[1] ~ normal(50, 20);
        beta[2] ~ normal(0, 3);
        beta[3] ~ normal(0, 3);
        sigma ~ normal(0, 10);
    }
"

fit.duncan.2 <- stan(model_code=duncan.model.2, 
                     model_name="Duncan regression, weakly informative priors",
                     seed=693089, data=data.duncan, iter=10000)

print(fit.duncan.2, pars=c("beta", "sigma"), digits=4)
brief(lm(prestige ~ I(income - mean(income)) + I(education - mean(education)), 
         data=Duncan))

linmod <- "
    data {
        int<lower=0> n; // number of cases
        int<lower=0> k; // number of regressors, less constant
        matrix[n, k] X; // model matrix, less constant
        vector[n] y;    // response vector
    }
    parameters {
        vector[k] beta;      // regression coefficients
        real alpha;          // regression intercept
        real<lower=0> sigma; // residual standard deviation
    }
    transformed parameters{
        vector[n] mu = alpha + X*beta;
    }
    model {
        y ~ normal(mu, sigma); // likelihood
    }
"

X <- model.matrix(~ income + education - 1, data=Duncan) # suppress intercept
head(X)
data.duncan.3 <- list(n=nrow(X), k=ncol(X), X=X, y=Duncan$prestige)

fit.duncan.3 <- stan(model_code=linmod, 
             model_name="Duncan regression, matrix formulation, flat priors",
             seed=332486, data=data.duncan.3, iter=10000)

print(fit.duncan.3, pars=c("alpha", "beta", "sigma"), digits=4)
print(fit.duncan.1, pars=c("beta", "sigma"), digits=4)

blm <- "
    data {
        int<lower=0> n; // number cases
        int<lower=0> p; // number of regressors
        matrix[n, p] X; // model matrix
        vector[n] y;    // response vector
    }
    parameters {
        vector[p] beta; // regression coefficients
        real<lower=0> sigma; // residual std. dev.
    }
    transformed parameters {
        vector[n] mu = X*beta; // expectation of y
    }
    model {
        y ~ normal(mu, sigma); // likelihood
    }
"

bayeslm <- function(formula, data, subset, na.action, contrasts=NULL, ...){
    if (!require(rstan)) stop ("rstan package not available")
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), 
               names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    X <- model.matrix(mt, mf, contrasts)
    n <- length(y)
    p <- ncol(X)
    Data <- list(n=n, X=X, y=y, p=p)
    stan(model_code=blm, model_name="Linear Model", data=Data, ...)
}

fit.duncan.4 <- bayeslm(prestige ~ income + education, data=Duncan,
                        seed=300732, iter=10000)

print(fit.duncan.4, pars=c("beta", "sigma"), digits=4)
print(fit.duncan.3, pars=c("alpha", "beta", "sigma"), digits=4)

brief(Blackmore, rows=c(5, 5)) # first and last subjects
nrow(Blackmore)
with(Blackmore, by(subject, group, function(x) length(unique(x))))

Blackmore$tran.exercise <- bcnPower(Blackmore$exercise, lambda=0.25, gamma=0.1)

library(lme4)
blackmore.mod.lmer <- lmer(tran.exercise ~ I(age - 8)*group + 
    (I(age - 8) | subject), data=Blackmore, REML=FALSE)
S(blackmore.mod.lmer)

blackmore.model.1 <- " 
    data {
        int<lower=0> n;          // total number of observations
        int<lower=0> J;          // number of subjects
        int subject[n];          // subject index
        vector[n] patient;       // patient dummy regressor
        vector[n] age;           // within-subject predictor age - 8 yrs
        vector[n] tran_exercise; // response variable, transformed exercise
    }
    parameters {
        matrix[J, 2] B;         // subject-specific intercepts and slopes 
                                //     (deviations)
        vector[4] beta;         // group-average intercepts and slopes
        real<lower=0> sigma;    // error standard deviation
        corr_matrix[2] Rho;     // correlation matrix of subject-specific 
                                //     coefficients
        vector<lower=0>[2] psi; // standard deviations of subject-specific 
                                //     coefficients
    }
    transformed parameters {
        matrix[2, 2] Psi = quad_form_diag(Rho, psi); 
                              // covariance matrix of subject-specific 
                              //     coefficients
        real rho = Rho[1, 2]; // correlation between subject-specific 
                              //     intercepts and slopes
        vector[n] mu = beta[1] + B[subject, 1] + beta[2]*patient + beta[3]*age
                         + beta[4] * patient .* age + B[subject, 2] .* age;
                              // conditional expectation of response
    }
    model{
        tran_exercise ~ normal(mu, sigma); // conditional distribution of response
        for (j in 1:J){
            B[j, ] ~ multi_normal(rep_vector(0, 2), Psi); 
                                    // distribution of subject-specific coefficents
        }
        // priors:
        beta  ~ normal(0, 10);
        sigma ~ normal(0, 10);
        psi ~ normal(0, 10);
        Rho ~ lkj_corr(2);
    }
"

data.blackmore <- with(Blackmore, list(n = nrow(Blackmore), 
    J = nlevels(subject), 
    subject = as.numeric(subject), 
    patient = as.numeric(group == "patient"), 
    age = age - 8,
    tran_exercise = tran.exercise))

system.time(fit.blackmore.1 <- stan(model_code=blackmore.model.1, chains=4,
                        model_name= "Hierarchical model for Blackmore data, weak priors",
                        seed=702478, data=data.blackmore, iter=10000, 
                        control=list(max_treedepth=20)))

print(fit.blackmore.1, pars=c("beta", "sigma", "psi", "rho"), digits=4)

library("rstanarm")

system.time(blackmore.mod.2.stanlmer <- stan_lmer(tran.exercise ~ I(age - 8)*group + 
    (I(age - 8) | subject), data=Blackmore))
print(blackmore.mod.2.stanlmer, digits=4)

blackmore.model.2 <- "
    data {
        int<lower=0> n;                     // total number of observations
        int<lower=0> J;                     // number of subjects
        int subject[n];                     // subject index
        vector[n] patient;                  // patient dummy regressor
        vector[n] age;                      // within-subject predictor
        vector[n] tran_exercise;            // response variable

        int<lower=0, upper=1> nonzero[n];   // exercise > 0?
        int n_nonzero;                      // number of nonzero observations
        int<lower=1, upper=n> 
            index_nonzero[n_nonzero];       // indices of nonzero observations

        // for effect display:
        int<lower=0> effect_n;              // number of values for effect display
        vector[effect_n] patient_effect;    // for effect display
        vector[effect_n] age_effect;        // for effect display
    }
    parameters {
        vector[J] b;         // subject-specific intercepts
        vector[4] beta;      // group-level coefficients
        real<lower=0> sigma; // error standard deviation
        real<lower=0> psi_b; // standard deviation of intercepts
        vector[J] z;         // subject-specific intercepts for logit part of model
        vector[4] zeta;      // group-level coefficients for logit part of model
        real<lower=0> psi_z; // standard deviation of subject-specific 
                             //     logit intercepts
    }
    transformed parameters {
        // conditional expection for linear part of model
        vector[n] mu = beta[1] + b[subject] + beta[2] * patient 
                + beta[3] * age + beta[4] * patient .* age; 
        // linear predictor for for logit part of model
        vector[n] eta = zeta[1]+ z[subject] + zeta[2] * patient
                + zeta[3] * age + zeta[4] * patient .* age;   
    }
    model{
        nonzero ~ bernoulli_logit(eta);         // logit part of model
        tran_exercise[index_nonzero] 
            ~ normal(mu[index_nonzero], sigma); // linear part of model
        b ~ normal(0, psi_b);       // distribution of intercepts for linear model
        z ~ normal(0, psi_z);       // distribution of intercepts for logit model
        // priors:
        sigma ~ normal(0, 10);
        beta  ~ normal(0, 10);
        zeta  ~ normal(0, 10);
        psi_b ~ normal(0, 10);
        psi_z ~ normal(0, 10);
    }
    generated quantities{
        vector[effect_n] eta_effect = zeta[1] + zeta[2] * patient_effect 
                + zeta[3] * age_effect + zeta[4] * patient_effect .* age_effect;
        vector[effect_n] mu_effect = beta[1] + beta[2] * patient_effect 
                + beta[3] * age_effect + beta[4] * patient_effect .* age_effect;
        mu_effect =  mu_effect .* (1. ./ (1. + exp( - eta_effect)));
    }
"

data.blackmore$nonzero <- as.numeric(Blackmore$exercise > 0)
data.blackmore$n_nonzero <- sum(data.blackmore$nonzero)
data.blackmore$index_nonzero <- which(as.logical(data.blackmore$nonzero))

data.blackmore$effect_n <- 12
(data.blackmore$patient_effect <- rep(0:1, each=6))
(data.blackmore$age_effect <- rep(seq(8, 18, by=2), 2) - 8)

fit.blackmore.2 <- stan(model_code=blackmore.model.2, chains=4,
                        model_name= "Hurdle model for Blackmore data, weak priors",
                        seed=620314, data=data.blackmore, iter=10000)

print(fit.blackmore.2, 
      pars=c("beta", "zeta", "sigma", "psi_b", "psi_z"), 
      digits=4)

mu <- extract(fit.blackmore.2, pars=c("mu_effect"))$mu_effect
mu.fit <- colMeans(mu)
mu.025 <- apply(mu, 2, quantile, probs=0.025)
mu.975 <- apply(mu, 2, quantile, probs=0.975)
age.effect <- rep(seq(8, 18, by=2), 2)
patient.effect <- factor(rep(c("control", "patient"), each=6))
Effect <- data.frame(exercise=bcnPowerInverse(mu.fit, lambda=0.25, gamma=0.1),
                     exercise.025=bcnPowerInverse(mu.025, lambda=0.25, gamma=0.1),
                     exercise.975=bcnPowerInverse(mu.975, lambda=0.25, gamma=0.1),
                     group=patient.effect, age=age.effect)
Effect

plot(c(8, 18), c(0, 8), type="n", xlab="Age (years)", 
     ylab="Exercise (hours/week)")
lines(exercise ~ age, data=Effect, subset=group=="control", 
      type="b", pch=17, lty=2, lwd=2)
lines(exercise ~ age, data=Effect, subset=group=="patient", 
      type="b", pch=16, lwd=2)
with(Effect[1:6, ], arrows(age.effect, exercise.025, age.effect, 
                           exercise.975, angle=90, lty=2, code=3, length=0.1))
with(Effect[7:12, ], arrows(age.effect, exercise.025, age.effect, 
                           exercise.975, angle=90, code=3, length=0.1))
legend("topleft", title="Group", legend=c("Patient", "Control"), lwd=2, lty=1:2, 
       pch=c(16, 17), inset=0.02)

