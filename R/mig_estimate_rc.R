#' Estimate Rogers-Castro migration age schedule

#' @description Given a set of ages and observed age-specific net migrants, estimate the parameters of a Roger-Castro model migration schedule.
#' Choose between a 7, 9, 11 or 13 parameter model.

#' @param ages numeric. A vector of integers for ages.
#' @param net_mig numeric. A vector of integers for observed age-specific net migrants.
#' @param pop numeric. A vector of integers for age-specific population.
#' @param pre_working_age logical (TRUE/FALSE). Whether or not to include pre working age component.
#' @param working_age logical (TRUE/FALSE). Whether or not to include working age component.
#' @param retirement logical (TRUE/FALSE). Whether or not to include retirement age component.
#' @param post_retirement logical (TRUE/FALSE). Whether or not to include post retirement age component.
#' @param ... additional inputs to stan, see ?rstan::stan for details.
#' @importFrom rstan stan extract
#' @import Rcpp
#' @importFrom stats quantile median
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise rename mutate ungroup
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @importFrom tibble as.tibble
#' @importFrom tidybayes gather_draws
#' @importFrom rstan extract
#' @export
#' @examples
#' # define ages, net migrants, and population
#' ages <- 0:80
#'net_mig <- c(202,215,167,188,206,189,164,
#'             158,197,185,176,173,167,198,
#'             203,237,249,274,319,345,487,
#'             491,521,505,529,527,521,529,
#'             507,484,467,439,399,399,380,
#'             368,310,324,289,292,270,269,
#'             285,254,245,265,257,258,263,
#'             253,346,293,332,346,349,355,
#'             386,346,344,352,331,320,307,
#'             320,310,258,254,243,256,263,
#'             183,169,172,160,166,113,132,
#'             111,130,110,113)
#'pop <- c(105505,105505,105505,105505,105505,
#'         106126,106126,106126,106126,106126,
#'         100104,100104,100104,100104,100104,
#'         114880,114880,114880,114880,114880,
#'         136845,136845,136845,136845,136845,
#'         136582,136582,136582,136582,136582,
#'         141935,141935,141935,141935,141935,
#'         134097,134097,134097,134097,134097,
#'         130769,130769,130769,130769,130769,
#'         133718,133718,133718,133718,133718,
#'         154178,154178,154178,154178,154178,
#'         145386,145386,145386,145386,145386,
#'         126270,126270,126270,126270,126270,
#'         108314,108314,108314,108314,108314,
#'         79827,79827,79827,79827,79827,59556,
#'         59556,59556,59556,59556,59556)
#'
#' # fit the model
#'
#' res <- mig_estimate_rc(ages, net_mig, pop,
#'                        pre_working_age = TRUE,
#'                        working_age = TRUE,
#'                        retirement = TRUE,
#'                        post_retirement = FALSE,
#'                        #optional inputs into stan
#'                        control = list(adapt_delta = 0.95, max_treedepth = 10)
#'                        )
#' \dontrun{
#' # plot the results and data
#' plot(ages, net_mig/pop, ylab = "migration rate", xlab = "age")
#' lines(ages, res[["fit_df"]]$median, col = "red")
#' legend("topright", legend=c("data", "fit"), col=c("black", "red"), lty=1, pch = 1)
#' }
mig_estimate_rc <- function(ages,
                            net_mig,
                            pop,
                            pre_working_age,
                            working_age,
                            retirement,
                            post_retirement,
                            ...){

  stopifnot(any(pre_working_age, working_age, retirement, post_retirement))

  # data for model input
  x <- ages
  pop <- pop
  y <- net_mig

  mig_data <- list(
    N = length(x),
    y = y,
    x = x,
    pop = pop,
    pre_working_age = as.numeric(pre_working_age), #binary
    working_age = as.numeric(working_age),
    retirement = as.numeric(retirement),
    post_retirement = as.numeric(post_retirement)
  )

  # model

  rc_flexible <- 'data {
  int<lower=0,upper=1> pre_working_age;         // 0 = no, 1 = yes
  int<lower=0,upper=1> working_age;             // 0 = no, 1 = yes
  int<lower=0,upper=1> retirement;              // 0 = no, 1 = yes
  int<lower=0,upper=1> post_retirement;         // 0 = no, 1 = yes
  int<lower=0> N;
  vector[N] x;                                  //ages
  int<lower=0> y[N];                            //age-specific net migrants
  vector[N] pop;                                //age-specific population size
}
parameters {
real<lower=0> alpha1[1*pre_working_age];
real<lower=0> alpha2[1*working_age];
real<lower=0> alpha3[1*retirement];
real<lower=0, upper=1> a1[1*pre_working_age];
real<lower=0, upper=1> a2[1*working_age];
real<lower=0, upper=1> a3[1*retirement];
real<lower=0, upper=1> a4[1*post_retirement];
real<lower=0> mu2[1*working_age];
real<lower=0, upper=max(x)> mu3[1*retirement];
real<lower=0> lambda2[1*working_age];
real<lower=0> lambda3[1*retirement];
real<upper=0.05> lambda4[1*post_retirement];
real<lower=0, upper=1> c;
}
transformed parameters {
vector[N] mu_rc;
vector[N] mu_rc_1;
vector[N] mu_rc_2;
vector[N] mu_rc_3;
vector[N] mu_rc_4;
vector[N] zero;

for(i in 1:N){
zero[i] = 0;
}

mu_rc_1 = pre_working_age==1?a1[1]*exp(-alpha1[1]*x):zero;
mu_rc_2 = working_age==1?a2[1]*exp(-alpha2[1]*(x - mu2[1]) - exp(-lambda2[1]*(x - mu2[1]))):zero;
mu_rc_3 = retirement==1?a3[1]*exp(-alpha3[1]*(x - mu3[1]) - exp(-lambda3[1]*(x - mu3[1]))):zero;
mu_rc_4 = post_retirement==1?a4[1]*exp(lambda4[1]*(x)):zero;
mu_rc = mu_rc_1 + mu_rc_2 + mu_rc_3 + mu_rc_4 + c;
}
model {
// likelihood

vector[N] log_lambda;

for (i in 1:N){
log_lambda[i] = mu_rc[i] + pop[i];
}

y ~ poisson(mu_rc .* pop);

//priors

if(pre_working_age==1){
alpha1 ~ normal(0,1);
a1 ~ normal(0,0.1);
}
if(working_age==1){
alpha2 ~ normal(0,1);
a2 ~ normal(0,0.1);
mu2 ~ normal(25,1);
lambda2 ~ normal(0,1);
}
if(retirement==1){
alpha3 ~ normal(0,1);
a3 ~ normal(0,0.1);
mu3 ~ normal(65,1);
lambda3 ~ normal(0,1);
}
if(post_retirement==1){
a4 ~ normal(0,0.05);
lambda4 ~ normal(0,0.01);
}
c ~ normal(min(to_vector(y) ./ pop),0.1);
}'


  # fit the model
  #rc_fit <- rstan::sampling(stanmodels$rc_flexible, data = mig_data, ...)
  rc_fit <- rstan::stan(model_code = rc_flexible, data = mig_data, ...)

  # extract the posterior samples
  list_of_draws <- rstan::extract(rc_fit)

  # create a matrix to store fitted values
  y_hat      <- matrix(nrow = length(list_of_draws[[1]]), ncol = length(x))
  these_pars <- list()
  parnames   <- names(list_of_draws)[grep("alpha|a[0-9]|mu[0-9]|lambda|^c$",names(list_of_draws))]
  for(j in 1:length(list_of_draws[[1]])){
    for(i in 1:length(parnames)){
      these_pars[[names(list_of_draws)[i]]] <- list_of_draws[[names(list_of_draws)[i]]][j]
    }
    y_hat[j,] <- mig_calculate_rc(ages = ages, pars = these_pars)
  }

  dfit <- tibble(age = x,
                 data = y/pop, median = apply(y_hat, 2, median),
                 lower = apply(y_hat, 2, quantile,0.025),
                 upper = apply(y_hat, 2, quantile, 0.975),
                 diff_sq = (!!sym("median") - !!sym("data"))^2)

  #TR: experimenting rm pipes re segfault error on osx...
  pars_df <- gather_draws(rc_fit, !!sym("a[0-9]\\[1\\]"),
                          !!sym("alpha[0-9]\\[1\\]"),
                          !!sym("mu[0-9]\\[1\\]"),
                          !!sym("lambda[0-9]\\[1\\]"),
                          !!sym("^c$"),
                          regex = TRUE) %>%
    dplyr::group_by(!!sym(".variable")) %>%
    summarise(median = median(!!sym(".value")),
              lower = quantile(!!sym(".value"), 0.025),
              upper = quantile(!!sym(".value"), 0.975)) %>%
    dplyr::ungroup() %>%
    dplyr::rename("variable" = !!sym(".variable")) %>%
    dplyr::mutate("variable" = gsub("\\[1\\]", "", "variable"))

  return(list(pars_df = pars_df, fit_df = dfit))

  # for sake of R CMD checks
  # .value <- .variable <- NULL
  # dt <- as.data.table(pars_df)
  # dt <-
  #   dt[, list(median = median( .value ),
  #             lower = quantile(.value, 0.025),
  #             upper = quantile(.value, 0.975)),
  #      by = list( .variable )] %>%
  #   setnames(".variable","variable") %>%
  #   as.tibble()

  return(list(pars_df = pars_df, fit_df = dfit))
}
