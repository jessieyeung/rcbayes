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
#' @importFrom rstan sampling extract
#' @import Rcpp
#' @importFrom stats quantile median
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise rename mutate ungroup
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @importFrom tibble as.tibble
#' @importFrom tidybayes gather_draws
#' @return A list of length 3. The first element, \code{pars_df}, is a data frame that provides parameter estimates with 95% credible intervals.
#' The second element, \code{fit_df}, is a data frame that shows the data and estimated migration rates at each age.
#' The third element, \code{check_converge}, is a data frame that provides the R-hat values and effective sample sizes.
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
#'
#' # fit the model
#' res <- mig_estimate_rc(ages, net_mig, pop,
#'                        pre_working_age = TRUE,
#'                        working_age = TRUE,
#'                        retirement = TRUE,
#'                        post_retirement = FALSE,
#'                        #optional inputs into stan
#'                        control = list(adapt_delta = 0.95, max_treedepth = 10),
#'                        iter = 10, chains = 1 #to speed up example
#'                        )
#' # plot the results and data
#' plot(ages, net_mig/pop, ylab = "migration rate", xlab = "age")
#' lines(ages, res[["fit_df"]]$median, col = "red")
#' legend("topright", legend=c("data", "fit"), col=c("black", "red"), lty=1, pch = 1)
#'
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

  # fit the model
  rc_fit <- rstan::sampling(stanmodels$rcmodel, data = mig_data, ...)
  #rc_fit <- rstan::stan(model_code = rc_flexible, data = mig_data, ...)

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
    dplyr::mutate(variable = gsub("\\[1\\]", "", variable))

  check_converge <- rstan::summary(rc_fit)$summary[1:length(parnames),c("mean", "se_mean", "n_eff", "Rhat")]

  return(list(pars_df = pars_df, fit_df = dfit, check_converge = check_converge))

}
