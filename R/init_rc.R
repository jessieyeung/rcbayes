#' Set initial values for Rogers-Castro migration model
#'
#' @description Choose initial values for parameters in the Rogers-Castro model in a strategic way based on your data.
#' Provide these initial values to improve convergence of model. Intended to be used with rcbayes::mig_estimate_rc as
#' an additional input into 'Stan'.
#'
#' @param ages numeric. A vector of integers for ages.
#' @param net_mig numeric. A vector of integers for observed age-specific net migrants.
#' @param pop numeric. A vector of integers for age-specific population.
#' @param pre_working_age logical (TRUE/FALSE). Whether or not you are including pre working age component.
#' @param working_age logical (TRUE/FALSE). Whether or not you are including working age component.
#' @param retirement logical (TRUE/FALSE). Whether or not you are including retirement age component.
#' @param post_retirement logical (TRUE/FALSE). Whether or not you are including post retirement age component.
#' @param nchains numeric. A positive integer specifying the number of Markov chains. Should be 4 unless changed otherwise.
#' @importFrom stats runif
#' @return A list of length \code{nchains}. Each element of the list is a list of numeric values.
#' Within the inner lists, there is one element for every model parameter.
#' @export
#'
#' @examples
#' # define ages, net migrants, and population
#'ages <- 0:80
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
#'#compute initial values
#'iv <- init_rc(ages, net_mig, pop, pre_working_age=TRUE,
#'              working_age=TRUE, retirement=TRUE, post_retirement=TRUE)
#'

init_rc <- function(ages, net_mig, pop, pre_working_age, working_age, retirement, post_retirement, nchains=4){
  rate <- net_mig / pop
  rate_ranked <- order(rate, decreasing=T)
  rate_ranked_by_age <- ages[rate_ranked]

  init_vals <- list()
  for (i in 1:nchains){
    init_vals[[i]] <- list(c = max(min(rate),0.0001)) #ensure init cond is positive using max fn
    if (pre_working_age){
      init_vals[[i]][['alpha1']] <- array(runif(1,0,1))
      init_vals[[i]][['a1']] <- array(max((rate[1] - min(rate)),0.0001))
    }
    if (working_age){
      init_vals[[i]][['alpha2']] <- array(runif(1,0,1))
      init_vals[[i]][['a2']] <- array(max(max(rate[ages>=15 & ages<=50]) - min(rate), 0.0001))
      init_vals[[i]][['mu2']] <- array(max(
        ages[rate_ranked[rate_ranked_by_age>=15 & rate_ranked_by_age<=50][1]],
        0.0001))
      init_vals[[i]][['lambda2']] <- array(runif(1,0,1))
    }
    if (retirement){
      init_vals[[i]][['alpha3']] <- array(runif(1,0,1))
      init_vals[[i]][['a3']] <- array(max(max(rate[ages>=55 & ages<=70]) - min(rate), 0.0001))
      init_vals[[i]][['mu3']] <- array(max(
        ages[rate_ranked[rate_ranked_by_age>=55 & rate_ranked_by_age<=70][1]],
        0.0001))
      init_vals[[i]][['lambda3']] <- array(runif(1,0,1))
    }
    if (post_retirement){
      init_vals[[i]][['a4']] <- array(max(rate[length(rate)], 0.0001))
      init_vals[[i]][['lambda4']] <- array(runif(1,-0.05,0.05))
    }
  }

  return(init_vals)
}
