#' Set initial values for Rogers-Castro migration model
#'
#' @description Choose initial values for parameters in the Rogers-Castro model in a strategic way based on your data.
#' Provide these initial values to improve convergence of model. Intended to be used with rcbayes::mig_estimate_rc as
#' an additional input into 'Stan'.
#'
#' @param ages numeric. A vector of integers for ages.
#' @param migrants numeric. A vector of integers for observed age-specific migrants.
#' @param pop numeric. A vector of integers for age-specific population or sample sizes, of which "migrants" experienced a migration event.
#' @param mx numeric. A vector of age-specific migration rates.
#' @param pre_working_age logical (TRUE/FALSE). Whether or not you are including pre working age component.
#' @param working_age logical (TRUE/FALSE). Whether or not you are including working age component.
#' @param retirement logical (TRUE/FALSE). Whether or not you are including retirement age component.
#' @param post_retirement logical (TRUE/FALSE). Whether or not you are including post retirement age component.
#' @param nchains numeric. A positive integer specifying the number of Markov chains. Should be 4 unless changed otherwise.
#' @param net_mig numeric. Deprecated argument, use migrants instead.
#' @importFrom stats runif
#' @return A list of length \code{nchains}. Each element of the list is a list of numeric values.
#' Within the inner lists, there is one element for every model parameter.
#' @export
#'
#' @examples
#' # Ex. 1: Using ages, migrants, and population
#' ages <- 0:80
#'migrants <- c(202,215,167,188,206,189,164,
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
#'iv <- init_rc(ages=ages, migrants=migrants, pop=pop,
#'              pre_working_age=TRUE,
#'              working_age=TRUE,
#'              retirement=TRUE,
#'              post_retirement=TRUE)
#'
#' # Ex 2: Using ages and mx
#' ages <- 0:80
#' mx <- c(0.001914601, 0.002037818, 0.001582863, 0.001781906,
#'         0.001952514, 0.001780902, 0.001545333, 0.001488796,
#'         0.001856284, 0.001743211, 0.001758172, 0.001728203,
#'         0.001668265, 0.001977943, 0.002027891, 0.002063022,
#'         0.002167479, 0.002385097, 0.002776811, 0.003003134,
#'         0.003558771, 0.003588001, 0.003807227, 0.003690307,
#'         0.003865687, 0.003858488, 0.003814558, 0.003873131,
#'         0.003712056, 0.003543659, 0.003290238, 0.003092965,
#'         0.002811146, 0.002811146, 0.002677282, 0.002744282,
#'         0.002311759, 0.002416161, 0.002155156, 0.002177528,
#'         0.002064710, 0.002057062, 0.002179416, 0.001942356,
#'         0.001873533, 0.001981783, 0.001921955, 0.001929434,
#'         0.001966826, 0.001892041, 0.002244159, 0.001900401,
#'         0.002153355, 0.002244159, 0.002263617, 0.002441776,
#'         0.002655001, 0.002379872, 0.002366115, 0.002421141,
#'         0.002621367, 0.002534252, 0.002431298, 0.002534252,
#'         0.002455057, 0.002381964, 0.002345034, 0.002243477,
#'         0.002363499, 0.002428126, 0.002292457, 0.002117078,
#'         0.002154659, 0.002004334, 0.002079497, 0.001897374,
#'         0.002216401, 0.001863792, 0.002182820, 0.001847001,
#'         0.001897374)
#'
#' # compute initial values
#' iv <- init_rc(ages=ages, mx=mx,
#'              pre_working_age=TRUE,
#'              working_age=TRUE,
#'              retirement=TRUE,
#'              post_retirement=TRUE)
init_rc <- function(ages,
                    migrants,
                    pop,
                    mx,
                    pre_working_age,
                    working_age,
                    retirement,
                    post_retirement,
                    nchains=4,
                    net_mig
                    ){

  # initial checks
  stopifnot(any(pre_working_age, working_age, retirement, post_retirement))
  if(!missing(net_mig)) stop("Argument net_mig deprecated, use migrants instead.")

  if(missing(ages)) stop("ages is missing")

  if(!missing(migrants) & !missing(pop)) {

    message("init_rc is using arguments ages, migrants, and pop")
    if(length(ages)!=length(migrants) | length(migrants)!=length(pop))
      stop("length of arguments ages, migrants and pop must be equal")
    rate <- migrants / pop

  } else if (!missing(mx)) {

    message("init_rc is using arguments ages and mx")
    if(length(ages)!=length(mx))
      stop("length of arguments ages and mx must be equal")
    rate <- mx

  } else {

    stop("init_rc requires either data for migrants and pop, or data for mx")

  }

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
