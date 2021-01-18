mig_calculate_rc <- function(ages,
                             pars){

  # parameter name groups
  comp1 <- c("a1", "alpha1")
  comp2 <- c("a2", "alpha2", "lambda2", "mu2")
  comp3 <- c("a3", "alpha3", "lambda3", "mu3")
  comp4 <- c("a4", "lambda4")


  # check for specific parameter groups
  if (any(comp1 %in% names(pars))){
    stopifnot(all(comp1 %in% names(pars)))
  }
  if (any(comp2 %in% names(pars))){
    stopifnot(all(comp2 %in% names(pars)))
  }
  if (any(comp3 %in% names(pars))){
    stopifnot(all(comp3 %in% names(pars)))
  }
  if (any(comp4 %in% names(pars))){
    stopifnot(all(comp4 %in% names(pars)))
  }

  pars_blank <- c(a1 = 0, alpha1 = 0,
                  a2 = 0, alpha2 = 0, mu2 = 0, lambda2 = 0,
                  a3 = 0, alpha3 = 0, mu3 = 0, lambda3 = 0,
                  a4 = 0, lambda4 = 0,
                  c = 0)

  pars_blank[names(pars)] <- pars
  pars       <- pars_blank

  x  <- ages
  mx <-
    # pre working age
    pars[["a1"]]*exp(-1 * pars[["alpha1"]]*x) +

    # working
    pars[["a2"]]*exp(-1 * pars[["alpha2"]] * (x - pars[["mu2"]]) -
                       exp(-1 * pars[["lambda2"]] * (x - pars[["mu2"]]))) +

    # retirement
    pars[["a3"]] * exp(-1 * pars[["alpha3"]] * (x - pars[["mu3"]]) -
                         exp(-1 * pars[["lambda3"]] * (x - pars[["mu3"]]))) +

    # post-retirement
    pars[["a4"]] * exp(pars[["lambda4"]] *x ) +

    # intensity parameter
    pars[["c"]]

  return(mx)
}
