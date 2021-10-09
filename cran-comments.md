## Test environments
* local R installation, R 4.1.1
* Fedora Linux, R-devel, clang, gfortran (via rhub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (via rhub)
* Windows Server 2008 R2 SP1, R-devel, 32⁄64 bit (via rhub)

## R CMD check results

0 errors | 0 warnings | 4 notes

* This is a new release.

* checking dependencies in R code ... NOTE
  Namespaces in Imports field not imported from:
  ‘RcppParallel’ ‘rstantools’
  
  RcppParallel and rstantools are a build-time dependencies.
  
* checking installed package size ... NOTE
  installed size is 49.1Mb
  sub-directories of 1Mb or more:
    libs  48.4Mb
    
  The libs subdirectory is then above the 1MB threshold. My understanding is that this inflation of the libs subdirectory is due to the use of Rcpp. The use of Rcpp is necessary due to the use of rstan, as at its core, rcbayes is a package that implements Stan models.  
  
* checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.  
  
  This is due to the fact that rcbayes follows the code guidelines for packages that depend on rstan. These are the same guidelines promoted by the rstantools package. 
  
  
