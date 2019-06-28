#' Installs all available packages from CRAN
#' @note  
#' As of 6/26/2019 installing all on Windows, is ~21GB, 1/2 million files, and 129k folders, but setting up a mirror of CRAN is ~210GB, this is because it holds packages for all versions
#' @export
cran.meta.install.all.packages <- function(){
  packs <- installed.packages() # Get the currently installed packages
  exc <- names(packs[,'Package'])  # Get the names in a vector
  av <- names(available.packages()[,1]) #Get names of available packages in Cran
  ins <- av[!av %in% exc] #Make a list of all packages that you havent installed
  install.packages(ins)# Install the desired packages
}