
# This script checks required packages for the main package and loads them.

requirements_check <- function() {
  ### Check packages
  tryCatch({
    required_packages <- c("stats")
    missing_packages <- required_packages[!sapply(required_packages, function(p) requireNamespace(p, quietly = TRUE))]
    
    if (length(missing_packages) > 0) {
      options(repos = c(CRAN = "https://cran.r-project.org"))
      install.packages(missing_packages)
    }
  }, error = function() {
    stop("Failed to install required packages.")
  })
  
  # Loading required packages
  tryCatch({
    library(required_packages, character.only = TRUE)
  }, error = function() {
    stop("Failed to load required packages.")
  })
}