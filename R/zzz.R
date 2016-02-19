.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thanks for downloading my PhD data. Enjoy!")
}

# Data documentation ====================================

#' Data from Simon A Jackson's PhD.
#'
#' A list of datasets containing results on various test for four samples
#' collected for research by Simon A Jackson.
#'
#' @format A list with four lists for each of four samples. Within each sample
#'   list, there are multiple data frames - one for for each test that was
#'   admininistered.
"phd"
