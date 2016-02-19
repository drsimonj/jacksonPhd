#' Compute mean accuracy as a percentage for each participant in Jackson Phd test data frame.
#'
#' Compute the mean of all 'a' columns (scored 1 for correct and 0 for
#' incorrect) for each participant (row) and multiply by 100.
#'
#' @param x Data frame from phd sample.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Accuracy vector.
#' @examples
#' computeAccuracy(phd[[1]]$EA)
computeAccuracy <- function(x, na.rm = TRUE) {
  rowMeans(x[, getCol(x, "a")], na.rm) * 100
}

#' Compute mean confidence as a percentage for each participant in Jackson Phd test data frame.
#'
#' Compute the mean of all 'c' columns (scored 1 for correct and 0 for
#' incorrect) for each participant (row).
#'
#' @param x Data frame from phd sample.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Confidence vector.
#' @examples
#' computeConfidence(phd[[1]]$EA)
computeConfidence <- function(x, na.rm = TRUE) {
  rowMeans(x[, getCol(x, "c")], na.rm)
}
