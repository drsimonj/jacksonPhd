#' Get item-level column names from Jackson Phd test data frame.
#'
#' Get item-level column names for answer accuracy (a), confidence (c), decision (d)
#' or time (t) from data frame.
#'
#' @export
#' @param x Data frame from phd sample.
#' @param i Character indicating information type to get. 'a' = accuracy, 'c' =
#'   confidence, 'd' = decision, 't' = time.
#' @return Vector of column names.
#' @examples
#' getCol(phd[[1]]$EA, 'a')
getCol <- function(x, i) {
  grep(paste0(i, "[0-9]"), names(x), value = T)
}

#' Return the number of test items for a given Jackson Phd test data frame.
#'
#' Queries and returns the number of accuracy columns.
#'
#' @export
#' @param x Data frame from phd sample.
#' @return Vector of column names.
#' @examples
#' nItem(phd[[1]]$EA)
nItem <- function(x) {
  length(getCol(x, 'a'))
}
