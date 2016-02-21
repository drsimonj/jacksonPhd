#' Get item-level column names from Jackson Phd test data frame.
#'
#' Get item-level column names for answer accuracy (a), confidence (c), decision (d)
#' or time (t) from data frame with regex call.
#'
#' @export
#' @param x Data frame from phd sample.
#' @param i Character indicating information type to get. 'a' = accuracy, 'c' =
#'   confidence, 'd' = decision, 't' = time.
#' @return Vector of column names.
#' @examples
#' getCol(phd[[1]]$EA, 'a')
#' getCol(phd[[1]]$EA, "a|c")
#' getCol(phd[[1]]$EA, "[ac]")
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

#' Convert data frame from phd sample to long format.
#'
#' @export
#' @param x Data frame from phd sample.
#' @param demo Matching demographics data frame from phd sample. If NULL, no
#'   demographics will be added.
#' @examples
#' x <- phd[[1]]$EA
#' head(stretchPhd(x))
#' x <- compute(x)
#' head(stretchPhd(x))
#' d <- phd[[1]]$demo
#' head(stretchPhd(x, d))
stretchPhd <- function(x, demo = NULL) {
  # Get n items in test
  ni <- nItem(x)

  # Stretch fixed variables (all but item-level)
  fixed <- colnames(x)[!(colnames(x) %in% getCol(x, "[acdt]"))]
  fixed <- sapply(fixed, function(fix) {
    rep(x[, fix], each = nItem(x))
  })

  # Stretch the item-level variables
  item <- rep(1:ni, times = nrow(x))
  i.correct <- as.vector(apply(x[, getCol(x, "a")], 1, c))
  i.confidence <- as.vector(apply(x[, getCol(x, "c")], 1, c))
  i.decision <- as.vector(apply(x[, getCol(x, "d")], 1, c))
  i.rt <- as.vector(apply(x[, getCol(x, "t")], 1, c))
  if (!length(i.rt)) i.rt <- NA

  # Bind fixed and stretched
  stretched <- cbind(fixed, data.frame(item, i.correct, i.confidence, i.decision, i.rt))

  # Bind demographic variables if available
  if (!is.null(demo)) {
    stretched <- merge(stretched, demo, by = "id")
  }

  # Return
  return (stretched)
}
