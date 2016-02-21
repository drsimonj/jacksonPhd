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

#' Check if all elements of a numeric vector fall within a tolerable range.
#'
#' @param x Numeric vector.
#' @param tol Tolerable range. Default = 0 to check if all elements are equal.
#' @param na.rm a logical value indicating whether NA values should be stripped
#'   before the computation proceeds. Default = FALSE
allWithin <- function(x, tol = 0, na.rm = FALSE) {
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) < tol
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

#' Apply a function to all data frames (except demographics) in the phd data set
#' (or copy of it).
#'
#' This is a convenience function to apply a function (FUN) to all data frames
#' within the phd data set (or copy of it).
#'
#' @export
#' @param all Data set on which to apply function. Either phd or copy of it.
#' @param FUN the function to be applied
#' @param ... optional arguments to FUN.
#' @examples
#' x <- phdApply(phd, compute)
#' sapply(x, names)
#' head(x[[1]]$EA)
phdApply <- function (all, FUN, ...) {
  lapply(all, function(sample) {

    # Get tests without demographics
    tests <- names(sample)
    tests <- tests[tests != "demo"]

    # Apply function to tests
    applied <- lapply(tests, function(test) {
      FUN(sample[[test]], ...)
    })
    names(applied) <- tests

    # Append demographics back on
    c(applied, sample["demo"])
  })
  #lapply(all, lapply, FUN, ...)
}
