#' Get item-level column names from Jackson Phd test data frame.
#'
#' Get item-level column names for answer accuracy (a), confidence (c), decision (d)
#' or time (t) from data frame with regex call.
#'
#' @export
#' @param x Data frame from phd sample.
#' @param i Character indicating information type to get. 'a' = accuracy, 'c' =
#'   confidence, 'd' = decision, 't' = time.
#' @param group Logical vector used to return a group of the candidate results.
#'   Default = c(TRUE) to return all results.
#' @return Vector of column names.
#' @examples
#' getCol(phd[[1]]$EA, 'a')
#' getCol(phd[[1]]$EA, "a|c")
#' getCol(phd[[1]]$EA, "[ac]")
#' getCol(phd[[1]]$EA, 'a', c(T, F))  # return off columns
getCol <- function(x, i, group = c(T)) {
  grep(paste0(i, "[0-9]"), names(x), value = T)[group]
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

#' Remove the 'demo' data frames from a phd data set.
#'
#' @export
#' @param phd.dat Data set of phd data.
removeDemo <- function(phd.dat) {
  lapply(phd.dat, function(sample) {
    demo <- names(sample) == "demo"
    sample[!demo]
  })
}

#' Append the 'demo' data frames from one phd data set onto another phd data
#' set.
#'
#' @export
#' @param phd.without Phd data set without the demo data frames.
#' @param phd.with Phd data set with the demo data frames
appendDemo <- function(phd.without, phd.with) {
  lapply(1:length(phd.with), function(sample) {
    c(phd.without[[sample]], phd.with[[sample]]["demo"])
  })
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

#' Apply a function to all data frames in a phd data set
#' (or copy of it).
#'
#' This is a convenience function to apply a function (FUN) to all data frames
#' within the phd data set (or copy of it).
#'
#' @export
#' @param phd.dat Data set on which to apply function. Either phd or copy of it.
#' @param FUN the function to be applied
#' @param ... optional arguments to FUN.
#' @param demo.rm a logical value indicating whether the demographic data frames
#'   should be removed before the analysis proceeds.
#' @examples
#' x <- phdApply(phd, compute)
#' sapply(x, names)
#' head(x[[1]]$EA)
#'
#' x <- phdApply(phd, compute, "accuracy")
#' x <- appendDemo(x, phd)  # append demographics back on
#' lapply(x, names)
#' head(x[[1]]$EA)
phdApply <- function (phd.dat, FUN, ..., demo.rm = TRUE) {

  if (demo.rm) {
    phd.dat <- removeDemo(phd.dat)
  }

  lapply(phd.dat, lapply, FUN, ...)
}
