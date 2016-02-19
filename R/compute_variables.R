#' Compute mean accuracy as a percentage for each participant in Jackson Phd test data frame.
#'
#' Compute the mean of all 'a' columns (scored 1 for correct and 0 for
#' incorrect) for each participant (row) and multiply by 100.
#'
#' @export
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
#' @export
#' @param x Data frame from phd sample.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Confidence vector.
#' @examples
#' computeConfidence(phd[[1]]$EA)
computeConfidence <- function(x, na.rm = TRUE) {
  rowMeans(x[, getCol(x, "c")], na.rm)
}

#' Compute the POST (Point Of Sufficient cerTainty) for each participant in Jackson Phd test data frame.
#'
#' @export
#' @param x Data frame from phd sample.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return POST vector. Any values < 0 or > 100 scored as NA.
#' @examples
#' computePost(phd[[1]]$EA)
computePost <- function(x) {
  dcs <- getCol(x, "d")
  crs <- getCol(x, "c")

  posts <- apply(x, 1, function(i) {
    fit  <- glm(i[dcs] ~ i[crs], family = binomial())
    -coef(fit)[[1]] / coef(fit)[[2]]
  })

  posts[is.na(posts) | posts < 0 | posts > 100] <- NA

  posts
}

#' Compute discrimination (confidence for correct answers - confidence for
#' incorrect answers) for each participant in Jackson Phd test data frame.
#'
#' @export
#' @param x Data frame from phd sample.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Discrimination vector.
#' @examples
#' computeDiscrimination(phd[[1]]$EA)
computeDiscrimination <- function(x, na.rm = TRUE) {
  acc <- getCol(x, "a")
  crs <- getCol(x, "c")

  tmp <- sapply(c(1, 0), function(a) {
    apply(x, 1, function(i) {
      pick <- i[acc] == a
      if (any(pick)) {
        return (mean(i[crs][pick], na.rm = na.rm))
      } else {
        return (NA)
      }
    })
  })
  tmp[, 1] - tmp[, 2]
}
