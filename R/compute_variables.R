#' Compute mean accuracy as a percentage for each participant in Jackson Phd test data frame.
#'
#' Compute the mean of all 'a' columns (scored 1 for correct and 0 for
#' incorrect) for each participant (row) and multiply by 100.
#' To be called with compute().
#'
#' @param x Data frame from phd sample.
#' @param group Logical vector used to return a group of the candidate results.
#'   See ?getCol for details.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Accuracy vector.
#' @examples
#' computeAccuracy(phd[[1]]$EA)
computeAccuracy <- function(x, group = c(T), na.rm = TRUE) {
  rowMeans(x[, getCol(x, "a", group)], na.rm) * 100
}

#' Compute mean confidence as a percentage for each participant in Jackson Phd test data frame.
#'
#' Compute the mean of all 'c' columns (scored 1 for correct and 0 for
#' incorrect) for each participant (row).
#' To be called with compute().
#'
#' @param x Data frame from phd sample.
#' @param group Logical vector used to return a group of the candidate results.
#'   See ?getCol for details.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Confidence vector.
#' @examples
#' computeConfidence(phd[[1]]$EA)
computeConfidence <- function(x,  group = c(T), na.rm = TRUE) {
  rowMeans(x[, getCol(x, "c", group)], na.rm)
}

#' Compute bias (over/underconfidence) for each participant.
#'
#' Compute the bias score for each participant in the data frame.
#' To be called with compute().
#'
#' @param x Data frame from phd sample.
#' @param group Logical vector used to return a group of the candidate results.
#'   See ?getCol for details.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Bias vector.
#' @examples
#' computeBias(phd[[1]]$EA)
computeBias <- function(x, group = c(T), na.rm = TRUE) {
  computeConfidence(x, group, na.rm) - computeAccuracy(x, group, na.rm)
}

#' Compute discrimination (confidence for correct answers - confidence for
#' incorrect answers) for each participant in Jackson Phd test data frame.
#' To be called with compute().
#'
#' @param x Data frame from phd sample.
#' @param group Logical vector used to return a group of the candidate results.
#'   See ?getCol for details.
#' @param na.rm logical. Should missing values (including NaN) be omitted from the calculations? Default = True
#' @return Discrimination vector.
#' @examples
#' computeDiscrimination(phd[[1]]$EA)
computeDiscrimination <- function(x, group = c(T), na.rm = TRUE) {
  acc <- getCol(x, "a", group)
  crs <- getCol(x, "c", group)

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

#' Compute the POST (Point Of Sufficient cerTainty) for each participant.
#'
#' To be called with compute().
#'
#' @param x Data frame from phd sample.
#' @param group Logical vector used to return a group of the candidate results.
#'   See ?getCol for details.
#' @return POST vector. Any values < 0 or > 100 scored as NA.
#' @examples
#' computePost(phd[[1]]$EA)
computePost <- function(x, group = c(T)) {
  dcs <- getCol(x, "d", group)
  crs <- getCol(x, "c", group)

  # Fit logistic regression model
  posts <- apply(x, 1, function(i) {
    # Return NA if all decisions or confidence ratings are the same
    if (allWithin(i[dcs], na.rm = TRUE) | allWithin(i[crs], na.rm = TRUE)) {
      return (NA)
    }

    fit  <- glm(i[dcs] ~ i[crs], family = binomial())
    -coef(fit)[[1]] / coef(fit)[[2]]
  })

  # Set any POST to NA if it's outside acceptable range (0, 100)
  posts[is.na(posts) | posts < 0 | posts > 100] <- NA

  posts
}

#' Compute a set of variables for each participant.
#'
#' Computes a set of variables for each participant in a Jackson PhD data frame.
#'
#' @export
#' @param x Data frame from phd sample.
#' @param vars Character vector of variables to be computed. Default/Allowed =
#'   c("accuracy", "confidence", "bias", "discrimination", "post")
#' @param group Logical vector used to return a group of the candidate results.
#'   See ?getCol for details.
#' @param suffix String. Will be added to column names.
#' @param bind logical. Should variable results be bound to original data frame x? Default = TRUE.
#' @param na.rm logical. Should missing values (including NaN) be omitted from
#'   the calculations? Default = True
#' @examples
#' head(compute(phd[[1]]$EA))
#' head(compute(phd[[1]]$EA, "accuracy"))
#' head(compute(phd[[1]]$EA, "post", c(T, F), ".odd"))
compute <- function(x, vars = c("accuracy", "confidence", "bias", "discrimination", "post"),
                    group = c(T), suffix = "", bind = TRUE, na.rm = TRUE) {

  # Calculate variables
  results <- sapply(vars, function(var) {
    switch(var,
           accuracy = computeAccuracy(x, group, na.rm),
           confidence = computeConfidence(x, group, na.rm),
           bias = computeBias(x, group, na.rm),
           discrimination = computeDiscrimination(x, group, na.rm),
           post = computePost(x, group))
  })
  colnames(results) <- paste0(colnames(results), suffix)

  # Return results
  if (bind) {
    return (cbind(x, results))
  }
  return (results)
}
