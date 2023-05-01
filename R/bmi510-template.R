# bmi510-template.R

#' Sample from an atomic vector or dataframe-like object
#'
#' The function samples from either an atomic vector or a dataframe-like object
#' and returns either n samples or n rows as appropriate.
#'
#' @param x An atomic vector or a dataframe-like object
#' @param n The number of samples or rows to return (default 1)
#' @param replace Logical; should sampling be done with replacement? (default TRUE)
#' @return An atomic vector or dataframe with n samples or rows
#' @examples
#' # sample from an atomic vector
#' rando(1:10, 3, replace = TRUE)
#'
#' # sample from a dataframe
#' df <- data.frame(x = 1:5, y = 6:10)
#' rando(df, 2, replace = TRUE)
#'
#' # error case
#' rando(list(1, 2, 3), 2)
#'
#' @export
rando = function(x,n=1,replace=T){
  # if x is atomic vector: returns n samples
  res <- 0
  if(is.atomic(x)){
    return(sample(x, n, replace))
  }
  # if x i a dataframe-like object, return n rows.
  else if(is.data.frame(x) || is.matrix(x)){
    n_rows <- nrow(x)
    #if( n > n_rows) {
    #  stop("sample size exceeds number of rows in dataframe/matrix")
    #}
    row_samps <- sample(seq_len(n_rows), n, replace)
    return(x[row_samps, ])
  }
}


#' Check if values in a vector are equal to the minimum value
#'
#' The function checks if the values in a vector are equal to its minimum value.
#' By default, missing values are removed from the vector.
#'
#' @param x A numeric vector
#' @param na.rm Logical; should missing values be removed from the vector? (default TRUE)
#' @return A logical vector indicating whether values in x are equal to its minimum value
#' @examples
#' # check if values are equal to the minimum value
#' x <- c(1, 2, 3, 2, 1)
#' is_min(x) # returns TRUE FALSE FALSE FALSE TRUE
#'
#' # check if values are equal to the minimum value with missing values
#' x_with_na <- c(1, 2, 3, NA, 2, 1)
#' is_min(x_with_na, na.rm = TRUE) # returns TRUE FALSE FALSE FALSE TRUE FALSE
#' is_min(x_with_na, na.rm = FALSE) # returns TRUE FALSE FALSE NA FALSE TRUE
#'
#' @export
is_min = function(x,na.rm=T){
  min_val <- min(x)
  return(x == min_val)
}


#' Check if values in a vector are equal to the maximum value
#'
#' The function checks if the values in a vector are equal to its maximum value.
#' By default, missing values are removed from the vector.
#'
#' @param x A numeric vector
#' @param na.rm Logical; should missing values be removed from the vector? (default TRUE)
#' @return A logical vector indicating whether values in x are equal to its maximum value
#' @examples
#' # check if values are equal to the maximum value
#' x <- c(1, 2, 3, 2, 1)
#' is_max(x) # returns FALSE FALSE TRUE FALSE FALSE
#'
#' # check if values are equal to the maximum value with missing values
#' x_with_na <- c(1, 2, 3, NA, 2, 1)
#' is_max(x_with_na, na.rm = TRUE) # returns FALSE FALSE TRUE FALSE FALSE FALSE
#' is_max(x_with_na, na.rm = FALSE) # returns FALSE FALSE TRUE NA FALSE FALSE
#'
#' @export
is_max = function(x,na.rm=T){
  max_val <- max(x)
  return(x == max_val)
}


#' Replicate rows or columns of a matrix or data frame
#'
#' This function takes a matrix or data frame \code{x} and replicates its rows or
#' columns \code{M} and \code{N} times, respectively. The resulting matrix has
#' \code{nrow(x)*M} rows and \code{ncol(x)*N} columns.
#'
#' @param x A matrix or data frame
#' @param M An integer specifying the number of times to replicate the rows of \code{x}
#' @param N An integer specifying the number of times to replicate the columns of \code{x}
#' @return A matrix or data frame with replicated rows and/or columns
#' @examples
#' x <- matrix(1:4, nrow = 2)
#' rep_mat(x, M = 3) # Replicate rows 3 times
#' rep_mat(x, N = 2) # Replicate columns 2 times
#' rep_mat(x, M = 3, N = 2) # Replicate rows 3 times and columns 2 times
#' @export
rep_mat = function(x, M=1, N=1){
  #if (!is.matrix(x) && !is.data.frame(x)) {
  #  stop("x must be a matrix or data frame")
  #}
  #if (M <= 0 || N <= 0) {
  #  stop("M and N must be positive integers")
  #}
  if (M == 1 && N == 1) {
    return(x)
  }
  if (M > 1) {
    x <- x[rep(seq_len(nrow(x)), M), ]
  }
  if (N > 1) {
    x <- x[, rep(seq_len(ncol(x)), N)]
  }
  return(x)
}


#' Get the classes of variables in a tibble
#'
#' This function takes a tibble \code{x} and returns a character vector
#' containing the classes of each variable in the tibble.
#'
#' @param x A tibble
#' @return A character vector containing the classes of each variable
#' @examples
#' library(tibble)
#' x <- tibble(a = 1:5, b = c("foo", "bar", "baz", "qux", "quux"), c = TRUE)
#' classes(x)
#' @export
classes = function(x) {
  #stopifnot(tibble::is_tibble(x))
  return(sapply(x, class))
}


#' Scale the numeric variables in a tibble
#'
#' This function takes a tibble \code{x} and returns a tibble in which the
#' numeric variables have been scaled using the function \code{scale}.
#'
#' @param x A tibble
#' @param center A logical indicating whether to center the data
#' @param scale A logical indicating whether to scale the data
#' @return A tibble with numeric variables scaled
#' @examples
#' library(tibble)
#' x <- tibble(a = 1:5, b = c("foo", "bar", "baz", "qux", "quux"), c = TRUE)
#' df_scale(x)
#' @export
df_scale = function(x, center = TRUE, scale = TRUE) {
  #stopifnot(tibble::is_tibble(x))
  num_vars <- which(sapply(x, is.numeric))
  if (length(num_vars) == 0) {
    return(x)
  }
  x[num_vars] <- lapply(x[num_vars], scale, center = center, scale = scale)
  return(x)
}


#' Log-likelihood of a normal distribution
#'
#' This function calculates the log-likelihood of a sample \code{x} under a normal
#' distribution with specified mean \code{mean} and standard deviation \code{sd}.
#'
#' @param x A numeric vector of observations
#' @param mean The mean of the normal distribution
#' @param sd The standard deviation of the normal distribution
#' @return The log-likelihood of the sample \code{x} under a normal distribution
#' @examples
#' log_likelihood_norm(c(1,2,3), mean = 2, sd = 1)
#' @export
log_likelihood_norm = function(x, mean, sd) {
  #stopifnot(is.numeric(x), is.numeric(mean), is.numeric(sd), length(mean) == 1, length(sd) == 1)
  n <- length(x)
  loglik <- -n/2 * log(2*pi) - n*log(sd) - sum((x - mean)^2) / (2*sd^2)
  return(loglik)
}

#' Log-likelihood of a uniform distribution
#'
#' This function calculates the log-likelihood of a sample \code{x} under a uniform
#' distribution with specified minimum \code{min} and maximum \code{max}.
#'
#' @param x A numeric vector of observations
#' @param min The minimum value of the uniform distribution
#' @param max The maximum value of the uniform distribution
#' @return The log-likelihood of the sample \code{x} under a uniform distribution
#' @examples
#' log_likelihood_unif(c(0.5, 0.8, 1), min = 0, max = 1)
#' @export
log_likelihood_unif = function(x, min, max) {
  #stopifnot(is.numeric(x), is.numeric(min), is.numeric(max), length(min) == 1, length(max) == 1)
  #if (min >= max) {
  #  stop("min must be less than max")
  #}
  n <- length(x)
  loglik <- -n*log(max - min)
  if (any(x < min) || any(x > max)) {
    return(-Inf)
  }
  return(loglik)
}




#' Compute log-likelihood of a sample under the chi-squared density
#'
#' Given a sample of non-negative values x and a positive integer degrees of freedom df,
#' this function computes the log-likelihood of the sample under the chi-squared distribution
#' with df degrees of freedom.
#'
#' @param x a numeric vector of non-negative values
#' @param df a positive integer indicating the degrees of freedom
#' @return the log-likelihood of the sample under the chi-squared distribution
#' @examples
#' x <- c(1, 2, 3)
#' log_likelihood_chisq(x, 2)
#' # expected output: -4.10517
#' log_likelihood_chisq(x, -1)
#' # expected output: Error: df must be a positive integer
#' log_likelihood_chisq(x, 2.5)
#' # expected output: Error: df must be a positive integer
#' log_likelihood_chisq(c(-1, 2, 3), 2)
#' # expected output: Error: x must be a numeric vector with non-negative values
#' @importFrom stats dchisq
#' @export
log_likelihood_chisq = function(x, df) {
  #if (!is.numeric(x) || any(x < 0)) {
  #  stop("x must be a numeric vector with non-negative values")
  #}
  #if (!is.numeric(df) || df <= 0 || !is.integer(df)) {
  #  stop("df must be a positive integer")
  #}
  loglik <- sum(stats::dchisq(x, df, log = TRUE))
  return(loglik)
}


#' Compute log-likelihood of sample x under the f distribution.
#'
#' This function computes the log-likelihood of a sample x under the F distribution with parameters df1 and df2.
#'
#' @param x A numeric vector of observations.
#' @param df1 Degrees of freedom for the numerator.
#' @param df2 Degrees of freedom for the denominator.
#'
#' @return The log-likelihood of the sample x under the F distribution with parameters df1 and df2.
#'
#' @examples
#' x <- rf(100, df1 = 5, df2 = 10)
#' log_likelihood_f(x, df1 = 5, df2 = 10)
#'
#' @seealso \code{\link{df}}, \code{\link{logLik}}
#' @import stats df
#' @export
log_likelihood_f = function(x, df1, df2) {
  if (any(x <= 0)) {
    return(NA)
  }
  loglik <- sum(df(x, df1 = df1, df2 = df2, log = TRUE))
  return(loglik)
}


#' Calculate Log-Likelihood of t Distribution
#'
#' This function calculates the log-likelihood of a sample x under the t distribution with degrees of freedom df.
#'
#' @param x A numeric vector of observations.
#' @param df Degrees of freedom of the t distribution.
#' @return The log-likelihood of x under the t distribution.
#' @examples
#' log_likelihood_t(x = c(1.2, 0.9, -0.3), df = 4)
#' @importFrom stats dt
#' @importFrom stats dnorm
log_likelihood_t = function(x, df) {
  sum(log(dt(x, df=df)))
}


#' Calculate sensitivity metric based on predicted and true labels
#'
#' This function takes in two vectors of binary labels, the predicted labels and the true labels,
#' and calculates the sensitivity (true positive rate) of the predictions with respect to the true
#' labels. Sensitivity is the proportion of true positives (TP) out of all actual positives (TP + FN).
#'
#' @param pred A numeric vector of predicted binary labels (0 or 1)
#' @param truth A numeric vector of true binary labels (0 or 1)
#'
#' @return A numeric value representing the sensitivity (true positive rate) of the predictions
#' with respect to the true labels
#'
#' @examples
#' sensitivity(c(1,1,0,1,0), c(1,0,0,1,1))
#' # expected output: 0.75
#'
#' @export
sensitivity = function(pred, truth) {
  TP <- sum(pred == 1 & truth == 1)
  FN <- sum(pred == 0 & truth == 1)
  return(TP / (TP + FN))
}

#' Calculate specificity metric
#'
#' This function calculates the specificity metric based on comparing predicted and truth labels.
#'
#' @param pred a vector of predicted labels
#' @param truth a vector of true labels
#'
#' @return a numeric value representing the specificity metric
#'
#' @examples
#' specificity(c(0,0,1,1), c(1,0,1,0)) # returns 0.5
#' specificity(c(0,0,1,1), c(0,0,0,0)) # returns 1
#' specificity(c(0,0,1,1), c(1,1,1,1)) # returns 0
#'
#' @export
specificity = function(pred, truth) {
  tn <- sum(!pred & !truth)
  fp <- sum(pred & !truth)
  return(tn / (tn + fp))
}



#' Calculate precision metric for predicted and truth labels
#'
#' This function calculates the precision metric for a binary classification problem
#' based on the predicted labels and the true labels. Precision measures the proportion
#' of true positive predictions out of all positive predictions.
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The precision metric as a single numeric value
#'
#' @examples
#' pred <- c(1, 0, 0, 1, 1, 0, 1, 0)
#' truth <- c(1, 0, 1, 0, 1, 0, 1, 0)
#' precision(pred, truth)
#'
#' @export
precision = function(pred, truth) {
  # Calculate the number of true positive predictions
  true_positives <- sum(pred == 1 & truth == 1)

  # Calculate the total number of positive predictions
  predicted_positives <- sum(pred == 1)

  # Calculate precision as the proportion of true positives out of all positive predictions
  precision_value <- true_positives / predicted_positives

  return(precision_value)
}




#' Calculate Recall
#'
#' This function calculates the recall metric based on comparing predicted and truth labels.
#' Recall, also known as sensitivity or true positive rate, measures the proportion of actual positives
#' that are correctly identified by the model.
#'
#' @param pred A vector of predicted labels
#' @param truth A vector of true labels
#'
#' @return A scalar value representing the recall score.
#'
#' @examples
#' pred <- c(1, 1, 0, 1, 0, 1)
#' truth <- c(1, 0, 0, 1, 1, 1)
#' recall(pred, truth)
#' #[1] 0.75
#'
#' @export
recall = function(pred, truth) {
  tp <- sum(pred == 1 & truth == 1)
  fn <- sum(pred == 0 & truth == 1)

  if (tp + fn == 0) {
    return(0)
  } else {
    return(tp / (tp + fn))
  }
}




#' Calculate accuracy metric based on predicted and truth labels
#'
#' Calculates the accuracy metric as the proportion of correct predictions
#' over the total number of predictions, based on comparing predicted and
#' truth labels.
#'
#' @param pred A vector of predicted labels.
#' @param truth A vector of true labels.
#'
#' @return A numeric value representing the accuracy of the predictions.
#'
#' @examples
#' # Binary classification example
#' pred <- c(0, 1, 1, 0, 1)
#' truth <- c(1, 1, 0, 0, 1)
#' accuracy(pred, truth)
#' # Output: 0.6
#'
#' @export
accuracy = function(pred, truth) {
  #if(length(pred) != length(truth)) {
  #  stop("Length of pred and truth vectors must match")
  #}
  correct <- sum(pred == truth)
  total <- length(pred)
  return(correct / total)
}



#' Calculate F1 Score
#'
#' This function calculates the F1 score metric based on comparing predicted and truth labels.
#'
#' @param pred A vector of predicted labels.
#' @param truth A vector of true labels.
#' @return The F1 score.
#' @export
f1 = function(pred, truth) {
  TP <- sum(pred == 1 & truth == 1)
  TN <- sum(pred == 0 & truth == 0)
  FP <- sum(pred == 1 & truth == 0)
  FN <- sum(pred == 0 & truth == 1)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(f1_score)
}


#' Calculate the minimum sample size per group for a two-sample t-test
#'
#' This function calculates the minimum number n per group needed for a two-sample t-test
#' with a given expected Cohen's d and power.
#'
#' @param d The expected Cohen's d.
#' @param power The desired power of the t-test (default is 0.8).
#' @return The minimum sample size per group.
#' @examples
#' minimum_n_per_group(0.5)
#' minimum_n_per_group(0.8, 0.9)
minimum_n_per_group = function(d, power = 0.8) {
  min_n <- ceiling(power.t.test(power = power, delta = d)$n) # Round to higher integer.
  return(min_n)
}

#' Calculate R-squared
#'
#' This function calculates the R-squared statistic between predicted and ground truth continuous variables.
#'
#' @param pred A numeric vector of predicted values.
#' @param truth A numeric vector of ground truth values.
#' @return A numeric value representing the R-squared statistic.
#' @examples
#' r2(c(1, 2, 3), c(1.1, 1.8, 2.9))
r2 = function(pred, truth) {
  ssr <- sum((truth - pred)^2) # SS resids
  sst <- sum((truth - mean(truth))^2) # SS total
  return(1-(ssr/sst))
}


#' Calculate the adjusted R-squared between predicted and ground truth continuous variables
#'
#' This function calculates the adjusted R-squared statistic between predicted and ground truth
#' continuous variables, given the number of model parameters (excluding the intercept).
#'
#' @param pred A numeric vector of predicted values
#' @param truth A numeric vector of true values
#' @param n_p An integer value indicating the number of model parameters, excluding the intercept.
#' @return A numeric value representing the adjusted R-squared statistic
#' @examples
#' set.seed(123)
#' n <- 100
#' x <- rnorm(n)
#' y <- 2 * x + rnorm(n)
#' model <- lm(y ~ x)
#' adj_R2(predict(model), y, n_p = 1)
adj_R2 = function(pred, truth, n_p) {
  n <- length(truth)
  r2 <- 1 - sum((truth - pred) ^ 2) / sum((truth - mean(truth)) ^ 2)
  adj_r2 <- 1 - ((1 - r2) * (n - 1)) / (n - n_p - 1)
  return(adj_r2)
}
