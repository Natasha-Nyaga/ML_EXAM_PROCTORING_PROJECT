#####################################################################
# R code for the Mahalanobis (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( MASS );

## trainMahalanobis
# Train a Mahalanobis detector
##
# The Mahalanobis detector calculates the means and the eigen values
# and vectors of the covariance matrix of the training data, and it
# stores them in the detector object.
##
# Y: a matrix wherein each row is a timing vector
##
# Returns an (opaque) trained detector object

trainMahalanobis <- function( Y ) {

  means <- colMeans(Y);
  eig <- eigen( cov( sweep( Y, 2, means ) ) );
  
  obj <- list( type = 'Mahalanobis',
              means = means,
              eig = eig );

  return( obj );
}

## classifyMahalanobis
# Generate anomaly scores using a trained Mahalanobis detector
##
# Each test sample is first centered by the means of the training
# data, and then the anomaly score is calculated as the Mahalanobis
# distance from the origin.  The anomaly score is this distance.
# Technically, we "sphere" the data and then calculate Euclidean
# distance, which is mathematically equivalent to the Mahalanobis
# distance.
##
# obj: a trained Mahalanobis detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifyMahalanobis <- function( obj, Y ) {

  E <- obj$eig$vectors;
  D <- diag( obj$eig$values );

  Y <- sweep( Y, 2, obj$means );
  Y <- Y %*% E %*% sqrt( ginv( D ) ) %*% t(E);

  res <- data.frame( score = sqrt( rowSums( abs(Y)^2 ) ) );

  return( res );
}
