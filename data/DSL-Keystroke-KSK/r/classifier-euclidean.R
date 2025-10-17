#####################################################################
# R code for the Euclidean (login-type) detector
# (by Kevin Killourhy)
#####################################################################

## trainEuclidean
# Train a Euclidean detector
##
# The Euclidean detector calculates the column means of the training
# data, and stores these in the detector object.
##
# Y: a matrix wherein each row is a timing vector
##
# Returns an (opaque) trained detector object

trainEuclidean <- function( Y ) {

  obj <- list( type = 'Euclidean',
              means = colMeans(Y) );

  return( obj );
}

## classifyEuclidean
# Generate anomaly scores using a trained Euclidean detector
##
# Each test sample is first centered by the means of the training
# data, and then the anomaly score is calculated as the Euclidean
# distance from the origin.
##
# obj: a trained Euclidean detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y.

classifyEuclidean <- function( obj, Y ) {

  Y <- scale( Y, center=obj$means, scale=FALSE );

  res <- data.frame( score = sqrt( rowSums( abs(Y)^2 ) ) );

  return( res );
}
