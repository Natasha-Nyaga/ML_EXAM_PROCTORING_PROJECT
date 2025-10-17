#####################################################################
# R code for the Manhattan (login-type) detector
# (by Kevin Killourhy)
#####################################################################

## trainManhattan
# Train a manhattan detector
##
# The Manhattan detector calculates the column means of the training
# data, and stores these in the detector object.
##
# Y: a matrix wherein each row is a timing vector
##
# Returns an (opaque) trained detector object

trainManhattan <- function( Y ) {

  obj <- list( type = 'Manhattan',
              means = colMeans(Y) );

  return( obj );
}

## classifyManhattan
# Generate anomaly scores using a trained manhattan detector
##
# Each test sample is first centered by the means of the training
# data, and then the anomaly score is calculated as the Manhattan
# (city block) distance from the origin.
##
# obj: a trained manhattan detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifyManhattan <- function( obj, Y ) {

  Y <- scale( Y, center=obj$means, scale=FALSE );

  res <- data.frame( score = rowSums( abs(Y) ) );

  return( res );
}
