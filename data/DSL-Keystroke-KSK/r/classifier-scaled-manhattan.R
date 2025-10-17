#####################################################################
# R code for the Scaled-Manhattan (login-type) detector
# (by Kevin Killourhy)
#####################################################################

## trainScaledManhattan
# Train a scaled-manhattan detector
##
# The scaled-Manhattan detector calculates the column means and
# standard deviations of the training data, and stores these in the
# detector object.
##
# Y: a matrix wherein each row is a timing vector
##
# Returns an (opaque) trained detector object

trainScaledManhattan <- function( Y ) {

  obj <- list( type = 'ScaledManhattan',
              means = colMeans(Y),
              sds   = apply( Y, 2, sd ) );

  return( obj );
}

## classifyScaledManhattan
# Generate anomaly scores using a trained scaled-manhattan detector
##
# Each test sample is first centered and scaled by the means and
# standard deviations of the training data, and then the Manhattan
# (city block) distance from the origin.  The anomaly score is this
# distance.
##
# obj: a trained scaled-manhattan detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifyScaledManhattan <- function( obj, Y ) {

  Y <- scale( Y, center=obj$means, scale=obj$sds );

  res <- data.frame( score = rowSums( abs(Y) ) );

  return( res );
}
