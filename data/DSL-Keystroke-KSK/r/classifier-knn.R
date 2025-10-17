#####################################################################
# R code for the kNN (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( nnclust );

## trainKNN
# Train a kNN detector
##
# The kNN detector scales the training data, giving each column unit
# variance, and it builds a KD-Tree of the scaled data.  It stores the
# scaling factors and the tree in the detector object.
##
# Y: a matrix wherein each row is a timing vector
# k: the number of nearest neighbors to use in calculating distances
##
# Returns an (opaque) trained detector object

trainKNN <- function( Y, k=1 ) {

  stopifnot( k == 1 );
  
  means <- colMeans( Y );
  sds <- apply( Y, 2, sd );

  Yscaled <- scale( Y, center=means, scale=sds );
  
  obj <- list( type = 'KNN',
              means = means,
              sds = sds,
              k = k,
              Yscaled = Yscaled );

  return( obj );
}

## classifyKNN
# Generate anomaly scores using a trained kNN detector
##
# For eact test sample, k nearest training samples are found, along
# with the Euclidean distances to each of the k.  The average distance
# between these k points and the test sample is calculates.  The
# anomaly score is this average distance.
##
# obj: a trained kNN detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y.

classifyKNN <- function( obj, Y ) {

  stopifnot( obj$k == 1 );
  
  Yscaled <- scale( Y, center=obj$means, scale=obj$sds );
  
  nns <- nnfind( from=obj$Yscaled, to=Yscaled );

  thescores <- nns$dist;

  res <- data.frame( score = thescores );

  return( res );
}
