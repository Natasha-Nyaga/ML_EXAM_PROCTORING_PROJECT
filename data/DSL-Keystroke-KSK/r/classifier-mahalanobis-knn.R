#####################################################################
# R code for the Mahalanobis kNN (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( MASS );
library( nnclust );

## trainMahalanobisKNN
# Train a Mahalanobis kNN detector
##
# The Mahalanobis kNN detector scales the training data, giving each
# column unit variance, and it builds a KD-Tree of the scaled data.
# It stores the scaling factors and the tree in the detector object.
##
# Y: a matrix wherein each row is a timing vector
# k: the number of nearest neighbors to use in calculating distances
##
# Returns an (opaque) trained detector object

trainMahalanobisKNN <- function( Y, k=1 ) {

  stopifnot( k == 1 );
  
  means <- colMeans( Y );
  eig <- eigen( cov( sweep( Y, 2, means ) ) );

  E <- eig$vectors;
  D <- diag( eig$values );
  
  Ysphere <- Y %*% E %*% sqrt( ginv( D ) ) %*% t(E);
  
  obj <- list( type = 'MahalanobisKNN',
              means = means,
              eig = eig,
              k = k,
              Ysphere = Ysphere );
  
  return( obj );
}

## classifyMahalanobisKNN
# Generate anomaly scores using a trained Mahalanobis kNN detector
##
# For eact test sample, k nearest training samples are found, along
# with the Euclidean distances to each of the k.  The average distance
# between these k points and the test sample is calculates.  The
# anomaly score is this distance.
##
# obj: a trained Mahalanobis kNN detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifyMahalanobisKNN <- function( obj, Y ) {

  stopifnot( obj$k == 1 );
  
  E <- obj$eig$vectors;
  D <- diag( obj$eig$values );
  
  Ysphere <- Y %*% E %*% sqrt( ginv( D ) ) %*% t(E);
  
  nns <- nnfind( from=obj$Ysphere, to=Ysphere );

  thescores <- nns$dist;
  
  res <- data.frame( score = thescores );

  return( res );
}
