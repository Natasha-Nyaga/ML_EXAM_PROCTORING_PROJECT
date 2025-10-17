#####################################################################
# R code for a k-Means (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( stats );

## trainKMeans
# Train a k-Means detector
##
# While this detector is inspired by the Kang Hwang, and Cho (2007)
# k-Means detector, it is simpler.  Their detector scales the
# distances to each center by the average distance to those same
# centers.  Presumably that scaling was found to be beneficial, but in
# preliminary tests using the strong data, I found that a simple
# distance calculation after scaling all the variables to have unit
# variance produced better results.  By default k is set to 3.
##
# Y: a matrix wherein each row is a timing vector
# k: the number of clusters to train
# iter.max: the number of iterations to run
# nstart: the number of random restarts to run
# mindist: minimum distance since a point equal to a center causes
# a divide-by-zero
##
# Returns an (opaque) trained detector object

trainKMeans <- function( Y, 
                        k=3,
                        iter.max=20,
                        nstart=10,
                        mindist=1e-10 ) {

  means <- colMeans( Y );
  sds <- apply( Y, 2, sd );
  Yscale <- scale( Y, center=means, scale=sds );
  
  km <- kmeans( Yscale, centers=k, iter.max=iter.max, nstart=nstart );

  obj <- list( type = 'kMeans',
              means = means,
              sds = sds,
              km = km,
              k = k,
              mindist = mindist );

  return( obj );
}

## classifyKMeans
# Generate anomaly scores using a trained k-Means detector.  We find
# the distance to each centroid and then the centroid with the minimum
# distance.  The anomaly score is the Euclidean distance to this
# centroid.
##
# obj: a trained KMeans detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifyKMeans <- function( obj, Y ) {

  Yscale <- scale( Y, center=obj$means, scale=obj$sds );

  k <- obj$k;
  km <- obj$km;

  d <- ncol(Y);
  n <- nrow(Y);
  
  # Find the distance between the Ys and a centroid

  Ydists <- function( cs ) {
    Yctr <- sweep( Yscale, 2, cs );
    return( sqrt( rowSums( abs( Yctr )^2 ) ) );
  }
  
  # Find the distance from each point in Y to each center
  D <- sapply( 1:k, function(i) Ydists( km$centers[i,] ) );

  # Reorganize into a matrix since if n=1, sapply returns a vector
  D <- matrix( D, nrow=n, ncol=k );

  # Find the column with the minimum value for each row
  idxs <- apply( D, 1, which.min );

  # Extract the distance to the nearest centroid
  cdist <- D[ cbind( 1:n , idxs ) ];
  cdist <- pmax( cdist, obj$mindist );
  
  res <- data.frame( score = cdist );

  return( res );
}
