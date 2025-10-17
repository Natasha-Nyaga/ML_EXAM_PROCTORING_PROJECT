#####################################################################
# R code for a Outlier-Counting z-score (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( stats );

## trainOutlierCount
# Train a outlier-counting (z-score) detector
##
# This detector attempts to reproduce the behavior of the statistical
# detection component of Haider et al (2000).  To do so, the training
# function must track the mean and standard deviation of each feature.
##
# Y: a matrix wherein each row is a timing vector
# outalpha: the threshold z-score which outliers exceed
##
# Returns an (opaque) trained detector object

trainOutlierCount <- function( Y, outalpha=.05 ) {

  means <- colMeans( Y );
  sds <- apply( Y, 2, sd );

  obj <- list( type = 'OutlierCount',
              means = means,
              sds = sds,
              outalpha = outalpha );

  return( obj );
}

## classifyOutlierCount
# Generate anomaly scores using an outlier counting detector
##
# This function calculates the score.  It first standardizes the test
# data using the values from the training data.  Then, it counts the
# number of features that exceed an alpha-confidence-interval.
##
# obj: a trained SVM detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifyOutlierCount <- function( obj, Y ) {

  Yscale <- scale( Y, center=obj$means, scale=obj$sds );
  outcount <- rowSums( abs( Yscale ) > qnorm( 1-obj$outalpha/2 ) );

  res <- data.frame( score = outcount );
    
  return( res );
}
