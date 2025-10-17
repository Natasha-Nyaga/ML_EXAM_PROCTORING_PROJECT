#####################################################################
# R code for the SVM (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( kernlab );

## trainSVM
# Train a SVM detector
##
# The SVM anomaly detector was described by Yu and Cho (2003), who
# listed Scholkopf et al (2001) as the primary source.  Trains a
# one-class support-vector machine on the training data.  Requires the
# kernlab library.  The SVM does have a nu parameter designed to
# enable one to choose the target false-alarm rate.  We aim for 5%.
##
# Y: a matrix wherein each row is a timing vector
##
# Returns an (opaque) trained detector object

trainSVM <- function( Y ) {

  svm <- ksvm( Y, type = 'one-svc', nu=.05 );

  obj <- list( type = 'SVM',
              svm = svm );

  return( obj );
}

## classifySVM
# Generate anomaly scores using a trained SVM detector
##
# Each test sample is compared to the separator learned by the SVM.
# That (signed) distance is flipped to calculate the anomaly score so
# that non-self scores are higher/positive and self scores are
# lower/negative.
##
# obj: a trained SVM detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifySVM <- function( obj, Y ) {

  thescores <- -predict( obj$svm, Y, type='decision' );

  res <- data.frame( score = thescores );

  return( res );
}
