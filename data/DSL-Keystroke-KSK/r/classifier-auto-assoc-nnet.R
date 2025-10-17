#####################################################################
# R code for the Auto-Associative Neural Net (login-type) detector
# (by Kevin Killourhy)
#####################################################################

library( AMORE );

## trainAutoAssocNNet
# Train an auto-associative neural net detector
##
# Build a novelty detector based on the auto-associative MLP algorithm
# described by Cho, Han, Han, and Kim (2000) and (in more detail) in
# Hwang and Cho (1999).  The each row of the training data matrix (Y)
# is treated as a data point in nrow(Y)-dimensional space.
##
# Y: a matrix wherein each row is a timing vector
##
# hcount: the number of hidden nodes.  Despite Hwang and Cho's
# assertion that "fewer hidden units than input or output units" is a
# "structural characteristic" of an AaMLP used for novelty detection,
# the Cho et al (2000) detector specifically used "the same number of
# hidden units as input units" so that is what we do by default.
##
# lrate: the learning rate for the neural network.  We default to 0.1
# / 10 because Cho et al used 0.1, but that diverges, possibly because
# his times were in milliseconds while ours are in seconds.  We played
# around with parameters for the Strong benchmark and got best results
# by reducing by a factor of 10.
##
# mom: the momentum of learning.  We default to 0.3 / 10 since Cho et
# al used 0.3, and we scaled the learning rate by a factor of 10.
##
# epochs: the number of epochs to train.  We default to 500 because
# that is what Cho et al used.  Note that elsewhere in the Cho et al
# paper, when they describe the detector in more abstract detail, they
# say that it is trained iteratively until the error term no longer
# decreases.
## 
# etype: the type of error estimate used.  We default to LMS (least
# mean squares) since mean squared error is used in equation (4) of
# Hwang and Cho.
##
# atype: the type of activation function used in the hidden nodes.
# We default to a sigmoid activation function because that is what is
# described by Cho et al:  f(z) = (1/(1+exp(-z))).
##
# otype: the type of activation function used in the output nodes.
# These default to linear because that's what happens in the network
# described by Cho et al.
##
# method: the method used to train the detector.  We use ADAPTgdwm
# because the method is either adaptive or batch gradient descent with
# momentum, so it will be either ADAPTgdwm or BATCHgdwm.
##
# rcount: the number of times to repeat the training (with a random
# set of initial weights) in order to find different local minima.
# The Cho et al paper simply says that "several" runs are used.  We
# use 5 as a default.
##
# Returns an (opaque) trained detector object

trainAutoAssocNNet <- function( Y,
                               hcount  = ncol(Y),
                               lrate   = 0.1 / 10,
                               mom     = 0.3 / 10,
                               epochs  = 500,
                               etype   = 'LMS',
                               atype   = 'sigmoid',
                               otype   = 'purelin',
                               method  = 'ADAPTgdwm',
                               rcount  = 5 ) {

  d <- ncol(Y);

  trainfunc <- list(ADAPTgd   = ADAPTgd.MLPnet,
                    ADAPTgdwm = ADAPTgdwm.MLPnet,
                    BATCHgd   = BATCHgd.MLPnet,
                    BATCHgdwm = BATCHgdwm.MLPnet)[[ method ]];
  stopifnot( ! is.null( trainfunc ) );

  errorfunc <- list(LMS  = error.LMS,
                    LMLS = error.LMLS,
                    TAO  = error.TAO)[[ etype ]];
  stopifnot( ! is.null( errorfunc ) );

  # For however many runs are specified, build and train a neural
  # net, and keep track of the one with the smallest error.

  net <- NA;
  err <- Inf;

  for( ridx in 1:rcount ) {

    # Begin by building a new neural network with the same number of
    # input and output nodes and hcount hidden nodes in a single
    # hidden layer.

    net1 <- newff( n.neurons            = c(d,hcount,d),
                  learning.rate.global  = lrate,
                  momentum.global       = mom,
                  error.criterium       = etype,
                  hidden.layer          = atype,
                  output.layer          = otype,
                  method                = method );

    # Then train the network for the desired number of epochs.  Give
    # the same data for input and output so that the learned behavior
    # is auto-associative, that is, the behavior is to reproduce the
    # input.

    net1 <- trainfunc( net1, Y, Y, epochs );

    Yp <- sim.MLPnet( net1, Y );
    stopifnot( ! any( is.na(Yp) ) );

    err1 <- errorfunc( list( Yp, Y, net1 ) );

    if( err1 < err ) {
      err <- err1;
      net <- net1;
    }
  }

  obj <- list( type = 'AutoAssocNNet',
              net = net,
              trainfunc = trainfunc,
              errorfunc = errorfunc );

  return( obj );
}

## classifyAutoAssocNNET
# Generate anomaly scores using a trained auto-associative
# neural-network detector
##
# Each test sample is run through the neural network to produce an
# output vector.  The error between that output vector and the input
# is calculated and used as the anomaly score.
##
# obj: a trained auto-assoc neural-net detector object
# Y: a matrix of test data wherein each row is a timing vector
##
# Returns a data frame with an anomaly score for each row of Y

classifyAutoAssocNNet <- function( obj, Y ) {

  net <- obj$net;
  Yp <- sim.MLPnet( net, Y );

  n <- nrow(Y);
  errorfunc <- obj$errorfunc;
  thescores <- sapply( 1:n, function(i) errorfunc( list(Yp[i,],Y[i,],net) ) );

  res <- data.frame( score = thescores );

  return( res );
}
