#############################################################################
# R Code for Evaluating Classifiers                                         #
# (Kevin Killourhy)                                                         #
#############################################################################

source( 'classifier-manhattan.R' );
source( 'classifier-scaled-manhattan.R' );
source( 'classifier-euclidean.R' );
source( 'classifier-mahalanobis.R' );
source( 'classifier-knn.R' );
source( 'classifier-mahalanobis-knn.R' );
source( 'classifier-svm.R' );
source( 'classifier-auto-assoc-nnet.R' );
source( 'classifier-kmeans.R' );
source( 'classifier-outliercount.R' );

datafiles <-
  list(Strong     = 'data-typing/strong.txt',
       Simple     = 'data-typing/simple.txt',
       Numeric    = 'data-typing/numeric.txt');

classifiers <-
  list(
       Euclidean = list(
         train     = trainEuclidean,
         classify  = classifyEuclidean ),
       Manhattan = list(
         train     = trainManhattan,
         classify  = classifyManhattan ),
       ScaledManhattan = list(
         train     = trainScaledManhattan,
         classify  = classifyScaledManhattan ),
       OutlierCount = list(
         train     = trainOutlierCount,
         classify  = classifyOutlierCount ),
       AutoAssocNNet = list(
         train     = trainAutoAssocNNet,
         classify  = classifyAutoAssocNNet ),
       Mahalanobis = list(
         train     = trainMahalanobis,
         classify  = classifyMahalanobis ),
       KNN = list(
         train     = trainKNN,
         classify  = classifyKNN ),
       MahalanobisKNN = list(
         train     = trainMahalanobisKNN,
         classify  = classifyMahalanobisKNN ),
       KMeans = list(
         train     = trainKMeans,
         classify  = classifyKMeans ),
       SVM = list(
         train     = trainSVM,
         classify  = classifySVM ));

featurepatterns <- list('Hold.DD.UD.Ret' = '^(H|DD|UD)',
                        'Hold.DD.Ret'    = '^(H|DD)',
                        'Hold.UD.Ret'    = '^(H|UD)',
                        'DD.UD.Ret'      = '^(DD|UD)',
                        'Hold.Ret'       = '^(H)',
                        'DD.Ret'         = '^(DD)',
                        'UD.Ret'         = '^(UD)',
                        'Hold.DD.UD'     = '^(H|DD|UD)(?!.*Return.*)',
                        'Hold.DD'        = '^(H|DD)(?!.*Return.*)',
                        'Hold.UD'        = '^(H|UD)(?!.*Return.*)',
                        'DD.UD'          = '^(DD|UD)(?!.*Return.*)',
                        'Hold'           = '^(H)(?!.*Return.*)',
                        'DD'             = '^(DD)(?!.*Return.*)',
                        'UD'             = '^(UD)(?!.*Return.*)');

########################################################################
# Miscellaneous Helper Functions
########################################################################

## ac
# as.character syntactic sugar
ac <- as.character;

## foldr
# This function implements a list fold starting from the right (end).
##   FUN = function( val0, listelement )
# must return something with the same type as val0.

foldr <- function( FUN, val0, thelist ) {
  n <- length( thelist );
  while( n > 0 ) {
    val0 <- FUN( val0, thelist[[n]] );
    n <- n-1;
  }
  return( val0 );
}

########################################################################
# Experiment Management Functions
########################################################################

## getdata
# This function retrieves a password timing table based either on its
# filename (file) or on a datafile name (name: e.g., Strong, Simple,
# or Numeric).  It uses the global variable to cache the data set so
# recurring lookups are cheap.

getdata.cache <- list();

getdata <- function( name, file = datafiles[[name]] ) {

  stopifnot( ! is.null(file) );
  stopifnot( length(name) == 1 );

  if( is.null( getdata.cache[[ file ]] ) ) {
    cat("Loading",file,"\n");
    X <- read.table( file, header=TRUE );
    X <- X[ order( X$subject, X$sessionIndex ), ];
    X$psr <- unlist( tapply( X$subject, X$subject, seq_along ) );
    X$totaltime <- rowSums( X[ , grep( '^(H|UD)\\.', names(X) ) ] );
    getdata.cache[[ file ]] <<- X;
  }

  X <- getdata.cache[[ file ]];
  
  return( X );
}

## getsubjects
# This function takes a list of data sets (typingtasks).  If
# do.intersect=TRUE, it returns a list of subjects that are in all of
# the data sets.  If do.intersect is false, it returns a list of
# subjects in any of the data sets.

getsubjects <- function( typingtasks, do.intersect=FALSE ) {
  subjs <- lapply( typingtasks, function(dname)
                  levels( getdata( dname )$subject ) );
  
  if( length( subjs ) == 1 ) return( subjs[[1]] );
  
  if( do.intersect ) {
    return( sort( foldr( intersect, subjs[[1]], subjs[-1] ) ) );
  } else {
    return( sort( unique( unlist( subjs ) ) ) );
  }
}

## expandsstag
# This function expands a typingtask name (typingtask), genuine user
# subject name (usersubject), and subject-set tag (sstag) into a list of
# subjects.  The specifics depend on the tag
## all: all the subjects in the data set (except usersubject)
## int-D1-...-Dn: all subjects in D1...Dn intersection (except usersubject)
## set-s1-s2-...-sn: subjects s1, s2, ..., sn (except usersubject)
# If no usersubject is provided (usersubject=NULL), then the genuine user is
# not subtracted from the set.  Being able to subtract the genuine
# user is a convenience when the tags are used to identify impostors
# for test data.

expandsstag <- function( typingtask, usersubject=NULL, sstag='all' ) {
  tagwords <- strsplit( sstag, '-' ) [[1]];

  if( tagwords[1] == 'all' && length(tagwords) == 1 ) {

    subjects <- getsubjects( typingtask );

  } else if( tagwords[1] == 'int' && length(tagwords) > 1 ) {

    subjects <- getsubjects( tagwords[-1], do.intersect=TRUE );

  } else if( tagwords[1] == 'set' && length(tagwords) > 1 ) {

    subjects <- tagwords[-1];

  } else {

    stop("Unable to parse sstag word",tagwords[1]);

  }

  if( ! is.null(usersubject) )
    subjects <- setdiff( subjects, usersubject );

  return( subjects );
}

## psrstr
# The per-subject repetitions to use to train and test the
# classifiers.  These functions just convert, say '1:5' into
# c(1,2,3,4,5).  The optional argument is effectively the environment
# for the string evaluation, so if T=c(1,100) then one could denote
# 101:200 by T[2]+1:100.

psrstr <- function( pstr, T=NULL ) eval( parse( text=pstr ) );

## get.Aname
# This function converts a row or rows of experiment design data frame
# (A) into a string or directory name for naming or saving/loading the
# data.  If the (file) is FALSE it returns a human-readable string.
# If TRUE, it returns the location of a cache file for evaluation data
# associated with the row.  If (namesep) is NA, the column name is not
# included in the string.  Otherwise, it is separated from the value
# by (namesep).

get.Aname <- function( A, file=FALSE, namesep='-', collapse='/', cachedir,
                      suffix='/cache.Rdata' ) {

  if( is.na( namesep ) ) {
    anames <- apply( A, 1, function(Ai) paste( Ai, collapse=collapse ) );
  } else {
    anames <- apply( A, 1, function(Ai)
                    paste( names(A), Ai, sep=namesep, collapse=collapse ) );
  }
  anames <- gsub( ' ', '', anames );
  anames <- gsub( ':', '-', anames );

  if( file )
    anames <- sprintf( '%s/%s%s', cachedir, anames, suffix );

  return( anames );
}

## cache.save
# This function takes a list of object names and saves those objects
# that exist to the specified cache file (file).  It's basically just
# a wrapper around the save function, but it creates nonexistent
# directory paths.  It also

cache.save <- function( vars, file, verbose=FALSE ) {
  thedir <- dirname( file );
  if( ! file.exists( thedir ) ) {
    stopifnot( dir.create( thedir, recursive=TRUE ) );
  }
  vars <- intersect( vars, ls( envir=parent.frame() ) );
  stopifnot( length( vars ) > 0 );
  save( list=vars, file=file, envir=parent.frame() );
  if( verbose ) cat("Saved",paste(vars,collapse=' '),"to",file,"\n");
}

############################################################################
# Classifier Evaluation Functions
############################################################################

## classifiereval
# Runs an evaluation trial with a given classifier, typingtask,
# genuine-user subject, amount of training data, and feature set.  The
# classifier is trained on the user's training data and then used to
# score all of the data.  These are saved in an appropriate cachefile
# for further analysis.

classifiereval <- function( Ai,
                           trainfun = classifiers[[ cname ]]$train,
                           classfun = classifiers[[ cname ]]$classify,
                           cname = ac( Ai$classifier ),
                           typingtask = ac(Ai$typingtask),
                           usersubject = ac(Ai$usersubject),
                           trainpsr = ac(Ai$trainpsr),
                           trainpsrs = psrstr( trainpsr ),
                           X = getdata( typingtask ),
                           featureset = ac(Ai$featureset),
                           force = FALSE,
                           quiet = FALSE,
                           cachedir = NA,
                           cachefile = get.Aname( Ai,
                             file=TRUE,
                             cachedir=cachedir ) ) {

  stopifnot( nrow(Ai) == 1 );

  if( ! quiet ) {
    print( Ai );
  }
  
  if( ( ! force ) &&
     ( ! is.na(cachedir) ) &&
     file.exists( cachefile ) &&
     file.info(cachefile)$size > 0 ) {

    load( file=cachefile );
    return( invisible( theresults ) );
    
  }
  
  X1 <- subset( X, subject == usersubject & psr %in% trainpsrs );

  stopifnot( ! is.null( featurepatterns[[ featureset ]] ) );
  featurepattern <- featurepatterns[[ featureset ]];
  
  Y1 <- as.matrix( X1[ , grep( featurepattern, names(X), perl=TRUE ) ] );
  Y <- as.matrix( X[ , grep( featurepattern, names(X), perl=TRUE ) ] );

  repeat {
  
    obj <- trainfun( Y1 );

    theresults <- classfun( obj, Y );

    if( any( is.na( theresults$score ) ) ) {
      cat("Repeating train/class sequence (NAs found)\n");
    } else {
      break;
    }
  }

  gc();

  if( ! is.na(cachedir) ) {
    cache.save( 'theresults', file=cachefile );
  }

  return( invisible( theresults ) );
}

## getresults
# This function takes a design matrix that has been evaluated,
# retrieves the specified evaluation data, and identifies those scores
# corresponding to the specified genuine and impostor test sets.  The
# results are matched with both the sample's meta-data (e.g., the
# subject, session, and repetition), and also the row of the design
# matrix and returned for analysis.

getresults <- function( Ai,
                       stestpsr,
                       ntesttag,
                       ntestpsr,
                       cachedir ) {

  # Retrieve the data
  
  X <- getdata( ac( Ai$typingtask ) );
  
  load( get.Aname( Ai, file=TRUE, cachedir=cachedir ) );
  stopifnot( exists( 'theresults', where=environment(), inherits=FALSE ) );

  # Enumerate all combinations of stestpsr, ntesttag, and ntestpsr

  Bi <- expand.grid( stestpsrs = stestpsr,
                    ntesttags  = ntesttag,
                    ntestpsrs  = ntestpsr );

  Bi <- adply( Bi, 1, function( Bij ) {

    ntesttag <- ac(Bij$ntesttag);
    stestpsr <- ac(Bij$stestpsr);
    ntestpsr <- ac(Bij$ntestpsr);

    nsubjects <- expandsstag( ac(Ai$typingtask), ac(Ai$usersubject), ntesttag );

    T <- range( psrstr( ac( Ai$trainpsr ) ) );
  
    smask <- ( ac( X$subject ) == ac( Ai$usersubject ) &
              X$psr %in% psrstr( stestpsr, T=T ) );
    nmask <- ( X$subject %in% nsubjects &
              X$psr %in% psrstr( ntestpsr, T=T ) );
    allmask <- smask | nmask;

    Bij <- data.frame( Ai, Bij,
                      X[ allmask, - grep( '^(H|DD|UD)\\.', names(X) ) ],
                      score = theresults$score[ allmask ],
                      row.names = NULL );

    return( Bij );
  } );

  Bi <- transform( Bi, subject = droplevels( subject ) );
  Bi <- transform( Bi, ground = ( ac(usersubject) != ac(subject) ) );

  return( Bi );
}

## modifythreshold
# This function takes a data.frame of evaluation results (B), and
# modifies (or creates) the label and error columns, as though a
# different threshold were used.  The threshold is specified in terms
# of a target average false alarm rate for a particular grouping
# variable list (passed to ddply), so for instance, to have a
# per-classifier threshold set, you would use .(classifier) and to
# have a per-classifier/user threshold, you would use
# .(classifier,usersubject).

modifythreshold <- function( B, group, farate, quiet=FALSE ) {

  prog <- if( quiet ) 'none' else 'text';
  
  B2 <- ddply( B, group, function( Bi ) {
    Biuser <- subset( Bi, ac(usersubject) == ac(subject) );
    newthresh <- quantile( Biuser$score, 1-farate );
    Bi$label <- Bi$score > newthresh;
    Bi$thresh <- newthresh;
    return( Bi );
  }, .progress=prog );
  B2$error <- ( B2$label != B2$ground );
  return( B2 );
}

