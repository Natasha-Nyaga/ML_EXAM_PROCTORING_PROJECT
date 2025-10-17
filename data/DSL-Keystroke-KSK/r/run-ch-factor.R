############################################################################
# R Code for Chapter: Screening Keystroke-Dynamics Factors                 #
# (Kevin Killourhy)                                                        #
############################################################################

source( 'r/analysis.R', chdir=TRUE );

########################################################################
# Configuration Variables
########################################################################

evalcache <- 'tmp-factor-eval';
seedfile <- '.ch-factor-seed.Rdata';

imagedir <- 'tmp-factor-images';
resultsfile <- sprintf( '%s/Dimp.txt', imagedir );
validfilea <- sprintf( '%s/Dimpva.txt', imagedir );
validfileb <- sprintf( '%s/Dimpvb.txt', imagedir );
validfilec <- sprintf( '%s/Dimpvc.txt', imagedir );
cacheformat <- sprintf( '%s/Dimp%%s-%%04d.Rdata', imagedir );

contrastfile1 <- 'data-contrasts/ch-factor-contrast-1.txt';
contrastfile2 <- 'data-contrasts/ch-factor-contrast-2.txt';

topclassifiers <- c('ScaledManhattan','MahalanobisKNN','SVM');

asubjects <- sprintf('s%03d',
                     c(16, 17, 18, 19, 20, 21, 24, 25, 26, 27,
                       28, 29, 30, 31, 32, 33, 50, 51, 52, 53,
                       54, 55, 56, 57, 58, 59));

asubjecttag <- sprintf('set-%s', paste(asubjects,collapse='-'));

vasubjects <- sprintf('s%03d',
                      c( 2,  3,  4,  5,  7,  8, 10, 11, 12, 13,
                        15, 22, 34, 35));
vasubjecttag <- sprintf('set-%s', paste(vasubjects,collapse='-'));

vbsubjects <- sprintf('s%03d',
                      c( 2,  3,  4,  5,  7,  8, 10, 11, 12, 13,
                        15, 23));
vbsubjecttag <- sprintf('set-%s', paste(vbsubjects,collapse='-'));

vcsubjects <- sprintf('s%03d',
                      c(23, 34, 35, 36, 37, 38, 39, 40, 41, 42,
                        43, 44, 46, 47));
vcsubjecttag <- sprintf('set-%s', paste(vcsubjects,collapse='-'));

############################################################################
# Experiment
############################################################################

## get.designmatrix
# This function generates the design matrix for evaluating the various
# classifiers.

get.designmatrix <- function( typingtask = c('Strong','Simple','Numeric'),
                             usersubjects = asubjects ) {

  train.window <- c( 5, 25, 50, 100 );
  train.start <- seq( 1, 196, by = 5 );

  A <- expand.grid(typingtask   = typingtask,
                   featuresets  = names( featurepatterns ),
                   classifier   = topclassifiers,
                   usersubject  = usersubjects,
                   train.window = train.window,
                   train.start  = train.start );

    # For the fixed update window (case 1), we have that
    #     test.end1 == (train.start1 + train.window1 - 1) + 100
    #               == train.start1 + train.window1 + 99
    # For the sliding update window (case 2), we have that
    #     test.end2 == (train.start2 + train.window2 - 1) + 5
    #               == train.start2 + train.window2 + 4
    # We only want the sliding update to test data from the fixed update, so
    #     test.end2 <= test.end1
    #       ==> train.start2 + train.window2 + 4
    #             <= train.start1 + train.window1 + 99
    #       ==> train.start2 + train.window2 <= 1 + train.window1 + 95
    #       ==> train.start2 + train.window2 <= train.window1 + 96
    # Since we intend to compare cases 1 and 2 with equal train.windows,
    #     train.window1 == train.window2
    #       ==> train.start2 + train.window1 <= train.window1 + 96
    #       ==> train.start2 <= 96
    # Since it is also true that
    #     train.start1 == 1 <= 96
    # We can use this to prune the number of cases we run

  A <- subset( A, train.start <= 96 );

  A <- transform( A,
                 trainpsr = sprintf('%d:%d',
                   train.start,
                   train.start + train.window - 1) );

  A$train.start <- NULL;
  A$train.window <- NULL;
  
  return( A );
}

## clear
# Remove all the intermediary data (i.e., the cache of evaluation
# results and the saved image of important data objects).
##
# resetseed: if true, remove the seed file as well
##
# Returns nothing useful

clear <- function( resetseed=FALSE ) {

  # Remove temporary directories
  unlink( evalcache, recursive=TRUE );
  
  # Reset the random seed
  if( resetseed ) {
    unlink( seedfile );
    runif(1);
    seed <- .Random.seed;
    save( seed, file=seedfile );
    cat( "Wrote", seedfile,"\n" );
  }

}

## evalhelper
# Takes a subset of the design matrix and processes it fully: conduct
# each evaluation, retrieve the results, tune the classifiers,
# consolidate the runs corresponding to each updating result, relabel
# the columns of the data frame, aggregate the results into
# per-user/impostor miss rates, and return the resulting data set.

evalhelper <- function( Ai, ntesttag=asubjecttag ) {
  
  stopifnot( nrow(Ai) == 80 );

  # Conduct the evaluations

  a_ply( Ai, 1, classifiereval, cachedir=evalcache );

  # Retrieve the no-updating outcomes
                  
  Ai1 <- subset( Ai, grepl( '1:', Ai$trainpsr ) );
                   
  Bi1 <- adply( Ai1, 1, getresults,
               stestpsr = 'T[2]+(1:100)',
               ntestpsr = c('1:50','151:200'),
               ntesttag = ntesttag,
               cachedir = evalcache );

  # Retrieve the updating outcomes
                  
  Bi2 <- adply( Ai, 1, getresults,
               stestpsr = 'T[2]+(1:5)',
               ntestpsr = c('1:50','151:200'),
               ntesttag = ntesttag,
               cachedir = evalcache );

  # Combine with an updating column
                  
  Bi <- rbind(data.frame( Bi1, updating = FALSE ),
              data.frame( Bi2, updating = TRUE ));

  # Convert the trainpsr range to an amount of training

  psrsplits <- strsplit( ac( Bi$trainpsr ), ':' );
  tstart <- as.numeric( sapply( psrsplits, '[[', 1 ) );
  tend <- as.numeric( sapply( psrsplits, '[[', 2 ) );
  Bi$trainamt <- factor( tend - tstart + 1 );

  # Modify the threshold for each condition to have 5% fa rate

  Bi <- modifythreshold( Bi, .(trainamt,updating), farate=.05, quiet=TRUE );

  # Focus on the misses
                  
  Bi <- subset( Bi, usersubject != subject );

  # Calculate the miss rates
  
  Di <- ddply( Bi, .(trainamt,updating,subject,ntestpsrs),
              function( Bij ) {
                data.frame( error = mean( Bij$error ) );
              } );

  # Find the constants of the design matrix

  anames <- names( Ai )[ sapply( names(Ai), function(aname) {
    length( unique( Ai[[aname]] ) ) == 1;
  } ) ];

  Di <- data.frame( unique( Ai[ , anames ] ), Di, row.names=NULL );
  
  return( Di );
}

# Do the evaluation in a cached fashion, in case of memory exhaustion
# issues.  Postprocess the results into the appropriate columns.

doevaluation <- function( A, resultsfile, tag='', ... ) {

  if( ! file.exists( resultsfile ) ) {

    # Create the miss results data frame for fixed false-alarm rate

    As <- dlply( A, .(typingtask,featuresets,classifier,usersubject), identity,
                .progress = 'text' );

    if( ! file.exists( imagedir ) ) dir.create( imagedir );
    
    l_ply( 1:length(As), function( i ) {
      imagefilei <- sprintf( cacheformat, tag, i );
      if( ! file.exists( imagefilei ) ) {
        Dimpi <- evalhelper( As[[i]], ... );
        save( Dimpi, file = imagefilei );
      }
    }, .progress='text' );

    Dimp <- ldply( 1:length(As), function( i ) {
      imagefilei <- sprintf( cacheformat, tag, i );
      stopifnot( file.exists( imagefilei ) );
      load( imagefilei );
      return( Dimpi );
    }, .progress='text' );

    write.table( Dimp, file=resultsfile, row.names=FALSE, col.names=TRUE,
                quote = FALSE );

  }

  # Load the miss results from the cache in the image file
    
  Dimp <- read.table( resultsfile, header=TRUE );
  Dimp$trainamt <- factor( Dimp$trainamt );
    
  # Ensure the factors are ordered as per the experimental design

  Dimp <- aligndataframe( Dimp, A );
  
  # Reformulate the feature sets into binary factors
  
  featurevars <- sort(unique(unlist(strsplit(levels(Dimp$featuresets),'\\.'))));
  for( featurevar in featurevars ) {
    fmask <- grepl( featurevar, Dimp$featuresets );
    fvals <- factor( ifelse( fmask, '+', '-' ), levels=c('+','-') );
    Dimp[[ featurevar ]] <- fvals;
  }

  # Apply the variance-stabilizing transformation
  
  Dimp <- transform( Dimp, VSerror = vsscore( error ) );

  # Convert factors to have meaningful names

  Dimp <- transform( Dimp, 
                    typingtask = factor( typingtask, 
                      levels = c('Strong','Simple','Numeric') ),
                    updating = factor( ifelse( updating,
                      'Sliding',
                      'None' ),
                      levels = c('None', 'Sliding') ),
                    impfam = factor( ifelse( ntestpsrs == '1:50',
                      'Low',
                      'High' ),
                      levels = c('Low','High') ) );
  
  return( Dimp );
}

run <- function() {

  # Clear cache (and possibly set new random seed)
  # clear( resetseed = TRUE );
  # clear();
  
  # Run the evaluations, compile the error rates, stabilize the variance

  load( seedfile );
  .Random.seed <<- seed;

  ######################################################################
  # Evaluation Procedure
  ######################################################################

  # Construct the design matrix
  
  A <- get.designmatrix();

  # Do the evaluation in piecemeal, caching fashion
  
  Dimp <- doevaluation( A, resultsfile, tag='', ntesttag = asubjecttag );
  
  # Tabulate the number of subjects in each typing task

  tbl <- data.frame(Strong = c(length(asubjects), length(vasubjects)),
                    Simple = c(length(asubjects), length(vbsubjects)),
                    Numeric = c(length(asubjects), length(vcsubjects)),
                    row.names = c(
                      'Evaluation Pool',
                      'Validation Pool'));

  latex.data.frame( tbl, file = 'figs/table-6-1.tex' );

  # Aggregate the miss rates for creating trellis-plot slices

  factorlist <- c('classifier','typingtask','Hold','DD','UD','Ret','trainamt',
                  'updating','impfam');

  Dagg <- ddply( Dimp, factorlist,
                function( Di ) c( error = mean( Di$error ) ) );

  # Plot classifier error rates for each amount of training and task

  trellis.device( file = 'figs/figure-6-1.eps',
                 theme = theme, height = 3, width = 8 );
  print( xyplot( error ~ trainamt | typingtask,
                ddply( Dagg, .(trainamt,typingtask,classifier),
                      function(Di) c(error=mean(Di[['error']])) ),
                group = classifier,
                layout = c(3,1),
                type = 'b',
                xlab = 'Training Amount',
                ylab = 'Average Miss Rate',
                auto.key = list( space='right' ),
                scales = list( rot=c(90,0), alternating=FALSE ) ) );
  dev.off();
  
  # Plot error rates for each combination of timing features

  trellis.device( file = 'figs/figure-6-2.eps', 
                 theme = theme, height = 8, width = 8 );
  print( xyplot( error ~ trainamt | Hold:DD:UD:Ret,
                ddply( Dagg, .(trainamt,Hold,DD,UD,Ret,classifier),
                      function(Di) c(error=mean(Di[['error']])) ),
                group = classifier,
                layout = c(2,7),
                type = 'b',
                strip = function(...)
                strip.default( ..., strip.names=c(TRUE,TRUE)),
                xlab = 'Training Amount',
                ylab = 'Average Miss Rate',
                auto.key = list( space='right' ),
                scales = list( rot=c(90,0), alternating=FALSE ) ) );
  graphics.off();

  # Plot error rates by updating strategy

  trellis.device( file = 'figs/figure-6-3.eps',
                 theme = theme, height = 3, width = 7 );
  print( xyplot( error ~ trainamt | updating,
                ddply( Dagg, .(trainamt,updating,classifier),
                      function(Di) c( error=mean(Di[['error']] ) ) ),
                group = classifier,
                layout = c(2,1),
                type = 'b',
                strip = function(...)
                strip.default( ..., strip.names=c(TRUE,TRUE)),
                xlab = 'Training Amount',
                ylab = 'Average Miss Rate',
                auto.key = list( space='right' ),
                scales = list( rot=c(90,0), alternating=FALSE ) ) );
  graphics.off();
  
  # Plot error rate by impostor familiarity level

  trellis.device( file = 'figs/figure-6-4.eps',
                 theme = theme, height = 3, width = 7 );
  print( xyplot( error ~ trainamt | impfam,
                ddply( Dagg, .(trainamt,impfam,classifier),
                      function(Di) c( error=mean(Di[['error']] ) ) ),
                group = classifier,
                layout = c(2,1),
                type = 'b',
                strip = function(...)
                strip.default( ..., strip.names=c(TRUE,TRUE)),
                xlab = 'Training Amount',
                ylab = 'Average Miss Rate',
                auto.key = list( space='right' ),
                scales = list( rot=c(90,0), alternating=FALSE ) ) );
  graphics.off();

  ######################################################################
  # Statistical Analysis #1: Feature sets
  ######################################################################

  # Slice out the subset of the data used in this analysis
  
  Dimp1 <- subset( Dimp, trainamt == '100'
                  & updating == 'None'
                  & ntestpsrs == '1:50' );

  # Run stepwise model selection on the family of formulas
  
  Mmiss1 <- stepfitlmers( VSerror ~ 1 + (1|usersubject) + (1|subject),
                         VSerror ~ ( classifier
                                    * typingtask
                                    * ( Hold + DD + UD )^2
                                    * Ret
                                    + (1|usersubject) + (1|subject) ),
                         Dimp1,
                         REML=FALSE,
                         verbose=TRUE );

  # Find the best miss model and fit with REML

  idx <- which.min( Mmiss1$bic );
  formulamiss1 <- formula( ac(Mmiss1$model[ idx ]) );
  mmiss1 <- fitlmer( formulamiss1, Dimp1, REML=TRUE );
  mmissvc1 <- VarCorr( mmiss1 );

  # Extract and tabulate the parameter estimates

  tabulateparams( mmiss1, file='figs/table-6-3.tex', center=FALSE );
  
  # Read in the contrasts written manually to file

  E1 <- read.table('data-contrasts/ch-factor-contrast-1.txt', header=TRUE);
  K1 <- createcontrastmatrix( E1, sep=' vs.~' );

  mmissht1 <- glht( mmiss1, as.matrix(K1) );
  mmisshtsum1 <- summary( mmissht1 );
  
  # Format the test results for printing

  tbl <- with( mmisshtsum1[['test']], 
              data.frame('effect'  = coefficients,
                         'stderr'  = sigma,
                         't.stat'  = tstat,
                         'p.value' = pvalues ) );

  tbl[['p.value']] <- ifelse( tbl[['p.value']] < .0001, 
                             '<.0001', 
                             sprintf('%.4f', tbl[['p.value']]) );

  # Print the hypothesis-testing table
  
  latex.data.frame( tbl, file = 'figs/table-6-4.tex',
                   collines = ncol(tbl)-1,
                   tablesym = 'r',
                   colformats.num = '%.3f',
                   quiet = TRUE );

  ######################################################################
  # Statistical Analysis #2: Many-factor analysis
  ######################################################################

  # Slice out the subset of data used in this analysis
  
  Dimp2 <- subset( Dimp, featuresets == 'Hold.UD.Ret' );
    
  # Run stepwise model selection on the family of formulas
  
  Mmiss2 <- stepfitlmers( VSerror ~ 1 + (1|usersubject) + (1|subject),
                         VSerror ~ ( classifier
                                    * typingtask
                                    * trainamt
                                    * updating
                                    * impfam
                                    + (1|usersubject) + (1|subject) ),
                         Dimp2,
                         REML=FALSE,
                         verbose=TRUE );

  # Find the best miss model and fit with REML

  idx <- which.min( Mmiss2$bic );
  formulamiss2 <- formula( ac(Mmiss2$model[ idx ]) );
  mmiss2 <- fitlmer( formulamiss2, Dimp2, REML=TRUE );
  mmissvc2 <- VarCorr( mmiss2 );

  # Extract and tabulate the parameter estimates
  
  tabulateparams( mmiss2, file = 'figs/table-6-6.tex', center=FALSE );

  # Read in the contrasts written manually to file

  K2 <- read.table('data-contrasts/ch-factor-contrast-2.txt', header=TRUE);

  mmissht2 <- glht( mmiss2, as.matrix(K2) );
  mmisshtsum2 <- summary( mmissht2 );
  
  # Format the test results for printing

  tbl <- with( mmisshtsum2[['test']], 
              data.frame('effect'  = coefficients,
                         'stderr'  = sigma,
                         't.stat'  = tstat,
                         'p.value' = pvalues ) );

  tbl[['p.value']] <- ifelse( tbl[['p.value']] < .0001, 
                             '<.0001', 
                             sprintf('%.4f', tbl[['p.value']]) );

  # Print the subset of test results for amount of training

  tbl1 <- tbl[ 1:27, ];

  tbl1 <- cbind(c('Strong',rep('',8),'Simple',rep('',8),'Numeric',rep('',8)),
                rep(c('ScaledManhattan','','',
                      'MahalanobisKNN','','',
                      'SVM','',''),3),
                rep(c('5--25','25--50','50--100'),9),
                tbl1);

  latex.data.frame( tbl1, file = 'figs/table-6-7.tex',
                   rownames = NULL,
                   collines = c(3,ncol(tbl1)-1),
                   rowlines = seq(3,nrow(tbl1),by=3),
                   colnames = c('typingtask','classifier','trainamt',
                     'effect','stderr','t-stat','p-value'),
                   tablesym = 'r',
                   colformats.num = '%.3f' );

  # Print the subset of test results for impostor familiarity

  tbl2 <- tbl[ 28:36, ];

  tbl2 <- cbind(c('Strong','','','Simple','','','Number','',''),
                rep(c('ScaledManhattan','MahalanobisKNN','SVM'),3),
                tbl2);

  latex.data.frame( tbl2, file = 'figs/table-6-8.tex',
                   rownames = NULL,
                   colnames = c('typingtask','classifier',
                     'effect','stderr','t-stat','p-value'),
                   collines = c(2,ncol(tbl2)-1),
                   rowlines = seq(3,nrow(tbl1),by=3),
                   colformats.num = '%.3f',
                   tablesym = 'r' );

  # Print the subset of test results for updating strategy

  tbl3 <- tbl[ 37:42, ];

  tbl3 <- cbind(c('ScaledManhattan','MahalanobisKNN','SVM',
                  'typingtask','Strong','Simple','Number'),
                rbind( tbl3[1:3,], NA, tbl3[4:6,] ));

  latex.data.frame( tbl3, file = 'figs/table-6-9.tex',
                   collines = c(1,ncol(tbl3)-1),
                   rownames = NULL,
                   rowlines = c(3,4,7),
                   na.sym = '',
                   colnames = c('classifier',
                     'effect','stderr','t-stat','p-value'),
                   tablesym = 'r',
                   colformats.num = '%.3f' );
  
  ######################################################################
  # Validation
  ######################################################################

  # Construct the design matrices for the secondary evaluations
  
  Ava <- get.designmatrix( typingtask='Strong', usersubjects=vasubjects );
  Avb <- get.designmatrix( typingtask='Simple', usersubjects=vbsubjects );
  Avc <- get.designmatrix( typingtask='Numeric', usersubjects=vcsubjects );

  # Do the evaluations in piecemeal, caching fashion, and combine

  Dimpva <- doevaluation( Ava, validfilea, tag='va', ntesttag=vasubjecttag );
  Dimpvb <- doevaluation( Avb, validfileb, tag='vb', ntesttag=vbsubjecttag );
  Dimpvc <- doevaluation( Avc, validfilec, tag='vc', ntesttag=vcsubjecttag );

  Dimpv <- rbind( Dimpva, Dimpvb, Dimpvc );
    
  ######################################################################
  # Validation #1: Feature sets
  ######################################################################

  # Slice out the subset of data used in this validation

  Dimpv1 <- subset( Dimpv, trainamt == '100'
                   & updating == 'None'
                   & impfam == 'Low' );
  
  # Estimate confidence intervals

  mci <- meanvarests( VSerror ~ ( classifier + typingtask + Hold + DD + UD +
                                 classifier:typingtask + Hold:DD + Hold:UD + 
                                 DD:UD + classifier:Hold + classifier:DD +
                                 classifier:UD + typingtask:Hold +
                                 typingtask:DD + typingtask:UD +
                                 classifier:Hold:DD + classifier:Hold:UD +
                                 classifier:DD:UD + typingtask:Hold:DD +
                                 typingtask:Hold:UD + typingtask:DD:UD +
                                 classifier:typingtask:Hold ),
                     Dimp1, mmiss1, 'usersubject' );
  
  mmissuserci1 <- transform( mci, 
                            oest = vsinverse(est),
                            olb = vsinverse(lb),
                            oub = vsinverse(ub) );

  # Create the per-user/impostor prediction plot

  Dm <- Dimpv1[ , c('classifier','typingtask','Hold','DD','UD','usersubject',
                    'error') ];
  names( Dm )[ which( names( Dm ) == 'error' ) ] <- 'value';
  Dtbl <- cast( Dm, classifier+typingtask+Hold+DD+UD ~ usersubject,
               fun.aggregate = mean );

  mmissuserci1 <- aligndataframe( mmissuserci1, mmissuserci1 );
  Dtbl <- aligndataframe( Dtbl, mmissuserci1 );
  
  trellis.device( file = 'figs/figure-6-6.eps', onefile = FALSE,
                 theme = theme, height = 11, width = 9 );

  print( xyplotpint( oest ~ classifier | typingtask*(Hold:DD:UD),
                    mmissuserci1,
                    Dtbl,
                    layout = c(3,7),
                    strip = function(...)
             strip.default( ..., strip.names=c(TRUE,TRUE)),
             xlab = '',
             ylab = 'Miss Rate',
             scales = list( rot=c(90,0), alternating=FALSE ) ) );

  graphics.off();

  # Create the QQ-plots

  makeqqplots( VSerror ~ ( classifier + typingtask + Hold + DD + UD +
                          classifier:typingtask + Hold:DD + Hold:UD + 
                          DD:UD + classifier:Hold + classifier:DD +
                          classifier:UD + typingtask:Hold + typingtask:DD +
                          typingtask:UD + classifier:Hold:DD + 
                          classifier:Hold:UD + classifier:DD:UD +
                          typingtask:Hold:DD + typingtask:Hold:UD +
                          typingtask:DD:UD + classifier:typingtask:Hold ),
              Dimpv1, mmiss1,
              plotfmt = 'figs/figure-6-7-%s.eps' );

 
  ######################################################################
  # Validation #2: Many-factor analysis
  ######################################################################

  # Slice out the subset of data used in this validation

  Dimpv2 <- subset( Dimpv, featuresets == 'Hold.UD.Ret' );

  # Estimate confidence intervals
  
  mci <- meanvarests( VSerror ~ ( classifier + typingtask + trainamt +
                                 updating + impfam + classifier:typingtask +
                                 classifier:trainamt + typingtask:trainamt +
                                 classifier:updating + typingtask:updating +
                                 classifier:impfam + typingtask:impfam + 
                                 classifier:typingtask:trainamt +
                                 classifier:typingtask:impfam ),
                     Dimp2, mmiss2, 'usersubject' );

  mmissuserci2 <- transform( mci, 
                            oest = vsinverse(est),
                            olb = vsinverse(lb),
                            oub = vsinverse(ub) );

  # Create the per-user/impostor prediction plot

  Dm <- Dimpv2[ , c('classifier','typingtask','trainamt','updating','impfam',
                    'usersubject','error') ];
  names( Dm )[ which( names( Dm ) == 'error' ) ] <- 'value';
  Dtbl <- cast( Dm, classifier+typingtask+trainamt+updating+impfam ~
               usersubject, fun.aggregate = mean );

  mmissuserci2 <- aligndataframe( mmissuserci2, mmissuserci2 );
  Dtbl <- aligndataframe( Dtbl, mmissuserci2 );
  
  trellis.device( file = 'figs/figure-6-9+10-%d.eps', onefile = FALSE,
                 theme = theme, height = 11, width = 9 );

  print( xyplotpint( oest ~ classifier | trainamt*typingtask*updating*impfam,
                    mmissuserci2,
                    Dtbl,
                    layout = c(4,6),
                    between = list( x=0, y=c(0,0,1,0,0) ),
                    strip = function(...)
                    strip.default( ..., strip.names=c(TRUE,TRUE)),
                    xlab = '',
                    ylab = 'Miss Rate',
                    scales = list( rot=c(90,0), alternating=FALSE ) ) );

  graphics.off();

  # Create the QQ-plots

  makeqqplots( VSerror ~ ( classifier + typingtask + trainamt + updating +
                          impfam + classifier:typingtask + 
                          classifier:trainamt + typingtask:trainamt +
                          classifier:updating + typingtask:updating +
                          classifier:impfam + typingtask:impfam + 
                          classifier:typingtask:trainamt +
                          classifier:typingtask:impfam ),
              Dimpv2, mmiss2,
              plotfmt = 'figs/figure-6-11-%s.eps' );


}

print( run );
