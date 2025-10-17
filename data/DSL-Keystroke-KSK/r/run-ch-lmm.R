############################################################################
# R Code for Chapter: Linear Mixed-Effects Models (LMMs)                   #
# (Kevin Killourhy)                                                        #
############################################################################

source( 'r/analysis.R', chdir=TRUE );

########################################################################
# Configuration Variables
########################################################################

evalcache <- 'tmp-lmm-eval';
seedfile <- '.ch-lmm-seed.Rdata';

IMAGEVARS <- c('Dimp','mmiss1','mmiss2','mmissci','Dvimp');

############################################################################
# Experiment
############################################################################

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

## run
# Main function to reproduce the tables and figures of results

run <- function() {

  # Clear cache (and possibly set new random seed)
  # clear( resetseed = TRUE );
  
  clear();
  
  # Construct the data set for the evaluation

  load( seedfile );
  .Random.seed <<- seed;
  
  ######################################################################
  # Evaluation Procedure
  ######################################################################

  # Construct the design matrix
  
  subjsubset <- sprintf('s%03d',c(10,13,7,8,11,5,3,2,4,12));
  
  A <- expand.grid( typingtask = 'Strong',
                   classifier  = c('SVM','ScaledManhattan','MahalanobisKNN'),
                   featureset  = c('Hold','DD'),
                   usersubject = subjsubset,
                   trainpsr    = '1:100' );

  # Run the evaluation
  
  a_ply( A, 1, classifiereval, cachedir=evalcache );

  ntesttag <- sprintf( 'set-%s', paste( subjsubset, collapse='-' ) );
  B <- adply( A, 1, getresults,
             stestpsr = 'T[2]+(1:100)',
             ntestpsr = '1:50',
             ntesttag = ntesttag, cachedir=evalcache );

  # Tune each classifier for a 5% false-alarm rate
  
  B <- modifythreshold( B, .(featureset,classifier), farate=.05 );

  # Calculate the per-user/impostor miss rates
  
  Dimp <- ddply( subset( B, usersubject != subject ),
                .(featureset,classifier,usersubject,subject),
                function( Bi )
                data.frame( error = mean( Bi$error ) ) );
  
  # Perform the variance-stabilizing transformation
  
  Dimp <- transform( Dimp, VSerror = vsscore( error ) );

  # Reorder the subjects from least to greatest VS score
  
  subjorder <- levels( reorder( Dimp$usersubject, Dimp$VSerror ) );

  # Rename the subjects (since this is just illustrative)
  
  Dimp <- transform( Dimp,
                    classifier = factor(classifier,labels=c('A','B','C')),
                    featureset = factor(featureset,levels=c('Hold','DD')),
                    usersubject = factor(usersubject,
                      levels = subjorder,
                      labels = sprintf('s%03d', 1:10)),
                    subject = factor(subject,
                      levels = subjorder,
                      labels = sprintf('s%03d', 1:10)) );

  # Print the table of results to sanity check it
  
  print( ddply(Dimp, .(featureset,classifier), function(Di)
               data.frame(nobs    = nrow( Di ),
                          error   = mean( Di$error ),
                          VSerror = mean( Di$VSerror ) ) ) );

  # Generate the trellis plot of evaluation results (Figure 3.1)

  X <- Dimp;
  X[['usersubject']] <- reorder( X[['usersubject']], X[['VSerror']], mean );

  trellis.device( file = 'figs/figure-3-1.eps',
                 theme = theme, height=5, width=8 );

  print( dotplot( 100*error ~ usersubject | featureset * classifier, X,
                 group = subject,
                 layout = c(2,3),
                 xlab = 'Genuine-User Subject',
                 ylab = 'Miss Rate (%)',
                 strip = function(...) 
                 strip.default( ..., strip.names=c(TRUE,TRUE) ),
                 scales = list( rot = c(90,0), alternating = FALSE ),
                 auto.key = list( 
                   space = 'right',
                   title = 'Impostor\nSubject',
                   cex.title = 1 ) ) );
  graphics.off();

  ######################################################################
  # Statistical Analysis
  ######################################################################

  # Fit the first LMM
  
  mmiss1 <- fitlmer( VSerror ~ ( classifier + featureset
                                + (1|usersubject) + (1|subject) ),
                    Dimp, REML = TRUE );

  # Extract and tabulate the parameter estimates

  tabulateparams( mmiss1, file='figs/table-3-1.tex', center=FALSE );

  # Fit the second LMM

  mmiss2 <- fitlmer( VSerror ~ ( classifier * featureset
                                + (1|usersubject) + (1|subject) ),
                    Dimp, REML = TRUE );

  # Extract and tabulate the parameter estimates

  tabulateparams( mmiss2, file='figs/table-3-2.tex', center=FALSE );

  # Compare the two models' BIC scores

  mmiss1ml <- fitlmer( VSerror ~ ( classifier + featureset
                                  + (1|usersubject) + (1|subject) ),
                      Dimp, REML = FALSE );

  mmiss2ml <- fitlmer( VSerror ~ ( classifier * featureset
                                  + (1|usersubject) + (1|subject) ),
                      Dimp, REML = FALSE );

  printf("\t\tBIC\nModel 1:\t%g\nModel 2:\t%g\n", BIC(mmiss1ml), BIC(mmiss2ml));

  # Use all-pairs contrasts for both factors (classifier and feature set)

  mmissht <- glht( mmiss1, mcp( classifier='Tukey', featureset='Tukey' ) );
  mmisshtsum <- summary( mmissht );

  # Print the test results
  
  tbl <- with( mmisshtsum[['test']],
              data.frame('effect'   = coefficients,
                         'stderr'   = sigma,
                         't.stat'   = tstat,
                         'p.value'  = pvalues) );

  tbl[['p.value']] <- ifelse( tbl[['p.value']] < .0001,
                             '<.0001',
                             sprintf('%.4f', tbl[['p.value']]) );

  latex.data.frame( tbl, file = 'figs/table-3-3-b.tex',
                   collines = ncol(tbl)-1,
                   tablesym = 'r',
                   colformats.num = '%.3f',
                   center = FALSE );

  ######################################################################
  # Validation
  ######################################################################

  # Construct the design matrix for the secondary evaluation

  vsubjsubset <- sprintf('s%03d',c(15,16,17,18,19,20,21,22,24,25));
  
  Av <- expand.grid( typingtask = 'Strong',
                    classifier  = c('SVM','ScaledManhattan','MahalanobisKNN'),
                    featureset  = c('Hold','DD'),
                    usersubject = vsubjsubset,
                    trainpsr    = '1:100' );

  # Run the secondary evaluation
  
  a_ply( Av, 1, classifiereval, cachedir=evalcache );

  ntesttag <- sprintf( 'set-%s', paste( vsubjsubset, collapse='-' ) );
  Bv <- adply( Av, 1, getresults,
              stestpsr = 'T[2]+(1:100)',
              ntestpsr = '1:50',
              ntesttag = ntesttag, cachedir=evalcache );

  # Tune each classifier for a 5% false-alarm rate
  
  Bv <- modifythreshold( Bv, .(featureset,classifier), farate=.05 );

  # Calculate the per-user/impostor miss rates
  
  Dvimp <- ddply( subset( Bv, usersubject != subject ),
                 .(featureset,classifier,usersubject,subject),
                 function( Bi )
                 data.frame( error = mean( Bi$error ) ) );

  # Perform the variance-stabilizing transformation
  
  Dvimp <- transform( Dvimp, VSerror = vsscore( error ) );

  # Rename the classifiers and features (for illustrative purposes)
  
  Dvimp <- transform( Dvimp,
                     classifier = factor(classifier,labels=c('A','B','C')),
                     featureset = factor(featureset,levels=c('Hold','DD')),
                     usersubject = factor(usersubject,
                       levels = levels( usersubject ),
                       labels = sprintf('s%03d', 11:20)),
                     subject = factor(subject,
                       levels = levels( subject ),
                       labels = sprintf('s%03d', 11:20)) );

  # Print the table of results to sanity check it
  
  print( ddply(Dvimp, .(featureset,classifier), function(Di)
               data.frame(nobs    = nrow( Di ),
                          error   = mean( Di$error ),
                          VSerror = mean( Di$VSerror ) ) ) );

  # Create a trellis plot for the validation data set

  X <- Dvimp;
  
  X[['usersubject']] <- reorder( X[['usersubject']], X[['VSerror']], mean );

  trellis.device( file = 'figs/figure-3-4.eps',
                 theme = theme, height=5, width=8 );

  print( dotplot( 100*error ~ usersubject | featureset * classifier, X,
                 group = subject,
                 layout = c(2,3),
                 xlab = 'Genuine-User Subject',
                 ylab = 'Miss Rate (%)',
                 strip = function(...) 
                 strip.default( ..., strip.names=c(TRUE,TRUE) ),
                 scales = list( rot = c(90,0), alternating = FALSE ),
                 auto.key = list( 
                   space = 'right',
                   title = 'Impostor\nSubject',
                   cex.title = 1 ) ) );
  graphics.off();
  
  # Estimate prediction intervals

  mci1 <- meanvarests( VSerror ~ classifier + featureset,
                      Dimp, mmiss1, 'usersubject' );
  mci2 <- meanvarests( VSerror ~ classifier + featureset,
                      Dimp, mmiss1, 'subject' );
  mmissci <- rbind(data.frame( mci1, vname = 'usersubject' ),
                   data.frame( mci2, vname = 'subject' ));
  mmissci <- transform( mmissci,
                       oest = vsinverse(est),
                       olb = vsinverse(lb),
                       oub = vsinverse(ub) );
  
  # Reorder all the factors for consistency

  ord <- list( classifier = factor( NA, levels=c('A','B','C') ),
              featureset  = factor( NA, levels=c('Hold','DD') ) );

  Dimp <- aligndataframe( Dimp, ord, except=c('usersubject','subject') );
  Dvimp <- aligndataframe( Dvimp, ord, except=c('usersubject','subject') );

  mmissci <- aligndataframe( mmissci, ord );

  # Create prediction-interval plots

  mmissuserci <- subset( mmissci, vname == 'usersubject' );

  D2m <- Dvimp[ , c('classifier','featureset','usersubject','error') ];
  names( D2m )[ which( names(D2m) == 'error' ) ] <- 'value';
  D2tbl <- cast( D2m, classifier + featureset ~ usersubject,
                fun.aggregate = mean );
  D2tbl <- aligndataframe( D2tbl, ord );

  trellis.device( file = 'figs/figure-3-5.eps', 
                 theme = theme, height = 4, width = 8 );

  xyplotpint( oest ~ classifier | featureset,
             mmissuserci,
             D2tbl,
             layout = c(2,1),
             strip = function(...)
             strip.default( ..., strip.names=c(TRUE,TRUE)),
             xlab = 'Classifier',
             ylab = 'Miss Rate',
             scales = list( rot=c(90,0), alternating=FALSE ) );
  graphics.off();

  # Create QQ-plots

  makeqqplots( VSerror ~ classifier + featureset, Dvimp, mmiss1, 
              plotfmt = 'figs/figure-3-6-%s.eps' );

  
}

print( run );
