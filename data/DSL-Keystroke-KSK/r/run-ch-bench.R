############################################################################
# R Code for Chapter: Benchmarking Keystroke-Dynamics Classifiers          #
# (Kevin Killourhy)                                                        #
############################################################################

source( 'r/analysis.R', chdir=TRUE );

########################################################################
# Configuration Variables
########################################################################

evalcache <- 'tmp-bench-eval';
seedfile <- '.ch-bench-seed.Rdata';

asubjects <- sprintf('s%03d',
                     c(2,  3,  4,  5,  7,  8,  10, 11, 12, 13,
                       15, 16, 17, 18, 19, 20, 21, 22, 24, 25,
                       26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
                       36, 37, 38, 39, 40, 41, 42, 43, 44, 46,
                       47, 48, 49, 50, 51, 52, 53, 54, 55, 56,
                       57));
asubjecttag <- sprintf('set-%s', paste(asubjects,collapse='-'));

vsubjects <- sprintf('s%03d',
                     c(58, 59, 60, 74, 75, 76, 77, 78, 79, 80,
                       81, 82, 83, 88));
vsubjecttag <- sprintf('set-%s', paste(vsubjects,collapse='-'));

############################################################################
# Experiment
############################################################################

## get.designmatrix
# This function generates the design matrix for evaluating the various
# classifiers.  Each classifier is trained once for each genuine user in
# the data set.

get.designmatrix <- function( usersubjects=asubjects ) {
  A <- expand.grid(typingtask  = 'Strong',
                   classifier  = names( classifiers ),
                   featureset  = 'Hold.DD.Ret',
                   usersubject = usersubjects,
                   trainpsr    = '1:200');

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

## run
# Main function to reproduce the tables and figures of results

run <- function() {

  # Clear cache (and possibly set new random seed)
  # clear( resetseed = TRUE );

  #clear();
  
  # Run the evaluations, compile the error rates, stabilize the variance

  load( seedfile );
  .Random.seed <<- seed;

  ######################################################################
  # Evaluation Procedure
  ######################################################################

  # Construct the design matrix
  
  A <- get.designmatrix();

  # Run the evaluation
  
  a_ply( A, 1, classifiereval, cachedir=evalcache );

  B <- adply( A, 1,
             getresults,
             stestpsr='T[2]+(1:200)',
             ntestpsr='1:50',
             ntesttag=asubjecttag, cachedir=evalcache,
             .progress='text' );

  # Tune each classifier for a 5% false-alarm rate
  
  B <- modifythreshold( B, .(classifier,usersubject), farate=.05 );
  
  # Calculate the per-user/impostor miss rate
  
  Dimp <- ddply( subset( B, usersubject != subject ),
                .(classifier,usersubject,subject),
                function( Bi )
                data.frame( error = mean( Bi$error ) ) );

  # Apply the variance-stabilizing transformation
  
  Dimp <- transform( Dimp, VSerror = vsscore( error ) );

  # Define a natural ordering on the values of the factors

  corder <- levels( reorder( Dimp$classifier, Dimp$error ) );
  usorder <- levels( reorder( Dimp$usersubject, Dimp$error ) );

  ord <- data.frame( classifier = factor( NA, levels = corder ),
                    usersubject = factor( NA, levels = usorder ) );

  # Create the table of aggregate results

  Btbl <- ddply( B, .(classifier), function(Bi) {
    data.frame(fa   = with(Bi, mean( error[usersubject==subject])),
               miss = with(Bi, mean( error[usersubject!=subject])));
  } );

  Btbl <- aligndataframe( Btbl, ord );

  Btbl <- transform( Btbl, fa = 100 * fa, miss = 100 * miss );

  latex.data.frame( Btbl, file = 'figs/table-4-2.tex',
                   rownames = NULL,
                   colnames = c('Classifier','False-Alarm Rate','Miss Rate'),
                   tablesym = 'r',
                   colformats.num = '%.1f' );

  # Create the per-user miss-rate boxplots

  Dimpsort <- aligndataframe( Dimp, ord );
  
  trellis.device( file = 'figs/figure-4-4+5-%d.eps', onefile = FALSE,
                 theme = theme, height = 9, width = 8 );

  print( bwplot( error ~ usersubject | classifier, Dimpsort,
                layout  = c(1,5),
                scales = list( rot=c(90,0), alternating=FALSE ),
                ylab   = 'Miss Rate',
                panel   = function(xs,ys,...) { 
                  panel.bwplot(xs,ys,...); 
                  panel.abline(h=mean(ys),col='red')
                } ) );

  dev.off();
  
  ######################################################################
  # Statistical Analysis
  ######################################################################

  # Set up the formulas for the miss-rate analyses
  
  missfs <- getlmerformulafamily( 'VSerror',
                                 fixefmin = ~ 1,
                                 fixefmax = ~ classifier,
                                 ranefmin = ~ usersubject + subject,
                                 ranefmax = ~ usersubject + subject,
                                 frintmin = ~ 1,
                                 frintmax = ~ 1 );

  # Run model selection for the false-alarm-rate and miss-rate analyses

  Mmiss <- fitlmers( missfs, Dimp, REML=FALSE );
  print( Mmiss );
  
  # Find the best miss model and fit with REML
  
  idx <- which.min( Mmiss$bic );
  mmiss <- fitlmer( missfs[[idx]], Dimp, REML=TRUE );
  mmissvc <- VarCorr( mmiss );

  # Extract and tabulate the parameter estimates

  tabulateparams( mmiss, file='figs/table-4-3.tex', center=FALSE );
  
  # Use all-pairs contrasts to establish which classifiers are
  # significantly better than other classifiers.

  mmissht <- glht( mmiss, mcp( classifier='Tukey' ) );
  mmisshtsum <- summary( mmissht );

  # Print the test results

  tbl <- with( mmisshtsum[['test']], 
              data.frame('effect'  = coefficients,
                         'stderr'  = sigma,
                         't.stat'  = tstat,
                         'p.value' = pvalues ) );

  tbl[['p.value']] <- ifelse( tbl[['p.value']] < .0001, 
                             '<.0001', 
                             sprintf('%.4f', tbl[['p.value']]) );

  latex.data.frame( tbl, file = 'figs/table-4-4.tex',
                   rowlines = cumsum( 9:2 ),
                   collines = ncol(tbl)-1,
                   tablesym = 'r',
                   colformats.num = '%.3f' );
  
  ######################################################################
  # Validation
  ######################################################################

  # Construct the design matrix for the secondary evaluation

  Av <- get.designmatrix( usersubjects=vsubjects );

  # Run the secondary evaluation
  
  a_ply( Av, 1, classifiereval, cachedir=evalcache );

  Bv <- adply( Av, 1,
              getresults,
              stestpsr='T[2]+(1:200)',
              ntestpsr='1:50',
              ntesttag=vsubjecttag, cachedir=evalcache,
              .progress='text' );

  # Tune each classifier for a 5% false-alarm rate
  
  Bv <- modifythreshold( Bv, .(classifier,usersubject), farate=.05 );
  
  # Calculate the per-user/impostor miss rates

  Dvimp <- ddply( subset( Bv, usersubject != subject ),
                 .(classifier,usersubject,subject),
                 function( Bi )
                 data.frame( error = mean( Bi$error ) ) );

  # Perform the variance-stabilizing transformation
  
  Dvimp <- transform( Dvimp, VSerror = vsscore( error ) );
  
  # Create the table of aggregate validation results
  
  Bvtbl <- ddply( Bv, .(classifier), function(Bi) {
    data.frame(fa   = with(Bi, mean( error[usersubject==subject])),
               miss = with(Bi, mean( error[usersubject!=subject])));
  } );

  Bvtbl <- aligndataframe( Bvtbl, ord, except=c('usersubject','subject') );

  Bvtbl <- transform( Bvtbl, fa = 100 * fa, miss = 100 * miss );

  latex.data.frame( Bvtbl, file = 'figs/table-4-5.tex',
                   rownames = NULL,
                   colnames = c('Classifier','False-Alarm Rate','Miss Rate'),
                   tablesym = 'r',
                   colformats.num = '%.1f' );
  
  # Estimate confidence intervals
  
  mmissci <- meanvarests( VSerror ~ classifier, Dimp, mmiss, 'usersubject' );
  mmissci <- transform( mmissci,
                       vname = 'usersubject',
                       oest = vsinverse(est),
                       olb = vsinverse(lb),
                       oub = vsinverse(ub) );

  # Create the per-user/impostor prediction plot

  Dvimpsort <- aligndataframe( Dvimp, ord, except='usersubject' );
  mmissci <- aligndataframe( mmissci, ord, except='usersubject' );

  Xi1 <- Dvimpsort[ , c('classifier','usersubject','subject','error') ];
  names(Xi1)[4] <- 'value';

  Xi1tbl <- cast( Xi1, classifier ~ usersubject, fun.aggregate=mean );
  Xi1tbl <- aligndataframe( Xi1tbl, mmissci );

  trellis.device( file = 'figs/figure-4-7.eps',
                 theme = theme, height = 5, width = 8 );
  print( xyplotpint( oest ~ classifier, mmissci, Xi1tbl,
                    xlab = '',
                    ylab = 'Miss Rate',
                    scales = list( rot=c(90,0) ) ) );
  dev.off();

  # Create QQ-plots

  makeqqplots( VSerror ~ classifier, Dvimp, mmiss,
              plotfmt = 'figs/figure-4-8-%s.eps' );
  
}

print( run );
