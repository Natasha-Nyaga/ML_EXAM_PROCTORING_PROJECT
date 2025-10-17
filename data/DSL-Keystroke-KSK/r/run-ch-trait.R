############################################################################
# R Code for Chapter: Personal Traits and Keystroke Dynamics (#2)          #
# (Kevin Killourhy)                                                        #
############################################################################

source( 'r/analysis.R', chdir=TRUE );

########################################################################
# Configuration Variables
########################################################################

seedfile <- '.ch-trait-seed.Rdata';

contrastfile <- 'data-contrasts/ch-trait-contrast.txt';

demodata <- 'data-survey/survey.txt';

benchevaldata <- 'tmp-bench-eval';

topclassifiers <- c('ScaledManhattan','OutlierCount','Mahalanobis','KNN',
                    'MahalanobisKNN','KMeans','SVM');

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
# This function generates the design matrix from the benchmark
# investigation (for 7 of the 10 classifiers).  It is used here to
# retrieve the evaluation results from that investigation for the
# current investigation.

get.designmatrix <- function( usersubjects=asubjects ) {
  A <- expand.grid(typingtask  = 'Strong',
                   classifier  = topclassifiers,
                   featureset  = 'Hold.DD.Ret',
                   usersubject = usersubjects,
                   trainpsr    = '1:200');

  return( A );
}

## get.demodata
# Retrieve the data from the file containing the survey results, and
# order its contents for convenience.

get.demodata <- function() {

  X <- read.table( demodata, header=TRUE );

  X <- aligndataframe( X,
                      list(gender = factor( levels=c('Male','Female') ),
                           age = factor( levels=c('18-30','31+') ),
                           handedness = factor( levels=c('Right','Left') ),
                           typingskill = factor( levels=c('Other','Touch') )) );

  X <- X[ , c('subject','gender','age','handedness','typingskill') ];

  return( X );
}
  
## clear
# Remove all the intermediary data (i.e., the cache of evaluation
# results and the saved image of important data objects).
##
# resetseed: if true, remove the seed file as well
##
# Returns nothing useful

clear <- function( resetseed=FALSE ) {

  # Remove temporary directories (nothing to do)
  
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
  
  # Run the evaluations, compile the error rates, stabilize the variance

  load( seedfile );
  .Random.seed <<- seed;

  ######################################################################
  # Evaluation Procedure
  ######################################################################

  # Retrieve the results of the benchmark evaluation

  A <- get.designmatrix();
  B <- adply( A, 1,
             getresults,
             stestpsr='T[2]+(1:200)',
             ntestpsr='1:50',
             ntesttag=asubjecttag, cachedir=benchevaldata,
             .progress='text' );
  B <- modifythreshold( B, .(classifier,usersubject), farate=.05 );
  Dimp <- ddply( subset( B, usersubject != subject ),
                .(classifier,usersubject,subject),
                function( Bi )
                data.frame( error = mean( Bi$error ) ) );
  Dimp <- transform( Dimp, VSerror = vsscore( error ) );

  # Retrieve the validation results

  Av <- get.designmatrix( usersubjects=vsubjects );
  Bv <- adply( Av, 1,
              getresults,
              stestpsr='T[2]+(1:200)',
              ntestpsr='1:50',
              ntesttag=vsubjecttag, cachedir=benchevaldata,
              .progress='text' );
  Bv <- modifythreshold( Bv, .(classifier,usersubject), farate=.05 );
  Dvimp <- ddply( subset( Bv, usersubject != subject ),
                 .(classifier,usersubject,subject),
                 function( Bi )
                 data.frame( error = mean( Bi$error ) ) );
  Dvimp <- transform( Dvimp, VSerror = vsscore( error ) );

  # Retrieve the survey results

  Dsurvey <- get.demodata();

  # Split the data into evaluation and validation

  X <- droplevels( subset( Dsurvey, subject %in% asubjects ) );
  Xv <- droplevels( subset( Dsurvey, subject %in% vsubjects ) );
  
  # Duplicate the survey results, once for users, once for impostors
  
  Xu <- X;
  names(Xu) <- c('usersubject','usergender','userage','userhand','userstyle');

  Xi <- X;
  names(Xi) <- c('impsubject','impgender','impage','imphand','impstyle');
  
  Xvu <- Xv;
  names(Xvu) <- c('usersubject','usergender','userage','userhand','userstyle');

  Xvi <- Xv;
  names(Xvi) <- c('impsubject','impgender','impage','imphand','impstyle');
  
  # Match the benchmark results with the user and impostor personal traits
  
  DXimp <- merge( Xu, Dimp, by.x = 'usersubject', by.y = 'usersubject' );
  DXimp <- merge( DXimp, Xi, by.x = 'subject', by.y = 'impsubject' );

  DXvimp <- merge( Xvu, Dvimp, by.x = 'usersubject', by.y = 'usersubject' );
  DXvimp <- merge( DXvimp, Xvi, by.x = 'subject', by.y = 'impsubject' );

  # Establish an ordering on the classifier and other factors

  corder <- levels( reorder( DXimp$classifier, DXimp$error ) );
  
  ord <- list( classifier = factor( NA, levels=corder ),
              userage     = factor( NA, levels=levels(X[['age']]) ),
              usergender  = factor( NA, levels=levels(X[['gender']]) ),
              userhand    = factor( NA, levels=levels(X[['handedness']]) ),
              userstyle   = factor( NA, levels=levels(X[['typingskill']]) ),
              impage      = factor( NA, levels=levels(X[['age']]) ),
              impgender   = factor( NA, levels=levels(X[['gender']]) ),
              imphand     = factor( NA, levels=levels(X[['handedness']]) ),
              impstyle    = factor( NA, levels=levels(X[['typingskill']]) ) );
  
  # Tabulate personal traits for model-building and validation groups

  Xs <- rbind( data.frame( pool='Evaluation Pool', X ),
              data.frame( pool='Validation Pool', Xv ) );

  Xs[['typingskill']] <- relevel( Xs[['typingskill']], 'Touch' );

  Xsm <- ddply( Xs, .(pool,age,gender,handedness,typingskill), function(Xi) {
    data.frame( value = nrow( Xi ) );
  } );

  Xtbl <- merge( merge( cast( Xsm, pool ~ age, sum ),
                       cast( Xsm, pool ~ gender, sum ) ),
                merge( cast( Xsm, pool ~ handedness, sum ),
                      cast( Xsm, pool ~ typingskill, sum ) ) );

  latex.data.frame( Xtbl, file = 'figs/table-5-1.tex',
                   rownames = NULL,
                   coltitle = list( c(1,'c|',''),
                     c(2,'c|','Age Range'),
                     c(2,'c|','Gender'),
                     c(2,'c|','Dom. Hand'),
                     c(2,'c|','Typ. Style')),
                   colnames = c('',names(Xtbl)[-1]),
                   collines = seq( 1, ncol(Xtbl), by=2 ),
                   tablesym = 'r' );

  # Plot the empirical results broken down by age
  
  trellis.device( file = 'figs/figure-5-1-a.eps',
                 theme = theme, height = 4.5, width = 8 );

  print( bwplot( error ~ classifier | userage * impage,
                aligndataframe( DXimp, ord ),
                layout = c(2,2),
                ylab = 'Miss Rate',
                scales = list( alternating = FALSE, rot=c(90,90) ),
                strip = function(...)
                strip.default( ..., strip.names=c(TRUE,TRUE)),
                panel  = function(xs,ys,...) {
                  panel.bwplot(xs,ys,...);
                } ) );

  graphics.off();

  # Plot the empirical results broken down by gender

  trellis.device( file = 'figs/figure-5-1-b.eps',
                 theme = theme, height = 4.5, width = 8 );

  print( bwplot( error ~ classifier | usergender * impgender,
                aligndataframe( DXimp, ord ),
                layout = c(2,2),
                ylab = 'Miss Rate',
                scales = list( alternating = FALSE, rot=c(90,90) ),
                strip = function(...)
                strip.default( ..., strip.names=c(TRUE,TRUE)),
                panel  = function(xs,ys,...) {
                  panel.bwplot(xs,ys,...);
                } ) );

  graphics.off();

  # Plot the empirical results broken down by handedness

  trellis.device( file = 'figs/figure-5-2-a.eps',
                 theme = theme, height = 4.5, width = 8 );

  print( bwplot( error ~ classifier | userhand * imphand,
                aligndataframe( DXimp, ord ),
                layout = c(2,2),
                ylab = 'Miss Rate',
                scales = list( alternating = FALSE, rot=c(90,90) ),
                strip = function(...)
                strip.default( ..., strip.names=c(TRUE,TRUE)),
                panel  = function(xs,ys,...) {
                  panel.bwplot(xs,ys,...);
                } ) );

  graphics.off();

  # Plot the empirical results broken down by typing style
  
  trellis.device( file = 'figs/figure-5-2-b.eps',
                 theme = theme, height = 4.5, width = 8 );

  print( bwplot( error ~ classifier | userstyle * impstyle,
                aligndataframe( DXimp, ord ),
                layout = c(2,2),
                ylab = 'Miss Rate',
                scales = list( alternating = FALSE, rot=c(90,90) ),
                strip = function(...)
                strip.default( ..., strip.names=c(TRUE,TRUE)),
                panel  = function(xs,ys,...) {
                  panel.bwplot(xs,ys,...);
                } ) );

  graphics.off();
  
  ######################################################################
  # Statistical Analysis
  ######################################################################

  # Run stepwise model selection on the family of formulas
      
  Mmiss <- stepfitlmers( VSerror ~ 1 + (1|usersubject) + (1|subject),
                        VSerror ~ (classifier * usergender * impgender
                                   + classifier * userage * impage
                                   + classifier * userhand * imphand
                                   + classifier * userstyle * impstyle
                                   + (1|usersubject) + (1|subject)),
                        DXimp, REML=FALSE );
  
  # Find the best miss model and fit with REML

  idx <- which.min( Mmiss$bic );
  formulamiss <- formula( ac(Mmiss$model[ idx ]) );
  mmiss <- fitlmer( formulamiss, DXimp, REML=TRUE );
  mmissvc <- VarCorr( mmiss );
  
  # Extract and tabulate the parameter estimates

  tabulateparams( mmiss, file='figs/table-5-2.tex', center=FALSE );
  
  # Read in the contrasts written manually to file

  K <- as.matrix( read.table( contrastfile, row.names=1, header=TRUE ) );
  mmissht <- glht( mmiss, K );
  mmisshtsum <- summary( mmissht );

  # Format the test results for printing

  tbl <- with( mmisshtsum[['test']], 
              data.frame('effect'  = coefficients,
                         'stderr'  = sigma,
                         't.stat'  = tstat,
                         'p.value' = pvalues ) );

  tbl[['p.value']] <- ifelse( tbl[['p.value']] < .0001, 
                             '<.0001', 
                             sprintf('%.4f', tbl[['p.value']]) );

  # Create the table of typing-style hypothesis-test results

  tbl1b <- tbl[1:6,];

  tbl1a <- rbind(c('touch', 'other', 'other', 'other'),
                 c('other', 'touch', 'other', 'other'),
                 c('touch', 'touch', 'other', 'other'),
                 c('touch', 'other', 'other', 'touch'),
                 c('touch', 'touch', 'touch', 'other'),
                 c('touch', 'touch', 'other', 'touch'));

  tbl1 <- cbind( tbl1a, tbl1b );

  latex.data.frame( tbl1, file = 'figs/table-5-3.tex',
                   rownames = NULL,
                   colnames = c(
                     'user style','imp.~style',
                     'user style','imp.~style',
                     names( tbl1b )),
                   coltitle = list( 
                     c(2,'c|','Pair \\#1'),
                     c(2,'c||','Pair \\#2'),
                     c(3,'c|',''),
                     c(1,'c','')),
                   collines = c(2,4,4,7),
                   tablesym = 'r',
                   colformats.num = '%.3f' );
  
  # Create the table of gender-effect hypothesis-test results

  tbl2 <- tbl[6+(1:7),];

  latex.data.frame( tbl2, file = 'figs/table-5-4.tex',
                   rownames = c(
                     'ScaledManhattan',
                     'OutCount',
                     'Mahalanobis',
                     'KNN',
                     'MahalanobisKNN',
                     'KMeans',
                     'SVM'),
                   collines = ncol(tbl2)-1,
                   tablesym = 'r',
                   colformats.num = '%.3f' );
  
  ######################################################################
  # Validation
  ######################################################################

  # Estimate confidence intervals
  
  mci1 <- meanvarests( VSerror ~ classifier * usergender
                      + usergender * impgender
                      + userstyle * impstyle,
                      DXimp, mmiss, 'usersubject' );
  mci2 <- meanvarests( VSerror ~ classifier * usergender
                      + usergender * impgender
                      + userstyle * impstyle,
                      DXimp, mmiss, 'subject' );
  mmissci <- rbind(data.frame( mci1, vname = 'usersubject' ),
                   data.frame( mci2, vname = 'subject' ));
  mmissci <- transform( mmissci,
                       oest = vsinverse(est),
                       olb = vsinverse(lb),
                       oub = vsinverse(ub) );

  # Aggregate the per-user error rates

  Dm <- DXvimp[ , c('classifier','usergender','impgender',
                    'userstyle','impstyle',
                    'usersubject','error') ];
  names( Dm )[ which( names(Dm) == 'error' ) ] <- 'value';
  Dmtbl <- cast( Dm, ( classifier+usergender+impgender+userstyle+impstyle ~
                      usersubject ), fun.aggregate = mean );
  
  # Ensure the same ordering of the factors

  mmissci <- aligndataframe( mmissci, ord );
  mmissci <- subset( mmissci, vname == 'usersubject' );

  Dmtbl <- aligndataframe( Dmtbl, ord );
  
  # Create the per-user/impostor prediction plot

  trellis.device( file = 'figs/figure-5-4+5-%d.eps', 
                 onefile = FALSE, theme = theme, height = 9, width = 8 );
  print( xyplotpint( oest ~ classifier |
                    impstyle*userstyle*usergender*impgender,
                    mmissci,
                    Dmtbl,
                    layout = c(2,4),
                    between = list( x=0, y=c(0,1,0) ),
                    strip = function(...)
                    strip.default( ..., strip.names=c(TRUE,TRUE)),
                    xlab = '',
                    ylab = 'Miss Rate',
                    scales = list( rot=c(90,0), alternating=FALSE ) ) );
  graphics.off();

  # Create QQ-plots

  makeqqplots( VSerror ~ classifier*usergender + 
              usergender*impgender +
              userstyle*impstyle, DXvimp, mmiss, 
              plotfmt = 'figs/figure-5-6-%s.eps' );

}

print( run );

