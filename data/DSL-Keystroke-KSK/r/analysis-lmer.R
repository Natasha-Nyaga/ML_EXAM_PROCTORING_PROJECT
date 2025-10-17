#############################################################################
# R Code for Conducting LMM (lmer) Model Selection and Parameter Estimation #
# (Kevin Killourhy)                                                         #
#############################################################################

## aligndataframe
# This function takes a data frame (X), and a reference data frame
# (R).  It reorders all the factors in X that are also in R, so that
# they are ordered with the same level ordering in R, and sorted
# (lexicographically across factors in the order they appear in R).

aligndataframe <- function( X, R, except=NULL ) {

  for( xname in setdiff( rev( names(R) ), except ) ) {
    if( ( ! is.null( X[[xname]] ) ) && is.factor( X[[xname]] ) ) {

      naokmask <- is.na( X[[xname]] );
      
      rorder <- levels( R[[ xname ]] );

      X[[xname]] <- factor( X[[xname]], levels=rorder );

      stopifnot( all( ! is.na( X[[xname]][ ! naokmask ] ) ) );

      X <- X[ order( match( X[[xname]], rorder ) ), ];
      
    }
  }
  return( X );
}

## formulastring
# A simple function to convert a formula to a single-line string
# representation by deparsing it, merging the multiple lines, and
# removing initial spaces.

formulastring <- function( theformula )
  paste( sub( '^ +', '', deparse( theformula ) ), collapse='' );

## getterms
# Returns a list of the terms in a formula

getterms <- function( theformula ) {
  theterms <- terms( theformula );
  thefactormatrix <- attr( theterms, 'factor' );
  thefactors <- colnames( thefactormatrix );
  return( thefactors );
}

## termstring
# Turns a formula into a string of the unique terms in the formula.
# Can be used to identify the formula and check for equality between
# formulas. 

termstring <- function( theformula ) {
  thefactors <- getterms( theformula );
  if( is.null( thefactors ) ) return( '1' );
  splitfactors <- strsplit( thefactors, ':' );
  newfactors <- sapply( splitfactors, function(x) paste(sort(x),collapse=':') );
  sortfactors <- sort( newfactors );
  thestring <- paste( sortfactors, collapse='+' );
  return( thestring );
}

## getformulafamily
# This function repeatedly calls add.scope to generate a set of all
# formulas from a given minimum and maximum formula, while preserving
# the principle of hierarchy.

getformulafamily <- function( minformula, maxformula ) {

  formulas <- list();
  queue <- list( minformula );
  
  while( length( queue ) > 0 ) {

    theformula <- queue[[1]];
    queue <- queue[-1];

    thestring <- termstring( theformula );
    formulas[[ thestring ]] <- theformula;

    addfactors <- add.scope( theformula, maxformula );

    for( addfactor in addfactors ) {
      addupdate <- formula( sprintf( '~ . + (%s)', addfactor ) );
      newformula <- update( theformula, addupdate );
      newstring <- termstring( newformula );
      
      if( is.null( formulas[[newstring]] ) && is.null( queue[[newstring]] ) ) {
        queue[[ length(queue)+1 ]] <- newformula;
        names( queue )[ length(queue) ] <- newstring;
      }
    }
  }
  
  return( formulas );
}

## getlmerformulafamily
# This function generates a family of formulas that are formatted for
# lmer.  The variables involved must be specified separately.  The
# <outvar> argument is a string, giving the name of the outcome
# variable.  The <fixefmin> and <fixefmax> are formulas for the
# smallest and largest combination of fixed effects, respectively
# (e.g., ~ 1 and ~ C*T*F).  Likewise, the <ranefmin> and <ranefmax>
# are formulas for the smallest and largest combination of random
# effects (e.g., ~ U + I and ~ U*I).  This first generates all the
# random effect formulas.  Then it generates all the fixed-effects
# formulas.  Then, for each random effect, it generates a list of all
# the different ways the fixed effects can be added either as fixed
# effects or as interactions with the random effects.  This will
# undoubtedly create some bizarre, high-order models that are
# difficult to interpret.  So long as they are legal, we fit them.
# Then we just hope that model selection will punt them.

getlmerformulafamily <- function( outvar,
                                 fixefmin, fixefmax,
                                 ranefmin, ranefmax,
                                 frintmin = fixefmin,
                                 frintmax = fixefmax,
                                 verbose = TRUE ) {

  if( verbose ) cat("Generating random formulas ...\n");
  ranformulas <- getformulafamily( ranefmin, ranefmax );

  if( verbose ) cat("Generating fixed formulas ...\n");
  fixformulas <- getformulafamily( fixefmin, fixefmax );

  if( verbose ) cat("Generating fixed-random-intercept formulas ...\n");
  frformulas <- getformulafamily( frintmin, frintmax );

  formulas <- list();
  
  if( verbose ) cat("Merging ...\n");
  for( ranformula in ranformulas ) {

    rterms <- colnames( attr( terms( ranformula ), 'factors' ) );

    X <- expand.grid( lapply( c(1,rterms), function(i)
                             if( i == 1 ) 1:length(fixformulas)
                             else 1:length( frformulas) ) );
    names( X ) <- c( 'fixef', rterms );
    
    newformulas <- alply( X, 1, function( xi ) {

      # Create a seed formula containing only the response and the
      # main effect.
      
      fstr <- sprintf( '%s ~ %s',
                      outvar,
                      ac( fixformulas[[ xi[[1]] ]] )[2] );

      # For each of the random effects terms, append the appropriate
      # formula.  If the substitution contains anything other than an
      # intercept, don't fit the intercept.

      for( i in 2:length(xi) ) {

        fixefterm <- ac( frformulas[[ xi[[i]] ]] )[2];
        
        ffmt <- if( fixefterm == '1' ) '+ (%s|%s)' else '+ (%s+0|%s)';
        
        fsup <- sprintf( ffmt, fixefterm, names( xi )[i] );

        fstr <- paste( fstr, fsup, sep=' ' );
      }

      as.formula( fstr );
    } );

    formulas <- append( formulas, newformulas );
  }

  if( verbose ) cat("Done\n");
  return( formulas );
}

## fitlmer
# Just call the lmer fitting function for the given data and formula.
# Do a substitution to ensure that the call is pretty.

fitlmer <- function( theformula, data, ... ) {

  # Do a little munging to ensure that the actual formula rather than
  # a variable name appears in the model, and fit the LMM model.
    
  xpr <- substitute( lmer( theformula, data, ... ),
                    list( theformula=theformula ) );
  m <- eval( xpr );

  return( m );
}

## fitlmers
# Takes the same arguments as fitlmer except that a list of formulas
# can be provided, not a single formula.

fitlmers <- function( theformulas, data, ..., print.progress=TRUE ) {

  prog <- if( print.progress ) 'text' else 'none';
  
  minbic <- Inf;
  
  M <- ldply( theformulas, function( theformula ) {

    m <- fitlmer( theformula, data, ... );

    if( BIC( m ) < minbic ) {
      print( m, correlation=FALSE );
      minbic <<- BIC(m);
    }
    
    return( data.frame(aic   = AIC( m ),
                       bic   = BIC( m ),
                       model = formulastring( theformula ) ) );
  }, .progress=prog );

  return( M );
}

## stepfitlmers
# Takes min and max model formulas.  These must contain random effect
# terms, and the random effects must not change between min and max.
# It only does stepwise fitting on the fixed-effect terms.  It returns
# a data frame of BIC scores and model equations.

stepfitlmers <- function( minformula, maxformula, data, ... ) {

  theformula <- maxformula;

  m <- fitlmer( theformula, data, ... );
  
  M <- data.frame( aic=AIC(m), bic=BIC(m), model=formulastring(formula(m)) );
  
  while( TRUE ) {

    dropfactors <- drop.scope( theformula, minformula );

    if( length( dropfactors ) == 0 ) break;
    
    Mi <- ldply( dropfactors, function( dropfactor ) {

      dropupdate <- formula( sprintf( '~ . - (%s)', dropfactor ) );
      newformula <- update( theformula, dropupdate );

      m <- fitlmer( newformula, data, ... );

      data.frame( aic=AIC(m), bic=BIC(m), model=formulastring(formula(m)) );
    } );

    midx <- which.min( Mi$bic );

    theformula <- formula( ac( Mi$model[ midx ] ) );

    print( theformula );

    M <- rbind( M, Mi );
  }
  
  return( M );
}

## lmerpredict
# This function implements fixed-effect predictions using lmer objects
# (not implemented because prediction is ambiguous; it could involve
# random effects).

lmerpredict <- function( m, newdata ) {
  mm <- model.matrix( terms(m), newdata );
  return( mm %*% fixef( m ) );
}

# These functions are the variance stabilizing transformation and its
# inverse for binomially distributed random variables p = y/n (where y
# is the number of successes and n is the number of draws).  The
# angles are in gradians (pi radians == 200 gradians == 180 degrees).
# Since R's angles are in radians, we make the conversion ourselves.
# This isn't truly necessary, but mapping to the range [0,100] is nice
# and clean.

vsscore <- function( ps )
  asin( sqrt( ps ) ) / pi * 200;

vsinverse <- function( xs, xsreal = pmax( pmin( xs, 100 ), 0 ) )
  sin( xsreal / 200 * pi )^2;


############################################################################
# Model Visualization Functions
############################################################################

## tabulateparams
# This function takes the fixed-effects slot of a lmer model and
# writes out a nicely formatted fixed-effects table.  The first row is
# dedicated to the baseline.  The remaining rows have one column per
# factor, and the estimated difference from the baseline.  The format
# of the table is
#
## mu             x0
## A   a1         x1
##     a2         x2
## B      b1      x3
##        b2      x4
## C         c1   x5
##           c2   x6
## A:B a1 b1      x7
##     a1 b2      x8
##     a2 b1      x9
##     a2 b2      x10
#
## Note that fx = m@fixef for an lmm (m)

tabulateparams <- function( m, file, fx=fixef(m), VC=VarCorr(m), ... ) {

  ######################################
  # Fixed effects
  
  # Pop off the baseline
  stopifnot( names(fx)[1] == '(Intercept)' );
  fxbase <- fx[1];
  fx <- fx[-1];

  # Special-case the intercept-only model
  if( length( fx ) == 0 ) {

    tbl <- data.frame( 'Parameters' = '($\\mu$) baseline',
                      estimate = fxbase );
    nf <- 0;
    factorrows <- tbl[[1]];
    
  } else {
  
    # Split the remaining rows into separate factors (i.e., split
    # intercepts)
    fvstrs <- strsplit( names( fx ), ':' );

    # Create a list with one element per row in the fixed-effects table,
    # and each element is a named list with the factor as the name and
    # the level as the value.  (conventionally, factors are lower case;
    # levels upper case)
    fvlist <- lapply( fvstrs, function( fvstr ) {
      fstr <- sub( '^([A-Z]*[a-z]*).*$', '\\1', fvstr );
      vstr <- sub( '^[A-Z]*[a-z]*(.*)$', '\\1', fvstr );
      names( vstr ) <- fstr;
      return( vstr );
    } );
    fstrs <- lapply( fvlist, names );
    
    factorrows <- sapply( fstrs, paste, collapse=':' );
    factorrows[ duplicated( factorrows ) ] <- '';

    tbl <- data.frame( 'Parameters' = c( '($\\mu$) baseline', factorrows ),
                      stringsAsFactors = FALSE );

    # Create a column for each of the factors
    factornames <- unique( unlist( fstrs ) );
    nf <- length( factornames );
    factoridx <- 1:nf;
    names( factoridx ) <- factornames;
  
    for( i in 1:nf ) {
      factorname <- factornames[i];
      stopifnot( ! is.null( m@frame[[factorname]] ));
      basevalue <- levels( as.factor( m@frame[[factorname]] ) )[1];
      valuerow <- sapply( fvlist, function( fvent ) {
        if( factorname %in% names( fvent ) ) fvent[ factorname ] else '';
      } );
      tbl[[ i+1 ]] <- c( basevalue, valuerow );
    }

    names( tbl )[2:(nf+1)] <- factornames;

    # Create a column for the actual fixed effects
    tbl[[ nf+2 ]] <- c( fxbase, fx );
    names( tbl )[ nf+2 ] <- 'estimate';

  }

  ######################################
  # Random effects

  rsidx <- nrow( tbl );

  for( vname in c( names(VC), 'resid' ) ) {

    latexnamemap <- c('usersubject' = '$\\grs_{\\user}$',
                      'subject'     = '$\\grs_{\\impostor}$',
                      'resid'       = '$\\grs_{\\gre}$');
    
    matchidx <- match( vname, names( latexnamemap ) );
    latexname <- if( is.na( matchidx ) ) vname else latexnamemap[ vname ];
    
    if( vname == 'resid' ) {
      sds <- attr( VC, 'sc' );
    } else {
      sds <- sqrt( diag( VC[[vname]] ) );
    }

    stopifnot( length( sds ) == 1 );

    tbli <- data.frame( latexname );
    for( i in 2:(ncol(tbl)-1) ) tbli[[i]] <- '';
    tbli[[ ncol(tbl) ]] <- sds;
    names( tbli ) <- names( tbl );
    tbl <- rbind( tbl, tbli );
  }

  latex.data.frame( tbl, file = file,
                   rownames = NULL,
                   collines = c(1,nf+1,nf+2),
                   rowlines = c( 0, which( factorrows != '' ), rsidx, rsidx ),
                   bottomline = TRUE,
                   tablesym = 'r',
                   colformats.num = '%.2f',
                   allow.special = TRUE,
                   ... );

  return( invisible() );
}

## meanvarests
# This function retrieves mean estimates and prediction intervals for
# actual data based on a covariance matrix.  Different covariance
# matrices can be used when there are multiple random effects.

meanvarests <- function ( f, data, m, vcname, alpha=.05 ) {

  mf <- model.frame( f, data );

  mf$est <- lmerpredict( m, mf );

  mf[[1]] <- NULL; # remove the response variable
  
  mf <- unique( mf );

  vc <- VarCorr( m )[[ vcname ]];
  vars <- diag( vc );

  if( length( vars ) == 1 & names( vars ) == '(Intercept)' ) {

    thevar <- vars[[1]];
    mfv <- data.frame( mf, sd = sqrt( thevar ) );
    
  } else {
    
    fvstrs <- strsplit( names( vars ), ':' );
    stopifnot( all( sapply( fvstrs, length ) == 1 ) );

    fstrs <- sapply( fvstrs, function( fvstr ) {
      sub( '^([a-z]+).*$', '\\1', fvstr );
    } );
                  
    vstrs <- sapply( fvstrs, function( fvstr ) {
      sub( '^[a-z]+(.*)$', '\\1', fvstr );
    } );

    fname <- unique( fstrs );
    stopifnot( length( fname ) == 1 );

    stopifnot( fname %in% names( mf ) );

    ftags <- sprintf( '%s%s', fname, as.character( mf[[ fname ]] ) );

    mfv <- data.frame( mf, sd = sqrt( vars[ ftags ] ) );

  }

  z <- qnorm( 1-alpha/2 );

  mfv <- transform( mfv,
                   lb = est - z * sd,
                   ub = est + z * sd );

  return( mfv );
}

## xyplotpint
# Create a plot that overlays the model prediction interval (mci) and
# the results from the secondary evaluation (xtbl).

xyplotpint <- function( formula, mci, xtbl=NULL,
                       lbtxt='olb', ubtxt='oub', ... ) {

  estidx <- which( names( mci ) == 'est' );
  
  stopifnot( all( mci[ , 1:(estidx-1) ] == xtbl[ , 1:(estidx-1) ] ) );

  if( ! is.null( xtbl ) ) {
    xmat <- as.matrix( xtbl[ , -(1:(estidx-1)) ] );
  } else {
    xmat <- NULL;
  }
  
  xyplot( formula, mci, subscripts = TRUE,
         ...,
         prepanel = function( x, y, subscripts, ... ) {
           prepanel.default.xyplot( x, c(y,
                                         mci[subscripts,lbtxt],
                                         mci[subscripts,ubtxt],
                                         xmat),
                                   subscripts=subscripts, ... );
         },
         panel = function( xs, ys, subscripts, ... ) {
           xs <- as.numeric( xs );
           lbs <- mci[subscripts,lbtxt];
           ubs <- mci[subscripts,ubtxt];
           if( ! is.null( xmat ) ) {
             xmati <- xmat[ subscripts, ];
           }
           panel.segments( xs, lbs, xs, ubs, lty = 1, col='gray' );
           if( ! is.null( xmat ) ) {
             panel.points( xs, jitter(xmati), pch = 1, col='blue', cex=1 );
           }
           panel.points( xs, lbs, pch = '-', cex=3 );
           panel.points( xs, ubs, pch = '-', cex=3 );
           panel.xyplot( xs, ys, ..., pch=20 );
         } );
}
      

## makeqqplots
# Perform the calculations on the secondary data set to create the
# QQ-plots and assess model predictions and assumptions.  Subtract out
# the fixed effects predicted by the model, and then estimate per-user
# and per-impostor effects for each subject.  Subtract these out, and
# find the remaining residuals.  Standardize the effects and residuals
# based on the variance terms in the model, and compare them to a
# standard normal using QQ-plots.

makeqqplots <- function( f, D, m, plotfmt ) {

  VC <- VarCorr( m );

  # Create the model frame with the output (y) and the fixed effects
  mf <- model.frame( f, D );

  # Get the response variable name (the first in the model frame);
  rname <- names( mf )[1];

  # Get the fixed-effect factor names  

  fnames <- colnames( attr( terms( mf ), 'factors' ) );
  
  # Predict all the values
  mf$predict <- lmerpredict( m, mf );

  # Calculate the overall residuals
  mf$feresid <- mf[[ rname ]] - mf$predict;

  # For each random-effect factor, estimate the average effects in D
  # and plot on a QQ plot versus the model deviation.
  
  for( vname in names( VC ) ) {

    mfv <- mf;
    mfv[[ vname ]] <- D[[ vname ]];
    
    # Create a model frame with the main effect for the grouping
    # variable in D.  The first variable in a model frame is the
    # response.  Retrieve the estimated variance of the random effect
    # and calculate standardized residuals.

    mfv <- ddply( mfv, vname, function(mfi) {
      transform( data.frame( effect = mean( mfi$feresid ) ) );
    } );

    vars <- diag( VC[[ vname ]] );

    stopifnot( length( vars ) == 1 );

    stdresids <- mfv$effect / sqrt( vars );

    # Plot the standardized effects on a qqplot

    if( ! is.na( plotfmt ) ) {
    
      file <- sprintf( plotfmt, vname );

      trellis.device( file = file, theme = theme, height = 3, width = 3 );
    }
    
    print( qqmath( stdresids, panel = function( ... ) {
      panel.abline( a=0, b=1 );
      panel.qqmath( ... );
    },
                  xlab = 'Standard Normal Quantiles',
                  ylab = 'Empirical Quantiles' ) );

    if( ! is.na( plotfmt ) ) {
      graphics.off();
    }

    # Merge the average effect into the full track.  Uniquify the
    # random-effect estimate and residual names, and zero out the
    # response to avoid duplication.

    emap <- mfv$effect;
    names( emap ) <- ac( mfv[[ vname ]] );

    ename <- sprintf( '%s.effect', vname );

    mf[[ vname ]] <- D[[ vname ]];
    mf[[ ename ]] <- emap[ ac( mf[[ vname ]] ) ];
  }

  # Add the residual standard deviation

  effectnames <- grep( '^.*\\.effect$', names( mf ) );

  effects <- apply( mf[ , effectnames, drop=FALSE ], 1, sum );
  resids <- mf$feresid - effects;
  
  residsd <- attr( VC, 'sc' );

  stdresids <- resids / residsd;

  if( ! is.na( plotfmt ) ) {
    file <- sprintf( plotfmt, 'resid' );
    trellis.device( file = file, theme = theme, height = 3, width = 3 );
  }
  
  print( qqmath( stdresids, panel = function( ... ) {
    panel.abline( a=0, b=1 );
    panel.qqmath( ... );
  },
                xlab = 'Standard Normal Quantiles',
                ylab = 'Empirical Quantiles' ) );
  
  if( ! is.na( plotfmt ) ) {
    graphics.off();
  }

  invisible( mf );
}

## createcontrastmatrix
# Takes an effect matrix and turns it into a all-pairs contrast
# matrix.  This is sort of a scorched-earth approach to comparisons,
# but presumably if things are highly correlated, these tests will be
# more powerful.

createcontrastmatrix <- function( E, sep=' - ' ) {

  if( ! is.matrix( E ) ) E <- as.matrix( E );
  
  idxs <- combn( nrow(E), 2 );

  K <- t( apply( idxs, 2, function( xs ) E[xs[2],] - E[xs[1],] ) );

  rownames( K ) <- sprintf('%s%s%s',
                           rownames(E)[idxs[2,]],
                           sep,
                           rownames(E)[idxs[1,]]);

  return( K );
}
