############################################################################
# R Code for Chapter: Background on Keystroke Dynamics                     #
# (Kevin Killourhy)                                                        #
############################################################################

library( plyr );
library( reshape );

source( 'r/latex-table.R' );

ac <- as.character;

run <- function() {

  # Read the coded lit-review results into a data frame

  L <- read.table('data-litrev/survey-res-inscope.txt', header=TRUE);

  # Merge the results of the 1st and 2nd IEEE searches, and incorporate
  # the ndltd (thesis) search results into the other extra results.
  
  L[['db']][ L[['db']] == 'db:ieee2' ] <- 'db:ieee';
  L[['db']][ L[['db']] == 'db:ndltd' ] <- 'db:extra';
  L[['db']] <- L[['db']][ , drop=TRUE ];

  # Prepare the pretty-print names for every code

  dbmap <- c('db:acm'        = 'ACM Digital Library',
             'db:ieee'       = 'IEEE Xplore',
             'db:scidirect'  = 'Science Direct',
             'db:springer'   = 'SpringerLink',
             'db:extra'      = 'Other Sources');

  typemap <- c('article'       = 'Article',
               'inproceedings' = 'Proceedings',
               'incollection'  = 'Chapter',
               'mastersthesis' = 'Masters Thesis',
               'phdthesis'     = 'PhD Thesis',
               'techreport'    = 'Tech Report');

  # Count the number of sources by database and type and print it.
  
  Lcount <- ddply( L, .(db,type), function(Li) c(value=nrow(Li)) );
  Lcount <- transform( Lcount,
                      db = factor( ac(db), levels=names(dbmap) ),
                      type = factor( ac(type), levels=names(typemap) ));

  tbl <- cast( Lcount, db ~ type );
  rownames( tbl ) <- ac( tbl$db );
  tbl$db <- NULL;

  rnames <- c( dbmap[rownames(tbl)], 'Total' );
  cnames <- sprintf( '\\rotatebox{90}{%s}',
                    c( typemap[colnames(tbl)],
                      'Total') );

  tbl <- cbind( tbl, 'Total'=apply( tbl, 1, sum, na.rm=TRUE ) );
  tbl <- rbind( tbl, 'Total'=apply( tbl, 2, sum, na.rm=TRUE ) );

  latex.data.frame( tbl, file = 'figs/table-2-1.tex',
                   rownames = rnames,
                   colnames = cnames,
                   tablesym = 'r',
                   na.sym   = '-',
                   collines = ncol( tbl )-1,
                   rowlines = nrow( tbl )-1 );

  # Map the various statistical codes to one of three groups (none,
  # one-way, and multi-way inferential technique).

  statmap1 <- c('none'                    = 'No Inferential Statistics');
  statmap2 <- c('t-test'                  = 'One-factor-at-a-time Inferences',
                '1way-anova'              = 'One-factor-at-a-time Inferences',
                'chisq'                   = 'One-factor-at-a-time Inferences',
                'kappa'                   = 'One-factor-at-a-time Inferences',
                'wilcoxon'                = 'One-factor-at-a-time Inferences',
                'kruskal-wallis'          = 'One-factor-at-a-time Inferences',
                'kruskal-wallis+confint'  = 'One-factor-at-a-time Inferences');
  statmap3 <- c('confint'                 = 'Multiple-factor Inferences',
                'errbar'                  = 'Multiple-factor Inferences',
                'modsel-bic'              = 'Multiple-factor Inferences',
                'repmes-anova'            = 'Multiple-factor Inferences');
  statmap <- c(statmap1,statmap2,statmap3);

  # Tabulate the sources by type and statistical group
  
  Lnew <- L;
  Lnew$stat <- factor( statmap[ ac( Lnew$stat ) ],
                      levels = c(
                        'No Inferential Statistics',
                        'One-factor-at-a-time Inferences',
                        'Multiple-factor Inferences') );

  Lstat <- ddply( Lnew, .(type,stat), function(Li) c(value=nrow(Li)) );

  Lstat <- transform( Lstat,
                     type = factor( ac(type), levels=names(typemap) ) );
  tbl <- cast( Lstat, stat ~ type );

  rownames( tbl ) <- ac( tbl$stat );
  tbl$stat <- NULL;

  rnames <- rownames(tbl);
  cnames <- sprintf( '\\rotatebox{90}{%s}',
                    c(typemap[colnames(tbl)],
                      'Total' ) );

  tbl <- cbind( tbl, 'Total'=apply( tbl, 1, sum, na.rm=TRUE ) );

  latex.data.frame( tbl, file = 'figs/table-2-2.tex',
                   rownames = rnames,
                   colnames = cnames,
                   tablesym = 'r',
                   na.sym   = '-',
                   collines = ncol(tbl)-1,
                   rowlines = nrow(tbl) );

}

print( run );
