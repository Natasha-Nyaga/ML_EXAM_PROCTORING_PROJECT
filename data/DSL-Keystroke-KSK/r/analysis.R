############################################################################
# R Code for All Analyses                                                  #
# (Kevin Killourhy)                                                        #
############################################################################

library( lme4 );
library( plyr );
library( reshape );
library( multcomp );
library( lattice );

# Source the evaluation-specific and the lmm-specific functions

source( 'analysis-eval.R' );
source( 'analysis-lmer.R' );

# Source the latex-table printer

source( 'latex-table.R' );

# Set the number of digits for pretty printing

options( digits = 3 );

# Set the trellis-plot theme to generate postscript

theme <- col.whitebg();
theme[['superpose.line']][['col']] <- 
  theme[['superpose.line']][['col']][1:6];
theme[['superpose.symbol']][['col']] <- 
  theme[['superpose.symbol']][['col']][1:6];
options( device = function( ... ) 
        postscript( ..., paper = 'special', horizontal = FALSE ) );
