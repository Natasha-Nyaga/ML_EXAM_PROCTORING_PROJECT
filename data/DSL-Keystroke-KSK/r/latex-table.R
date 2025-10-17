############################################################################
# latex-table.R                                                            #
# (Kevin Killourhy)                                                        #
############################################################################

# Functions for including R objects (primarily data frames) in latex

# Helper function:
#   printf
#   escape.latex
# Primary function:
#   latex.data.frame
# Wrapper functions:
#   latex.vector
#   latex.matrix
#   latex.table

###########################################################################
# Helper functions
###########################################################################

# Simply a wrapper to sprintf that handles the common case where we
# want to immediately print the results of sprintf.  I.e., just like
# printf everywhere else.

printf <- function( fmt, ..., file="", append=FALSE, sep="", collapse="" ) {
  cat(sprintf( fmt, ... ),
      file=file,
      append=append,
      sep=sep,
      collapse=collapse );
}

# Little function to take every underscore in an object and escape it
# so that it displays as an underscore in latex, and likewise with any
# other special characters that we find that break latex.

escape.latex <- function( x ) {
  x <- gsub( '_', '\\_', x, fixed=TRUE );
  x <- gsub( '%', '\\%', x, fixed=TRUE );
  x <- gsub( '#', '\\#', x, fixed=TRUE );
  return( x );
}

###########################################################################
# Primary function
###########################################################################

# Primary function is latex.data.frame for converting a data frame to
# a latex table.

# X is a data frame.

# file is the name of the file to print it to (e.g., foo.tex)

# rownames is a vector with length equal to nrow(X) or NULL.  If
# not NULL, the values are printed down the left-most column, as
# the names of each row.  Usually, this is something like 1:nrow(X),
# indicating the row number.

# colnames is a vector with length equal to ncol(X) or NULL.  It
# is interpreted similarly to rownames.  If non-NULL, the values are
# printed at the top of each column.

# coltitle is commonly a singleton that (if present) will be printed
# across the top row of the table (using multicolumn) to span the
# width of the row.  An alternative coltitle is a list of triples
# c(a1,b1,c1) that will be treated as the arguments to multicolumn,
# and can be used to label groups of columns.

# tablesym is the default value used in the latex tabular column
# description regarding the formatting of the row.  (E.g., 'c' will
# center all columns, 'r' will right-justify them, and 'l' will
# left-justify them.  By default, every column will be centered.  If
# tablesyms is specified, this value is ignored.

# tablesyms is a vector with length equal to ncol(X), and it
# contains values used in the latex tabular column description for
# each row.  By default, the same thing is done to every column, and
# is determined by the tablesym value.

# rowname.tablesym is a singleton that contains the value that will be
# used in the latex tabular column description regarding the
# formatting of that column of rownames (if present).  By default, it
# is 'l' indicating that the row will be left justified.

# colformats.num is the default (C-style formatting) that will be
# performed in the printing of numerical columns.  By default, they
# will be printed using '%g' but if columns should instead be printed
# with 3 decimal digits, '%.3f' could be used instead, for instance.
# If colformats is specified directly, this default is ignored.

# colformats.int is the default (C-style formatting) that will be
# performed in the printing of integer columns.  Integer columns are
# rarer than numerical columns, and it is appropriate to print them
# using '%d' by default but '%g' and other formats are possible.

# colformats.char is the default C-style formatting for character and
# factor columns.  It's hard to see this being anything other than
# '%s'.  If colformats is specified directly, this default is ignored.

# colformats is a list with length equal to ncol(X), and it contains
# the C-style formatting characters for each column.  By default, this
# will be colformats.num for numeric columns, and colformats.char for
# character and factor columns, but it can be specified directly to
# tailor the printing of each column.

# rownameformat is a singleton with the C-style formatting used to
# print the column of row names (if present).

# rowlines is a list of rows (of X) after which to print a horizontal
# line.  Note that if you want a horizontal line after the last row,
# you do not need to use this argument.  Instead, use bottomline=TRUE.

# collines is a list of columns (of X) after which to print a
# vertical line.  Note that, as with rowlines, don't use this to put
# a line on the right-hand border of the table.

# coltitleline is a boolean indicating whether to draw a line between
# the column title (if present) and the next row

# coltitlelinestr is a string indicating what string is printed to
# create the columntitleline (e.g., \hline)

# rownameline is a boolean indicating whether to separate the row
# names (if present) and the actual data columns by a vertical line.

# colnameline is a boolean indicating whether to separate the column
# names (if present) and the next row by a horizontal line.  By
# default, do this

# borderlines is a 4-tuple ( l, r, t, b ) of booleans indicating
# whether the top, bottom, left, and right borders (respectively) of
# the table should be delimited by lines.  This is used to set the
# values of leftline, rightline, topline, and bottomline, and is
# ignored when they are set separately.

# leftline is a boolean indicating whether a vertical line should be
# drawn on the left side of the table.

# rightline is a boolean indicating whether a vertical line should be
# drawn on the right side of the table.

# topline is a boolean indicating whether a horizontal line should be
# drawn at the top of the table.

# bottomline is a boolean indicating whether a horizontal line should
# be drawn at the bottom of the table.

# na.sym is a singleton which will be used to designate NA values in
# the table.  Common values might be 'NA', '*', or even '' to leave
# the entry blank.

# allow.special is a boolean indicating whether values in the table
# containing special latex characters will be transformed to protect
# the specials.  If allow.special=TRUE, nothing is done.  If FALSE,
# they are escaped.

# tabularpkg is a singleton which lists the name of the environment
# that will be used to enclose the table.  By default, this is the
# 'tabular' environment, however, for particularly long tables, the
# 'longtable' environment provides its own page breaks.  It requires
# that the including source file use the longtable package.

# center is a boolean indicating whether the table is to be wrapped in
# a center environment.

# landscape is a boolean indicating whether the table is to be wrapped
# in a landscape environment.  Note that this requires use of the
# lscape package in the latex source.

# prematter is a list of strings of latex that will be injected into
# the preamble (of a standalone document) right before the
# begin-document.

# frontmatter is a list of strings of latex that will be injected into
# the latex after any centering or landscaping but before tabulating.
# It can be used to adjust the font size and add a title.

# backmatter, same as frontmatter, but at the back

# headmatter, same as frontmatter, but appearing after the column title
# and column names have been printed (usually, if set, it would be set
# to "\\endhead" in a longtable)

# rowmatter, a list with length equal to the number of rows,
# specifying what to print at the end of the row.  Down deep, this is
# how rowlines is implemented (i.e., each rowline is turned into an
# \\hline in the rowmatter.  Note that, unlike the other *matter
# options, the list is not concatenated together.  Each element of the
# list is a string, and is printed after a different row.

# quiet is a boolean indicating whether to print that a file was
# printed.  We want to be quiet inside Sweave.

# append is a boolean indicating whether an existing file is to be
# overwritten (when false) or simply appended with the table data
# (when true).

# standalone is a boolean indicating that header and tail information
# are to be added to ensure that the tex file can be compiled directly
# by latex (rather than included as part of another document)

# standalone.top is a boolean indicating to print header information

# standalone.bot is a boolean indicating to print bottom/tail
# information, and together with append and standalone.top, this can
# be used to print multiple tables to a single standalone file.

# do.texdo is a boolean indicating whether to extract the file prefix
# (e.g., remove the .tex from the filename) and to run the texdo
# command on the file prefix.  Note that this requires the texdo
# command be installed (it's a script that Kevin wrote).

latex.data.frame <- function( X, file,

                             rownames    = escape.latex(dimnames(X)[[1]]),
                             colnames    = escape.latex(dimnames(X)[[2]]),

                             coltitle    = NULL,
                             
                             tablesym    = 'c',
                             tablesyms   = rep(tablesym, ncol(X)),
                             rowname.tablesym  = 'l',

                             colformats.num  = '%g',
                             colformats.int  = '%d',
                             colformats.char = '%s',

                             colformats    = colformats.def,
                             rownameformat = '%s',

                             rowlines      = c(),
                             collines      = c(),

                             rownameline     = TRUE,
                             colnameline     = TRUE,
                             coltitleline    = is.null(colnames),

                             coltitlelinestr = '\\hline',
                             
                             borderlines    = c(l=0,r=0,t=0,b=0),
                             leftline       = borderlines['l'],
                             rightline      = borderlines['r'],
                             topline        = borderlines['t'],
                             bottomline     = borderlines['b'],

                             hirows        = NULL,
                             hicols        = NULL,
                             himtx         = NULL,
                             hicmd         = 'textbf',
                             
                             na.sym        = '*',
                             allow.special = FALSE,
                             
                             tabularpkg    = 'tabular',
                             center        = TRUE,
                             landscape     = FALSE,

                             prematter     = NULL,
                             frontmatter   = NULL,
                             backmatter    = NULL,
                             headmatter    = NULL,
                             rowmatter     = rep( "", nr ),
                             
                             quiet         = FALSE,
                             append        = FALSE,
                             standalone    = FALSE,
                             standalone.top = standalone,
                             standalone.bot = standalone,

                             do.texdo       = FALSE ) {

  # First, derive some useful constants from the arguments
  do.coltitle <- !is.null( coltitle );
  do.colnames <- !is.null( colnames );
  do.rownames <- !is.null( rownames );

  nr <- nrow( X );
  nc <- ncol( X );
  
  # Determine default formats for each of the columns
  
  coltypes <- sapply( X, class );

  colformatdef.map <- c( numeric  = colformats.num,
                        integer   = colformats.int,
                        logical   = colformats.char,
                        character = colformats.char,
                        factor    = colformats.char );
  colformatdef.idxs <- match( coltypes, names(colformatdef.map) );
  stopifnot( all( ! is.na( colformatdef.idxs ) ) );
  colformats.def <- colformatdef.map[ colformatdef.idxs ];

  # Go through each column of the data frame, format it, and add it to
  # the new stringified data frame

  tbl <- list();
  
  for( ci in 1:nc ) {

    coltype <- coltypes[ ci ];
    colformat <- colformats[ ci ];
    if( is.na(colformat) ) stop("missing format for [",ci,"] ",coltype);

    cvals <- X[[ ci ]];

    # Separate the values into NAs and defined values
    na.mask <- is.na( cvals );
    cvs <- cvals[ ! na.mask ];
    
    # Convert factors and logicals to characters
    if( coltype == 'factor' || coltype == 'logical' )
      cvs <- as.character( cvs );

    # Format the string as specified
    cvs <- sprintf( colformat, cvs );

    # Escape special latex characters
    if( !allow.special ) cvs <- escape.latex( cvs );
    
    # Add the NAs back in now that we've stringified
    cvstrs <- c();
    cvstrs[ ! na.mask ] <- cvs;
    cvstrs[ na.mask ] <- na.sym;
    
    tbl[[ ci ]] <- cvstrs;
    
  }
  
  # Turn the list into an official matrix of characters
  tbl <- as.matrix( data.frame( tbl ) );
  
  # If we plan to wrap any table entries in latex formatting (e.g. to
  # make it bold or to rotate it), now would be the place to apply
  # those changes directly to tbl[ i, j ];
  
  # Wrap those data cells with TRUE values in the himtx in textbf or
  # whatever the hicmd is.  First, we add rows/cols so that the matrix
  # has the same dimension as the tbl (rather than the data frame).
  if( ! is.null(himtx) ) {
    himtx[ is.na(himtx) ] <- FALSE;
    tbl[ himtx ] <- sprintf( "\\%s{%s}", hicmd, tbl[ himtx ] );
  }

  # Do likewise with highlighted rows and columns
  if( !is.null( hirows ) ) {
    tbl[ hirows, ] <- sprintf( "\\%s{%s}", hicmd, tbl[ hirows, ] );
    rownames[ hirows ] <- sprintf( "\\%s{%s}", hicmd, rownames[ hirows ] );
  }

  if( !is.null( hicols ) ) {
    tbl[ , hicols ] <- sprintf( "\\%s{%s}", hicmd, tbl[ , hicols ] );
    colnames[ hicols ] <- sprintf( "\\%s{%s}", hicmd, colnames[ hicols ] );
  }
  
  # Create the table format string (that fills in the blank in
  # {tabular}{_____}).  Compose it symbol-by-symbol.

  tsyms <- c();

  # Add leftmost vline (if present)
  if( leftline ) tsyms <- c( tsyms, rep('|', leftline) );

  # Add rowname symbol and rowname vlines (if present)
  if( do.rownames ) {
    tsyms <- c( tsyms, rowname.tablesym );
    if( rownameline ) tsyms <- c( tsyms, rep('|',rownameline) );
  }

  # Add each column symbol and vlines (if present)
  for( ci in 1:nc ) {
    tsyms <- c( tsyms, tablesyms[ci] );

    cbn <- sum( ci == collines );
    if( cbn ) tsyms <- c( tsyms, rep('|',cbn) );
  }

  # Add rightmost vline (if present)
  if( rightline ) tsyms <- c( tsyms, rep('|', rightline) );

  # Collapse symbol list to a string
  tablestr <- paste( tsyms, collapse="" );

  # Merge rowlines into the rowmatter
  for( ri in rowlines )
    rowmatter[ri] <- paste(rowmatter[ri],"\\hline",sep="");
  
  # Redirect output to the specified file
  sink( file, type='output', append=append );

  # If we're stand alone, print the preamble
  if( standalone.top ) {
    printf("\\documentclass{article}\n");
    printf("\n");
    printf("\\usepackage{rotating}\n");
    printf("\\usepackage[textheight=9.5in,textwidth=7in]{geometry}\n");
    if( landscape ) printf("\\usepackage{lscape}\n");
    if( tabularpkg == 'longtable' ) printf("\\usepackage{longtable}\n");
    printf("\\pagestyle{empty}\n");
    printf("\n");
    if( ! is.null(prematter) ) cat(paste(prematter),sep="",collapse="\n");
    printf("\\begin{document}\n");
    printf("\n");
  }
  
  # Print the header information for the table
  printf("%% The following table was generated by latex.data.frame in R\n");
  if( landscape ) printf("\\landscape\n");
  if( center ) printf("\\begin{center}\n");
  if( ! is.null(frontmatter) ) cat(paste(frontmatter),sep="",collapse="\n");
  printf("\\begin{%s}{%s}\n", tabularpkg, tablestr);

  # Print the top line of the table
  if( topline ) printf( "\\hline\n" );

  # Print the column title (if present)
  if( do.coltitle ) {

    if( ! is.list( coltitle ) ) {
      # Convert the simple singleton coltitle into more general list form
      coltitle <- list( c( nc, 'c', coltitle ) );
    }

    cttostr <- function(ct)
      sprintf("\\multicolumn{%s}{%s}{%s}", ct[1], ct[2], ct[3]);

    coltitlestr <- paste( sapply( coltitle, cttostr ), collapse=" & \n" );

    if( do.rownames ) printf( " & " );
    printf("%s\n", coltitlestr);
    printf("\\\\");
    if( coltitleline ) {
      printf("%s",coltitlelinestr);
    }
    printf("\n");
  }
  
  # Print the column names (if present)
  if( do.colnames ) {
    if( do.rownames ) printf( " & " );
    for( ci in 1:nc ) {
      printf(" %s\n", colnames[ci]);
      if( ci < nc ) printf( " & " );
    }
    printf("\\\\");
  }

  if( colnameline ) for( i in 1:colnameline ) printf( "\\hline" );
  if( ! is.null(headmatter) ) cat(paste(headmatter),sep="",collapse="\n");
  printf("\n");
  
  # Print each row of the table
  for( ri in 1:nr ) {
    if( do.rownames ) printf( "%s & \n", rownames[ri] );
    for( ci in 1:nc ) {
      printf(" %s\n", tbl[ ri, ci ] );
      if( ci < nc ) printf( " & " );
    }
    printf("\\\\");

    # Print the hlines (if present)
    printf("%s\n",rowmatter[ri]);
  }
    
  # Print the bottom hline (if present)
  if( bottomline ) for( i in 1:bottomline ) printf( "\\hline" );
  
  # Print the wrapup material
  printf("\\end{%s}\n", tabularpkg);
  if( ! is.null(backmatter) ) cat(paste(backmatter),sep="",collapse="\n");
  if( center ) printf("\\end{center}\n");

  # Print the standalone end-material
  if( standalone.bot ) printf("\\end{document}\n");
  
  # Cancel output redirection
  sink( NULL );
  
  if( !append && !quiet ) cat( file, "printed\n" );

  # If do.texdo then do some filename parsing and run the command.
  if( do.texdo ) {

    dirs <- strsplit( file, '/' )[[1]];
    stopifnot( length(dirs) > 0 );
    
    filename <- dirs[ length(dirs) ];
    dirs <- dirs[ -length(dirs) ];

    filepfx <- sub( '\\.tex$', '', filename );

    if( length( dirs ) ) {
      cmd <- sprintf("cd %s; texdo %s", paste( dirs, collapse='/' ), filepfx)
    } else {
      cmd <- sprintf("texdo %s", filepfx);
    }
    
    cat(cmd,"\n");
    system(cmd);
  }
}


###########################################################################
# Wrapper functions
###########################################################################

# This function wrapper organizes a quick-and-dirty method of
# splitting too-wide and too-long tables into multiple pages.  The
# too-long-ness is handled by using the 'longtable' tabular package by
# default.  The too-wide-ness is handled by specifying a maximum
# number of columns per page, and a set of columns that must appear on
# every page.  The latex files MUST be standalone.

latex.data.frame.multipage <- function( X, file,

                                       colmax    = 7,
                                       colall    = c(),
                                       
                                       colnames  =
                                       escape.latex(dimnames(X)[[2]]),
           
                                       coltitle  = NULL,
                                       
                                       tablesym  = 'c',
                                       tablesyms = rep(tablesym,ncol(X)),

                                       collines  = c(),

                                       coltitleline = is.null(colnames),

                                       landscape = FALSE,
                                       do.texdo  = TRUE,
                                       ... ) {

  # How many non-preserved columns are there in the data frame
  colrest <- setdiff( 1:ncol(X), colall );

  # How many non-preserved columns will go into each page
  colrestmax <- colmax - length(colall);
  stopifnot( colrestmax > 0 );

  # How many pages of columns will we have
  pagen <- ceiling( length(colrest) / colrestmax );

  for( i in 1:pagen ) {

    # Identify the index into colrest of the 1st and last column of this page
    crminidx <- (i-1)*colrestmax + 1;
    crmaxidx <- min( i*colrestmax, length(colrest) );
    stopifnot( crminidx <= crmaxidx );
    
    # Extract the indices of all the columns that will be printed on page
    cidxs <- sort( c(colall, colrest[ crminidx:crmaxidx ]) );

    # Formulate the modified arguments for the latex.data.frame function
    colnames1 <- colnames[ cidxs ];
    tablesyms1 <- tablesyms[ cidxs ];
    collines1 <- intersect( collines, cidxs );

    headmatter <- if( ! is.null(colnames) ) '\\endhead' else NULL;
    
    standalone.top <- i==1;
    standalone.bot <- i==pagen;
    append <- i > 1;
    do.texdo1 <- do.texdo && i == pagen;
    landscape1 <- landscape && i == 1;

    X1 <- X[ , cidxs, drop=FALSE ];
    
    # We can't deal with parsing column titles at this point
    if( ! is.null( coltitle ) )
      stop("Cannot yet deal with splitting column titles over multiple pages");

    latex.data.frame( X1, file,
                     colnames     = colnames1,
                     tablesyms    = tablesyms1,
                     collines     = collines1,
                     
                     coltitle     = coltitle,
                     coltitleline = coltitleline,

                     tabularpkg   = 'longtable',
                     headmatter   = headmatter,
                     
                     standalone.top = standalone.top,
                     standalone.bot = standalone.bot,
                     append         = append,
                     
                     landscape      = landscape1,
                     do.texdo       = do.texdo1,

                     ... );
  }
}



# First convert a vector into a matrix (wrapping it to 8-wide as
# needed) and then feed the result to latex.data.frame.

latex.vector <- function( v, file,
                         ncol.max=8,
                         ncol=min( ncol.max, length(v) ),
                         byrow=TRUE,
                         rownames=NULL,
                         colnames=NULL,
                         na.blank=TRUE,
                         first.hline.double=FALSE,
                         ... ) {

  k <- length( v );
  length( v ) <- ncol * ceiling( k/ncol );
  tbl <- matrix( v, ncol=ncol, byrow=byrow );
  print( tbl );
  latex.table( tbl, file,
              split.collabel=NULL,
              first.hline.double=first.hline.double,
              rownames=rownames,
              colnames=colnames,
              na.blank=na.blank,
              ... );
}

###########################################################################

# Cast a matrix to a data frame and then run latex.data.frame

latex.matrix <- function( mtx, file, ... ) {
  latex.data.frame( as.data.frame( mtx ), file, ... );
}

###########################################################################

# Cast a table to a data frame and then run latex.data.frame

latex.table <- function( tbl, file, ... ) {
  latex.data.frame( as.data.frame( tbl ), file, ... );
}
