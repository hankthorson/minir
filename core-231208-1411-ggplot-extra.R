
#---------------------------------------
USER_HOME <- Sys.getenv("USERPROFILE");
EXTRA_LIB <- file.path(Sys.getenv('USERPROFILE'), "RscriptExtra"); 
CRAN_REPOS <- 'http://cran.us.r-project.org';
dir.create(EXTRA_LIB, showWarnings=FALSE, recursive=TRUE);
.libPaths( c(EXTRA_LIB, .libPaths() ) );

#---------------------------------------
ipack <- rownames( installed.packages() );
if( !("ggplot2" %in% ipack) ) install.packages("ggplot2", repos=CRAN_REPOS);
if( !("gridExtra" %in% ipack) ) install.packages("gridExtra", repos=CRAN_REPOS);

#---------------------------------------
library(ggplot2);
library(gridExtra);

#---------------------------------------
month_diff <- function(sd, ed) {
    sd <- as.POSIXlt(sd);
    ed <- as.POSIXlt(ed);
    d <- 12 * (ed$year - sd$year) + (ed$mon - sd$mon);
    return(d);
}

#---------------------------------------
ggplot_head <- function(df, top=7, lab='NA') {
   tt <- sprintf("rows=%d cols=%d label=%s", nrow(df), ncol(df), lab);
   df <- head(df, top);
   g <- ggplot() + ggtitle(tt) + annotation_custom(tableGrob(df));
   g <- g + theme_void();
   return(g);
}


#---------------------------------------
png_close <- function() {
  muted <- dev.off();
}

#---------------------------------------
ggplot_array <- function(...) {
  args <- list(...);

  sel <- sapply(args, FUN=function(gg) { is.ggplot(gg) });
  grobs <- args[sel];

  ncol <- args$ncol;
  if( is.null(ncol) ) ncol <- 1;

  grid.arrange(grobs=grobs, ncol=ncol);
}

#---------------------------------------
ggplot_hist <- function(vals, bins=30, lab='NA') {
  tdf <- data.frame(xx=vals);
  tt <- sprintf("rows=%d min=%f max=%f avg=%f label=%s", length(vals), min(vals), max(vals), mean(vals), lab);
  g <- ggplot(tdf) + ggtitle(tt) + xlab('') + ylab('') + geom_histogram(aes(x=xx), fill='yellow', color='white', bins=bins);
  return(g);
}

#---------------------------------------
ggplot_scatter <- function(U1, U2) {
  tdf <- data.frame(U1=U1, U2=U2);
  g <- ggplot(tdf) + xlab('') + ylab('') + geom_point(aes(x=U1, y=U2));
  return(g);
}

#---------------------------------------
ggplot_row <- function(...) {
  args <- list(...);

  sel <- sapply(args, FUN=function(gg) { is.ggplot(gg) });
  grobs <- args[sel];

  ncol <- args$ncol;
  if( is.null(ncol) ) ncol <- length(grobs);

  g <- ggplot() + annotation_custom(arrangeGrob(grobs=grobs, ncol=ncol));
  return(g);
}

#---------------------------------------
ggplot_table <- function(vals, base_ang=15, show_base=TRUE) {
  tdf <- data.frame(table(vals));
  tt <- sprintf("rows=%d uniq=%d", length(vals), nrow(tdf));
  g <- ggplot(tdf) + ggtitle(tt) + xlab('') + ylab('') + geom_bar(aes(x=vals, y=Freq), fill='yellow', color='white', stat="identity");
  g <- g + geom_text(aes(x=vals, y=Freq/2, label=Freq), angle=25);

  if(show_base) { g <- g + theme(axis.text.x = element_text(angle = base_ang) ) }
  else { g <- g + theme(axis.text.x = element_blank() ) }

  return(g);
}


#---------------------------------------
if(1>2) ggplot_array(
A=ggplot() + ggtitle("AA"), 
B=ggplot() + ggtitle("BB"), 
ncol=2);
