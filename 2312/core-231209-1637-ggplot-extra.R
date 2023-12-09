
#---------------------------------------
USER_HOME <- Sys.getenv("USERPROFILE");
EXTRA_LIB <- file.path(Sys.getenv('USERPROFILE'), "RscriptExtra"); 
CRAN_REPOS <- 'http://cran.us.r-project.org';
dir.create(EXTRA_LIB, showWarnings=FALSE, recursive=TRUE);
.libPaths( c(EXTRA_LIB, .libPaths() ) );

#---------------------------------------
ipack <- rownames( installed.packages() );
dpack <- c("ggplot2", "gridExtra", "digest"); 
for(pk in dpack) if( !(pk %in% ipack) ) install.packages(pk, repos=CRAN_REPOS);

#---------------------------------------
library(ggplot2);
library(gridExtra);
library(digest);


#---------------------------------------
runif_hashed <- function(N, app="abcd") {
   seq <- sapply(paste(1:N, app), digest, algo="md5");
   seq <- as.double(paste0("0.", gsub("\\D+", "", seq)));
   return(seq);
}  

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
   hf <- data.frame(panel=tt);
   df <- head(df, top);
   g <- ggplot(hf) + facet_wrap(ncol=1, panel ~ .) + annotation_custom(tableGrob(df));
#   g <- g + theme_void();
   return(g);
}

#---------------------------------------
rtrig <- function(sx=2, sy=1, alpha = pi/4) {
    sina <- sin(alpha); cosa <- cos(alpha);
    mat <- c(sx*cosa, -sy*sina, sx*sina, sy*cosa);
    return( matrix(data=mat, ncol=2) );
}


#---------------------------------------
rlatlong <- function(df = NULL, rot = rtrig(), org = c(103.8481747985502, 1.3613816232111224), N=5000) {
    if( is.null(df) ) df <- data.frame(XX=rnorm(N), YY=rnorm(N));
    if( is.double(df) ) df <- data.frame(XX=rnorm(df), YY=rnorm(df));
    res <- data.frame(as.matrix(df) %*% as.matrix(rot));
    names(res) <- names(df);
    return(res);
}


#---------------------------------------
png_close <- svg_close <- pdf_close <- function() {
  muted <- dev.off();
}

#---------------------------------------
ggplot_array <- function(...) {
  args <- list(...);

  sel <- sapply(args, FUN=function(gg) { is.ggplot(gg) });
  grobs <- args[sel];

  ncol <- args$ncol;
  if( is.null(ncol) ) ncol <- 1;

  mode <- args$mode;
  if( is.null(mode) ) mode <- "";
  if(mode == "") grid.arrange(grobs=grobs, ncol=ncol);
  if(mode == "g") return( ggplot() + annotation_custom(arrangeGrob(grobs=grobs, ncol=ncol)) );
}

#---------------------------------------
ggplot_panel <- function(tt='lorem', g=NULL) {
  ggplot(data.frame(panel=tt)) + facet_wrap(ncol=2, panel ~ .) + annotation_custom(g); 
}

#---------------------------------------
ggplot_hist <- function(vals, bins=30, lab='NA') {
  tdf <- data.frame(xx=vals);
  tt <- sprintf("rows=%d min=%f max=%f avg=%f label=%s", length(vals), min(vals), max(vals), mean(vals), lab);
  g <- ggplot(tdf) + ggtitle(tt) + xlab('') + ylab('') + geom_histogram(aes(x=xx), fill='yellow', color='white', bins=bins);
  return(g);
}


#---------------------------------------
no_axis_titles <- function() {
  theme(axis.title.x = element_blank(), axis.title.y = element_blank() );
}

#---------------------------------------
ggplot_contour <- function(gdf, xx="XX", yy="YY", legend=TRUE, thres=0.01) {
    gdf <- data.frame(XX=gdf[[xx]], YY=gdf[[yy]]);    
    ggplot(gdf, aes(x = XX, y = YY)) + geom_point() + geom_density_2d(aes(color=ifelse(..level.. <= thres, NA, ..level..)), size=1.5, show.legend=legend) + 
    labs(col='level') + coord_fixed() + no_axis_titles();
    
}

#---------------------------------------
ggplot_contour_filled <- function(gdf, xx="XX", yy="YY", legend=TRUE) {
    gdf <- data.frame(XX=gdf[[xx]], YY=gdf[[yy]]);    
    ggplot(gdf, aes(x = XX, y = YY)) + geom_point() + geom_density_2d_filled(aes(color=..level..), alpha=0.5, show.legend=legend) + coord_fixed() + no_axis_titles()
}

#---------------------------------------
ggplot_scatter <- function(U1 = NULL, U2 = NULL, data=NULL, lab='NA', fixed=TRUE) {
   if( !is.null(data) ) { U1 <- data[, 1]; U2 <- data[, 2]; }
   tdf <- data.frame(U1=U1, U2=U2);
   tdf$panel <- sprintf("rows=%d U1=[%f, %f] U2=[%f, %f] label=%s", nrow(tdf), min(U1), max(U1), min(U2), max(U2), lab);
   g <- ggplot(tdf) + facet_wrap(ncol=1, panel ~ .) + xlab('') + ylab('') + geom_point(aes(x=U1, y=U2));
   if(fixed) g <- g + coord_fixed();
   g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank() );
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

  g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank() );
  return(g);
}


#---------------------------------------
if(1>2) ggplot_array(
A=ggplot() + ggtitle("AA"), 
B=ggplot() + ggtitle("BB"), 
ncol=2);
