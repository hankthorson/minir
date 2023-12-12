
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
legend_bottom <- function() {  theme(legend.position="bottom") }

#---------------------------------------
rename <- function(df, ...) { names(df) <- unlist(list(...)); return(df) }

#---------------------------------------
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); }
scale_cy_c1_e3 <- function() { scale_y_continuous(labels=fmt_c1_e3); }
scale_cy_c1 <- function() { scale_y_continuous(labels=fmt_c1); }

#---------------------------------------
scale_fill_AVA <- function() { scale_fill_manual( values=list(amount='red', volume='green', asp='blue', void='cyan') ) }

#---------------------------------------
scale_fill_PVM <- function() { scale_fill_manual(values=list(major="#FFFF00", price="#707070", vol="#A0A0A0", mix="#E0E0E0") ) }


#---------------------------------------
order_vals <- function(vals) {
    sel <- order(vals);
    return(vals[sel]);
}


#---------------------------------------
write.html <- function(src=file.path(Sys.getenv("USERPROFILE"), "Desktop/book1/book1.dget")) {
    wbk <- dget(src);

    tar <- file.path(dirname(src), "book1.html");
    sink(tar);
    for(nk in order_vals(names(wbk))) {
        fk <- wbk[[nk]];
        cat(sprintf("<p>%s</p>\n", nk));
        cat(sprintf("<img src='images/%s' class='cran-image'>\n", basename(fk)));
    }

    sink(NULL);

    grid.table(data.frame(path=tar));
}

#---------------------------------------
write.page <- function(expr, wd=640, hg=400, tar=file.path(Sys.getenv("USERPROFILE"), "Desktop/book1/book1.dget"), tag="figure1", dual=TRUE) {
    if( file.exists(tar) ) { wbk <- dget(tar); }
    else { wbk <- list(); }

    tar1 <- file.path(dirname(tar), sprintf("images/%s.png", tag));
    dir.create(dirname(tar1), showWarnings=FALSE, recursive=TRUE);
    png(file=tar1, width=wd, height=hg); expr(); muted <- dev.off();    

    wbk[[tag]] <- tar1;
    dir.create(dirname(tar), showWarnings=FALSE, recursive=TRUE);
    dput(wbk, tar);

    if(dual) expr();
}

#---------------------------------------
ggplot_water <- function(gdf=dataset, fill_seq=c("major", "price", "vol", "mix"), scy=scale_cy_c1_e3, text_col='black', 
text_fmt=fmt_c1_e3, text_ang=0, major="major", width=0.45, legend=FALSE) {
    gdf$fill <- factor(gdf$fill, levels=fill_seq);
    gdf <- gdf[order(gdf$FY, gdf$fill), ];
    gdf$FY_long <- factor(gdf$FY_long, levels=gdf$FY_long);
    
    s <- 0;
    for(k in 1:nrow(gdf)) {
        if(gdf$fill[k] == major) { gdf[k, "y1"] <- 0; gdf[k, "y2"] <- s <- gdf$amt[k]; gdf[k, "y3"] <- s/2;  }
        else  { gdf[k, "y1"] <- s; s <- s + gdf$amt[k]; gdf[k, "y2"] <-s; gdf[k, "y3"] <- min(gdf[k, "y1"], gdf[k, "y2"]); }
       
    }

    gdf$x1 <- as.integer(gdf$FY_long) - width;
    gdf$x2 <- gdf$x1 + 2*width;

    g <- ggplot(gdf) + geom_text(aes(x=FY_long, y=0, label=''), show.legend=FALSE);
    g <- g + scale_x_discrete(labels =gdf$FY_short );
    if( !is.null(scy) ) g <- g + scy();

    g <- g + geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=legend);
    g <- g + geom_text(aes(x=(x1+x2)/2, y=y3, label=text_fmt(amt), group=fill), vjust=+1.5, color=text_col, angle=text_ang, show.legend=FALSE);
    g <- g + no_axis_titles() + legend_bottom() + labs(fill="");
 
    print(g);
#    grid.table(gdf);
}

#---------------------------------------
ggplot_trend3 <- function(U1, cols, fmt=fmt_c1, scy=scale_cy_c1, snz=scale_fill_AVA, legend=TRUE) {
    gdf <- data.frame();
    for(nk in names(cols)) gdf <- rbind(gdf, data.frame(xx=U1, fill=nk, yy=cols[[nk]]));
    gdf$fill <- factor(gdf$fill, levels=names(cols));
    
    g <- ggplot(gdf) + geom_bar(aes(x=xx, y=yy, fill=fill), color='white', stat="identity", position="dodge2", show.legend=legend);
    g <- g + geom_text(aes(x=xx, y=yy/2, label=fmt(yy), group=fill), position=position_dodge(width=0.9), show.legend=FALSE);
    g <- g + no_axis_titles() + legend_bottom() + labs(fill="");
    if( !is.null(scy) ) g <- g + scy();
    if( !is.null(snz) ) g <- g + snz();
    return(g);
}


#---------------------------------------
ggplot_trend <- function(U1, U2, U3="void", U4="void", fmt=fmt_c1_e3, scy=scale_cy_c1_e3, snz=scale_fill_AVA) {
    gdf <- data.frame(U1=U1, U2=U2, U3=U3, U4=U4);
    g <- ggplot(gdf) + geom_bar(aes(x=U1, y=U2, fill=U3), color='white', stat="identity", show.legend=FALSE);
    g <- g + geom_text(aes(x=U1, y=U2/2, label=fmt(U2)), show.legend=FALSE);
    g <- g + no_axis_titles();
    if( !is.null(scy) ) g <- g + scy();
    if( !is.null(snz) ) g <- g + snz();
    return(g);
}


#---------------------------------------
card13 <- c("A","2","3","4","5","6","7","8","9","10","J", "Q", "K");
card52 <- c(paste(card13, "SP"), paste(card13, "CL"), paste(card13, "DM"), paste(card13, "HT"));
card12 <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec");

#---------------------------------------
ceil_AZ <- function(U1) { sapply(64 + ceiling(U1 * 26), FUN=intToUtf8) }
ceil_JQK <- function(U1) {  card52[ ceiling(U1 * 52) ]; }
ceil_JD <- function(U1) { card12[ ceiling(U1 * 12) ]; }

#---------------------------------------
floor6 <- function(U1, mul=6, fmt="F%d") { sprintf(fmt, floor(U1 * mul)); }

#---------------------------------------
ceil6 <- function(U1, mul=6, fmt="C%d") { sprintf(fmt, ceiling(U1 * mul)); }


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
ggplot_scatter <- function(U1 = NULL, U2 = NULL, data=NULL, lab='NA', fixed=FALSE) {
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
