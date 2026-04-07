library(RColorBrewer)
colfunc <- colorRampPalette(c("#B30E31", "#E19FAA"))




par(family="serif")


# The lancet Red?
colfunc <- colorRampPalette(c("#B30E31", "#E19FAA"))
cols <- colfunc(4)
makeTransparent = function(..., alpha=0.5) {

  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")

  alpha = floor(255*alpha)
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)

  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }

  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)

  return(newColor)

}



round.choose <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}




















# cols=NULL
# lty=NULL
# pch=NULL
# type=NULL
# lwd=NULL
# cex.pt=NULL
# cex=1
# main=NA
# xtitle=""
# ytitle=""
# plott=TRUE
# leg=TRUE
# leg.lab=NULL
# legtitle=""
# leg.pos="topright"
# export=FALSE
# directory=getwd()
# export.name="fig"
# wid=6
# len=5
# ylimit=NULL
# stderror=NULL
# invert=FALSE
# x = dta$edu4[dta$dis=="Overall CVD"]
# y = dta$HR[dta$dis=="Overall CVD"]

lineplot <- function(x, y, 
                     cols=NULL, lty=NULL, pch=NULL, type=NULL, lwd=NULL, liness="lines", alpha=0.2,
                     cex=1, cex.pt=NULL, main=NA, ylimit=NULL, stderror=NULL, errorbars="shaded", code95=3,
                     xtitle="", ytitle="", xticklab=TRUE, yticklab=TRUE,
                     plott=TRUE, leg=TRUE, leg.lab=NULL, legtitle="", leg.pos="topright",
                     vabline=NULL, habline=NULL, col_abline="#a00000", lty_abline=2,
                     invert=FALSE,
                     mar=c(1,1,0,1), oma=c(2,2,1,0),
                     export=FALSE, directory=getwd(), export.name="fig", wid=6, len=5) {
  
  # IF GRAPH INVERSION IS REQUIRED --------------------------------------------------------------
  if(invert==TRUE) {
    X = y
    Y = x
    
    x = X
    y = Y
  }
  
  # IF X DOES NOT HAVE SAME NUMBER OF COLUMNS AS Y ----------------------------------------------
  if(is.null(ncol(x))) {
    x <- matrix(rep(x,ncol(y)), ncol = ncol(y), nrow = length(x))
  }
  
  # COVNERT Y & X INTO MATRIX TO ACCOMMODATE MORE THAN ONE LINE ---------------------------------
  y <- y %>% as.matrix %>% apply(2, as.numeric)
  x <- x %>% as.matrix %>% apply(2, as.numeric)
  
  
  # IF PARAMETERS UNSPECIFIED -------------------------------------------------------------------
  if(is.null(cols)){
    cols <- brewer.pal(ncol(y), "Dark2")
    # cols <- c(
    #   "#AD002AFF", "#0099B4FF", "#42B540FF", "#925E9FFF", "#00468BFF", "#ED0000FF", "#1B1919FF", "#FDAF91FF", "#ADB6B6FF"
    # )[1:ncol(y)]
  }
  
  if(is.null(lty)){
    lty <- rep(1,ncol(y))
  }
  
  if(is.null(pch)){
    pch <- rep(NULL,ncol(y))
  }
  
  if(is.null(type)){
    type <- rep("b",ncol(y))
  }
  
  if(is.null(lwd)){
    lwd <- rep(1,ncol(y))
  }
  
  if(is.null(cex.pt)){
    # cex.pt <- rep(1,ncol(y))
    cex.pt <- lapply(rep(1,ncol(y)),function(x)x)
  }
  
  
  # IF PARAMETER LENGTH DOES NOT MATCH NUMBER OF LINES ------------------------------------------
  if(length(cols)<ncol(y)){
    cols <- c(cols,rep(cols[1],ncol(y)-length(cols)))
  }
  
  if(length(lty)<ncol(y)){
    lty <- c(lty,rep(lty[1],ncol(y)-length(lty)))
  }
  
  if(length(pch)<ncol(y)){
    pch <- c(pch,rep(pch[1],ncol(y)-length(pch)))
  }
  
  if(length(type)<ncol(y)){
    type <- c(type,rep(type[1],ncol(y)-length(type)))
  }
  
  if(length(lwd)<ncol(y)){
    lwd <- c(lwd,rep(lwd[1],ncol(y)-length(lwd)))
  }
  
  if(length(cex.pt)<ncol(y)){
    cex.pt <- c(cex.pt,rep(cex.pt[1],ncol(y)-length(cex.pt)))
  }
  
  
  
  
  # EXPORT THE PLOT -----------------------------------------------------------------------------
  if(export==TRUE){
    tiff(paste0(directory,"/",export.name,".tif"),
       width = wid, height = len, units = 'in', res = 300, compression = "lzw")
  }
  
  # SET THE GRAPH PARAMETERS --------------------------------------------------------------------
  # opar <- par()
  par(family="serif") # Times New Roman 
  par(oma = oma,
      mar = mar,
      mgp = c(1,0.5,0.03),  # distance form graph area to axis title, label, line respectively
      cex = 1*cex,
      cex.axis = 0.8*cex,
      cex.main = 0.8*cex,
      lwd = 0.8)
  
  # WORK OUT MIN & MAX VALUES IN X & Y (Y PRESENTED IN A MAX OF TWO DIGITS) ---------------------
  xlimit <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  
  if(is.null(stderror)) {yy=y} else {yy <- cbind(y,stderror)}
  if(is.null(ylimit)){
    if(min(yy, na.rm = TRUE) <10) {ya=0} else {ya <- (min(yy, na.rm = TRUE) %>% log10 %>% floor) + 1} # number of digits
    scalefactor <- 0
    if(ya>3) {
      scalefactor <- 10^(ya-2)
      y  <- y/scalefactor
      yy <- yy/scalefactor
      if(is.null(stderror)) {stderror=NULL} else {stderror = stderror/scalefactor}
    }
    yby <- ((max(yy, na.rm = TRUE) - min(yy, na.rm = TRUE)) / 10)
    yfac <- (yby %>% log10 %>% floor) + 1
    yfac <- (10^yfac) / 10
    # yby <- yby %>% round.choose(yfac,1)
    ymin <- min(yy, na.rm = TRUE) %>% round.choose(yfac,0)
    ymax <- (max(yy, na.rm = TRUE) + (0.05*max(yy, na.rm = TRUE)))  %>% round.choose(yfac,1)
    ylimit <- c(ymin, ymax)
  } else {scalefactor=0}
  
  
  # IF GRAPH INVERSION IS REQUIRED --------------------------------------------------------------
  if(invert==TRUE) {
    XLIMIT = ylimit
    YLIMIT = xlimit
    xlimit = XLIMIT
    ylimit = YLIMIT
    
    X = y
    Y = x
    x = X
    y = Y
  }
  
  # BLANK PLOT ----------------------------------------------------------------------------------
  plot(x=xlimit,
       y=ylimit,
       type = "n",
       bty = "n",
       xlab = NA,
       ylab = NA,
       frame.plot = FALSE,
       xaxt = "n",
       yaxt = "n"
       )
  
  # ABLINES -------------------------------------------------------------------------------------
  if(!is.null(vabline)) {
    abline(v=vabline, col=col_abline, lty=lty_abline)
  }
  
  if(!is.null(habline)) {
    abline(h=habline, col=col_abline, lty=lty_abline)
  }
  
  # AXES & LABELS -------------------------------------------------------------------------------
  if(xticklab==TRUE){
    axis(1, tcl=-0.3, col=NA, col.ticks=1, cex=cex)
  }
  lines(x=par("usr")[1:2], y=rep(par("usr")[3],2), lwd=3)
  mtext(xtitle, side=1, outer = TRUE, cex=0.90*cex, font=2, line=0.7)
  
  if(yticklab==TRUE){
    axis(2, tcl=-0.3, col=NA, col.ticks=1, las=2)
  }
  lines(x=rep(par("usr")[1],2), y=par("usr")[3:4], lwd=3)
  
  if(scalefactor==0) {ytitle=ytitle} else {
    if(scalefactor==1000) {ytitle=paste0(ytitle, " (thousands)")}
    if(scalefactor==1000000) {ytitle=paste0(ytitle, " (millions)")} else {
      ytitle=paste0(ytitle, " (x", scalefactor %>% commas, ")")
    }
    }
  mtext(ytitle, side=2, outer = TRUE, cex=0.90*cex, font=2, line=0.7, adj=0.59)
  
  
  # PLOT DATA -----------------------------------------------------------------------------------
  if(plott==TRUE){
    for(ln in 1:ncol(y)) {
      if(!is.null(stderror)) {
        xx <- x
        xx=cbind(x[,ln]+(xx/20)[2], x[,ln]-(xx/20)[2]) %>% as.data.frame
        if(errorbars!="shaded"){
          arrows(x0=xx[,ln], y0=y[,ln]-(1.96*stderror[,ln]), x1=xx[,ln], y1=y[,ln]+(1.96*stderror[,ln]), code=code95, angle=90, length=0.03, col=cols[ln]) #col="gray75")
        } else {
          polygon(
            x=c(xx[,ln], rev(xx[,ln])),
            y=c(y[,ln]-(1.96*stderror[,ln]), rev(y[,ln]+(1.96*stderror[,ln]))),
            col = cols[ln] %>% makeTransparent(alpha = alpha),
            border = NA
          )
        }
        lines(x=xx[,ln], y=y[,ln], lty=lty[ln], col=cols[ln], pch=pch[ln], type=type[ln], lwd=lwd[ln], cex=cex.pt[[ln]])
      } else {
        
        if(liness=="lines") {
          lines(x=x[,ln], y=y[,ln], lty=lty[ln], col=cols[ln], pch=pch[ln], type=type[ln], lwd=lwd[ln], cex=cex.pt[[ln]])
        }
        
        
        if(liness=="poly") {
          lines(x=x[,ln], y=y[,ln], lty=lty[ln], col=cols[ln], pch=pch[ln], type=type[ln], lwd=lwd[ln], cex=cex.pt[[ln]])
          polygon(
            x=c(x[,ln], rev(x[,ln])),
            y=c(y[,ln], rep(0,length(y[,ln]))),
            col = cols[ln] %>% makeTransparent(alpha = alpha),
            border = NA
          )
        }
        
        
        if(liness=="bar") {
          for(bbb in seq_along(x[,ln])) {
            polygon(
              x=c(bbb-0.45, bbb+0.45, bbb+0.45, bbb-0.45),
              y=c(0,0, y[bbb,ln], y[bbb,ln]),
              col = cols[ln] %>% makeTransparent(alpha = alpha),
              border = cols[ln]
            )
            lines(x=par("usr")[1:2], y=rep(par("usr")[3],2), lwd=2)
          }
        }
        
        
        
      }
    }
  }
  
  # TITLE ---------------------------------------------------------------------------------------
  if(!is.na(main)){
    mtext(paste0(" ",main), adj = 0, cex=0.95*cex, font=2)
  }
  
  # LEGEND ---------------------------------------------------------------------------------------
  if(leg==TRUE) {
    if(is.null(leg.lab)) {leg.lab=colnames(y)}
    # any data in top right corner?
    # xleg <- (par("usr")[1]:par("usr")[2]) %>% summary %>% .[5] %>% as.vector
    # xleg
    # yleg <- (par("usr")[3]:par("usr")[4]) %>% summary %>% .[5] %>% as.vector
    # y[]
    # 
    # 
    # 
    # leg.position
    legend(leg.pos,
           legend = leg.lab,
           title = legtitle,
           bty="n", lty=lty, pch=pch, lwd=lwd, cex=cex,
           col=cols
           
    )
  }
  
  
  
  
  
  if(export==TRUE){
    dev.off()
  }
  # par(opar)
}
















