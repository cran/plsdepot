#'@title Plot PLS-R1 basic results
#'
#'@description
#'Plot method for objects of class \code{"plsreg1"}. This function plots either
#'the variables or the observations, on the selected components (i.e. scores). 
#'Variables are plotted inside the circle of correlations. Observations are plotted
#'on a scatter plot.
#'
#'@details
#'Variables are displayed using the correlations in \code{$cor.xyt}.
#'
#'@param x An object of class \code{"plsreg1"}.
#'@param what What to plot. Options are \code{"variables"} and \code{"observations"}.
#'@param comps An integer vector of length two to indicate which components to plot.
#'@param where Where to plot the observations. A character vector of length two to
#'indicate which components to use when plotting observations. This parameter will take
#'into account the values in \code{comps}. Possible options are:
#'\code{c("t","u")} for using x-y components, \code{c("t","t")}, for using x components,
#'and \code{c("u","u")} for using y components. Default \code{c("t","t")}.
#'@param cex Character expansion for labels and points.
#'@param col.xlabels Color for labels of X-block variables.
#'@param col.ylabel Color for labels of Y-block variables.
#'@param yfont Integer for specifying which font to use for Y-block labels.
#'See \code{font} in graphical parameters \code{\link{par}}.
#'@param pos Position for the text (see graphical paramaters \code{\link{par}}).
#'@param offset When \code{pos} is specified, this value gives the offset of the labels.
#'@param col.xarrows Color for the X-block arrows.
#'@param col.yarrows Color for the Y-block arrows.
#'@param lwd The line width of arrows.
#'@param length Length of the edges of the arrow head (in inches).
#'@param angle Angle from the shaft of the arrow to the edge of the arrow head.
#'@param col.points Color for the points when \code{what="observations"}.
#'@param pch Plotting character symbol to use (see \code{\link{par}}).
#'@param pt.bg Background (fill) color for the points given by \code{pch=21:25}.
#'@param show.names Logical indicating whether to show labels of points.
#'Only used when \code{what="observations"}.
#'@param xpd Logical for controlling clipping region of names and labels.
#'@param xlab A title for the x axis.
#'@param ylab A title for the y axis.
#'@param main Main title of the plot.
#'@param col.main Color of main title.
#'@param cex.main Character expansion of main title.
#'@param col.axis Color of axis annotations (tick marks and labels).
#'@param show.grid Logical indicating whether to show grid lines.
#'@param col.grid Color of grid lines. Only used when \code{show.grid=TRUE}.
#'@param \dots Further arguments are passed to labels or points.
#'@author Gaston Sanchez
#'@seealso \code{\link{plsreg1}}
#'@method plot plsreg1
#'@export
#'@examples
#'
#'  \dontrun{
#'  # load dataset cornell
#'  data(cornell)
#'
#'  # apply plsreg1
#'  myplsr1 = plsreg1(cornell[,1:7], cornell[,8,drop=FALSE])
#'  
#'  # plot variables (circle of correlations)
#'  plot(myplsr1, what="variables")
#'
#'  # plot observations (as points) using components (t1,u1)
#'  plot(myplsr1, what="observations", comps=c(1,1), where=c("t","u"))
#' 
#'  # plot observations with names using components (t1,u1)
#'  plot(myplsr1, what="observations", comps=c(1,1), 
#'       where=c("t","u"), show.names=TRUE)
#'
#'  # plot observations (as points) using components (t1,t2)
#'  plot(myplsr1, what="observations", comps=c(1,2), where=c("t","t"))
#'
#'  # plot observations (as points) using components (u1,u2)
#'  plot(myplsr1, what="observations", comps=c(1,2), where=c("u","u"))
#'  }
#'
plot.plsreg1 <- 
  function(x, what = "variables", comps = c(1,2), where=c("t","t"),
           cex = 1, col.xlabels = "#5592e3", col.ylabel = "#fe9429",
           yfont=2, pos=NULL, offset=0.1,
           col.xarrows = "#5b9cf255", col.yarrows = "#fe942955",
           lwd=3, length = 0, angle = 0,
           col.points = "#5592e3", pch = 21, pt.bg = "#5b9cf255",
           show.names = FALSE, xpd=TRUE,
           xlab = NULL, ylab = NULL, main = NULL, col.main="gray35", 
           cex.main=1.2, col.axis = "gray40",
           show.grid = TRUE, col.grid = "gray95", ...)
  {
    # =======================================================
    # check inputs
    # =======================================================
    if (!is.numeric(comps) || length(comps)>2)
      comps = 1:2
    # how many components
    nc = ncol(x$x.scores)
    if (!comps[1] %in% 1:nc || !(comps[2] %in% 1:nc)) 
      stop("\nInvalid vector of components")
    k1 = comps[1]
    k2 = comps[2]
    # how many variables
    p = nrow(x$cor.xyt)
    
    ## PLOT
    if (what == "variables")
    { 
      # =======================================================
      # circle of correlations
      # =======================================================
      z = seq(0, 2*pi, l=100)
      op = par(oma = rep(0,4), mar = c(4,3,3,2), pty = "s", xpd=xpd)
      plot.new()
      plot.window(xlim=c(-1.1,1.1), ylim=c(-1.1,1.1), asp=1)
      axis(side=1, labels=FALSE, lwd=0, lwd.ticks=1, col="gray75")
      mtext(seq(-1, 1, 0.5), side=1, at=seq(-1, 1, 0.5), line=0.5, 
            col=col.axis, cex=0.8)
      axis(side=2, labels=FALSE, lwd=0, lwd.ticks=1, col="gray75")
      mtext(seq(-1, 1, 0.5), side=2, at=seq(-1, 1, 0.5), line=0.7, 
            col=col.axis, cex=0.8, las=2)    
      box(col="gray70")
      if (show.grid)
        abline(h=seq(-1, 1, 0.5), v=seq(-1, 1, 0.5), col=col.grid, xpd=FALSE)
      # axis labs
      if (is.null(xlab)) xlab = paste("axis", k1)
      if (is.null(ylab)) ylab = paste("axis", k2)
      mtext(xlab, side=1, at=0, line=2.5, col=col.axis)
      mtext(ylab, side=2, at=0, line=2.5, col=col.axis)
      lines(cos(z), sin(z), lwd=2, col="gray90")
      segments(-1, 0, 1, 0, col="gray90")
      segments(0, -1, 0, 1, col="gray90")
      # arrows
      col.arrows = c(rep(col.xarrows, p-1), col.yarrows)
      arrows(x0=rep(0,p), y0=rep(0,p), x1=x$cor.xyt[,k1], y1=x$cor.xyt[,k2],
             length=length, angle=angle, lwd=lwd, col=col.arrows)
      # text
      if (is.null(pos))
      {
        neg <- x$cor.xyt[,k1] < 0
        pos = rep(4, p)
        pos[neg] = 2
      }
      col.labels = c(rep(col.xlabels, p-1), col.ylabel)
      fonts = c(rep(1,p-1), yfont)
      text(x$cor.xyt[,k1], x$cor.xyt[,k2], labels=rownames(x$cor.xyt), 
           font=fonts, col=col.labels, cex=cex, pos=pos, offset=offset, ...)
      # main title
      if (is.null(main))
        main = "Circle of Correlations"
      mtext(main, side=3, at=par("usr")[1], line=1, adj=0, cex=cex.main, col=col.main)
      par(op)
    } else {
      # =======================================================
      # observations
      # =======================================================
      # check components for where
      if (!is.character(where) || length(where)>2)
        where = c("t","t")
      waux = paste(where, collapse="")
      tmp = grep(waux, c("tt", "tu", "uu"))
      if (length(tmp) == 0)
        stop("\nInvalid argument 'where'")
      # where values
      ws = switch(as.character(tmp), 
                  '1' = cbind(x$x.scores[,k1], x$x.scores[,k2]),
                  '2' = cbind(x$x.scores[,k1], x$y.scores[,k2]),
                  '3' = cbind(x$y.scores[,k1], x$y.scores[,k2]))
      # axis labs
      axlabs = switch(as.character(tmp), 
                      '1' = c("t","t"),
                      '2' = c("t","u"),
                      '3' = c("u","u"))
      if (is.null(xlab)) 
        xlab = paste(axlabs[1], k1, sep="_")
      if (is.null(ylab)) 
        ylab = paste(axlabs[2], k2, sep="_")
      # plot
      plot(ws[,1], ws[,2], type="n", axes=FALSE,
           xlab=xlab, ylab=ylab, col.lab=col.axis)
      axis(side=1, col.axis=col.axis, col=col.axis, lwd=0.5, cex.axis=0.9)
      axis(side=2, las=2, col.axis=col.axis, col=col.axis, lwd=0.5, cex.axis=0.9)
      abline(h=0, v=0, col="gray90", lwd=2)
      box(col="gray70")
      # points
      if (!show.names)
      {
        points(ws[,1], ws[,2], col=col.points, pch=pch, bg=pt.bg,
               lwd=1.5, cex=cex)
      } else {
        text(ws[,1], ws[,2], labels=rownames(x$x.scores), 
             col=col.xlabels, cex=cex, pos=pos, offset=offset, ...)
      }
      if (is.null(main))
        main = "Map of Observations"
      mtext(main, side=3, at=par("usr")[1], line=1, adj=0, cex=cex.main, col=col.main)
    }
  }
