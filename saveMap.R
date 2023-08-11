saveMap <-
function(dat, grp, smpPath, saveFinal=TRUE) {
  
  coast <- read.shapefile(file.path(system.file("HF_maps/ne_10m_coastline", 
                                                package="EcoTransLearn"), 
                                    "ne_10m_coastline"))
  land <-read.shapefile(file.path(system.file("HF_maps/ne_10m_land", 
                                              package="EcoTransLearn"), 
                                  "ne_10m_land"))
  border <- read.shapefile(file.path(system.file("HF_maps/Borders", 
                                                 package="EcoTransLearn"), 
                                     "ne_10m_admin_0_boundary_lines_land"))
  xlim=c(min(dat$Longitude)-1,max(dat$Longitude)+1)
  ylim=c(min(dat$Latitude)-1,max(dat$Latitude)+1)
  
  for (i in grp) {
    cat("\nProjecting", i, "abundances on map...")
    
    X11()
    par(mar=c(1.3,1.3,1,0.5))
    par(tcl=-0.5)
    par(oma=c(2.4,2.8,1,0))
    basemap(xlim, ylim,  xaxt="n",yaxt="n", ylab="",xlab="",main="", bg="white")
    draw.shape(coast, type="line",col="black" )
    draw.shape(land, type="poly", col="grey60")
    draw.shape(border, type="line", col="black")
    points(x=dat$Longitude, y=dat$Latitude, col=alpha("orange", 0.4), 
           pch=19, cex=dat$CountVol[which(dat$Group==i)]/1000)
    axis(side=1, cex.axis=1)
    axis(side=2, cex.axis=1)
    mtext(i, side=3, font=2, cex=0.8, adj=0, line=0.15, col="black")
    northarrow(loc=c(par("xaxp")[2]-2, par("yaxp")[1])+0.25, 
               size=0.34, cex=0.9)
    
    if (isTRUE(saveFinal)) {
      png(file.path(smpPath, paste(basename(smpPath), "_", i, ".png", sep="")), 
          2000, 2000, res=300)
      par(mar=c(1.3,1.3,1,0.5))
      par(tcl=-0.5)
      par(oma=c(2.4,2.8,1,0))
      basemap(xlim, ylim,  xaxt="n",yaxt="n", ylab="",xlab="",
              main="", bg="white")
      draw.shape(coast, type="line",col="black" )
      draw.shape(land, type="poly", col="grey60")
      draw.shape(border, type="line", col="black")
      points(x=dat$Longitude, y=dat$Latitude, col=alpha("orange", 0.4), 
             pch=19, cex=dat$CountVol[which(dat$Group==i)]/1000)
      axis(side=1, cex.axis=1)
      axis(side=2, cex.axis=1)
      mtext(i, side=3, font=2, cex=0.8, adj=0, 
            line=0.15, col="black")
      northarrow(loc=c(par("xaxp")[2]-2, par("yaxp")[1])+0.25, 
                 size=0.34, cex=0.9)
      dev.off()
    }
    cat("\nDone!")
  }
}