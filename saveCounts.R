saveCounts <-
function(dat, grp, smpPath, saveFinal=TRUE) {
  
  for (i in grp) {
    cat("\nPlotting", i, "counts...")
    
    X11()
    datTemp <- dat[which(dat$Group==i), ]
    datTemp <- datTemp[order(datTemp$Date), ]
    plot(datTemp$Date, datTemp$CountVol, type="l", col="blue",
         xlab="", ylab="Counts", main=i, lwd=2)

    if (isTRUE(saveFinal)) {
      png(file.path(smpPath, paste(basename(smpPath), "_", i, ".png", sep="")), 
          width=1280, height=720)
      plot(datTemp$Date, datTemp$CountVol, type="l", col="blue",
           xlab="", ylab="Counts", main=i, lwd=2)
      dev.off()
    }
    cat("\nDone!")
  }
}
