saveFinalPlot <-
function(dat, smpPath, saveFinal=TRUE) {
  
  # Absolute counts
  X11(width=1280, height=720)
  p <- ggplot(data=dat, aes(x=Date, y=CountVol, fill=Group))
  p <- p + geom_area(position='stack') + theme_bw() + theme() +
    theme(text=element_text(size=20))
  p <- p + labs(x="Time", y="Counts", fill="")
  print(p)
  
  if (isTRUE(saveFinal)) {
    png(file.path(smpPath, paste(basename(smpPath), "_COUNTS.png", sep="")),
        width=1280, height=720)
    p <- ggplot(data=dat, aes(x=Date, y=CountVol, fill=Group))
    p <- p + geom_area(position='stack') + theme_bw() + theme() +
      theme(text=element_text(size=20))
    p <- p + labs(x="Time", y="Counts", fill="")
    print(p)
    dev.off()
  }
  
  # Relative counts
  X11(width=1280, height=720)
  p <- ggplot(data=dat, aes(x=Date, y=as.numeric(Relative), fill=Group))
  p <- p + geom_area(position='stack') + theme_bw() + theme() +
    theme(text=element_text(size=20))
  p <- p + labs(x="", y="Relative Abundances (%)", fill="")
  print(p)
  
  if (isTRUE(saveFinal)) {
    png(file.path(smpPath, paste(basename(smpPath), "_RELATIVE.png", sep="")),
        width=1280, height=720)
    p <- ggplot(data=dat, aes(x=Date, y=as.numeric(Relative), fill=Group))
    p <- p + geom_area(position='stack') + theme_bw() + theme() +
      theme(text=element_text(size=20))
    p <- p + labs(x="", y="Relative Abundances (%)", fill="")
    print(p)
    dev.off()
  }
}