# Export results in CSV files
exportRes <- function(smp="", predictions, imgNames, prob=0.5, smpPath, colgPath, saveAll=TRUE) {
  predicted_proba <- apply(predictions, 1, max, na.rm=TRUE)
  predicted_idx <- which(predicted_proba >= prob)
  percent_used <- length(predicted_idx)/nrow(predictions)*100
  predicted_classes <- predictions[predicted_idx, ]
  predicted_names <- colnames(predicted_classes)[apply(predicted_classes, 1, which.max)]
  imgNames <- imgNames[predicted_idx]
  imgNames <- cbind(imgNames, predicted_names)
  res <- table(predicted_names)
  res <- res[grepl("[A-Z]", names(res))]
  res_abd <- res
  res_rel <- res/sum(res)*100
  res_rel2 <- data.frame(Sample=smp, Group=colnames(predictions)[grepl("[A-Z]", colnames(predictions))], 
                         Relative=0, Count=0, Prob_threshold=prob, Percent_used=percent_used)
  res_rel2$Relative[match(names(res_rel), res_rel2$Group)] <- res_rel
  res_rel2$Count[match(names(res_abd), res_rel2$Group)] <- res_abd
  res_rel <- res_rel2
  res_rel$Group <- factor(res_rel$Group, levels = res_rel$Group)
  
  if (isTRUE(saveAll)) {
    write.table(imgNames, file.path(smpPath, paste(basename(colgPath), "_CLASSIF.csv", sep="")), 
              sep=";", dec=".", row.names = FALSE, col.names = c("Filename","Class"))
    write.table(res_rel, file.path(smpPath, paste(basename(colgPath), ".csv", sep="")), 
              sep=";", dec=".", row.names = FALSE)
    savePlot(dat=res_rel, smpPath, colgPath)
  }
  return(res_rel)
}

# Save plots for individuals samples
savePlot <- function(dat, smpPath, colgPath) {
  png(file.path(smpPath, paste(basename(colgPath), "_RELATIVE.png", sep="")), width = 1280, height = 720)
  gg <- ggplot(dat, aes(x=Group, y=Relative)) +
    geom_col() + ylim(0,max(dat$Relative)+8) + geom_bar(stat="identity") +
    geom_text(aes(label=round(Relative, 2)), vjust=-6) + 
    ylab("Relative proportion (%)") + xlab("") + theme(panel.grid.minor.x = element_blank()) +
    scale_fill_discrete(name = "") + theme_bw() +
    theme(axis.text.x=element_text(size=12, angle=45, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.y = element_text(size=14),
          legend.text = element_text(size=12))
  print(gg)
  dev.off()
  
  png(file.path(smpPath, paste(basename(colgPath), "_COUNTS.png", sep="")), width = 1280, height = 720)
  gg <- ggplot(dat, aes(x=Group, y=Count)) +
    geom_col() + ylim(0,max(dat$Count)+5000) + geom_bar(stat="identity") +
    geom_text(aes(label=round(Count, 2)), vjust=-6) + 
    ylab("Counts") + xlab("") + theme(panel.grid.minor.x = element_blank()) +
    scale_fill_discrete(name = "") + theme_bw() +
    theme(axis.text.x=element_text(size=12, angle=45, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.y = element_text(size=14),
          legend.text = element_text(size=12))
  print(gg)
  dev.off()
}

# Save final plots for all samples
saveFinalPlot <- function(dat, smpPath, saveFinal=TRUE) {
  # Absolute counts
  X11(width = 1280, height = 720)
  p <- ggplot(data = dat, aes(x = Date, y = CountVol, fill = Group))
  p <- p + geom_area(position = 'stack') + theme_bw() + theme() +
    theme(text = element_text(size=20))
  p <- p + labs(x = "Time", y = "Counts", fill = "")
  print(p)
  
  if (isTRUE(saveFinal)) {
    png(file.path(smpPath, paste(basename(smpPath), "_COUNTS.png", sep="")),
        width = 1280, height = 720)
    p <- ggplot(data = dat, aes(x = Date, y = CountVol, fill = Group))
    p <- p + geom_area(position = 'stack') + theme_bw() + theme() +
      theme(text = element_text(size=20))
    p <- p + labs(x = "Time", y = "Counts", fill = "")
    print(p)
    dev.off()
  }
  
  # Relative counts
  X11(width = 1280, height = 720)
  p <- ggplot(data = dat, aes(x = Date, y = as.numeric(Relative), fill = Group))
  p <- p + geom_area(position = 'stack') + theme_bw() + theme() +
    theme(text = element_text(size=20))
  p <- p + labs(x = "", y = "Relative Abundances (%)", fill = "")
  print(p)
  
  if (isTRUE(saveFinal)) {
    png(file.path(smpPath, paste(basename(smpPath), "_RELATIVE.png", sep="")),
        width = 1280, height = 720)
    p <- ggplot(data = dat, aes(x = Date, y = as.numeric(Relative), fill = Group))
    p <- p + geom_area(position = 'stack') + theme_bw() + theme() +
      theme(text = element_text(size=20))
    p <- p + labs(x = "", y = "Relative Abundances (%)", fill = "")
    print(p)
    dev.off()
  }
}

# Save map
saveMap <- function(dat, meta, grp, smpPath, saveFinal=TRUE) {
  coast <- read.shapefile("HF_maps/ne_10m_coastline/ne_10m_coastline")
  land <-read.shapefile("HF_maps/ne_10m_land/ne_10m_land")
  border <- read.shapefile("HF_maps/Borders/ne_10m_admin_0_boundary_lines_land")
  xlim=c(min(meta$Longitude)-1,max(meta$Longitude)+1)
  ylim=c(min(meta$Latitude)-1,max(meta$Latitude)+1)
  
  for (i in grp) {
    cat("\nProjecting", i, "abundances on map...")
    #png(file.path(smpPath, paste(basename(smpPath), "_", i, ".png", sep="")), 
    #    2000, 2000, res = 300)
    X11()
    par(mar = c(1.3,1.3,1,0.5))
    par(tcl = -0.5)
    par(oma = c(2.4,2.8,1,0))
    basemap(xlim, ylim,  xaxt="n",yaxt="n", ylab="",xlab="",main="", bg="white")
    draw.shape(coast, type = "line",col = "black" )
    draw.shape(land, type = "poly", col = "grey60")
    draw.shape(border, type = "line", col = "black")
    points(x = meta$Longitude, y = meta$Latitude, col = alpha("orange", 0.4), 
           pch = 19, cex = dat$CountVol[which(dat$Group==i)]/1000)
    axis(side=1, cex.axis = 1)
    axis(side=2, cex.axis = 1)
    mtext(i, side = 3, font = 2, cex = 0.8, adj = 0, line = 0.15, col = "black")
    northarrow(loc = c(par("xaxp")[2]-2, par("yaxp")[1])+0.25, size = 0.34, cex = 0.9)
    
    if (isTRUE(saveFinal)) {
      png(file.path(smpPath, paste(basename(smpPath), "_", i, ".png", sep="")), 
          2000, 2000, res = 300)
      par(mar = c(1.3,1.3,1,0.5))
      par(tcl = -0.5)
      par(oma = c(2.4,2.8,1,0))
      basemap(xlim, ylim,  xaxt="n",yaxt="n", ylab="",xlab="",main="", bg="white")
      draw.shape(coast, type = "line",col = "black" )
      draw.shape(land, type = "poly", col = "grey60")
      draw.shape(border, type = "line", col = "black")
      points(x = meta$Longitude, y = meta$Latitude, col = alpha("orange", 0.4), 
             pch = 19, cex = dat$CountVol[which(dat$Group==i)]/1000)
      axis(side=1, cex.axis = 1)
      axis(side=2, cex.axis = 1)
      mtext(i, side = 3, font = 2, cex = 0.8, adj = 0, line = 0.15, col = "black")
      northarrow(loc = c(par("xaxp")[2]-2, par("yaxp")[1])+0.25, size = 0.34, cex = 0.9)
      dev.off()
    }
    cat("\nDone!")
  }
}