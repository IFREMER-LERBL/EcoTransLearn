saveIndPlot <-
function(dat, smpPath, colgPath) {
  
  png(file.path(smpPath, paste(basename(colgPath), "_RELATIVE.png", sep="")), 
      width=1280, height=720)
  gg <- ggplot(dat, aes(x=Group, y=Relative)) +
    geom_col() + ylim(0,max(dat$Relative)+8) + geom_bar(stat="identity") +
    geom_text(aes(label=round(Relative, 2)), vjust=-6) + 
    ylab("Relative proportion (%)") + xlab("") + 
    theme(panel.grid.minor.x=element_blank()) +
    scale_fill_discrete(name="") + theme_bw() +
    theme(axis.text.x=element_text(size=12, angle=45, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12))
  print(gg)
  dev.off()
  
  png(file.path(smpPath, paste(basename(colgPath), "_COUNTS.png", sep="")), 
      width=1280, height=720)
  gg <- ggplot(dat, aes(x=Group, y=Count)) +
    geom_col() + ylim(0,max(dat$Count)+5000) + geom_bar(stat="identity") +
    geom_text(aes(label=round(Count, 2)), vjust=-6) + 
    ylab("Counts") + xlab("") + theme(panel.grid.minor.x=element_blank()) +
    scale_fill_discrete(name="") + theme_bw() +
    theme(axis.text.x=element_text(size=12, angle=45, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12))
  print(gg)
  dev.off()
}