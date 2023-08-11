exportRes <-
function(smp="", predictions, imgNames, prob=0.5, smpPath, 
         colgPath, saveAll=TRUE) {
  
  predicted_proba <- apply(predictions, 1, max, na.rm=TRUE)
  predicted_idx <- which(predicted_proba >= prob)
  low_predicted_idx <- which(predicted_proba < prob)
  percent_used <- length(predicted_idx)/nrow(predictions)*100
  predicted_classes <- predictions[predicted_idx, ]
  predicted_names <- colnames(predicted_classes)[apply(predicted_classes, 1, 
                                                       which.max)]
  imgNames <- imgNames[predicted_idx]
  imgNames <- cbind(imgNames, predicted_names)
  res <- table(predicted_names)
  res_abd <- res
  res_rel <- res/sum(res)*100
  res_rel2 <- data.frame(Sample=smp, Group=colnames(predictions), 
                         Relative=0, Count=0, Prob_threshold=prob, 
                         Percent_used=percent_used)
  res_rel2$Relative[match(names(res_rel), res_rel2$Group)] <- res_rel
  res_rel2$Count[match(names(res_abd), res_rel2$Group)] <- res_abd
  res_rel <- res_rel2
  res_rel$Group <- factor(res_rel$Group, levels=res_rel$Group)
  
  if (isTRUE(saveAll)) {
    write.table(imgNames, file.path(smpPath, paste(basename(colgPath), 
                                                   "_CLASSIF.csv", sep="")), 
              sep=";", dec=".", row.names=FALSE, 
              col.names=c("Filename","Class"))
    write.table(res_rel, file.path(smpPath, paste(basename(colgPath), 
                                                  ".csv", sep="")), 
              sep=";", dec=".", row.names=FALSE)
    saveIndPlot(dat=res_rel, smpPath, colgPath)
  }
  return(res_rel)
}