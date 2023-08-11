settingsWindow <-
function(dev="FlowCam_GRAYSCALE", zipFile="", prob=0.5, eachRes="1") {
  
  # Create new window for settings
  tt <- tktoplevel()
  tktitle(tt) <- "SETTINGS"
  tkwm.resizable(tt, FALSE, FALSE)

  # Acquisition devices
  fontTitle <- tkfont.create(family="Arial", size=12, weight="bold")
  fontText <- tkfont.create(family="Helvetica", size=8, weight="bold")
  tkgrid(tk2label(tt, text="ACQUISITION DEVICE", font=fontTitle), 
         row=1, sticky="w")
  
  devValue <- tclVar(dev)
  dev.title <- c("Cyto-Sense-Sub", "FlowCam_COLOR", "FlowCam_GRAYSCALE", 
                 "Imaging_FlowCytoBot", "Photo(micro)graph", "ZooScan")
  callback <- function() print(tclvalue(devValue))
  sapply(dev.title, function(i) {
    radio_button <- ttkradiobutton(tt, variable=devValue, text=i, value=i)
    tkgrid(radio_button, padx=20, pady=5, sticky="w")
  })
  
  # Classification options
  tkgrid(tk2label(tt, text="TRANSFER LEARNING", font=fontTitle), 
         row=9, sticky="w")
  
  tt$env$butDataSelect <- tk2button(tt, text="(Re-)Build a model", 
                                    image="update", compound="left", 
                                    width=30, command=function() {
    
    # Select the model to build
    ttt <- tktoplevel()
    tktitle(ttt) <- "Train/build a new model"
    tkwm.resizable(ttt, FALSE, FALSE)
    
    dirButton <- tk2button(ttt, text="SELECT TRAIN/TEST DIRECTORIES", 
                           image="folder", compound="left", width=30, 
                           command=function() {
                             trainPath <<- tk_choose.dir(default=getwd())
                             if (is.na(trainPath)) {
                               tkmessageBox(message="No directory selected!")
                             } else {
                               tkgrid(tklabel(ttt, text=basename(trainPath), 
                                              font=fontText, 
                                              foreground="darkgreen"), 
                                      padx=c(50,50), row=2, 
                                      column=0, columnspan=2)
                             }
                           })
    tkgrid(tklabel(ttt, text=""), row=0)
    tkgrid(tklabel(ttt, text=""), row=2)
    tkgrid(dirButton, row=1, padx=c(50,10), column=0, columnspan=2)
    
    dataVarList <- tk2listbox(ttt, selectmode="single", activestyle="dotbox",
                              height=5, width=30, autoscroll="none", 
                              background="white")
    tkgrid(tk2label(ttt, text="SELECT THE MODEL TO BUILD:"), 
           row=3, column=0, pady=c(10,0), sticky="n", padx=c(50,0))
    tkgrid(dataVarList, row=4, column=0, columnspan=2, rowspan=4)
    allMod <- c("DenseNet201", "InceptionV3", "ResNet50", "VGG16", "VGG19")
    sapply(allMod, function(x) tkinsert(dataVarList, "end", x))
    tkselection.set(dataVarList, 0, 0)
    
    epoch <- 20
    epochValue <- tclVar(as.character(epoch))
    ttt$env$epoch <- tkentry(ttt, textvariable=epochValue, width=5, 
                             background="white")
    tkgrid(tklabel(ttt,text="EPOCHS"), row=4, sticky="w", column=2)
    tkgrid(ttt$env$epoch, padx=5, row=4, sticky="e", column=3, padx=c(0,50))

    batch_size <- 20
    batch_sizeValue <- tclVar(as.character(batch_size))
    ttt$env$batch_size <- tkentry(ttt, textvariable=batch_sizeValue, width=5, 
                                  background="white")
    tkgrid(tklabel(ttt,text="BATCH SIZE"), row=5, sticky="w", column=2)
    tkgrid(ttt$env$batch_size, padx=5, row=5, sticky="e", column=3, padx=c(0,50))

    img_width <- 224
    img_widthValue <- tclVar(as.character(img_width))
    ttt$env$img_width <- tkentry(ttt, textvariable=img_widthValue, width=5, 
                                 background="white")
    tkgrid(tklabel(ttt,text="IMAGE WIDTH (px)"), row=6, sticky="w", column=2)
    tkgrid(ttt$env$img_width, padx=5, row=6, sticky="e", column=3, padx=c(0,50))

    img_height <- 224
    img_heightValue <- tclVar(as.character(img_height))
    ttt$env$img_height <- tkentry(ttt, textvariable=img_heightValue, width=5, 
                                  background="white")
    tkgrid(tklabel(ttt,text="IMAGE HEIGHT (px)"), row=7, sticky="w", column=2)
    tkgrid(ttt$env$img_height, padx=5, row=7, sticky="e", column=3, padx=c(0,50))

    dataug <- "1"
    ttt$env$dataug <- tk2checkbutton(ttt, text="Use Data Augmentation")
    dataugValue <- tclVar(dataug)
    tkconfigure(ttt$env$dataug, variable=dataugValue)
    tkgrid(ttt$env$dataug, padx=c(70,50), row=8, sticky="w", 
           column=0, columnspan=2)
    tkgrid(tklabel(ttt, text=""), row=9)
    
    pc.title <- c("Build on this computer", "Generate a .PY file")
    pc <- pc.title[1]
    pcValue <- tclVar(pc)
    callback <- function() print(tclvalue(pcValue))
    sapply(pc.title, function(i) {
      radio_button <- ttkradiobutton(ttt, variable=pcValue, text=i, value=i)
      tkgrid(radio_button, padx=c(70,50), sticky="w", column=0, columnspan=2)
    })
    
    ValidButton <- tk2button(ttt, text="BUILD THE MODEL", width=-6, 
                             command=function() {
      selected_mod <<- allMod[as.numeric(tkcurselection(dataVarList))+1]
      if (!length(selected_mod)) {
        tkmessageBox(message="No model selected...!")
      } else {
        epoch <<- as.numeric(tclvalue(epochValue))
        batch_size <<- as.numeric(tclvalue(batch_sizeValue))
        img_width <<- as.numeric(tclvalue(img_widthValue))
        img_height <<- as.numeric(tclvalue(img_heightValue))
        
        pc <<- as.character(tclvalue(pcValue))
        if (pc=="Build on this computer") {
          ans <- tkmessageBox(message="This requires that Python (version 3.7) and TensorFlow (open-source library) be installed on this computer...!\n\nContinue?",
                       type="yesno")
          if (tclvalue(ans)=="yes") {
            ans <- tkmessageBox(message="This operation can take several hours...!\n\nContinue?",
                                type="yesno", icon="warning")
            if (tclvalue(ans)=="yes") {
              # Edit and create a specific python script
              tx  <- readLines(system.file("python_scripts", 
                                           "templateScriptTL.py", 
                                           package="EcoTransLearn"))
              
              tx[which(tx=="device")] <- paste(tx[which(tx=="device")], " = '", 
                                               as.character(tclvalue(devValue)),
                                               "'", sep="")
              tx[which(tx=="modelName")] <- paste(tx[which(tx=="modelName")], 
                                                  " = '", 
                                                  selected_mod,"'", sep="")
              tx[which(tx=="weightPath")] <- paste(tx[which(tx=="weightPath")], 
                                                   " = '", 
                                                   list.files(system.file("weights", 
                                                                          package="EcoTransLearn"), 
                                                              pattern=selected_mod, 
                                                              full.names = TRUE),
                                                   "'", sep="")
              tx[which(tx=="trainPath")] <- paste(tx[which(tx=="trainPath")], 
                                                  " = '", 
                                                  file.path(trainPath, "train"),
                                                  "'", sep="")
              tx[which(tx=="testPath")] <- paste(tx[which(tx=="testPath")], 
                                                 " = '", 
                                                 file.path(trainPath, "test"),
                                                 "'", sep="")
              tx[which(tx=="BATCH_SIZE")] <- paste(tx[which(tx=="BATCH_SIZE")], 
                                                   " = ", batch_size, sep="")
              tx[which(tx=="EPOCH")] <- paste(tx[which(tx=="EPOCH")], 
                                              " = ", 
                                              epoch, sep="")
              tx[which(tx=="img_width")] <- paste(tx[which(tx=="img_width")], 
                                                  " = ", img_width, sep="")
              tx[which(tx=="img_height")] <- paste(tx[which(tx=="img_height")], 
                                                   " = ", img_height, sep="")
              tx[which(tx=="data_aug")] <- paste(tx[which(tx=="data_aug")], 
                                                 " = ", 
                                                 as.integer(dataug), sep="")
              modSavedPath <- file.path(trainPath, "saved_models")
              if (!dir.exists(modSavedPath))
                dir.create(modSavedPath, recursive=TRUE)
              tx[which(tx=="save_dir")] <- paste(tx[which(tx=="save_dir")], 
                                                 " = '", 
                                                 file.path(trainPath, "saved_models"),
                                                 "'", sep="")
              writeLines(tx, con=file.path(trainPath, paste(selected_mod, 
                                                            "_script.py", 
                                                            sep="")))
              reticulate::source_python(file.path(trainPath, paste(selected_mod, 
                                                                   "_script.py", 
                                                                   sep="")))
            }
          }
        } else {
          # Edit and create a specific python script
          tx  <- readLines(system.file("python_scripts", "templateScriptTL.py",
                                       package="EcoTransLearn"))
         
          tx[which(tx=="device")] <- paste(tx[which(tx=="device")], " = '", 
                                           as.character(tclvalue(devValue)),
                                           "'", sep="")
          tx[which(tx=="modelName")] <- paste(tx[which(tx=="modelName")], 
                                              " = '", 
                                              selected_mod,"'", sep="")
          tx[which(tx=="weightPath")] <- paste(tx[which(tx=="weightPath")], 
                                               " = '", 
                                               list.files(system.file("weights", 
                                                                      package="EcoTransLearn"), 
                                                          pattern=selected_mod, 
                                                          full.names=TRUE),
                                               "'", sep="")
          tx[which(tx=="trainPath")] <- paste(tx[which(tx=="trainPath")], 
                                              " = '", 
                                              file.path(trainPath, "train"),
                                              "'", sep="")
          tx[which(tx=="testPath")] <- paste(tx[which(tx=="testPath")], 
                                             " = '", 
                                              file.path(trainPath, "test"),
                                             "'", sep="")
          tx[which(tx=="BATCH_SIZE")] <- paste(tx[which(tx=="BATCH_SIZE")], 
                                               " = ", batch_size, sep="")
          tx[which(tx=="EPOCH")] <- paste(tx[which(tx=="EPOCH")], " = ", 
                                          epoch, sep="")
          tx[which(tx=="img_width")] <- paste(tx[which(tx=="img_width")], 
                                              " = ", img_width, sep="")
          tx[which(tx=="img_height")] <- paste(tx[which(tx=="img_height")], 
                                               " = ", img_height, sep="")
          tx[which(tx=="data_aug")] <- paste(tx[which(tx=="data_aug")],
                                             " = ", as.integer(dataug), sep="")
          modSavedPath <- file.path(trainPath, "saved_models")
          if (!dir.exists(modSavedPath))
            dir.create(modSavedPath, recursive=TRUE)
          tx[which(tx=="save_dir")] <- paste(tx[which(tx=="save_dir")], 
                                             " = '", 
                                             file.path(trainPath, "saved_models"),
                                             "'", sep="")
          writeLines(tx, con=file.path(trainPath, paste(selected_mod, 
                                                        "_script.py", sep="")))
          tkmessageBox(message = paste("Python scripts created and saved in:\n",
                                       trainPath, sep=""))
        }
        tkdestroy(ttt)
      }
    })
    tkgrid(ValidButton, padx=c(50,50), pady=c(10, 15), column=0, columnspan=2)
    tkwait.window(ttt)
  })
  tkgrid(tt$env$butDataSelect, padx=20, pady=5, row=10, columnspan=2)
  
  tt$env$butSelect <- tk2button(tt, text="Select a model", image="python", 
                                compound="left", width=30, command=function() {
    zipFile <<- tclvalue(tkgetOpenFile(filetypes="{ {zip Files} {.zip} } { {Compressed Files} {.zip} }"))
    if (!nchar(zipFile)) {
      tkmessageBox(message="No ZIP file selected!")
    } else {
      zipFiles <- unzip(zipfile=zipFile, list=TRUE)
      tkgrid(tklabel(tt, text=paste("ZIP file:", basename(zipFile), sep=" "), 
                     font=fontText, foreground="darkgreen"), sticky="w", 
             row=12, column=0)
      tkgrid(tklabel(tt, text=paste("Model:", sub("\\_.*", "", 
                                                  zipFiles$Name[1]), sep=" "), 
                     font=fontText, foreground="darkgreen"), sticky="w", 
             row=13, column=0)
      train_res <- read.table(unz(zipFile, 
                                  zipFiles$Name[grep("history", zipFiles$Name)]), 
                              sep=";", dec=".", header=TRUE)
      tkgrid(tklabel(tt, text=paste("Number of epochs:", 
                                    train_res$epoch[nrow(train_res)]+1, 
                                    sep=" "), 
                     font=fontText, foreground="darkgreen"), sticky="w", 
             row=14, column=0)
      tkgrid(tklabel(tt, text=paste("Training accuracy:", 
                                    train_res$accuracy[nrow(train_res)], 
                                    sep=" "), 
                     font=fontText, foreground="darkgreen"), sticky="w", 
             row=15, column=0)
      tkgrid(tklabel(tt, text=paste("Test accuracy:", 
                                    train_res$val_accuracy[nrow(train_res)], 
                                    sep=" "), 
                     font=fontText, foreground="darkgreen"), sticky="w", 
             row=16, column=0)
      
      trueLab <- read.csv(unz(zipFile, zipFiles$Name[grep("test", zipFiles$Name)]), 
                          header=TRUE, sep=",")
      trueLab <- trueLab[['Label']]
      predLab <- read.csv(unz(zipFile, zipFiles$Name[grep("predictions", 
                                                          zipFiles$Name)]), 
                          header=FALSE, sep=";")
      pred <- apply(X=predLab, MARGIN=1, FUN=max)
      predInd <- apply(X=predLab, MARGIN=1, FUN=which.max)-1
      
      classNames <- read.csv(unz(zipFile, zipFiles$Name[grep("classnames", 
                                                             zipFiles$Name)]), 
                             header=FALSE, sep=",")$V1
      for (cl in 1:length(classNames))
        predInd[which(predInd==cl-1)] <- classNames[cl]
      conf <- confusion(as.factor(predInd), as.factor(trueLab))
      
      X11()
      hist(pred, breaks=8, xlab="Class probability", 
           ylab="Number of images", main="", col="darkgreen",
           labels=paste0(round(hist(pred, breaks=8, plot=FALSE)$counts/length(pred)*100, 1), "%"))
      
      X11()
      confusionImage(conf, mar=c(3.1,15.1,6.1,3.1))
      
      X11()
      par(mfrow=c(2,1),     # 2x2 layout
          oma=c(2,2,0,0),   # two rows of text at the outer left and bottom margin
          mar=c(1,1,0,1),   # space for one row of text at ticks and to separate plots
          mgp=c(2,1,0),     # axis label at 2 rows distance, tick labels at 1 row
          xpd=NA)
      plot(train_res$epoch, train_res$accuracy, type="l", col="darkgreen", 
           lwd=2, xlab="", ylab="Accuracy", xaxt="n")
      lines(train_res$epoch, train_res$val_accuracy, type="l", 
            col="darkblue", lwd=2)
      grid(lwd=2)
      legend("bottomright", legend=c("Training","Validation"), lty=c(1,1),
             lwd=5, cex=1.5, box.lty=0, col=c("darkgreen","darkblue"))
      plot(train_res$epoch, train_res$loss, type="l", col="darkgreen", lwd=2, 
           xlab="Epoch", ylab="Loss")
      lines(train_res$epoch, train_res$val_loss, type="l", 
            col="darkblue", lwd=2)
      grid(lwd=2)
    }
  })
  tkgrid(tt$env$butSelect, padx=20, pady=5, row=11, columnspan=2)
  
  ## Outputs options
  tkgrid(tk2label(tt, text="CSV AND GRAPHICAL OUTPUTS", font=fontTitle), 
         row=17, sticky="w")
  
  tt$env$eachRes <- tk2checkbutton(tt, text="Export results for each sample")
  eachResValue <- tclVar(eachRes)
  tkconfigure(tt$env$eachRes, variable=eachResValue)
  tkgrid(tt$env$eachRes, padx=20, pady=5, row=18, sticky="w")
  
  probValue <- tclVar(as.character(prob))
  tt$env$prob <- tkentry(tt, textvariable=probValue, width=5, 
                         background="white")
  tkgrid(tklabel(tt,text="Classification probability threshold\n(lower=not considered)"), 
         tt$env$prob, padx=20, pady=5, row=19, sticky="w")
  
  onOK <- function() {
    # Acquisition
    dev <<- as.character(tclvalue(devValue))
    
    # Preprocessing
    zipFile <<- as.character(zipFile)
    
    # Graphical outputs
    eachRes <<- as.character(tclvalue(eachResValue))
    prob <<- as.numeric(tclvalue(probValue))

    if (!nchar(zipFile)) {
      tkmessageBox(message="Please select a valid Python file for classification...")
    } else {
      tkdestroy(tt)
    }
  }
  tt$env$butOK <- tk2button(tt, text="OK", width=-6, command=onOK)
  tkgrid(tt$env$butOK, padx=10, columnspan=2, pady=c(0, 15))
  tkwait.window(tt)
  
  return(list(dev=dev, zipFile=zipFile, eachRes=eachRes, prob=prob))
}