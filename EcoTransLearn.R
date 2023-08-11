EcoTransLearn <-
function() {
  
  graphics.off()
  tcl("image", "create", "photo", "barplt", file=system.file("images", 
                                                             "barplot.gif", 
                                                             package="EcoTransLearn"))
  tcl("image", "create", "photo", "csv", file=system.file("images", 
                                                          "csv.gif", 
                                                          package="EcoTransLearn"))
  tcl("image", "create", "photo", "fish", file=system.file("images", 
                                                           "fish.gif", 
                                                           package="EcoTransLearn"))
  tcl("image", "create", "photo", "folder", file=system.file("images", 
                                                             "folder.gif", 
                                                             package="EcoTransLearn"))
  tcl("image", "create", "photo", "ifremerLogo", file=system.file("images", 
                                                                  "ifremer_logo_resized_small.gif", 
                                                                  package="EcoTransLearn"))
  tcl("image", "create", "photo", "map", file=system.file("images", 
                                                          "map.gif", 
                                                          package="EcoTransLearn"))
  tcl("image", "create", "photo", "phyto", file=system.file("images", 
                                                            "phyto_flowcam.gif", 
                                                            package="EcoTransLearn"))
  tcl("image", "create", "photo", "plus", file=system.file("images", 
                                                           "plus.gif", 
                                                           package="EcoTransLearn"))
  tcl("image", "create", "photo", "python", file=system.file("images", 
                                                             "python.gif", 
                                                             package="EcoTransLearn"))
  tcl("image", "create", "photo", "settingsFile", file=system.file("images", 
                                                                   "settings.gif", 
                                                                   package="EcoTransLearn"))
  tcl("image", "create", "photo", "update", file=system.file("images", 
                                                             "update.gif", 
                                                             package="EcoTransLearn"))
  
  fontTitle <- tkfont.create(family="Helvetica",size=18,weight="bold")
  fontText <- tkfont.create(family="Helvetica", size=8, weight="bold")
  
  tk2theme("alt")
  mainWindow <- tktoplevel()
  tkwm.resizable(mainWindow, FALSE, FALSE)
  tktitle(mainWindow) <- "EcoTransLearn (version 1.0-2)"
  tkgrid(tklabel(mainWindow, text=""), row=0, column=1, columnspan=4)
  
  # Default options
  settings <- list(dev="FlowCam_GRAYSCALE", zipFile="", eachRes="1", prob=0.5)
  
  ##############################
  ###   'DIRECTORY' button   ###
  ##############################
  # Select the directory containing samples
  dirButton <- tk2button(mainWindow, text="SELECT DATA DIRECTORY", 
                         image="folder", compound="left", width=30, 
                         command=function() {
    smpPath <<- tk_choose.dir(default=getwd())
    if (is.na(smpPath)) {
      tkmessageBox(message="No directory selected!")
    } else {
      tkgrid(tklabel(mainWindow, text=basename(smpPath), font=fontText, 
                     foreground="darkgreen"), padx=c(50,0), row=4, column=1, 
             columnspan=4)
      tkconfigure(settingsButton, state="normal")
      tkconfigure(OKButton, state="normal")
      tkconfigure(viewButton, state="normal")

      settings <<- settingsWindow(dev=settings$dev, zipFile=settings$zipFile, 
                                  eachRes=settings$eachRes, prob=settings$prob)
    }
  })
  tkgrid(tklabel(mainWindow, text=""), row=2)
  tkgrid(dirButton, row=3, padx=c(50,0), column=1, columnspan=4)

  #############################
  ###   'SETTINGS' button   ###
  #############################
  settingsButton <- tk2button(mainWindow, text="", image="settingsFile", 
                              compound="center", width=2, 
                              state="disabled", command=function() {
    settings <<- settingsWindow(dev=settings$dev, zipFile=settings$zipFile,
                                eachRes=settings$eachRes, prob=settings$prob)
  })
  tkgrid(settingsButton, row=3, padx=c(0,10), column=5, sticky="w")
  
  #############################
  ###   'CLASSIFY' button   ###
  #############################
  OKButton <- tk2button(mainWindow, text="CLASSIFY", image="update", 
                        compound="left", width=30, state="disabled", 
                        command=function() {
    
    tt <- tktoplevel()
    tktitle(tt) <- "Select groups"
    tkwm.resizable(tt, FALSE, FALSE)
    dataVarList <- tk2listbox(tt, selectmode="multiple", 
                              activestyle="dotbox", height=15, width=40, 
                              autoscroll="none", background="white")
    tkgrid(tk2label(tt, 
                    text="Please select groups-of-interest (multiple selection):"), 
           row=1, column=0, pady=c(10,0), sticky="w")
    tkgrid(dataVarList, row=2, column=0)
    
    if (!nchar(settings$zipFile)) {
      tkmessageBox(message="No Python file selected for Transfer Learning...")
      settings <<- settingsWindow(dev=settings$dev, zipFile=settings$zipFile, 
                                  eachRes=settings$eachRes, prob=settings$prob)
    } else {
      zipFiles <- unzip(zipfile=settings$zipFile, list=TRUE)
      allGrps <- read.table(unz(settings$zipFile, 
                                zipFiles$Name[grep("classnames",
                                                   zipFiles$Name)]), 
                            sep=";")$V1
      upperCase <- grep("^[A-Z]", allGrps)
      lowerCase <- grep("^[a-z]", allGrps)
      classes <- allGrps[c(upperCase, lowerCase)]
    }
    sapply(allGrps, function(x) tkinsert(dataVarList, "end", x))
    tkselection.set(dataVarList, 0, "end")
    
    ValidButton <- tk2button(tt, text="OK", width=-6, 
                             command=function() {
      grp_tracked <<- allGrps[as.numeric(tkcurselection(dataVarList))+1]
      if (!length(grp_tracked)) {
        tkmessageBox("No group selected...!")
      } else {
        tkdestroy(tt)
      }
    })
    tkgrid(ValidButton, padx=10, pady=c(10, 15))
    tkwait.window(tt)
    
    dirs <- list.dirs(smpPath, recursive=FALSE)
    stack_res <- NULL
    nb_prev_files <- 0
    if (file.exists(file.path(smpPath, paste(basename(smpPath), "_RES.csv", 
                                             sep="")))) {
      stack_res <- read.table(file.path(smpPath, paste(basename(smpPath), 
                                                       "_RES.csv", sep="")), 
                              sep=";", header=TRUE)
      prev_files <- unique(stack_res$Sample)
      dirs <- dirs[-which(basename(dirs) %in% unique(stack_res$Sample))]
      nb_prev_files <- length(prev_files)
    }
    vigsPath <- file.path(dirname(smpPath), "temp/unknown")
    if (!dir.exists(vigsPath))
      dir.create(vigsPath, recursive=TRUE)
    meta <- NULL
    
    if (length(dirs) >= 1) {
      cat("\n", nb_prev_files, "samples already analysed!", "\n")
      
      ttstop <- tktoplevel()  # Create a new toplevel window
      tktitle(ttstop) <- "Stop Windows"
      tkwm.resizable(ttstop, FALSE, FALSE)
      done <- tclVar(0)
      STOP.but <- tkbutton(ttstop, text="  Stop analysis  ",
                           command=function() tclvalue(done) <- 1)
      tkgrid(STOP.but)
      tkbind(ttstop, "<Destroy>", function() tclvalue(done) <- 2)
      tkfocus(ttstop)
      
      cpt <- 1
      for (colgPath in dirs) {
        smp <- basename(colgPath)
        cat("\nSample", cpt, "of", length(dirs), ":", smp)
        
        # Process images
        cat("\nImages processing... ")
        # FlowCam raw data (collages)
        if (grepl("FlowCam", settings$dev)) {
          ImgProcess(colgPath, vigsPath, rgbCol=grepl("COLOR", settings$dev))
        } else {      # Other images (individuals)
          list.of.img <- list.files(colgPath)
          file.copy(file.path(colgPath, list.of.img), vigsPath)
        }
        cat("\nDone!")
        
        # Run Python scripts
        cat("\nImages classification... ")
        if (!nchar(settings$zipFile)) {
          tkmessageBox(message="No model selected for classification...")
          settings <<- settingsWindow(dev=settings$dev, 
                                      zipFile=settings$zipFile, 
                                      eachRes=settings$eachRes, 
                                      prob=settings$prob)
        } else {
          unzip(settings$zipFile, files=zipFiles$Name[grep("h5",
                                                           zipFiles$Name)],
                exdir=dirname(vigsPath))
          modelPath <- file.path(dirname(vigsPath), 
                                 zipFiles$Name[grep("h5", zipFiles$Name)])
          modelName <- sub("\\_.*", "", zipFiles$Name[1])
          imgPath <- dirname(vigsPath)
          reticulate::source_python(system.file("python_scripts", 
                                                "imgClassification.py", 
                                                package="EcoTransLearn"))
          resTL <- imgClassification(modelName, imgPath, modelPath, 
                                     batch_size=as.integer(20),
                                     img_width=as.integer(128), 
                                     img_height=as.integer(128))
          predictions <- resTL[[1]]
          imgNames <- resTL[[2]]
          colnames(predictions) <- classes
          write.table(predictions, file.path(smpPath, paste(basename(colgPath), 
                                                            "_PRED.csv",
                                                            sep="")), 
                      sep=";", dec=".", row.names=FALSE)
        }
        cat("Done!")
        
        # Save results
        cat("\nSave results... ")
        res_rel <<- exportRes(smp=smp, 
                              predictions, 
                              imgNames, 
                              prob=settings$prob,
                              smpPath=smpPath, 
                              colgPath=colgPath, 
                              saveAll=as.logical(as.integer(settings$eachRes)))
        stack_res <- rbind(stack_res, res_rel)
        
        f <- list.files(dirname(vigsPath), full.names=TRUE, recursive=TRUE)
        file.remove(f)
        write.table(stack_res, file.path(smpPath, paste(basename(smpPath), 
                                                        "_RES.csv", sep="")), 
                    row.names=FALSE, col.names=TRUE, sep=";")
        cat("\nDone!\n")
        
        cpt <- cpt+1
        doneVal <- as.integer(tclvalue(done))
        if(doneVal != 0) break
      }
      tkdestroy(ttstop)
      if (doneVal == 0) tkmessageBox(message="Analysis finished okay")
      if (doneVal == 1) tkmessageBox(message="You interrupted the analysis")
      if (doneVal == 2) tkmessageBox(message="You destroyed the dialog!")
      
    } else {
      tkmessageBox(message=paste(paste(nb_prev_files, "samples already analysed!"),
                                 "No (new)data to process...!", sep="\n"))
    }
  })
  tkgrid(tklabel(mainWindow, text="      "), row=5)
  tkgrid(OKButton, pady=c(0,10), padx=c(50,0), row=6, column=1, 
         columnspan=4)
  
  #########################
  ###   'VIEW' button   ###
  #########################
  viewButton <- tk2button(mainWindow, text="VIEW", image="barplt", 
                          compound="left", width=30, state="disabled", 
                          command=function() {
    metaFile <<- tclvalue(tkgetOpenFile(filetypes="{ {csv Files} {.csv} } { {CSV Files} {.csv} }", 
                                         title="Select metadata file..."))
    if (!nchar(metaFile)) {
      tkmessageBox(message="No file selected for metadata...\nPlease generate a template with the dedicated tool!")
    } else {
      if (!file.exists(file.path(smpPath, paste(basename(smpPath), "_RES.csv", 
                                                sep="")))) {
        tkmessageBox(message="No results found... Please classify data first!")
      } else {
        meta <- read.table(metaFile, sep=";", dec=",", header=TRUE)
        dat <- read.table(file.path(smpPath, paste(basename(smpPath), "_RES.csv", 
                                                   sep="")), 
                          sep=";", dec=".", header=TRUE)
        
        # Select groups of interest
        tt <- tktoplevel()
        tktitle(tt) <- "Select groups"
        tkwm.resizable(tt, FALSE, FALSE)
        
        dataVarList <- tk2listbox(tt, selectmode="multiple", 
                                  activestyle="dotbox", height=15, 
                                  width=40, autoscroll="none", 
                                  background="white")
        tkgrid(tk2label(tt, text="Please select groups-of-interest (multiple selection):"), 
               row=1, column=0, pady=c(10,0), sticky="w")
        tkgrid(dataVarList, row=2, column=0)
        sapply(unique(dat$Group), function(x) tkinsert(dataVarList, "end", x))
        tkselection.set(dataVarList, 0)
        
        saveOpt <- "1"
        tt$env$saveOpt <- tk2checkbutton(tt, text="Save all generated plots")
        saveOptValue <- tclVar(saveOpt)
        tkconfigure(tt$env$saveOpt, variable=saveOptValue)
        tkgrid(tt$env$saveOpt, padx=20, pady=5, sticky="w")
        
        ValidButton <- tk2button(tt, text="OK", width=-6, 
                                 command=function() {
          grp <<- unique(dat$Group)[as.numeric(tkcurselection(dataVarList))+1]
          saveOpt <<- as.character(tclvalue(saveOptValue))
          
          tkdestroy(tt)
          if (!length(grp))
            tkmessageBox("No groups selected...!")
        })
        tkgrid(ValidButton, padx=10, pady=c(10, 15))
        tkwait.window(tt)
        
        # Barplots
        if(any(!is.na(meta$Date))) {
          dat$Date <- NA
          for (j in 1:NROW(meta))
            dat$Date[which(dat$Sample == meta$Sample[j])] <- meta$Date[j]
          dat$Date <- as.POSIXct(dat$Date)
          
          if (any(is.numeric(meta$Volume_ml))) {
            dat$CountVol <- NA
            for (j in 1:NROW(meta))
              dat$CountVol[which(dat$Sample == meta$Sample[j])] <- 
                round(dat$Count[which(dat$Sample ==
                                        meta$Sample[j])]*1000/meta$Volume_ml[j])
          }
          cat("\nPlotting Counts and Relative Counts...")
          datGrp <- dat[which(dat$Group %in% grp), ]
          saveFinalPlot(datGrp, smpPath, saveFinal=as.logical(as.integer(saveOpt)))

          datForm <- matrix(data=0, nrow=length(unique(dat$Date)), 
                            ncol=length(unique(dat$Group)))
          for (f in 1:length(unique(dat$Date)))
            datForm[f, ] <- dat$CountVol[which(dat$Date==unique(dat$Date)[f])]
          rownames(datForm) <- as.character(unique(dat$Date))
          colnames(datForm) <- unique(dat$Group)
          write.table(datForm, file.path(smpPath, paste(basename(smpPath), 
                                                        "_RES_FORMAT.csv", 
                                                        sep="")), 
                      row.names=TRUE, col.names=NA, sep=";")
          
          cat("\nDone!\n")
        }

        # Maps
        if(any(is.numeric(meta$Longitude)) && any(is.numeric(meta$Latitude))) {
          dat$Longitude <- 0
          dat$Latitude <- 0
          for (j in 1:NROW(meta)) {
            dat$Longitude[which(dat$Sample == meta$Sample[j])] <- meta$Longitude[j]
            dat$Latitude[which(dat$Sample == meta$Sample[j])] <- meta$Latitude[j]
          }
          if (any(is.numeric(meta$Volume_ml))) {
            dat$CountVol <- NA
            for (j in 1:NROW(meta))
              dat$CountVol[which(dat$Sample == meta$Sample[j])] <- 
                round(dat$Count[which(dat$Sample == meta$Sample[j])]*1000/meta$Volume_ml[j])
          }
          saveMap(dat, grp, smpPath, saveFinal=as.logical(as.integer(saveOpt)))
          
        } else {
          if (any(is.numeric(meta$Volume_ml))) {
            dat$CountVol <- NA
            for (j in 1:NROW(meta))
              dat$CountVol[which(dat$Sample == meta$Sample[j])] <- 
                round(dat$Count[which(dat$Sample == meta$Sample[j])]*1000/meta$Volume_ml[j])
          }
          saveCounts(dat, grp, smpPath, saveFinal=as.logical(as.integer(saveOpt)))
        }
      }
    }
  })
  tkgrid(viewButton, row=7, padx=c(50,0), pady=c(0,10), column=1, columnspan=4)
  
  #########################
  ###   'MORE' button   ###
  #########################
  # Select the Python script for CNN
  moreButton <- tk2button(mainWindow, text="", image="plus",
                          width=2, command=function() {
    addToolsWindow()
  })
  tkgrid(moreButton, row=13, column=6, sticky="e")
  
  tkgrid(tklabel(mainWindow, text="TRANSFER LEARNING\nfor MARINE ECOSYSTEM MONITORING", 
                 font=fontTitle, foreground="dodgerblue4"), padx=c(0,40), 
         row=0, column=0, rowspan=2, columnspan=13)
  l <- ttklabel(mainWindow, image="ifremerLogo", compound="image")
  tkgrid(l, row=9, column=1, columnspan=4, rowspan=2, padx=c(50,0))
  tkgrid(tklabel(mainWindow, text="Institut Francais de Recherche\npour l Exploitation de la MER", 
                 font=fontText, foreground="dodgerblue4"),
         row=11, column=1, columnspan=4, padx=c(50,0))
  tkgrid(tklabel(mainWindow, text="Departement Oceanographie et Dynamique des ecosystemes", 
                 font=fontText, foreground="dodgerblue4"),
         row=12, column=1, columnspan=4, padx=c(50,0))
  tkgrid(tklabel(mainWindow, text="Unite Littoral", 
                 font=fontText, foreground="dodgerblue4"),
         row=13, column=1, columnspan=4, padx=c(50,0))
  l <- ttklabel(mainWindow, image="phyto", compound="image")
  tkgrid(l, row=2, column=0, rowspan=9, padx=c(40,0))
  l <- ttklabel(mainWindow, image="fish", compound="image")
  tkgrid(l, row=2, column=6, rowspan=9, padx=c(0,40))
}