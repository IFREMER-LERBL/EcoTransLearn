addToolsWindow <- function(opt="Process FlowCam data (collages)") {
  
  # Create new window for additional tools
  tt <- tktoplevel()
  tktitle(tt) <- "MORE..."
  tkwm.resizable(tt, FALSE, FALSE)
  
  # Acquisition devices
  fontTitle <- tkfont.create(family="Arial", size=12, weight="bold")
  fontText <- tkfont.create(family="Helvetica", size=8, weight="bold")
  tkgrid(tk2label(tt, text="What do you want to do?", font=fontTitle), row=1, sticky="w")
  
  optValue <- tclVar(opt)
  opt.title <- c("Process FlowCam data (collages)","Process CytoSense/Sub data", 
                 "Generate train and test .CSV files","Generate metadata file",
                 "Install Python and libraries","Close all figures","Read tutorial", "About...")
  callback <- function() print(tclvalue(optValue))
  sapply(opt.title, function(i) {
    radio_button <- ttkradiobutton(tt, variable = optValue, 
                                   text = i, value = i)
    tkgrid(radio_button, padx=20, pady=5, sticky="w")
  })
  
  onOK <- function() {
    opt <<- as.character(tclvalue(optValue))
    
    if (opt=="Process FlowCam data (collages)") {
      tkmessageBox(message = "PUT R SCRIPT HERE...")
    }
    
    if (opt=="Process CytoSense/Sub data") {
      tkmessageBox(message = "TO DO IF NECESSARY...")
    }
    
    if (opt=="Generate train and test .CSV files") {
      # Create CSV files for training and test datasets
      ttt <- tktoplevel()
      tktitle(ttt) <- "Create CSV files for train and test datasets"
      tkwm.resizable(ttt, FALSE, FALSE)
      
      dirButton <- tk2button(ttt, text = "SELECT TRAIN or TEST DIRECTORIES", image = "folder",
                             compound = "left", width = 40, command = function() {
        trtePath <<- tk_choose.dir(default = getwd())
        if (is.na(trtePath)) {
          tkmessageBox(message = "No directory selected!")
        } else {
          tkgrid(tklabel(ttt, text=basename(trtePath), font=fontText, 
                         foreground = "darkgreen"),
                 padx=c(50,50), row=2, column=0, columnspan=2)
        }
      })
      tkgrid(tklabel(ttt, text=""), row=0)
      tkgrid(tklabel(ttt, text=""), row=2)
      tkgrid(dirButton, row=1, padx=c(50,50), column=0, columnspan=2)
      
      ValidButton <- tk2button(ttt, text = "GENERATE CSV", image = "csv", 
                               compound = "left", width = 30, command = function() {
        if (is.na(trtePath)) {
          tkmessageBox(message = "No directory selected!")
        } else {
          dattrte <- NULL
          imgf <- list.dirs(trtePath, recursive = F)
          for (i in 1:length(imgf)) {
            datTempImg <- cbind(rep(basename(imgf[i]), length(list.files(imgf[i]))), list.files(imgf[i]))
            dattrte <- rbind(dattrte, datTempImg)
          }
          dattrte <- as.data.frame(dattrte)
          names(dattrte) <- c("Label", "Image")
          write.table(dattrte, paste(trtePath, ".csv", sep=""), row.names = F, sep=",")
                             
          tkdestroy(ttt)
        }
      })
      tkgrid(ValidButton, padx=c(50,50), pady = c(10, 15), column = 0, columnspan=2)
      tkwait.window(ttt)
    }
    
    if (opt=="Generate metadata file") {
      # Create CSV file for metadata
      ttt <- tktoplevel()
      tktitle(ttt) <- "Create CSV file for metadata"
      tkwm.resizable(ttt, FALSE, FALSE)
      
      dirButton <- tk2button(ttt, text = "SELECT DATA DIRECTORY", image = "folder",
                             compound = "left", width = 40, command = function() {
        metaPath <<- tk_choose.dir(default = getwd())
        if (is.na(metaPath)) {
          tkmessageBox(message = "No directory selected!")
        } else {
          tkgrid(tklabel(ttt, text=basename(metaPath), font=fontText, 
                         foreground = "darkgreen"),
                 padx=c(50,50), row=2, column=0, columnspan=2)
        }
      })
      tkgrid(tklabel(ttt, text=""), row=0)
      tkgrid(tklabel(ttt, text=""), row=2)
      tkgrid(dirButton, row=1, padx=c(50,50), column=0, columnspan=2)
      
      ValidButton <- tk2button(ttt, text = "GENERATE CSV", image = "csv", 
                               compound = "left", width = 30, command = function() {
        if (is.na(metaPath)) {
          tkmessageBox(message = "No directory selected!")
        } else {
          metaDirs <- list.dirs(metaPath, recursive = F)
          Sample <- NULL
          Date <- NULL
          Volume <- NULL
          for (i in metaDirs) {
            # Collect metadata
            Sample <- c(Sample, basename(i))
            smpName <- list.files(i, pattern = "_run_summary.txt", full.names = TRUE)
            restxt <- readLines(con <- file(smpName, encoding = "UTF-8"))
            Date <- c(Date, as.character(str_extract(restxt[grep("Start:", restxt)], "\\d+-\\d+-\\d+ \\d+:\\d+:\\d+")))
            Volume <- c(Volume, as.character(sub(".", ",", str_extract(restxt[grep("Volume Imaged:", restxt)], 
                                                                     "\\d+\\.*\\d*"), fixed = TRUE)))
            #meta <- rbind(meta, c(smp, volume))
            meta <- cbind(Sample, Date, Volume)
            close(con)
          }
          meta <- as.data.frame(meta)
          datmeta <- data.frame(Sample=Sample,
                                Date=Date,
                                Volume_ml=Volume,
                                Longitude=NA,
                                Latitude=NA,
                                Depth_m=1)
          
          write.table(datmeta, file.path(metaPath, paste(basename(metaPath), "_metadata.csv", sep="")), 
                      row.names = FALSE, sep=";")
          tkmessageBox(message = paste("Metadata file created and saved in:\n",
                                       file.path(metaPath, paste(basename(metaPath), "_metadata.csv", sep="")), sep=""))
          tkdestroy(ttt)
        }
      })
      tkgrid(ValidButton, padx=c(50,50), pady = c(10, 15), column = 0, columnspan=2)
      tkwait.window(ttt)
    }
    
    if (opt=="Install Python and libraries") {
      msg <- tkmessageBox(message = "1. Install 'Anaconda' environment: https://www.anaconda.com/products/individual
                  \n2. Install Python (version 3.7 or more)
                  \n3. Install 'tensorflow', 'sklearn', 'numpy' and 'pandas' libraries")
    }
    
    if (opt=="Close all figures") {
      graphics.off()
    }
    
    if (opt=="Read tutorial") {
      cat("Open 'ImgTLClass' package - Tutorial (PDF file)...")
      system('open "./tutorial/ImgTLClass_package_tutorial.pdf"')
      cat("Done!")
    }
    
    if (opt=="About...") {
      msg <- tkmessageBox(message = "ImgTLClass: version 1.0-0
                  \nLicense GNU: ...
                  \nIFREMER: French Research Institute for Exploitation of the Sea
                  \n\nOnce upon a time...")
    }
    
    tkdestroy(tt)
  }
  tt$env$butOK <- tk2button(tt, text="OK", width=10, command=onOK)
  tkgrid(tt$env$butOK, padx=10, columnspan=2, pady=c(0, 5))
  tkwait.window(tt)
}