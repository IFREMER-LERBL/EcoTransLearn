ImgProcess <-
function(datDir, tempDir, rgbCol=FALSE) {
  
  resPath <- as.character(format(Sys.time(), "%Y-%m-%d %Hh%Mm%Ss"))
  if (!length(datDir) || !file.exists(datDir) || !file.info(datDir)$isdir) 
    return(invisible(NULL))
  Lst <- dir(file.path(datDir), pattern="\\.lst$", full.names=TRUE)[1]
  if (length(Lst)) {
    dat <- readColFlowCAMlst(Lst, skip=2, read.ctx=TRUE)
    if (!is.data.frame(dat) && NROW(dat) < 1) 
      stop("Problem while importing FlowCAM data, or empty series")
    sampledir <- dirname(Lst)
    if (sampledir == ".") 
      sampledir <- getwd()
    label <- basename(sampledir)
    parentdir <- dirname(sampledir)
    odir <- setwd(parentdir)
    on.exit(setwd(odir))
    message("Creating vignettes...")
    tif <- dir(sampledir, pattern="[0-9]\\.tif$", full.names=TRUE)
    isCal <- grepl("^.*cal_image_[0-9]+\\.tif$", tif)
    if (!isTRUE(rgbCol)) {
      calFiles <- tif[isCal]
      if (length(calFiles) == 0) 
        stop("No background calibration image found")
      cals <- list()
      for (i in 1:length(calFiles)) {
        cals[[i]] <- readTIFF(source=calFiles[i])
        cdim <- dim(cals[[i]])
        if (length(cdim) == 3 && cdim[3] == 3) {
          cals[[i]] <- 0.2126 * cals[[i]][, , 1] + 0.7152 * 
            cals[[i]][, , 2] + 0.0722 * cals[[i]][, , 3]
        }
        if (length(dim(cals[[i]])) != 2) 
          stop("unrecognized calibration image type; ", 
               "cannot convert it to 8-bit grayscale")
      }
    }
    colFiles <- tif[!isCal]
    if (length(colFiles) == 0)
      stop("No collages found")
    colFile <- ""
    dat1 <- dat
    dat1$FIT_SaveX <- dat$FIT_SaveX + 1
    dat1$FIT_SaveY <- dat$FIT_SaveY + 1
    dat1$FIT_CaptureX <- dat$FIT_CaptureX + 1
    dat1$FIT_CaptureY <- dat$FIT_CaptureY + 1
    if (isTRUE(rgbCol)) {
      crop <- function(mat, coords) mat[coords[2]:coords[4], coords[1]:coords[3],]
    } else {
      crop <- function(mat, coords) mat[coords[2]:coords[4], coords[1]:coords[3]]
    }
    if (!isTRUE(rgbCol)) {
      gray <- attr(dat, "metadata")$CaptureParameters$ThresholdLight
      if (!length(gray)) {
        warning("Unknown threshold gray level; using 40")
        gray <- 40
      }
      gray <- gray/255
      threshold <- 1 - 2 * gray
    }
    nmax <- nrow(dat1)
    for (i in 1:nmax) {
      progress(i, nmax + 1)
      d <- dat1[i, ]
      if (as.character(d$FIT_Filename) != colFile) {
        filename <- as.character(d$FIT_Filename)
        collage <- readTIFF(source=file.path(sampledir, filename))
        colFile <- d$FIT_Filename
        colFiles <- colFiles[colFiles != filename]
        cdim <- dim(collage)
        if (!isTRUE(rgbCol)) {
          if (length(cdim) == 3 && cdim[3] == 3) {
            collage <- 0.2126 * collage[, , 1] + 0.7152 * 
              collage[, , 2] + 0.0722 * collage[, , 3]
          }
          if (length(dim(collage)) != 2) 
            stop("unrecognized collage image type; ", 
                 "cannot convert it to 8-bit grayscale")
        }
      }
      size <- c(d$FIT_PixelW, d$FIT_PixelH) - 1
      colCoords <- c(d$FIT_SaveX, d$FIT_SaveY)
      colCoords <- c(colCoords, colCoords + size)
      vig <- crop(collage, colCoords)
      vig2 <- vig
      if (!isTRUE(rgbCol)) {
        calCoords <- c(d$FIT_CaptureX, d$FIT_CaptureY)
        calCoords <- c(calCoords, calCoords + size)
        if (is.na(d$FIT_Calibration_Image)) 
          d$FIT_Calibration_Image <- 1
        back <- crop(cals[[d$FIT_Calibration_Image]], calCoords)
        vig2 <- 1 + vig - back - gray
        vig2[vig2 > 1] <- 1
        vig2[vig2 < 0] <- 0
      }
      VigName <- paste(label, i, sep="_")
      writeJPEG(image=vig2, target=file.path(tempDir,
                                             paste(VigName, ".jpg", sep="")), 
                quality=0.95)
    }
  }
}