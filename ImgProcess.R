# Create vignettes (grayscale or color)
ImgProcess <- function(datDir, tempDir, rgbCol=FALSE) {
  resPath <- as.character(format(Sys.time(), "%Y-%m-%d %Hh%Mm%Ss"))
  if (!length(datDir) || !file.exists(datDir) || !file.info(datDir)$isdir) 
    return(invisible(NULL))
  Lst <- dir(file.path(datDir), pattern = "\\.lst$", full.names = TRUE)[1]
  if (length(Lst)) {
    dat <- readFlowCAMlst(Lst, skip = 2, read.ctx = TRUE)
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
    tif <- dir(sampledir, pattern = "[0-9]\\.tif$", full.names = TRUE)
    isCal <- grepl("^.*cal_image_[0-9]+\\.tif$", tif)
    if (!isTRUE(rgbCol)) {
      calFiles <- tif[isCal]
      if (length(calFiles) == 0) 
        stop("No background calibration image found")
      cals <- list()
      for (i in 1:length(calFiles)) {
        cals[[i]] <- readTIFF(source = calFiles[i])
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
        collage <- readTIFF(source = file.path(sampledir, 
                                               filename))
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
      VigName <- paste(label, i, sep = "_")
      writeJPEG(image = vig2, target = file.path(tempDir, 
                                                 paste(VigName, ".jpg", sep="")), 
                quality = 0.95)
    }
  }
}

# Read FlowCam images (grayscale and color)
readColFlowCAMlst <- function (lst, skip = 2, read.ctx = TRUE) {
  if (!file.exists(lst)) 
    stop("'lst' must be an existing (.lst) file")
  skip <- as.integer(skip)[1]
  if (skip < 2) {
    warning("'skip' cannot be lower than 2... fixed!")
    skip <- 2
  }
  read.ctx <- isTRUE(as.logical(read.ctx))
  header <- scan(lst, what = character(), nlines = 2L, quiet = TRUE)
  if (as.integer(header[1]) >= 17 && substr(header[2], 1, 10) == 
      "num-fields") {
    nfields <- as.integer(strsplit(header[2], "|", 
                                   fixed = TRUE)[[1]][2])
    if (!length(nfields) || is.na(nfields) || nfields < 44) 
      stop("Unrecognized .lst file format: number of fields is ", 
           nfields)
    skip <- nfields + 2
    hcol <- scan(lst, what = character(), sep = "|", 
                 skip = 2L, nlines = nfields, quiet = TRUE)
    if (length(hcol) != nfields * 2) 
      stop("Unrecognized .lst file format: incorrect header")
    hcol <- matrix(hcol, ncol = 2, byrow = TRUE)
    cnames <- hcol[, 1]
    capital <- function(x) {
      s <- strsplit(x, "_")
      sapply(s, function(x) paste(toupper(substring(x, 
                                                    1, 1)), substring(x, 2), 
                                  sep = "", collapse = "_"))
    }
    
    cnames <- paste("FIT", capital(cnames), sep = "_")
    cnames <- sub("Abd", "ABD", cnames)
    cnames <- sub("Esd", "ESD", cnames)
    cnames <- sub("FIT_Ch([1-9])_Width", "FIT_Ch\\1_TOF", cnames)
    cnames[cnames == "FIT_Id"] <- "Id"
    cnames[cnames == "FIT_ABD_Area"] <- "FIT_Area_ABD"
    cnames[cnames == "FIT_ABD_Diameter"] <- "FIT_Diameter_ABD"
    cnames[cnames == "FIT_ESD_Diameter"] <- "FIT_Diameter_ESD"
    cnames[cnames == "FIT_Raw_Perimeter"] <- "FIT_Raw_Perim"
    cnames[cnames == "FIT_Raw_Convex_Perimeter"] <- "FIT_Raw_Convex_Perim"
    cnames[cnames == "FIT_Collage_File"] <- "FIT_Filename"
    cnames[cnames == "FIT_Timestamp"] <- "FIT_Timestamp1"
    cnames[cnames == "FIT_Image_X"] <- "FIT_SaveX"
    cnames[cnames == "FIT_Image_Y"] <- "FIT_SaveY"
    cnames[cnames == "FIT_Image_W"] <- "FIT_PixelW"
    cnames[cnames == "FIT_Image_H"] <- "FIT_PixelH"
    cnames[cnames == "FIT_Src_X"] <- "FIT_CaptureX"
    cnames[cnames == "FIT_Src_Y"] <- "FIT_CaptureY"
    cnames[cnames == "FIT_Src_Image"] <- "FIT_Source_Image"
    cnames[cnames == "FIT_Cal_Image"] <- "FIT_Calibration_Image"
    tab <- read.table(lst, header = FALSE, sep = "|", 
                      dec = ".", skip = skip, col.names = cnames)
    tab$FIT_Ch1_Peak <- NA
    tab$FIT_Ch1_TOF <- NA
    tab$FIT_Ch2_Peak <- NA
    tab$FIT_Ch2_TOF <- NA
    tab$FIT_Ch3_Peak <- NA
    tab$FIT_Ch3_TOF <- NA
    tab$FIT_Ch4_Peak <- NA
    tab$FIT_Ch4_TOF <- NA
    tab$FIT_High_U32 <- NA
    tab$FIT_Low_U32 <- NA
    tab$FIT_Total <- NA
    tab$FIT_Timestamp2 <- as.character(NA)
  }
  else {
    ncol <- length(read.table(lst, header = FALSE, sep = ":", 
                              dec = ".", skip = skip, nrows = 1))
    if (ncol == 44) {
      tab <- read.table(lst, header = FALSE, sep = ":", 
                        dec = ".", skip = skip, col.names = c("Id", 
                                                              "FIT_Cal_Const", "FIT_Raw_Area", 
                                                              "FIT_Raw_Feret_Max", "FIT_Raw_Feret_Min", 
                                                              "FIT_Raw_Feret_Mean", "FIT_Raw_Perim", 
                                                              "FIT_Raw_Convex_Perim", "FIT_Area_ABD", 
                                                              "FIT_Diameter_ABD", "FIT_Length", 
                                                              "FIT_Width", "FIT_Diameter_ESD", 
                                                              "FIT_Perimeter", "FIT_Convex_Perimeter", 
                                                              "FIT_Intensity", "FIT_Sigma_Intensity", 
                                                              "FIT_Compactness", "FIT_Elongation", 
                                                              "FIT_Sum_Intensity", "FIT_Roughness", 
                                                              "FIT_Feret_Max_Angle", "FIT_Avg_Red", 
                                                              "FIT_Avg_Green", "FIT_Avg_Blue", 
                                                              "FIT_PPC", "FIT_Ch1_Peak", "FIT_Ch1_TOF", 
                                                              "FIT_Ch2_Peak", "FIT_Ch2_TOF", 
                                                              "FIT_Ch3_Peak", "FIT_Ch3_TOF", 
                                                              "FIT_Ch4_Peak", "FIT_Ch4_TOF", 
                                                              "FIT_Filename", "FIT_SaveX", "FIT_SaveY", 
                                                              "FIT_PixelW", "FIT_PixelH", "FIT_CaptureX", 
                                                              "FIT_CaptureY", "FIT_High_U32", 
                                                              "FIT_Low_U32", "FIT_Total"))
      tab$FIT_Feret_Min_Angle <- NA
      tab$FIT_Edge_Gradient <- NA
      tab$FIT_Timestamp1 <- NA
      tab$FIT_Timestamp2 <- NA
      tab$FIT_Source_Image <- NA
      tab$FIT_Calibration_Image <- NA
    }
    else if (ncol == 47) {
      tab <- read.table(lst, header = FALSE, sep = ":", 
                        dec = ".", skip = skip, col.names = c("Id", 
                                                              "FIT_Cal_Const", "FIT_Raw_Area", 
                                                              "FIT_Raw_Feret_Max", "FIT_Raw_Feret_Min", 
                                                              "FIT_Raw_Feret_Mean", "FIT_Raw_Perim", 
                                                              "FIT_Raw_Convex_Perim", "FIT_Area_ABD", 
                                                              "FIT_Diameter_ABD", "FIT_Length", 
                                                              "FIT_Width", "FIT_Diameter_ESD", 
                                                              "FIT_Perimeter", "FIT_Convex_Perimeter", 
                                                              "FIT_Intensity", "FIT_Sigma_Intensity", 
                                                              "FIT_Compactness", "FIT_Elongation", 
                                                              "FIT_Sum_Intensity", "FIT_Roughness", 
                                                              "FIT_Feret_Max_Angle", "FIT_Feret_Min_Angle", 
                                                              "FIT_Avg_Red", "FIT_Avg_Green", 
                                                              "FIT_Avg_Blue", "FIT_PPC", "FIT_Ch1_Peak", 
                                                              "FIT_Ch1_TOF", "FIT_Ch2_Peak", 
                                                              "FIT_Ch2_TOF", "FIT_Ch3_Peak", 
                                                              "FIT_Ch3_TOF", "FIT_Ch4_Peak", 
                                                              "FIT_Ch4_TOF", "FIT_Filename", 
                                                              "FIT_SaveX", "FIT_SaveY", "FIT_PixelW", 
                                                              "FIT_PixelH", "FIT_CaptureX", "FIT_CaptureY", 
                                                              "FIT_Edge_Gradient", "FIT_Timestamp1", 
                                                              "FIT_Timestamp2", "FIT_Source_Image", 
                                                              "FIT_Calibration_Image"))
      tab$FIT_High_U32 <- NA
      tab$FIT_Low_U32 <- NA
      tab$FIT_Total <- NA
    }
    else stop("Unrecognized FlowCAM format")
  }
  tab$FIT_Volume_ABD <- (4/3) * pi * (tab$FIT_Diameter_ABD/2)^3
  tab$FIT_Volume_ESD <- (4/3) * pi * (tab$FIT_Diameter_ESD/2)^3
  tab$FIT_Aspect_Ratio <- tab$FIT_Width/tab$FIT_Length
  tab$FIT_Transparency <- 1 - (tab$FIT_Diameter_ABD/tab$FIT_Diameter_ESD)
  tab$FIT_Red_Green_Ratio <- tab$FIT_Avg_Red/tab$FIT_Avg_Green
  tab$FIT_Blue_Green_Ratio <- tab$FIT_Avg_Blue/tab$FIT_Avg_Green
  tab$FIT_Red_Blue_Ratio <- tab$FIT_Avg_Red/tab$FIT_Avg_Blue
  tab$FIT_Ch2_Ch1_Ratio <- tab$FIT_Ch2_Peak/tab$FIT_Ch1_Peak
  label <- basename(dirname(lst))
  if (label == ".") 
    label <- basename(getwd())
  ctx <- sub("\\.lst$", ".ctx", lst)
  if (read.ctx && file.exists(ctx)) {
    ctxData <- readFlowCAMctx(ctx)
  }
  else {
    ctxData <- list(Fraction = data.frame(Label = label, 
                                          Code = "", Min = -1, Max = -1), 
                    Process = data.frame(Label = label, Version = "1.0-0", 
                                         Method = "Direct VS import", MinSize = NA, 
                                         MaxSize = NA, UseECD = NA), 
                    Subsample = data.frame(Label = label, SubPart = 0.01, 
                                           SubMethod = 1, CellPart = 1, Replicates = 1,
                                           VolIni = 1, VolPrec = 0.1))
  }
  Sub <- ctxData$Subsample
  n <- nrow(tab)
  items <- tab$Id
  tab$Id <- NULL
  dil <- 1/(Sub$SubPart * Sub$CellPart * Sub$Replicates * Sub$VolIni)
  tab <- cbind(data.frame(Label = rep(label, n), Item = items, 
                          ECD = ecd(tab$FIT_Area_ABD)), tab, 
               data.frame(Dil = rep(dil, n)))
  attr(tab, "metadata") <- ctxData
  class(tab) <- "data.frame"
  tab
}