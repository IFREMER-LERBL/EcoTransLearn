# EcoTransLearn
Images classification by Transfer Learning for ecological studies

EcoTransLearn is an R-package including a simple Graphical User Interface (GUI) dedicated to image classification by Transfer Learning for various ecological studies, such as the identification of phytoplankton (via FlowCam or flow cytometers images), zoo-plankton (via ZooScan images or photomicrographs) or simple pictures of fish or benthic organisms.

## Installation

**R environment**

The version 1.0-0 of the EcoTransLearn package needs a recent version of R (version 4.0.x or upper). It can be directly downloaded on the CRAN website.

The R-packages needed by EcoTransLearn are: colorRamps, ggplot2, grid, jpeg, mapplots, maps, randomForest, reticulate, SDMTools, shapefiles, stringr, svDialogs, svMisc, tcltk2, tiff and zooimage.

**Python environment**

The EcoTransLearn package needs the version 3.7 (or upper) of Python.

The Python libraries needed by EcoTransLearn are: matplotlib, numpy, pandas, sklearn and tensorflow.

**EcoTransLearn installation**

EcoTransLearn is an R-package which can be downloaded (Releases/v1.0) and installed directly from the R console with the following command line: 

*install.packages("/.../EcoTransLearn_1.0.tar.gz", repos = NULL, type = "source")*

## Graphical User Interface

![image](https://user-images.githubusercontent.com/104447521/178229132-b9f46d4e-7164-49d4-b376-870c5dc7077a.png)

## General workflow

![image](https://user-images.githubusercontent.com/104447521/178229386-406f1266-6126-4845-a4bc-46482f9f246e.png)

## Funding

This work has been financially supported by the European Union (ERDF), the French State, the French Region Hauts-de-France and Ifremer, in the framework of the project CPER MARCO 2015-2021.

The JERICO-S3 project is funded by the European Commission's H2020 Frame-work Programme under grant agreement No. 871153. Project coordinator: Ifremer, France.

This work has been carried out through the project S3-EUROHAB (Sentinel-3 products for detecting EUtROphicationanf Harmful Algal Bloom events) funded by the European Regional Develoment Fund through the INTERREG France-Channel-England.

