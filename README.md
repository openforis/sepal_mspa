#### MSPA-analysis wrapper

> **Note**  
> :warning: This repository have been archived, the MSPA functionalities are now part of the [GWB app](https://github.com/sepal-contrib/gwb). :warning:

#############################################################

Purpose:  run MSPA for the user-selected forest mask in SEPAL

Based on the original version available in GuidosToolbox

Check here: http://forest.jrc.ec.europa.eu/download/software/guidos/mspa/

############################################################## 

This application is a Rstudio-Shiny wrapper of a custom script for FAO-FRA, SEPAL ( Ubuntu 16.04.3 LTS )

Adapted from: Peter Vogt <peter.vogt@ec.europa.eu>


#############################################################

Processing steps:

 1) test MSPA-compatibility of input image
 
 2) process for MSPA
 
 3) post-process (add back geoheader, amend to final file name, dostats)
