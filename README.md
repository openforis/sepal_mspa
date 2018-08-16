#### MSPA-analysis wrapper

#############################################################
# Purpose: 
#   run MSPA for the user-selected forest mask in SEPAL
#   Based on the original version available in GuidosToolbox

############################################################## 
# rstudio-Shiny wrapper of a custom script for FAO-FRA, SEPAL ( Ubuntu 16.04.3 LTS )
# more info at:
#  - http://forest.jrc.ec.europa.eu/download/software/guidos/mspa/

# Adapted from: Peter Vogt <peter.vogt@ec.europa.eu>


#############################################################
# Processing steps:
# 1) test MSPA-compatibility of input image
# 2) process for MSPA
# 3) post-process (add back geoheader, amend to final file name, dostats)
