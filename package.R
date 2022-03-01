if (! ("devtools" %in% rownames(installed.packages()))) { install.packages("devtools") }
base::require("devtools")

if (! ("roxygen2" %in% rownames(installed.packages()))) { install.packages("roxygen2") }
base::require("roxygen2")
getwd()
setwd("D:/ENSAI/Sanofi/Rcode/SynergyEvaluation")
devtools::create("SynergyEvaluation")

devtools::document()
devtools::build(pkg="R")

#usethat::use_vignette("introduction")

devtools::install(pkg="R")
#remove.packages("SynergyEvaluation")
#library(SynergyEvaluation)
