# Load libraries
library(tidyverse)
library(viridis)
library(ggthemes)
library(skimr)
library(knitr)
library(raster)
library(rasterVis)
library(progress)
library(feather)
library(rgdal)
library(sf)
library(tcltk)
library(readr)
library(stringr)
library(doParallel) # for parallel computing

#' Initialize variables
input.dir <- file.path(root.dir, "input")
data.dir <- file.path(root.dir, "data")
analysis.dir <- file.path(root.dir, 'Analysis')
lulc.dir <- file.path(root.dir, "Analysis/LULC_maps")
owl.dir <- file.path(root.dir, 'Analysis/owl_hsi')
owl.eval.dir <- file.path(root.dir, 'Analysis/owl_hsi/owl_eval')
kumataka.dir <- file.path(root.dir, 'Analysis/kumataka_hsi')
div.dir <- file.path(root.dir, 'Analysis/diversity')
energy.dir <- file.path(root.dir, 'Analysis/energy')

# parameters ------
agri.ids <- c(203, 301:305) # ID for agricultural land use type
cell.len <- 100 # m
tms.list <- 1:85
#                  E-needle    D-needle    D-broadfeaf                                                 grass-species
#                  todomatsu   karamatsu   shirakaba   harunire    hannoki     mizunara    yachidamo   bokusou     sasa
spp.name.list <- c("abiesach", "larikaem", "betuplat", "ulmudavi", "alnujapo", "quercris", "fraxmand", "pastgras", "sasagras")
pasture.prs <- 24 + 1

# scenario variables ------
setwd(root.dir)
file.name.list <- list.files()
file.names <- c(file.name.list)
scenario.name.list <- str_subset(file.names, pattern="_s")  # pick up file names including "_s"
climate.name.list <- c("MRI_rcp26", "MRI_rcp85")

# lookup tables -------
a.fct <- factor(x = c('A0', 'A45', 'A89', 'A134', 'A178', 'A223'),
                levels = c('A0', 'A45', 'A89', 'A134', 'A178', 'A223'))
a.ludf <- tibble(a = paste0('a', c(0, 45, 89, 134, 178, 223)), alevel = a.fct)
s.fct <- factor(format(seq(0, 1.0, by = 0.2), digits = 1))
s.ludf <- tibble(s = paste0('s', format(seq(0, 1.0, by = 0.2), nsmall = 1)), slevel = s.fct)
spp.ludf <- tibble(sid = 1:9, sppname = spp.name.list)
varname.fct <- factor(x = c('Diversity', 'HSIs', 'HSIk', 'RE'), 
                      levels = c('Diversity', 'HSIs', 'HSIk', 'RE'))
varname.ludf <- data.frame(varname = c('gridH', 'kumataka_hsi', 'owl_hsi', 'energy'),
                           varnamelevel = varname.fct)

# Figure settings ------
source(file.path(script.dir, 'figure_settings.R'))

# Funtions for analysis
source(file.path(script.dir, 'computeLULC.R'))
source(file.path(script.dir, 'computeOwlHSI.R'))
source(file.path(script.dir, 'evaluate_owl_habitat.R'))
source(file.path(script.dir, 'computeKumatakaHSI.R'))
source(file.path(script.dir, 'computeDiversity.R'))
source(file.path(script.dir, 'computeEnergy.R'))
package_list <- c("rgdal", "raster", "sf", "tidyverse", "readr", "viridis", "rasterVis", "ggthemes", "stringr", 'feather')
