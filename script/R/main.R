# Main script for RE-biodiversity-nexus-2020
# Author: Chihiro Haga

# initialize variables +++++++++++++++++++++++++++++++++++++++++++++++++
driveLetter <- "C"
root.dir <- paste0(driveLetter, ":/Projects/Project-RE-biodiversity-2020")
script.dir <- file.path(root.dir, "script/R")
source(file.path(script.dir, 'initialize.R'))

# MAIN ===================================================================
cpu_num <- parallel::detectCores(logical = TRUE) # get an available number of cpu
cl <- makePSOCKcluster(cpu_num)
registerDoParallel(cl)
objects <- ls(envir=parent.frame())
t.start <- Sys.time()
foreach (scenario = scenario.name.list, # roop iterator
         .packages = package_list, # packages
         .export=objects # objects
) %dopar% {
  for (climate in climate.name.list) {
    scenario.dir <- file.path(root.dir, scenario, climate)
    computeLULC()
    computeOwlHSI()
    evaluate_owl_habitat()
    computeKumatakaHSI()
    computeDiversity()
  }
}
stopCluster(cl) # Stop clusters for multiprocessing
cat(paste('Elapsed time', round(difftime(Sys.time(), t.start, units = 'hours'), digits = 2), ' (hours)\n\n'))

# Compute energy
computeEnergy()
