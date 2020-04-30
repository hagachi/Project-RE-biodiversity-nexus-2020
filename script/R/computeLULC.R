# Setting ==================================================================
# load function from script
source(paste0(script.dir, "/function_makeDominantSppMap.R"))

computeLULC <- function() {
  for(tms in tms.list){
    # Detect dominant species =================================
    dominant.spp.ras <- MakeDominantSppMap(root.dir, scenario.dir, scenario, tms, spp.name.list) # SEE 'function_makeDominantSppMap.R'
    out.name <- paste0(root.dir, "/Analysis/LULC_maps/dominantSpp-", scenario, "_", climate, "_", tms, ".tif")
    writeRaster(dominant.spp.ras, out.name, overwrite = TRUE)
  }
}
