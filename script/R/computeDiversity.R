# title: compute biomass and lulc diversity
# date: 2020.02.26
# auther: Chihiro Haga

computeDiversity <- function() {
  past.id <- 8
  other.ids <- c(21, 31)
  shdiversity.dfs <- list()
  biomass.by.spp.dfs <- list()
  iter <- 1
  for (tms in tms.list) {
    print(tms)
    # Read data --------------------
    lulc.ras <- raster(file.path(lulc.dir, paste0("dominantSpp-", scenario, "_", climate, "_", tms, ".tif")))
    biomass.stack <- stack(file.path(scenario.dir, 'OutputMaps', 'biomass', paste0(spp.name.list, '-', tms, '.tif')))
    # Ignore biomass on non-forest site
    for (lyr in 1:nlayers(biomass.stack)) {
      if (lyr == past.id) { # pasture grass
        biomass.stack[[lyr]] <- biomass.stack[[lyr]] * (1 - lulc.ras %in% other.ids)
      } else { # woody biomass
        biomass.stack[[lyr]] <- biomass.stack[[lyr]] * (1 - lulc.ras %in% c(past.id, other.ids))
      }
    }
    # save the modified agb -------
    names(biomass.stack) <- spp.name.list
    writeRaster(biomass.stack, file.path(div.dir, paste0('agbstk_', scenario, '_', climate, '_y', tms, '.tif')))
  }
}
