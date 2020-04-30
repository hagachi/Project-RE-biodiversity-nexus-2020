# ***********************************
# Function: detect dominant species
# ***********************************
# memo: prescriptionID 0-27 case

# x: raster stack of each species biomass
DetectDominantSpp <- function(x) {
	# determin maximum value in each cell
	max_AGB <- max(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]], x[[7]], 
	               x[[8]], x[[9]])
	if (x[[10]] == 25) {
		dominant <- 8
	} else if (x[[10]] == 26) {
		# solar power generation plant
		dominant <- 21
	} else {
		if (max_AGB == x[[1]]) {
			dominant <- 1
		} else if (max_AGB == x[[2]]) {
			dominant <- 2
		} else if (max_AGB == x[[3]]) {
			dominant <- 3
		} else if (max_AGB == x[[4]]) {
			dominant <- 4
		} else if (max_AGB == x[[5]]) {
			dominant <- 5
		} else if (max_AGB == x[[6]]) {
			dominant <- 6
		} else if (max_AGB == x[[7]]) {
			dominant <- 7
		} else if (max_AGB == x[[8]]) {
			dominant <- 8
		} else if (max_AGB == x[[9]]) {
			dominant <- 9
		}
    # if AGB = 0, it means there are no species (dominant = 0)
    if (max_AGB == 0){
        dominant <- 0
    }
	}
	# check if abandoned pastureland is expected to become wetland
	if (x[[10]] != 25 && x[[11]] == 1) {
		dominant <- 31
	}
  return(dominant)
}


MakeDominantSppMap <- function(root.dir, scenario.dir, scenario, tms, spp.name.list) {

	# read biomass of 10 species and prescript maps
	biomass.list <- paste0(scenario.dir, "/OutputMaps/biomass/", spp.name.list, "-", tms, ".tif")
	biomass.stack <- stack(biomass.list)
	prescript.name <- paste0(scenario.dir, "/OutputMaps/harvest/prescripts-", tms, ".img")
	prescript.ras <- raster::raster(prescript.name)
	wetland.name <- paste0(root.dir, "/data/bekanbe_wet_pasture.tif")
	wetland.ras <- raster::raster(wetland.name)

	# apply same spatial extent to make raster stack
	ext <- extent(biomass.stack)
	extent(prescript.ras) <- ext
	extent(wetland.ras) <- ext
	dat.stack <- stack(c(biomass.stack, prescript.ras, wetland.ras))
	dat.stack[is.na(dat.stack)] <- 0
	
	# ************************************
	# detect dominant species based on AGB and prescription
	# using "detectDominantSpp" function (SEE ABOVE)
	dominant.spp.ras <- calc(x = dat.stack, fun = DetectDominantSpp)
	# ************************************
	
	return(dominant.spp.ras)
}