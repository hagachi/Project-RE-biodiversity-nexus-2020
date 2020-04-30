# title: evaluate Owl habitat
# auther: marimi
# date: 2019.11.22

evaluate_owl_habitat <- function() {
  # Setting ==================================================================
  # Import dataset
  setwd(paste0(data.dir, "/owl"))
  riparian_zone_name <- 'riparian_zone.tif'
  riparian_zone_ras <- raster(riparian_zone_name)
  riparian_zone_df <- as.data.frame(riparian_zone_ras)
  
  # MAIN ==================================================================
  for (tms in tms.list) {
    # get input raster
    setwd(paste0(scenario.dir, "/OutputMaps/biomass"))
    total_biomass_name <- paste0("TotalBiomass-", tms, ".tif")
    total_biomass_ras <- raster(total_biomass_name)
    setwd(paste0(scenario.dir, "/OutputMaps/CohortStats/age-all-spp"))
    age_max_name <- paste0("AGE-MAX-", tms, ".img")
    age_max_ras <- raster(age_max_name)
    setwd(lulc.dir)
    dominantSpp.name <- paste0("dominantSpp-", scenario, "_", climate, "_", tms, ".tif")
    dominantSpp.ras <- raster(dominantSpp.name)
    isForest_ras <- dominantSpp.ras %in% c(1:7)
    
    forest_biomass_ras <- total_biomass_ras * isForest_ras
    forest_biomass_df <- data.frame(as.data.frame(forest_biomass_ras),
                                    as.data.frame(riparian_zone_ras),
                                    as.data.frame(age_max_ras))
    colnames(forest_biomass_df) <- c('biomass', 'PageNumber', 'maxAge')
    
    riparian_forest_4km_biomass_df <- forest_biomass_df %>%
      filter(biomass > 0) %>%
      filter(PageNumber < 128) %>%
      group_by(PageNumber) %>%
      summarise(forest_ha = n(),
                forest_biomass = sum(biomass),
                max_age = max(maxAge),
                mean_age = mean(maxAge),
                sd_age = sd(maxAge))
    setwd(owl.eval.dir)
    out.name <- paste0("riparian_forest_4km_biomass_", scenario, "_", climate, "_", tms,".csv")
    write.csv(riparian_forest_4km_biomass_df, out.name)
  }
}
