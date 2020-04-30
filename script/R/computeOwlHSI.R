#' ---
#' title: Calculate HSI for Blackiston's fish owl
#' date: 2019.02.15
#' author: Chihiro Haga
#' ---

computeOwlHSI <- function() {
  # load functions from script
  source(paste0(script.dir, '/function_owl_hsi.R'))
  # Initialize objects
  owl.data.dir <- paste0(data.dir, '/owl')
  # read static data
  setwd(owl.data.dir)
  # natural forest distribution map inside the watershed
  vegetation_name <- 'natural_forest.tif'
  vegetation_ras <- raster(vegetation_name)
  # natural forest distribution map outside the watershed
  outside_natural_forest_name <- 'natural_riparian_forest_area_pagenum.txt'
  outside_natural_forest_df <- read_csv(outside_natural_forest_name) %>%
    rename(outside_area_ha = SUM_area_h) %>%
    dplyr::select(PageNumber, outside_area_ha)
  # riparian zone distribution map inside the watershed
  riparian_zone_name <- 'riparian_zone.tif'
  riparian_zone_ras <- raster(riparian_zone_name)
  # stream length for each 4km grids
  stream_length_name <- 'stream_length_pagenumber.txt'
  stream_length_df <- read_csv(stream_length_name) %>%
    dplyr::select(PageNumber, SUM_length)%>%
    rename(length_m = SUM_length)
  lonlat_4km_name <- 'bekanbe_4km_lonlat.txt'
  lonlat_4km_df <- read_csv(lonlat_4km_name)
  # merge stream length and natural forest
  hsi_variable_df <- stream_length_df %>%
    full_join(outside_natural_forest_df, by = 'PageNumber')
  # stream line shape
  stream_name <- 'bekanbe_river.shp'
  stream_sf <- sf::read_sf(stream_name) %>%
    st_transform(crs = 4612)
  # Identify abiesach natural forest
  setwd(paste0(root.dir, "/data/owl"))
  initial_planted <- raster("todo_kara_planted.tif")
  initial_planted[is.na(initial_planted)] <- 0

  hsi_dfs <- list()
  hsi_iter <- 1
  for (tms in 1:85) {
    # Read raster data as data stack -----
    agb_dir <- paste0(root.dir, "/", scenario, "/", climate, "/OutputMaps/biomass")
    setwd(agb_dir)
    agb_names <- paste0(spp.name.list, '-', tms, '.tif')
    agb_stack <- stack(agb_names)
    names(agb_stack) <- spp.name.list
    # Read support information ------
    lulc_dir <-paste0(root.dir, "/Analysis/LULC_maps")
    setwd(lulc_dir)
    lulc_name <- paste0('dominantSpp-', scenario, '_', climate, '_', tms, '.tif')
    lulc_ras <- raster(lulc_name)
    isActivePasture <- lulc_ras == 8
    isPotentialWetland <- lulc_ras == 31
    isSolarPowerPlant <- lulc_ras == 21
    # Compute forest type -------
    forest_type_name <- file.path(owl.dir, paste0('forest_type_', scenario, '_', climate, "_", tms, '.tif'))
    if (file.exists(forest_type_name)) {
      forest_type_ras <- raster(forest_type_name)
    } else {
      # identify artificial forest ------
      artificial_ids <- c(1, 2, 6, 7, 9, 12, 15) + 1
      natural_ids <- c(4, 8, 10, 11, 13, 14, 16:23) + 1
      prescript_dir <- paste0(root.dir, "/", scenario, "/", climate, "/OutputMaps/harvest")
      setwd(prescript_dir)
      prescripts_name <- paste0("prescripts-", tms, ".img")
      prescripts_ras <- raster::raster(prescripts_name)
      if (tms == 1) {
        is_artificial_forest <- prescripts_ras %in% artificial_ids
        is_natural_forest <- prescripts_ras %in% natural_ids
      } else {
        iaf.buf <- prescripts_ras %in% artificial_ids
        inf.buf <- prescripts_ras %in% natural_ids
        iaf.buf[iaf.buf == 0] <- NA
        inf.buf[inf.buf == 0] <- NA
        is_artificial_forest[is_artificial_forest == 0] <- NA
        is_natural_forest[is_natural_forest == 0] <- NA
        is_artificial_forest <- merge(is_artificial_forest, iaf.buf)
        is_natural_forest <- merge(is_natural_forest, inf.buf)
      }
      is_artificial_forest[is.na(is_artificial_forest)] <- 0
      is_natural_forest[is.na(is_natural_forest)] <- 0
      # Compute forest types --------
      extent(vegetation_ras) <- extent(agb_stack)
      extent(is_artificial_forest) <- extent(agb_stack)
      extent(is_natural_forest) <- extent(agb_stack)
      extent(initial_planted) <- extent(agb_stack)
      dat_stack <- stack(agb_stack,
                         isActivePasture,
                         isPotentialWetland,
                         isSolarPowerPlant,
                         is_artificial_forest,
                         is_natural_forest,
                         initial_planted)
      names(dat_stack) <- c(spp.name.list, 
                            'isActivePasture',
                            'isPotentialWetland',
                            'isSolarPowerPlant',
                            "is_artificial", 
                            "is_natural",
                            "is_artificial_initial")
      forest_type_ras <- calc(dat_stack, compute_forest_type)
      # Fill ZERO sites with vegetationmap ---------
      fill_stack <- stack(forest_type_ras, vegetation_ras)
      names(fill_stack) <- c('landis', 'initial')
      forest_type_ras <- calc(fill_stack, fill_zero_forest_type)
      # Save the forest type map -------
      writeRaster(forest_type_ras, forest_type_name, overwrite = TRUE)
    }
    extent(isSolarPowerPlant) <- extent(riparian_zone_ras)
    isSolarPowerPlant[isSolarPowerPlant == 0] <- NA
    if (nrow(na.omit(as.data.frame(isSolarPowerPlant))) > 0) {
      distFromSolarPowerPlant <- distance(isSolarPowerPlant)
      forest_riparian_df <- data.frame(as.data.frame(forest_type_ras),
                                       as.data.frame(riparian_zone_ras),
                                       as.data.frame(distFromSolarPowerPlant))
    } else {
      d <- 300
      forest_riparian_df <- data.frame(as.data.frame(forest_type_ras),
                                       as.data.frame(riparian_zone_ras),
                                       dist = rep(d * 2, ncell(isSolarPowerPlant)))
    }
    colnames(forest_riparian_df) <- c('type', 'PageNumber', 'dist')
    write_csv(forest_riparian_df, 
              file.path(owl.dir, paste0("owl_ftype_soldist_", scenario, "_", climate, "_", tms, ".csv")))
    # Compute natural forest area within riparian zone for each spatial units ----------
    inside_watershed_natural_riparian_df <- forest_riparian_df %>%
      filter(type <= 5, type > 0) %>%   # select natural forest from 植生自然度
      filter(PageNumber < 128) %>%
      filter(dist > d) %>% # exclude near solar PV system
      group_by(PageNumber) %>%
      summarise(inside_area_ha = n())
    # Compute HSI for Blakiston's fish owl ---------------------
    hsi_df <- hsi_variable_df %>%
      full_join(inside_watershed_natural_riparian_df, by = 'PageNumber')
    hsi_df[is.na(hsi_df)] <- 0 # replace NA to ZERO
    hsi_df <- hsi_df %>%
      mutate(scenario = scenario,
             climate = climate,
             year = tms + 2015,
             area_integ_ha = (outside_area_ha + inside_area_ha) , # integrate and convert unit
             length_m = length_m, # convert unit
             hsi = 1/(1 + exp(-(-23.36 
                                + 9.32 * 10^(-3) * area_integ_ha 
                                + 5.026 * log(length_m + 1) 
                                - 0.326 * log(length_m + 1)^2))))
    setwd(owl.dir)
    write.csv(hsi_df, paste0("owl_hsi_", scenario, "_", climate, "_", tms, ".csv"))
  }
}
