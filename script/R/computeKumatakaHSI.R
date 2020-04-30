#' ---
#' title: Calculate HSI for kumataka
#' date: 2019.10.23
#' author: Chihiro Haga
#' ---

computeKumatakaHSI <- function() {
  # Load functions =================
  source(paste0(script.dir, '/function_kumataka_hsi.R'))
  # Read data ================
  setwd(paste0(root.dir, "/data/kumataka"))
  watershed_ras <- raster("bekambe_watershed.tif")
  ini_lutype_ras <- raster("kumataka_landuse_type.tif")
  code3_ras <- raster("bekanbe_3code_100m.tif")
  elev_slop_df <- read.csv("elevation_slope_bekanbe.csv")
  colnames(elev_slop_df) <- c("FID", "code3", "meaElev", "meaSlope")
  lonlat_1km_name <- '3code_long_lat.txt'
  lonlat_1km_df <- read_csv(lonlat_1km_name)
  colnames(lonlat_1km_df) <- c("FID","code3","lon","lat")
  # MAIN ==================================================================
  t.start <- proc.time()
  for (tms in tms.list) {
    # Compute LULC type for kumataka HSI =======================
    setwd(lulc.dir)
    domi_ras <- raster(paste0("dominantSpp-", scenario,"_", climate, "_", tms, ".tif"))
    setwd(paste0(scenario.dir, "/OutputMaps/biomass"))
    bro_biomass_ras <- mosaic(raster(paste0("alnujapo-", tms, ".tif")),
                              raster(paste0("betuplat-", tms, ".tif")),
                              raster(paste0("fraxmand-", tms, ".tif")), 
                              raster(paste0("quercris-", tms, ".tif")),
                              raster(paste0("ulmudavi-", tms, ".tif")), 
                              fun = sum)
    total_biomass_ras <-mosaic(bro_biomass_ras,
                               raster(paste0("abiesach-", tms, ".tif")),
                               raster(paste0("larikaem-", tms, ".tif")),
                               fun = sum)
    biomass_stack <- stack(total_biomass_ras, bro_biomass_ras, domi_ras)
    
    lutype_ras <- calc(biomass_stack, compute_landuse_type) # defined in function_kumataka_hsi.R
    extent(lutype_ras) <- extent(watershed_ras)
    lutype_merge_ras <- merge(lutype_ras, ini_lutype_ras)
    out_name <- paste0(kumataka.dir, "/lutype_", scenario, "_", climate, "_", tms, ".tif")
    writeRaster(lutype_merge_ras, out_name, overwrite=TRUE)
    # Compute length of forest edge =======================
    lutype_mat <- as.matrix(lutype_merge_ras)
    lutype_mat[is.na(lutype_mat)] <- 0 
    count_oa_mat <- matrix(0, ncol = ncol(lutype_mat), nrow = nrow(lutype_mat))
    for(rcntr in 1:nrow(lutype_mat)){
      for (ccntr in 1:ncol(lutype_mat)){
        if(lutype_mat[rcntr,ccntr]== 2 |
           lutype_mat[rcntr,ccntr]== 3){
          # Count open area grids
          count_oa <- 0
          if(rcntr>1){
            if(lutype_mat[rcntr-1,ccntr]==1){
              count_oa <- count_oa +1
            }
          }
          if(ccntr<ncol(lutype_mat)){
            if(lutype_mat[rcntr, ccntr+1]==1){
              count_oa <- count_oa +1
            }
          }
          if(rcntr<nrow(lutype_mat)){
            if(lutype_mat[rcntr+1, ccntr] == 1){
              count_oa <- count_oa +1
            }
          }
          if(ccntr > 1){
            if(lutype_mat[rcntr, ccntr-1] == 1){
              count_oa <- count_oa +1
            }
          }
          count_oa_mat[rcntr, ccntr] <- count_oa 
        }
      }
    }
    count_oa_ras <- raster(count_oa_mat)
    extent(count_oa_ras) <- extent(lutype_merge_ras)
    out_name <- paste0(kumataka.dir, "/count_openarea_", scenario, "_", climate, "_", tms, ".tif")
    writeRaster(count_oa_ras, out_name, overwrite=TRUE)
    # Summarise all explanation variables =======================
    area_foredge_stack <- stack(code3_ras,lutype_merge_ras, count_oa_ras)
    area_foredge_df <- as.data.frame(area_foredge_stack)
    colnames(area_foredge_df) <- c("code3", "lutype_id", "edgeCount")
    lu_variable_df <- area_foredge_df %>%
      mutate(isOpenArea=if_else(lutype_id==1,1,0), 
             isBroadOrMixed = if_else(lutype_id==2,1,0),
             foredge = edgeCount*100) %>%
      group_by(code3) %>%
      summarise(openArea=sum(isOpenArea)/n()*100,
                broadOrMixed=sum(isBroadOrMixed)/n()*100,
                forestEdge=sum(foredge)) %>%
      mutate(sum = openArea + broadOrMixed)
    # Compute HSI for kumataka =======================
    kumataka_hsi_df <- lu_variable_df %>%
      left_join(elev_slop_df, by="code3") %>%
      mutate(hsi=1/(1 + exp(-(-12.7853 
                              + 0.0018 * meaElev 
                              + 0.0987 * meaSlope 
                              + 0.107  * broadOrMixed
                              + 0.0851 * openArea
                              + 0.0001 * forestEdge))))
    # Save results =======================
    hsi_lonlat_df <- kumataka_hsi_df %>%
      full_join(lonlat_1km_df, by = 'code3')
    out_name <- paste0(kumataka.dir, "/kumataka_hsi_", scenario, "_", climate, "_", tms, ".csv")
    write.csv(hsi_lonlat_df, out_name)
    }
}
