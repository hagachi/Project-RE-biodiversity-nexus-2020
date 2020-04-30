# identify landuse type; openArea, broadLeafOrMixedForest, plantedForest.
compute_landuse_type <- function(dat_stack) {
  # dat_stack[1]:total_bio_ras
  # dat_stack[2]:broad_bio_ras
  # dat_stack[3]:domi_ras
  lutype_id <- NA
  if(dat_stack[[3]] == 8 |
     dat_stack[[3]] == 9 |
     dat_stack[[3]] == 31){
    # open area
    lutype_id <- 1
  } else if(dat_stack[[3]] == 21){   
    lutype_id <- 0
  } else {  
    if(dat_stack[[1]]>0){
      if(dat_stack[[2]] / dat_stack[[1]] > 0.3){
        lutype_id <- 2 
      } else {
        lutype_id <- 3
      }
    }
  }
  return(lutype_id)
}
