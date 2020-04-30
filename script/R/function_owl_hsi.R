#' # Define functions
# identify if the forest is natural or not.
compute_forest_type <- function(dat_stack) {
  forest_type_scores <- list()
  forest_type_id <- 0
  max_score <- 0
  if (dat_stack[['isActivePasture']] == 1 |
      dat_stack[['isPotentialWetland']] == 1 |
      dat_stack[['isSolarPowerPlant']] == 1 ) {
    if(dat_stack[["isActivePasture"]] == 1){
      return(10)
    }
    return(0)
  } else {
    # Calculate scores for each forest type
    if (dat_stack[["is_artificial"]] == 1) {
      forest_type_scores[[1]] <- 0
      forest_type_scores[[2]] <- 0
      forest_type_scores[[8]] <- dat_stack[['abiesach']] # clear cut and plant abies, ch200303
      forest_type_scores[[9]] <- dat_stack[['larikaem']] # plant larikaem, ch200303
    } else {
      if (dat_stack[["is_artificial_initial"]] == 1 &
          dat_stack[["is_natural"]] == 0) {
        forest_type_scores[[1]] <- 0
        forest_type_scores[[2]] <- 0
        forest_type_scores[[8]] <- dat_stack[['abiesach']] # clear cut and plant abies, ch200303
        forest_type_scores[[9]] <- dat_stack[['larikaem']] # plant larikaem, ch200303
      } else if (dat_stack[["is_artificial_initial"]] == 1 &
                 dat_stack[["is_natural"]] == 1){
        forest_type_scores[[1]] <- dat_stack[['abiesach']] + dat_stack[['quercris']] # selective and plant abies/quercus, ch200303
        forest_type_scores[[2]] <- dat_stack[['abiesach']] # selective and plant abies
        forest_type_scores[[8]] <- 0
        forest_type_scores[[9]] <- 0
      } else {
        forest_type_scores[[1]] <- dat_stack[['abiesach']] + dat_stack[['quercris']]
        forest_type_scores[[2]] <- dat_stack[['abiesach']]
        forest_type_scores[[8]] <- 0
        forest_type_scores[[9]] <- 0
      }
    }
    forest_type_scores[[3]] <- dat_stack[['ulmudavi']]
    forest_type_scores[[4]] <- dat_stack[['alnujapo']] + dat_stack[['fraxmand']]
    forest_type_scores[[5]] <- dat_stack[['alnujapo']]
    forest_type_scores[[6]] <- dat_stack[['betuplat']] + dat_stack[['quercris']]
    forest_type_scores[[7]] <- dat_stack[['sasagras']]
    forest_type_scores[[10]] <- dat_stack[['pastgras']]
    # Identify which forest type is the best ====================
    for (type_id in 1:length(forest_type_scores)) {
      if (forest_type_scores[[type_id]] > max_score) {
        max_score <- forest_type_scores[[type_id]]
        forest_type_id <- type_id
      }
    }
    return(forest_type_id)
  }
}




fill_zero_forest_type <- function(fill_stack) {
  if (fill_stack[['landis']] == 0) {
    return(fill_stack[['initial']])
  } else {
    return(fill_stack[['landis']])
  }
}


identify_artificial_forest <- function(dat_brick) {
  # clearcut_ids <- c(2,3,5,7:9,28:29) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!clearcutting
  # is_harvested_brick <- dat_brick %in% clearcut_ids
  is_harvested_brick <- dat_brick %in% artificial_ids
  is_harvested_brick[is_harvested_brick == 0] <- NA
  is_harvested <- merge(is_harvested_brick)
  is_harvested_brick[is.na(is_harvested_brick)] <- 0
  return(is_harvested)
}

identify_natural_forest <- function(dat_brick) { # as secondary forest
  # clearcut_ids <- c(10:24) # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!selective cutting
  # is_harvested_brick <- dat_brick %in% clearcut_ids
  is_harvested_brick <- dat_brick %in% natural_ids
  is_harvested_brick[is_harvested_brick == 0] <- NA
  is_harvested <- merge(is_harvested_brick)
  is_harvested_brick[is.na(is_harvested_brick)] <- 0
  return(is_harvested)
}
