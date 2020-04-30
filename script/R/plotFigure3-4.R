# Define function ==============================================
ComputeMean <- function(bek_df, dat.stack){
  dat.df <- 
    data.frame(bek_df,
               as.data.frame(dat.stack)) %>% 
    filter(!is.na(bekambe_watershed)) %>% 
    dplyr::select(-bekambe_watershed)
  return(apply(dat.df, MARGIN = 2, mean))
}

DataLoader <- function() {
  hsi.dfs <- list(); div.grid.dfs <- list(); div.dfs <- list()
  dfs_iter <- 1
  bek_df <- raster(file.path(data.dir, "kumataka/bekambe_watershed.tif")) %>% 
    as.data.frame()
  for (scenario in scenario.name.list) {
    for (climate in climate.name.list) {
      scenario.dir <- paste0(root.dir, '/', scenario, "/", climate)
      cat('\n', scenario.dir)
      # HSI =======================
      cat('\n    Reading HSI csvs...\n')
      owl.list <- list(); kumataka.list <- list()
      iter <- 1
      pb <- progress_bar$new(
        format = "      Processing [:bar] :percent eta: :eta",
        total = length(tms.list), clear = FALSE, width= 60)
      for (tms in tms.list) {
        pb$tick()
        owl.list[[iter]] <- 
          readr::read_csv(file.path(owl.dir, 
                                    paste0('owl_hsi_', scenario, '_', climate, '_', tms, '.csv')),
                          col_types = cols()) %>% 
          mutate(Time = tms)
        kumataka.list[[iter]] <- 
          readr::read_csv(file.path(kumataka.dir,
                                    paste0('kumataka_hsi_', scenario, '_', climate, '_', tms, '.csv')),
                          col_types = cols()) %>% 
          mutate(Time = tms)
        iter <- iter + 1
      }
      owl.df <- 
        bind_rows(owl.list) %>% 
        group_by(Time) %>% 
        summarise(val = mean(hsi, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(varname = 'owl_hsi')
      kumataka.df <- 
        bind_rows(kumataka.list) %>% 
        group_by(Time) %>% 
        summarise(val = mean(hsi, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(varname = 'kumataka_hsi')
      hsi.dfs[[dfs_iter]] <- 
        bind_rows(owl.df, kumataka.df) %>%
        mutate(scenario = scenario,
               climate = climate)
      # Shannon's diversity ========
      cat('    Reading Diversity rasters...')
      div.stack <-
        stack(file.path(div.dir,
                        paste0('sw_', scenario, '_', climate, '_y', tms.list, '.tif')))
      div.grid.dfs[[dfs_iter]] <-
        data.frame(Time = tms.list,
                   # val = cellStats(dsi.stack, mean),
                   val = ComputeMean(bek_df, div.stack),
                   varname = 'gridH',
                   scenario = scenario,
                   climate = climate)
      cat('\n    Reading Diversity csvs...\n')
      div.dfs[[dfs_iter]] <-
        read_csv(file.path(div.dir, paste0('h_', scenario, '_', climate, '.csv')), col_types = cols()) %>%
        rename(Time = time,
               val = H) %>%
        mutate(varname = 'H')
      dfs_iter <- dfs_iter + 1
    }
  }
  # Energy ==========================
  cat('\n    Reading Energy csv...\n')
  energy.df <- readr::read_csv(file.path(energy.dir, 'total_energy.csv'), col_types = cols()) %>% 
    dplyr::select(Time, scenario, climate, total_energy) %>% 
    dplyr::mutate(varname = 'energy') %>% 
    rename(val = total_energy)
  # Integrate data ===================
  all_df <- bind_rows(hsi.dfs, energy.df, div.grid.dfs, div.dfs) %>%
    tidyr::separate(scenario, into = c('a', 's'), sep = '_')
  write_feather(all_df, all_df_name)
  return(all_df)
}

# Read data ====================================================
all_df_name <- file.path(analysis.dir, 'all_df.feather')
if (file.exists(all_df_name)) {
  all_df <- read_feather(all_df_name)
} else {
  all_df <- DataLoader()
}


# Summarise data ----------
all_df <- 
  all_df %>% 
  filter(varname %in% c('gridH', 'kumataka_hsi', 'owl_hsi', 'energy')) %>% 
  left_join(a.ludf, by = 'a') %>% 
  left_join(s.ludf, by = 's') %>% 
  left_join(varname.ludf, by = 'varname') %>% 
  select(-a, -s, -varname) %>% 
  mutate(val = if_else(varnamelevel == 'RE', val / 1000, val))

summarised_df <- 
  all_df %>% 
  group_by(Time, varnamelevel, alevel, slevel) %>% 
  summarise(ymin = min(val), ymea = mean(val), ymax = max(val)) %>% 
  ungroup()


# Figure 3 ----------------------------------
all_plt <- 
  summarised_df %>% 
  mutate(varname = as.character(varnamelevel),
         varname = if_else(varname == 'RE', 'RE (PJ)', varname)) %>% 
  mutate(varname = fct_relevel(varname, 'Diversity', 'HSIs', 'HSIk')) %>% 
  ggplot(aes(x = Time + 2016, group = interaction(alevel, slevel, varnamelevel))) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = slevel), alpha = 0.2) +
  geom_line(aes(y = ymea, color = slevel)) +
  xlab('Year') +
  ylab(NULL) +
  labs(color = 'Fraction of Solar PV\nIntroduction',
       fill = 'Fraction of Solar PV\nIntroduction') +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  # facet_grid(~varnamelevel~alevel, scales = 'free', switch = 'y') +
  facet_grid(~varname~alevel, scales = 'free', switch = 'y') +
  theme_Publication() +
  theme(strip.placement = 'outside',
        panel.grid = element_blank(),
        # legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))
plot(all_plt)


# Figure 4 -----------------------------
# Extract features -----
DEMAND <- 1285 * 10^-3 # PJ
# BREAKS <- c(0, DEMAND / 2, DEMAND, DEMAND * 2, DEMAND * 5, DEMAND * 1000)
BREAKS <- c(0, 1.0, 5, 100000)
clevels <- c('Group 1', 'Group 2', 'Group 3')
ComputeFeatures <- function(evlTime) {
  duration <- 10
  ecol_features <- 
    # diff_df %>% 
    all_df %>%
    # filter(Time == evlTime) %>% 
    filter(Time > evlTime - duration) %>%
    filter(varnamelevel != varname.fct[which(varname.fct == 'RE')]) %>% 
    # rename(valfeat = valdiff) %>% 
    # rename(valfeat = val) %>%
    group_by(varnamelevel, climate, alevel, slevel) %>% 
    summarise(valfeat = mean(val)) %>% 
    select(varnamelevel, climate, alevel, slevel, valfeat) %>% 
    ungroup()
  energy_features <-
    # diff_df %>% 
    all_df %>%
    filter(varnamelevel == varname.fct[which(varname.fct == 'RE')]) %>% 
    filter(Time > evlTime - duration) %>% 
    group_by(varnamelevel, climate, alevel, slevel) %>% 
    # summarise(valfeat = sum(valdiff))
    summarise(valfeat = mean(val)) %>% 
    ungroup()
  return(bind_rows(ecol_features, energy_features))
}

features_wide <-
  ComputeFeatures(85) %>%
  spread(key = varnamelevel, val = valfeat) %>% 
  mutate(clevel = cut(RE/DEMAND, breaks = BREAKS, labels = clevels, include.lowest = TRUE))
features_long <- 
  features_wide %>% 
  gather(key = varnamelevel, val = valfeat, Diversity, HSIs, HSIk, RE)%>% 
  mutate(varnamelevel = factor(varnamelevel, 
                          levels = c('Diversity', 'HSIs', 'HSIk', 'RE')))
features_change_long <- 
  filter(features_long, alevel != 'A0') %>% 
  left_join(filter(features_long, alevel == 'A0') %>% 
              select(-alevel, -slevel, -clevel), 
            by = c('climate', 'varnamelevel')) %>% 
  # mutate(val = (valfeat.x - valfeat.y) / valfeat.y)
  mutate(val = valfeat.x / valfeat.y)
# parallel coordinate plot ------
parallel.plt <-
  features_change_long %>%
  filter(varnamelevel != varname.fct[which(varname.fct == 'RE')]) %>%
  ggplot(aes(x = varnamelevel, y = val,
             group = interaction(climate, alevel, slevel),
             color = slevel, linetype = climate)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1.0, color = 'red', size = 0.1) +
  scale_color_viridis(discrete = TRUE) +
  xlab('Ecological indicators') +
  ylab('Ratio of each indicator to A0 scenario in 2090-2100') +
  labs(color = 'Fraction of Solar PV\nIntroduction',
       linetype = 'Climate Change Scenario') +
  theme_Publication() +
  facet_grid(clevel ~ alevel) +
  theme(strip.placement = 'outside',
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
        # legend.position = "right")
plot(parallel.plt)
