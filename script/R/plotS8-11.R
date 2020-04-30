# Source code for S8 and 9

#' Figure S8 -----------------------------
ReadOwlCSVs <- function(path) {
  pb$tick()$print()
  d <- 300
  buf <- tail(str_split(path, pattern = '/')[[1]], 1) %>% 
    str_split(pattern = '_')
  a <- buf[[1]][4]
  s <- buf[[1]][5]
  climname <- paste(buf[[1]][6], buf[[1]][7], sep = '_')
  time <- str_remove(tail(buf[[1]], 1), pattern = '.csv')
  read_csv(path, col_types = cols()) %>% 
    filter(type <= 5, type > 0) %>%   # select natural forest
    filter(PageNumber < 128) %>% 
    filter(dist <= d) %>% # exclude near solar PV system
    group_by(PageNumber) %>% 
    summarise(solOverlap = n()) %>% 
    ungroup() %>% 
    mutate(climate = climname, a = a, s = s, Time = as.numeric(time))
}

owl.name <- file.path(analysis.dir, 'owl_soldist.feather')
if (file.exists(owl.name)) {
  owl.soldist.df <- read_feather(owl.name)
} else {
  owl.list <- list.files(path = owl.dir, pattern = 'owl_ftype_soldist', full.names = T)
  pb <- progress_estimated(length(owl.list))
  owl.soldist.df <- owl.list %>% 
    map(ReadOwlCSVs) %>% 
    reduce(bind_rows)
  write_feather(owl.soldist.df, owl.name)
}

owl.soldist.summary <-
  owl.soldist.df %>% 
  left_join(a.ludf, by = 'a') %>% 
  left_join(s.ludf, by = 's') %>% 
  group_by(Time, alevel, slevel, climate) %>% 
  summarise(totalOverlapArea = sum(solOverlap)) %>% 
  ungroup() %>% 
  group_by(Time, alevel, slevel) %>% 
  summarise(ymax = max(totalOverlapArea) * 10^-2,
            ymea = mean(totalOverlapArea) * 10^-2,
            ymin = min(totalOverlapArea) * 10^-2) %>% 
  ungroup()

owl.soldist.summary %>% 
  ggplot(aes(x = Time + 2015, y = ymea, ymin = ymin, ymax = ymax, 
             group = interaction(alevel, slevel),
             color = slevel, fill = slevel)) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line(size = 1) + 
  xlab('Year') +
  ylab(expression(paste('Area', ' (km'^{2}, ")"))) +
  labs(color = 'Fraction of Solar\nPower Introduction',
       fill = 'Fraction of Solar\nPower Introduction') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  facet_grid(~alevel, scales = 'free', switch = 'y') +
  theme_Publication(base_size = 10)  +
  theme(strip.placement = 'outside',
        axis.text.x = element_text(angle = 45, hjust = 1))


#' Figure S9 ------------------------------
ReadKumaCSVs <- function (path) {
  pb$tick()$print()
  buf <- tail(str_split(path, pattern = '/')[[1]], 1) %>% 
    str_split(pattern = '_')
  a <- buf[[1]][3]
  s <- buf[[1]][4]
  climname <- paste(buf[[1]][5], buf[[1]][6], sep = '_')
  time <- str_remove(tail(buf[[1]], 1), pattern = '.csv')
  read_csv(path, col_types = cols()) %>% 
    gather(key = key, val = val, meaElev, meaSlope, broadOrMixed, openArea, forestEdge) %>% 
    group_by(key) %>% 
    summarise(mean = mean(val, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(climate = climname, a = a, s = s, Time = as.numeric(time))
}

kuma.hsi.name <- file.path(analysis.dir, 'kumataka_summary.feather')
if (file.exists(kuma.hsi.name)) {
  kuma.hsi.df <- read_feather(kuma.hsi.name)
} else {
  kuma.list <- list.files(path = kumataka.dir, pattern = 'kumataka_hsi_a', full.names = T)
  pb <- progress_estimated(length(kuma.list))
  kuma.hsi.df <- kuma.list %>% 
    map(ReadKumaCSVs) %>% 
    reduce(bind_rows)
  write_feather(kuma.hsi.df, kuma.hsi.name)
}

kuma.hsi.df %>% 
  left_join(a.ludf, by = 'a') %>% 
  left_join(s.ludf, by = 's') %>% 
  filter(key != 'meaElev', key != 'meaSlope') %>% 
  group_by(Time, key, alevel, slevel) %>% 
  summarise(ymin = min(mean), ymea = mean(mean), ymax = max(mean)) %>% 
  ungroup() %>% 
  mutate(key = if_else(key == 'broadOrMixed', 'Broad or mixed\nforest ratio',
                       if_else(key == 'forestEdge', 'Length of\nforest edge (m)', 'Open area\n(m2)'))) %>% 
  ggplot(aes(x = Time, y = ymea, ymin = ymin, ymax = ymax, 
             group = interaction(key, alevel, slevel),
             color = slevel, fill = slevel)) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line(size = 1) + 
  xlab('Year') +
  ylab(NULL) +
  labs(color = 'Fraction of Solar\nPower Introduction',
       fill = 'Fraction of Solar\nPower Introduction') +
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  facet_grid(key~alevel, scales = 'free', switch = 'y') +
  theme_Publication(base_size = 10)  +
  theme(strip.placement = 'outside',
        axis.text.x = element_text(angle = 45, hjust = 1))



#' Figure S11 ----------------
ReadLog <- function(path) {
  buf <- str_split(path, '/')[[1]]
  climname <- buf[str_detect(buf, pattern = 'MRI_rcp')]
  read_csv(path) %>% 
    mutate(climate = climname)
}
necn.prob.est.df <- list.files(path = file.path(root.dir, 'a223_s0.0'), 
                               pattern = 'NECN-prob-establish-log.csv', 
                               recursive = TRUE, full.names = TRUE) %>% 
  map(ReadLog) %>% 
  reduce(bind_rows)

library(RcppRoll)
est.lutable <- 
  data.frame(key = c('AvgMinJanTempMult', 'AvgSoilMoistureMult', 'AvgTempMult', 'AvgProbEst'),
             keyname = c('a. AvgMinJanTempMult', 'b. AvgSoilMoistureMult', 'c. AvgTempMult', 'd. AvgProbEst'))
necn.prob.est.df %>% 
  filter(ClimateRegion == 'eco100') %>% 
  filter(Species %in% c('betuplat', 'alnujapo')) %>%
  gather(key = key, val = value, dplyr::starts_with('Avg')) %>% 
  left_join(est.lutable, by = 'key') %>% 
  group_by(Species, climate, key) %>% 
  arrange(Time) %>% 
  mutate(rmean = roll_mean(value, n = 10L, fill = NA)) %>% 
  ggplot(aes(x = Time + 2015, y = rmean, color = climate)) +
  geom_line() +
  xlab('Year') +
  ylab(NULL) +
  scale_color_colorblind() +
  facet_grid(Species~keyname, switch = 'y') +
  theme_Publication(base_size = 12) +
  theme(strip.placement = 'outside',
        axis.text.x = element_text(angle = 45, hjust = 1))
