#' ---
#' title: plotFigure2andS6-7
#' author: Chihiro Haga
#' date: 2020.03.06
#' ---

#' Figure 2 ------------------------------------
lulcChange <- function(syear, eyear) {
  dat <- stack(file.path(lulc.dir, paste0('dominantSpp-a0_s0.0_', climate, '_', c(syear, eyear), '.tif')))
  cat('\n  lulc crosstab btwn', syear + 2015, 'and', eyear + 2015, '\n', climate)
  print(kable(crosstab(dat)))
  cat('\n')
  knit_print(levelplot(dat, margin = F, main = paste0(climate, ', ', syear, ' and ', eyear)))
}
lulcChange(1, 85)

abb_eng <- 
  c('abiesach' = 'Sakhalin fir',
    'larikaem' = 'Larch',
    'betuplat' = 'Japanese white birch',
    'ulmudavi' = 'Japanese elm',
    'alnujapo' = 'Japanese alder',
    'quercris' = 'Japanese oak',
    'fraxmand' = 'Japanese ash',
    'pastgras' = 'Pasture grass',
    'sasagras' = expression(paste(italic('Sasa'), 'species')))

#' ## Plot LULC change
pa.name <- file.path(analysis.dir, 'patch_areas.feather')
pa.df <- read_feather(pa.name)
area_plt <-
  pa.df %>% 
  filter(metric == 'ca') %>%
  filter(alevel == 'A0') %>% 
  ggplot(aes(x = Time + 2015, y = value / 100, 
             group = interaction(class, metric, alevel, slevel, climate),
             fill = factor(class))) +
  geom_area() +
  scale_fill_manual(values = c("#35a16b", "#faf500",  "#ff2800", "#ff99a0", "#ff9900",   "#663300", "#9a0079", "darkseagreen2", "gray"),
                    limits = 1:9,
                    drop = FALSE,
                    name = "")+
  ylab(expression(paste('Area', ' (km'^{2}, ")"))) +
  facet_grid(~climate) +
  theme_Publication() +
  theme(legend.position = 'none',
        legend.text.align = 0,
        axis.title.x = element_blank())

#' ## Plot AGB change
sppbiom_df_name <- file.path(analysis.dir, 'sppbiom_df.feather')
sppbiom_df <- read_feather(sppbiom_df_name) %>% 
  left_join(a.ludf, by = 'a') %>% 
  left_join(s.ludf, by = 's') %>% 
  select(-a, -s)

agb_plt <- 
  sppbiom_df %>% 
  filter(alevel == 'A0', slevel == '0.0') %>%
  mutate(sppname = fct_relevel(sppname, spp.ludf$sppname)) %>% 
  ggplot(aes(x = Time + 2015, y = val * 10^2 * 10^-9,
             group = interaction(alevel, slevel, climate, sppname),
             fill = sppname)) +
  geom_area() +
  scale_fill_manual(values = c("#35a16b", "#faf500",  "#ff2800", "#ff99a0", "#ff9900",   "#663300", "#9a0079", "darkseagreen2", "gray"),
                    limits = spp.ludf$sppname,
                    drop = FALSE,
                    labels = abb_eng,
                    name = "Plant species name")+
  xlab('Year') +
  ylab(expression(paste('Total AGB', ' (Gg-biomass)'))) +
  facet_grid(~climate) +
  theme_Publication() +
  theme(legend.text.align = 0)

library(cowplot)
plot_grid(area_plt, agb_plt, nrow = 2, align = 'v', 
          rel_heights = c(1, 1.2), labels = c('(A)', '(B)'), label_fontfamily = 'serif')



#' Figure S7 -----------------------------
BiomByEco <- function() {
  brk <- c(0, 1000, 2000, 3000, 4000, 10000)
  # 1=U1000 forest
  # 2=1000  biomass energy
  # 3=2000  natural riparian zone
  # 4=3000  solar pv
  # 5=9999  managed pasture land
  cpu_num <- 8
  cl <- makePSOCKcluster(cpu_num)
  registerDoParallel(cl)
  objects <- ls(envir=parent.frame())
  t.start <- Sys.time()
  foreach (scenario = scenario.name.list, # roop iterator
           .packages = package_list, # packages
           .export=objects # objects
  ) %dopar% {
    agb.sum.dfs <- list()
    dfs_iter <- 1
    for (climate in climate.name.list) {
      # pb$tick()
      scenario.dir <- paste0(root.dir, '/', scenario, "/", climate)
      mng.ras <- raster(file.path(input.dir, 
                                  paste0('200304_mngMap_BaU_area', 
                                         str_sub(str_split(scenario, pattern = '_')[[1]][1], 2, 4),
                                         '_sol', 
                                         str_sub(str_split(scenario, pattern = '_')[[1]][2], 2, 4), '.tif'))) %>% 
        raster::cut(breaks = brk)
      mng.ras[is.na(mng.ras)] <- 1
      agb.stack <- 
        stack(file.path(div.dir,
                        paste0('agbstk_', scenario, '_', climate, '_y', tms.list, '.tif')))
      extent(agb.stack) <- extent(mng.ras)
      agb.sum.by.mng <-
        zonal(agb.stack, mng.ras, fun = 'sum')
      agb.sum.dfs[[dfs_iter]] <- 
        cbind(t(agb.sum.by.mng)[2:ncol(agb.sum.by.mng), ],
              expand.grid(spp.name.list, tms.list)) %>% 
        rename(sppname = Var1, Time = Var2) %>% 
        gather(key = mng, val = value, -Time, -sppname) %>% 
        mutate(varname = 'agb', scenario = scenario, climate = climate) %>% 
        tidyr::separate(scenario, into = c('a', 's'), sep = '_')
      dfs_iter <- dfs_iter + 1
    }
    agb.sum.dfs %>% 
      bind_rows() %>% 
      write_feather(file.path(div.dir, paste0('agb_spp_reg_', scenario, '.feather')))
  }
  stopCluster(cl) # Stop clusters for multiprocessing
  agb.df <- 
    list.files(path = div.dir, pattern = 'agb_spp_reg', full.names = TRUE) %>% 
    map(read_feather) %>% 
    reduce(bind_rows)
  write_feather(agb.df, agb.df.name)
  cat('\n  Elapsed time:', round(difftime(Sys.time(), t.start, units = 'hours'), digits = 2), ' (hours)\n\n')
  return(agb.df)
}

agb.df.name <- file.path(analysis.dir, 'agb_by_spp_reg.feather')
if (file.exists(agb.df.name)) {
  sppbiom.reg.df <- read_feather(agb.df.name)
} else {
  sppbiom.reg.df <- BiomByEco()
}

mng.ludf <- 
  data.frame(mng = as.character(c(1:5)),
             area = c('Forest', 'Riparian forest', 'REbiom', 'REsol', 'Pasture land'))
gbm2ggb <- 10^2 * 10^-9

for (mngid in unique(sppbiom.reg.df$mng)) {
  mng.df <- 
    filter(sppbiom.reg.df, mng == mngid) %>% 
    group_by(Time, climate, mng, varname, a, s) %>% 
    summarise(val = sum(value))
  ylimmax <- max(mng.df$val) * gbm2ggb
  for (clim in climate.name.list) {
    dat <- sppbiom.reg.df %>% 
      left_join(a.ludf, by = 'a') %>% 
      left_join(s.ludf, by = 's') %>% 
      left_join(mng.ludf, by = 'mng') %>% 
      filter(climate == clim, mng == mngid) %>% 
      mutate(slevel = paste0('S', slevel))
    if (nrow(dat) == 0) next
    if (mngid %in% c('4', '5')) next
    cat('\n  ', mngid, clim)
    plt <- dat %>% 
      ggplot(aes(x = Time + 2015, y = value * gbm2ggb, 
                 group = interaction(alevel, slevel, climate, sppname), 
                 fill = sppname)) +
      geom_area() +
      scale_fill_manual(values = sppcolors,
                        limits = spp.ludf$sppname,
                        drop = FALSE,
                        label = spp.ludf$sppname,
                        name = "")+
      ggtitle(paste(unique(dat$area), '\n', clim)) +
      ylim(0, ylimmax) +
      xlab('Year') +
      ylab(expression(paste('Total AGB', ' (Gg-biomass)'))) +
      facet_grid(slevel~alevel) +
      theme_Publication(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    knit_print(plt)
  }
}

