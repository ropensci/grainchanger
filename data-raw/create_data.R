# categorical and continuous data
cont_ls <- NLMR::nlm_mpd(65, 65)
cat_ls <- landscapetools::util_classify(cont_ls, weighting = c(0.25, 0.25, 0.25, 0.25))

# create grids
g_raster <- raster::aggregate(cont_ls, 13)
raster::values(g_raster) <- 1

g_sf <- as(g_raster, "SpatialPolygonsDataFrame")
g_sp <- sf::st_as_sf(g_sf)

# data for testing output still the same
nm_agg_shei <- nomove_agg(grainchanger::g_sf, 
                          grainchanger::cat_ls, 
                          "shei", lc_class = 0:3)

nm_agg_mean <- nomove_agg(grainchanger::g_sf, 
                          grainchanger::cont_ls, 
                          "mean")

wm_agg_shei <-  winmove_agg(grainchanger::g_sf, 
                            grainchanger::cat_ls, 
                            5, "rectangle", "shei", lc_class = 0:3)

wm_agg_mean <- winmove_agg(grainchanger::g_sf, 
                           grainchanger::cont_ls, 
                           5, "rectangle", "mean")

wm_shei_dat <- winmove(grainchanger::cat_ls, 
                       5, "rectangle", "shei", lc_class = 0:3)

wm_mean_dat <- winmove(grainchanger::cont_ls, 
        5, "rectangle", "mean")

# data for examples
usethis::use_data(cont_ls, cat_ls, g_sf)

# internal data
usethis::use_data(g_raster, g_sp, nm_agg_shei, nm_agg_mean, wm_agg_shei, wm_agg_mean, wm_shei_dat, wm_mean_dat, internal = TRUE)