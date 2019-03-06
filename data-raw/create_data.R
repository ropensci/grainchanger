# categorical and continuous data
cont_ls <- NLMR::nlm_mpd(65, 65, roughness = 0.9)
cat_ls <- landscapetools::util_classify(cont_ls, weighting = c(0.25, 0.25, 0.25, 0.25))

# create grids
g_raster <- raster::aggregate(cont_ls, 13)
raster::values(g_raster) <- 1

g_sp <- as(g_raster, "SpatialPolygonsDataFrame")
g_sf <- sf::st_as_sf(g_sf)

# data for testing output still the same
nm_agg_shei <- nomove_agg(g_sf, cat_ls, "shei", lc_class = 0:3)

nm_agg_mean <- nomove_agg(g_sf, cont_ls, "mean")

wm_agg_shei <-  winmove_agg(g_sf, cat_ls, 5, "rectangle", "shei", lc_class = 0:3)

wm_agg_mean <- winmove_agg(g_sf, cont_ls, 5, "rectangle", "mean")

wm_shei_dat <- winmove(cat_ls, 5, "rectangle", "shei", lc_class = 0:3)

wm_mean_dat <- winmove(cont_ls, 5, "rectangle", "mean")

torus = create_torus(cat_ls, 5)

# data for examples
usethis::use_data(cont_ls, cat_ls, g_sf, overwrite = TRUE, compress = "bzip2")

# internal data
usethis::use_data(g_raster, g_sp, nm_agg_shei, nm_agg_mean, wm_agg_shei, wm_agg_mean, wm_shei_dat, wm_mean_dat, torus, internal = TRUE, overwrite = TRUE, compress = "bzip2")
