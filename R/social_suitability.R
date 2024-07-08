
#' Connect Lakes with Outlet COMID
#'
#' @param mussel_proximity_df A previously created `get_mussel_proximity()` object.
#' @param type_size_position A previously created `get_type_size_position()` object.
#' @param huc12 A sf object with USGS HUC 12s.
#'
#' @return
#' @export
#'
#' @examples
link_mp_with_huc12 <- function(mussel_proximity_df,
                               type_size_position,
                               huc12) {

  # Need to get centroid of lakes and then associated HUC12s

  waterbodies <- type_size_position[['wb']]
  flowlines <- type_size_position[['flowlines']]

  lake_list <- sf::st_centroid(waterbodies) %>%
    sf::st_intersects(huc12)

  lake_list <- purrr::map_vec(lake_list, ~ifelse(is.null(.x), NA, .x))

  huc12s <- huc12[lake_list,]$huc12

  waterbodies$huc12 <- huc12s

  # we'll need to get COMIDs and HUC12 codes

  waterbody_huc12 <- read.csv('data/huc12_outlet_comids.csv') %>%
    dplyr::mutate(HUC_12 = as.character(HUC_12))

  flowlines <- flowlines %>% dplyr::left_join(waterbody_huc12 %>% dplyr::select(comid = 'COMID', huc12 = 'HUC_12'))

  # now join back with the waterbodies and nhdplus datasets

  flowlines <- flowlines %>% dplyr::left_join(mussel_proximity_df, by = 'huc12')

  waterbodies <- waterbodies %>% dplyr::left_join(mussel_proximity_df, by = 'huc12')

  fsland <- get_adminboundaries(sf::st_as_sfc(sf::st_bbox(flowlines))) %>%
    sf::st_make_valid() %>%
    sf::st_transform(sf::st_crs(flowlines))

  wild_water <- flowlines %>% sf::st_intersects(fsland)

  wild_water <- purrr::map_vec(wild_water, ~ifelse(is.null(.x), NA, .x))

  flowlines$fs_land <- wild_water

  ### now for waterbodies

  wild_water <- waterbodies %>% sf::st_intersects(fsland)

  wild_water <- purrr::map_vec(wild_water, ~ifelse(is.null(.x), NA, .x))

  waterbodies$fs_land <- wild_water

  ### bring it all together
  flowline_final_social <- flowlines %>%
    dplyr::mutate(mussel_proximity = dplyr::if_else(is.na(mussel_proximity) & fs_land == 1, 1, mussel_proximity),
                  mussel_proximity = dplyr::if_else(is.na(mussel_proximity), 1, mussel_proximity))

  waterbodies_final_social <- waterbodies %>%
    dplyr::mutate(mussel_proximity = dplyr::if_else(is.na(mussel_proximity) & fs_land == 1, 1, mussel_proximity),
                  mussel_proximity = dplyr::if_else(is.na(mussel_proximity), 1, mussel_proximity))

  flowline_final_social <- flowline_final_social %>%
    dplyr::group_by(huc12) %>%
    dplyr::mutate(dplyr::across(c('waterbody_type', 'waterbody_size_rec', 'waterbody_position'), ~ifelse(is.na(.x), 2, .x))) %>%
    dplyr::mutate(dplyr::across(c('waterbody_type', 'waterbody_size_rec', 'waterbody_position'), ~mean(.x , na.rm = T))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(huc12, by = 'huc12')


  list(flowline_final_social = flowline_final_social%>%
         sf::st_as_sf(),
       waterbodies_final_social = waterbodies_final_social)

}


#' Get Type, Size, Position
#' @param wb_layer A sf object of NHDPlus V2 waterbodies.
#' @param flowline_layer A sf object of NHDPLus V2 flowlines.
#' @param elevation A numeric. Used for waterbody upland/lowland filtering, in meters.
#'
#' @return
#' @importFrom dplyr "%>%"
#' @export
#'
#'
get_type_size_position <- function(waterbodies,
                                   flowline,
                                   elevation = 1500){



  waterbodies <- waterbodies %>%
    dplyr::mutate(area_acres = as.numeric(units::set_units(sf::st_area(.), 'acres')),
                  waterbody_type = dplyr::if_else(area_acres<25, 3, 4)
    )

  flowlines <- flowline %>%
    dplyr::mutate(waterbody_type = dplyr::if_else(streamorde <= 3,1,
                                                  dplyr::if_else(streamorde >3 & streamorde <=5 , 2, 4)))


  qts <- quantile(waterbodies$area_acres)

  waterbodies <- waterbodies %>% dplyr::mutate(waterbody_size_rec = dplyr::if_else(area_acres < qts[[2]], 1,
                                                                                   dplyr::if_else(area_acres >= qts[[2]] & area_acres < qts[[3]], 2,
                                                                                                  dplyr::if_else(area_acres >= qts[[3]] & area_acres < qts[[4]], 3, 4))))

  flowlines <- flowlines %>%
    dplyr::rowwise() %>%
    dplyr::mutate(gnis_name = dplyr::if_else(gnis_name == ' ', as.character(paste0(paste0(sample(letters, 4), collapse = ''), '_', sample(1:100000000, 1))), gnis_name)) %>%
    dplyr::group_by(gnis_name) %>%
    dplyr::mutate(dist = sum(lengthkm, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(waterbody_size_rec = dplyr::if_else(dist < 15, 1,
                                                      dplyr::if_else(dist >= 15 & dist < 30, 2,
                                                                     dplyr::if_else(dist >= 30 & dist < 60, 3, 4))))

  # from visual inspection we can see that lakes with > 300 acres are likely lowland waterbodies

  waterbodies <- waterbodies %>%
    dplyr::mutate(waterbody_position = dplyr::if_else(area_acres > 300, 4, NA_real_))

  # now get elevations for the ones that are below 300

  waterbodies_filtered <- waterbodies %>%
    dplyr::filter(is.na(waterbody_position)) %>%
    sf::st_centroid() %>%
    dplyr::select(-elevation) %>%
    elevatr::get_elev_point()

  waterbodies_filtered <- dplyr::bind_cols( waterbodies %>% dplyr::filter(is.na(waterbody_position)) %>%
                                            dplyr::select(comid) %>% sf::st_drop_geometry(),
                                            waterbodies_filtered %>% dplyr::select(ele = 'elevation') %>%
                                              sf::st_drop_geometry())

  # then join back with original

  ### just visually looked at the patterns and most valleys are below 1600 and high mountain lake are above

  waterbodies <- waterbodies %>%
    dplyr::left_join(waterbodies_filtered) %>%
    dplyr::mutate(waterbody_position = ifelse(!is.na(waterbody_position), waterbody_position,
                                              ifelse(ele > elevation, 3, 4)))

  flowlines <- flowlines %>%
    dplyr::mutate(waterbody_position = dplyr::if_else(streamorde <= 2, 1,
                                                      dplyr::if_else(streamorde > 2 & streamorde <=3, 2,
                                                                     dplyr::if_else(streamorde > 3 & streamorde <=5 , 3, 4))))

  list(wb = waterbodies,
       flowlines = flowlines)
}


#' Get Mussel Proximity
#'
#' @param boundary A sf object for clipping the area of interest.
#' @param non_usfs_roads A sf object with non USFS roads.
#' @param usfs_roads A sf object with USFS roads.
#' @param mussel_locations A sf object with Mussel Locations.
#' @param huc12 A sf object with USGS HUC 12s.
#' @param conda_path A file path to conda env, e.g. `r'{C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3}'`.
#' @param workspace A local file path to store file with `arcpy`.
#'
#' @return A tibble with mussel proximity by meters `mean_dist` and HUC 12.
#' @export
#'
#' @examples
get_mussel_proximity <- function (boundary,
                                  non_usfs_roads,
                                  usfs_roads,
                                  mussel_locations,
                                  huc12,
                                  conda_path = r'{C:\Program Files\ArcGIS\Pro\bin\Python\envs\arcgispro-py3}',
                                  workspace = getwd()) {

  # you'll need to figure out how to get your usfs simple feature; here it was local for me.

  usfs_roads <- usfs_roads %>%
                sf::st_intersection(sf::st_transform(boundary, sf::st_crs(.)))%>%
                sf::st_transform(3742)

  non_usfs_roads <- non_usfs_roads %>%
                  sf::st_intersection(sf::st_transform(boundary, sf::st_crs(.)))%>%
                  sf::st_transform(3742)

  roads_together <- dplyr::bind_rows(non_usfs_roads, usfs_roads) %>%
                    sf::st_as_sf() %>%
                    sf::st_cast('LINESTRING')

  # get the mussel locations
  mussel_locations <- mussel_locations %>%
                      sf::st_transform(crs = sf::st_crs(usfs_roads))

  # set to conda env
  reticulate::use_condaenv(conda_path)

  arcpy <- reticulate::import('arcpy')

  tog_roads <- tempfile(fileext = '.shp')

  sf::write_sf(roads_together %>% janitor::clean_names(), tog_roads)

  arcpy$env$workspace = workspace

  arcpy$CreateFileGDB_management(out_name = 'roads.gdb', out_folder_path = arcpy$env$workspace)

  arcpy$FeatureToLine_management(in_features =  tog_roads,
                                 out_feature_class = 'roads.gdb/together_roads',
                                 cluster_tolerance = '5 Meters')

  roads_together <- sf::read_sf(paste0(arcpy$env$workspace,'/roads.gdb'), layer = 'together_roads') %>%
                    sf::st_cast("LINESTRING")

  # also convert to a crs that is good for north to south, etc. 3742 EPSG for Idaho UTM 12

  net <- sfnetworks::as_sfnetwork(roads_together, directed = FALSE) %>%
         sfnetworks::activate("edges") %>%
         dplyr::mutate(weight = sfnetworks::edge_length()) %>%
         dplyr::filter(!tidygraph::edge_is_multiple()) %>%
         dplyr::filter(!tidygraph::edge_is_loop())

  # get the distances between the mussel_point and the nodes in the network

  distances <- sfnetworks::st_network_cost(net, from = mussel_locations, weights = 'weight')

  # now get the 'from' in the network
  edged_start <- net %>% sfnetworks::activate('edges') %>% dplyr::pull(from)

  # convert infinite to numeric
  net_final <- net %>%
                sfnetworks::activate('edges') %>%
                dplyr::mutate(new_weight = purrr::map(edged_start, ~as.numeric(dplyr::if_else(is.infinite(distances[[.x]]), NA_real_, as.numeric(distances[[.x]])))))

  # convert from meters to miles
  net_final_edges <- net_final %>%
                      sfnetworks::activate('edges') %>%
                      dplyr::mutate(new_weight = unlist(new_weight),
                             new_weight = ifelse(is.infinite(new_weight), NA_real_, new_weight),
                             new_weight_miles = as.numeric(new_weight*0.000621371)) %>%
                      dplyr::select(-new_weight) %>%
                      sf::st_as_sf()

  # adding huc12s and aggregating with a mean distance will make it easier for joining and not a lot of information lost

  # now combined with HUCS to make easier.

  huc_list <- sf::st_intersects(net_final_edges, sf::st_transform(huc12, crs = sf::st_crs(net_final_edges)))

  huc_list <- purrr::map_vec(huc_list, ~ifelse(is.null(.x), NA, .x))

  huc12s <- huc12[huc_list,]$huc12

  # now add back to the network
  net_final_edges$huc12 <- huc12s

  distance_by_huc <- net_final_edges %>%
                      dplyr::group_by(huc12) %>%
                      dplyr::summarise(mean_dist = mean(new_weight_miles, na.rm = TRUE)) %>%
                      dplyr::ungroup() %>%
                      sf::st_as_sf() %>%
                      sf::st_drop_geometry() %>%
                      dplyr::select(huc12, mean_dist)

  distance_by_huc <- distance_by_huc %>%
                      dplyr::mutate(
                          mussel_proximity = dplyr::case_when(
                            mean_dist > 400 ~ 1,
                            mean_dist > 300 & mean_dist <= 400 ~ 2,
                            mean_dist > 200 & mean_dist <= 300 ~ 3,
                            mean_dist <= 200 ~ 4,
                            TRUE ~ NA_real_
                          )
                    )

}

