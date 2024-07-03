# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "sf", "aisrisk") # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own: There are options to make it more dynamic but for no
# it just uses previously created data....
list(


  #tar_target(flowlines, sf::read_sf('data/test_data.gpkg', layer = 'flowlines')),
  tar_target(flowlines, sf::read_sf('dev/data/simple_features.gpkg', layer = 'nhdplus_idaho') %>% dplyr::select(-mussel_proximity)),

  #tar_target(waterbodies, sf::read_sf('data/test_data.gpkg', layer = 'waterbodies')),
  tar_target(waterbodies, sf::read_sf('dev/data/simple_features.gpkg', layer = 'waterbodies_idaho') %>% dplyr::select(-mussel_proximity)),

  #tar_target(huc12, sf::read_sf('data/test_data.gpkg', layer = 'huc12')),
  tar_target(huc12, sf::read_sf('dev/data/simple_features.gpkg', layer = 'huc12')),

  tar_target(type_size_position, get_type_size_position(waterbodies, flowlines)),

  tar_target(non_fs_roads, get_non_fs_roads(state = 'ID') %>% dplyr::filter(SystemCode != 'FD')),

  tar_target(usfs_roads, sf::read_sf('Z:/simple_features/roads/r1_rd_core.shp')),

  #tar_target(boundary,  sf::st_as_sfc(sf::st_bbox(huc12))),
  tar_target(boundary,  sf::read_sf('dev/data/simple_features.gpkg', layer = 'idaho')),

  tar_target(mussel_locations, sf::read_sf('dev/data/simple_features.gpkg', layer = 'mussel_locations')),

  tar_target(mussel_proximity_df, get_mussel_proximity(boundary,
                                                       non_usfs_roads = non_fs_roads,
                                                       usfs_roads = usfs_roads,
                                              mussel_locations = mussel_locations,
                                              huc12 = huc12,
                                              workspace = 'Z:/GIT/aisrisk/dev/scratch')),

  tar_target(link_up_sss, link_mp_with_huc12(mussel_proximity_df, type_size_position, huc12)),

  tar_target(streamtemp, get_streamtemp(boundary = boundary, c('SpoKoot','Clearwater River Basin', 'Salmon River Basin', 'Middle Columbia'))),

  tar_target(waterbodytemp, get_waterbodytemp(waterbodies = type_size_position$wb)),

  tar_target(wqp, get_wqp(statecode = 'ID', huc12 = huc12)),

  #tar_target(burp, get_burp(huc12 = huc12)),
  tar_target(burp, sf::read_sf('dev/data/simple_features.gpkg', layer = 'burp_final_cleaned_sf')),

  tar_target(link_up_hss, link_wqs_with_huc12(wqp, burp, streamtemp, waterbodytemp, link_up_sss)),

  tar_target(final_scores, get_final_scores(link_up_hss))


)
