get_non_fs_roads <- function(state = 'ID') {

  switch(
    state,
    'ID' = {

      id_roads_sf <- arcgislayers::arc_select(arcgislayers::arc_open('https://gisportalp.itd.idaho.gov/xserver/rest/services/RH_GeneralService/MapServer/1/'))%>%
        sf::st_transform(3742)

      ### this is because of an empty geometry; hopefully you won't have to deal with....
      empty_geom <- id_roads_sf %>% sf::st_geometry() %>% purrr::map(~purrr::is_empty(.x))

      id_roads_sf$empty_geom <- unlist(empty_geom)

      id_roads_sf %>% dplyr::filter(empty_geom == FALSE)  %>% dplyr::filter(SystemCode != 'FD')
    },
    'MT' = {

    }
  )

}


#' Get USDA-Forest Service Administration Boundaries
#'
#' @description
#' An area depicted as surface ownership parcels dissolved on the same ownership classification. Go to this URL for full metadata description: https://data.fs.usda.gov/geodata/edw/edw_resources/meta/S_USA.BasicOwnership.xml
#'
#'
#' @param boundary an object of class bbox, sfc or sfg used to filter query results based on a predicate function.
#' @param ... Arguments to pass to \link[arcgislayers] package `arc_select` function.
#' @return
#' @export
#'
get_adminboundaries <- function(boundary, ...) {

  url <- arcgislayers::arc_open('https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_BasicOwnership_01/MapServer/0')

  admin <- arcgislayers::arc_select(url, filter_geom = boundary, ...)

}
