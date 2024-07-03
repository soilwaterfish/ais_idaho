#' Link Water Quality Attributes
#' @description
#' This functions puts the pieces together for different water quality attributes. It uses a lot of joins and selecting so
#' syntax is very important.
#'
#' @param wqp A previously created `get_wqp()` object.
#' @param burp A previously created `get_burp()` object.
#' @param streamtemp A previously created `get_streamtemp()` object.
#' @param waterbodytemp A previously created `get_waterbodytemp()` object.
#' @param link_mp_with_huc12 A previously created `link_mp_with_huc12()` object.
#'
#' @return
#' @export
#'
link_wqs_with_huc12 <- function(wqp, burp, streamtemp, waterbodytemp, link_mp_with_huc12) {

  wqp_summarized <- wqp %>%
                    dplyr::select(characteristic_name, result_measure_value,
                           monitoring_location_identifier, huc12,
                           date) %>%
                    tidyr::pivot_wider(names_from = 'characteristic_name',
                                values_from = 'result_measure_value') %>%
                    janitor::clean_names() %>%
                    dplyr::group_by(huc12) %>%
                    dplyr::summarise(dplyr::across(p_h:hardness_non_carbonate, ~mean(.x, na.rm = TRUE))) %>%
                    dplyr::select(huc12,
                           p_h,
                           calcium,
                           conductivity,
                           hardness = 'hardness_ca_mg',
                           dissolved_oxygen = 'dissolved_oxygen_do')

  # now do the same with the BURP data

  burp_summarized <- burp %>%
                      sf::st_drop_geometry() %>%
                      dplyr::select(huc12, conductivity:dissolved_oxygen) %>%
                      dplyr::group_by(huc12) %>%
                      dplyr::summarise(dplyr::across(conductivity:dissolved_oxygen, ~mean(.x, na.rm = TRUE)))


  # now bind together and group summarize by huc12

  all_together_burp_wqp <- dplyr::bind_rows(wqp_summarized, burp_summarized) %>%
                            dplyr::group_by(huc12) %>%
                            dplyr::summarise(dplyr::across(dplyr::everything(), ~mean(.x, na.rm = TRUE)))

  # now we can do a long if/else

  final_ish_model <- all_together_burp_wqp %>%
    dplyr::mutate(
      p_h_model = dplyr::case_when(
        p_h > 0 & p_h <= 3.9 ~ 1,
        p_h > 4 & p_h <= 5.4~ 2,
        p_h > 5.5 & p_h <= 6.9 ~ 3,
        p_h > 7 & p_h <= 9.9 ~ 4,
        p_h > 10 & p_h <= 11 ~ 3,
        p_h > 11 & p_h <= 13 ~ 2,
        p_h > 13 & p_h <= 14 ~ 1,
        TRUE ~ 2
      ),
      calcium_model = dplyr::case_when(
        calcium > 0 & calcium <= 4 ~ 1,
        calcium > 4 & calcium <= 13~ 2,
        calcium > 13 & calcium <= 24 ~ 3,
        calcium > 24 & calcium <= 100 ~ 4,
        TRUE ~ 2
      ),
      hardness_model = dplyr::case_when(
        hardness > 0 & hardness <= 50 ~ 1,
        hardness > 50 & hardness <= 99~ 2,
        hardness > 99 & hardness <= 125 ~ 3,
        hardness > 125 & hardness <= 1000 ~ 4,
        TRUE ~ 2
      ),
      do_model = dplyr::case_when(
        dissolved_oxygen > 0 & dissolved_oxygen <= 3 ~ 1,
        dissolved_oxygen > 3 & dissolved_oxygen <= 7~ 2,
        dissolved_oxygen > 7 & dissolved_oxygen <= 12 ~ 3,
        dissolved_oxygen > 12 & dissolved_oxygen <= 50 ~ 4,
        TRUE ~ 2
      ),
      conductivity_model = dplyr::case_when(
        conductivity > 0 & conductivity <= 490 ~ 1,
        conductivity > 490 & conductivity <= 989~ 2,
        conductivity > 989 & conductivity <= 1499 ~ 3,
        conductivity > 1499 & conductivity <= 3000 ~ 4,
        TRUE ~ 2
      )
    )


  waterbody_huc12 <- read.csv('data/huc12_outlet_comids.csv') %>%
    dplyr::mutate(HUC_12 = as.character(HUC_12))

  stream_temp_id <- streamtemp %>%
                    dplyr::rename(temperature_water = 'S2_02_11') %>%
                    dplyr::left_join(waterbody_huc12 %>% dplyr::select('COMID', huc12 = 'HUC_12')) %>%
                    dplyr::group_by(huc12) %>%
                    dplyr::summarise(temperature_water = mean(temperature_water)) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(
                      temperature_water = (temperature_water*9/5) + 32,
                      temperature_model = dplyr::case_when(
                        temperature_water > 0 & temperature_water <= 40 ~ 1,
                        temperature_water > 40 & temperature_water <= 46~ 2,
                        temperature_water > 46 & temperature_water <= 56 ~ 3,
                        temperature_water > 56 & temperature_water <= 71 ~ 4,
                        temperature_water > 71 & temperature_water <= 75 ~ 3,
                        temperature_water > 75 & temperature_water <= 83 ~ 2,
                        temperature_water > 83 & temperature_water <= 120 ~ 1,
                        TRUE ~ 2
                      )) %>%
                    sf::st_drop_geometry() %>%
                    dplyr::select( huc12, temperature = 'temperature_water', temperature_model)

  waterbodies <- link_mp_with_huc12[['waterbodies_final_social']]
  flowlines <- link_mp_with_huc12[['flowline_final_social']]

  waterbodies_final_together <- waterbodies %>%
                                dplyr::select(huc12,comid, dplyr::starts_with('waterbody_'), mussel_proximity) %>%
                                dplyr::left_join(final_ish_model) %>%
                                dplyr::left_join(waterbodytemp, by = 'comid') %>%
                                dplyr::mutate(dplyr::across(dplyr::contains('_model'), ~ifelse(is.na(.x), 2, .x))) %>%
                                dplyr::mutate(dplyr::across(dplyr::starts_with('waterbody_'), ~round(.x)))



  flowline_final_together <- flowlines %>%
                            dplyr::select(huc12, dplyr::starts_with('waterbody_'), mussel_proximity) %>%
                            dplyr::left_join(final_ish_model) %>%
                            dplyr::left_join(stream_temp_id %>% dplyr::select(huc12, temperature, temperature_model))%>%
                            dplyr::mutate(dplyr::across(dplyr::contains('_model'), ~ifelse(is.na(.x), 2, .x))) %>%
                            dplyr::mutate(dplyr::across(dplyr::starts_with('waterbody_'), ~round(.x)))


  list(flowline_final_together = flowline_final_together,
       waterbodies_final_together = waterbodies_final_together)
}









#' Get Stream Temperature
#'
#' @param boundary A sf object for clipping the area of interest.
#' @param processing_units A character of the Processing Units, e.g. 'SpoKoot'.
#'
#' @return
#' @export
#' @note This is only for flowlines and not waterbodies.
#'
get_streamtemp <- function(boundary, processing_units) {

  nwtemp <- list()

  for(i in processing_units){

    nwtemp[[i]] <- fishguts::get_NorWestStreams(i)

  }


  stream_temp_id <- dplyr::bind_rows(nwtemp) %>% sf::st_as_sf()

  stream_temp_id <- stream_temp_id %>% sf::st_as_sf() %>% sf::st_intersection(sf::st_transform(boundary, sf::st_crs(stream_temp_id)))


}

#' Get WesternAIS Samples
#'
#' @param waterbodies A sf object with USGS waterbodies.
#' @description This function calls the `https://gis.psmfc.org/server/rest/services/WesternAIS/Quagga_and_Zebra_Mussel_Monitoring_Sites/MapServer/` API and filters sites based on
#' water temperature.
#' @return
#' @export
#'
#' @examples
get_waterbodytemp <- function(waterbodies) {
  #### create an empty list to put previous years data into

  monitorings_sites_quagga_zebra <- list()

  # loop through and call ESRI api
  for (i in c(1:8)){

    monitorings_sites_quagga_zebra[[i]] <- arcgislayers::arc_select(arcgislayers::arc_open(paste0('https://gis.psmfc.org/server/rest/services/WesternAIS/Quagga_and_Zebra_Mussel_Monitoring_Sites/MapServer/', i)),filter_geom = sf::st_bbox(waterbodies))


  }
  filter_list <- monitorings_sites_quagga_zebra[unlist(purrr::map(monitorings_sites_quagga_zebra,~!purrr::is_empty(.x)))]

  # clean up weird character geometries
  monitorings_sites_quagga_zebra_sf <- dplyr::bind_rows(filter_list) %>% sf::st_as_sf()

  monitorings_sites_quagga_zebra_sf$geom_char <- purrr::map(sf::st_geometry(monitorings_sites_quagga_zebra_sf), ~is.numeric(.x[[1]])) %>%
    unlist()

  monitorings_sites_quagga_zebra_sf <- monitorings_sites_quagga_zebra_sf %>% dplyr::filter(geom_char)


  monitorings_sites_quagga_zebra_sf <- monitorings_sites_quagga_zebra_sf[!sf::st_is_empty(sf::st_zm(monitorings_sites_quagga_zebra_sf)),,drop=FALSE]

  # filter and clean up

  ais_list <- monitorings_sites_quagga_zebra_sf %>% sf::st_intersects(sf::st_transform(waterbodies, sf::st_crs(monitorings_sites_quagga_zebra_sf)))

  ais_list <- lengths(ais_list) > 0

  # now we'll have each site within idaho

  monitorings_sites_quagga_zebra_sf <- monitorings_sites_quagga_zebra_sf[ais_list,]


  # you could really try and get as much as you can but
  # for our area the water temp is the most robust so we'll stick with that

  # clean up

  monitorings_sites_quagga_zebra_sf <- monitorings_sites_quagga_zebra_sf %>%
                                        dplyr::filter(!is.na(WATERTEMPF)) %>%
                                        dplyr::select(temperature_water = 'WATERTEMPF')


  wb_temp_list <- monitorings_sites_quagga_zebra_sf %>% sf::st_intersects(sf::st_transform(waterbodies, sf::st_crs(.)))

  wb_temp_list <- lengths(wb_temp_list) > 0

  temp_wb <- monitorings_sites_quagga_zebra_sf[wb_temp_list,]

  temp_wb <- temp_wb %>% sf::st_intersection(sf::st_transform(waterbodies, sf::st_crs(.)))


  temp_wb <- temp_wb %>%
                    dplyr::mutate(temperature_water = dplyr::if_else(temperature_water < 32, (temperature_water*9/5) + 32, temperature_water)) %>%
                    dplyr::group_by(comid) %>%
                    dplyr::mutate(temperature_water = mean(temperature_water, na.rm = TRUE)) %>%
                    dplyr::slice(1) %>%
                    dplyr::ungroup() %>%
                    dplyr::mutate(
                      temperature_model = dplyr::case_when(
                        temperature_water > 0 & temperature_water <= 40 ~ 1,
                        temperature_water > 40 & temperature_water <= 46~ 2,
                        temperature_water > 46& temperature_water <= 56 ~ 3,
                        temperature_water > 56 & temperature_water <= 71 ~ 4,
                        temperature_water > 71 & temperature_water <= 75 ~ 3,
                        temperature_water > 75 & temperature_water <= 83 ~ 2,
                        temperature_water > 83 & temperature_water <= 120 ~ 1,
                        TRUE ~ 2
                      )) %>%
                    sf::st_drop_geometry() %>%
                    dplyr::select(comid, temperature = 'temperature_water', temperature_model)

}

#' Get Water Quality Data
#'
#' @description
#' This function calls the Water Quality Portal from the \link[dataRetrieval] package to query specific water quality paramaters.
#' This will take a while to download and also will take a decent amount of space (> Gb) so be aware before running!
#'
#' @param statecode A statecode abreviation, e.g. 'ID'.
#' @param huc12 A sf object with USGS HUC 12s.
#'
#' @return
#' @export
#'
#' @examples
get_wqp <- function(statecode, huc12) {

  # we'll use these characteristic names to query the WQP

  wq_names <- c(
    "pH", "Bicarbonate",
    "Calcium", "Calcium carbonate",
    "Hardness, Ca, Mg", "Dissolved oxygen saturation",
    "Dissolved oxygen (DO)", "Conductivity",
    "Hardness, non-carbonate", "Alkalinity, carbonate" ,
    "Alkalinity, bicarbonate", "Hardness, magnesium" ,
    "Hardness, carbonate", "Total hardness",
    "Alkalinity, Phenolphthalein (total hydroxide+1/2 carbonate)", "Specific conductivity",
    "Hardness", "Calcium as CaCO3" ,
    "Alkalinity, Bicarbonate as CaCO3"
  )

  # Also, keep it to Idaho and within water.
  wqp <- dataRetrieval::readWQPdata(statecode = statecode,
                                          characteristicName = wq_names,
                                          sampleMedia = 'Water')

  ### we'll use this to intersect the huc12s and waterbodies

  wqp_site_info <- attr(wqp, 'siteInfo')

  wqp_sf <- sf::st_as_sf(wqp_site_info %>% dplyr::filter(!is.na(dec_lon_va)), coords = c('dec_lon_va', 'dec_lat_va'), crs = 4326)

  # now combined with HUCS to make easier.

  huc_list <- sf::st_intersects(wqp_sf, st_transform(huc12, crs = sf::st_crs(wqp_sf)))

  huc_list <- purrr::map_vec(huc_list, ~ifelse(is.null(.x), NA, .x))

  # now we'll have each site iwth a huc12

  wqp_sf$huc12 <- huc12[huc_list,]$huc12

  wqp_filtered <-   wqp %>%
    dplyr::filter(ResultSampleFractionText  %in% c('Total', 'Filtered, lab'),
           ActivityMediaSubdivisionName == "Surface Water") %>%
    dplyr::mutate(ResultMeasureValue = readr::parse_number(ResultMeasureValue),
           ResultMeasureValue  = dplyr::if_else(ResultMeasure.MeasureUnitCode  %in% c('ug/L', 'ug/l'),
                                         ResultMeasureValue*0.001, ResultMeasureValue),
           ResultMeasure.MeasureUnitCode = dplyr::if_else(ResultMeasure.MeasureUnitCode  %in% c('ug/L', 'ug/l'),
                                                   'mg/L',ResultMeasure.MeasureUnitCode)) %>%
    janitor::clean_names() %>%
    dplyr::tibble()%>%
    dplyr::mutate(date = lubridate::date(activity_start_date_time)) %>%
    dplyr::group_by(monitoring_location_identifier, date) %>%
    dplyr::mutate(result_measure_value = mean(result_measure_value, na.rm = T)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # now add the huc12 id to join later with BURP, etc

  wqp_filtered <- wqp_filtered %>%
    dplyr::left_join(wqp_sf %>% sf::st_drop_geometry() %>%
                dplyr::select(huc12,monitoring_location_identifier = 'MonitoringLocationIdentifier'))

}


#' Title
#' @description
#' This function webscrapes https://www2.deq.idaho.gov/water/BurpViewer/ website to get relevant water quality data (temperature, ph, do, etc). It is
#' recommended to do this in parallel because it will take a really long time to get all of the data!
#'
#' @param years A vector of years to query BURP with.
#' @param huc12 A sf object with USGS HUC 12s.
#'
#' @return A sf object.
#' @export
#'
#' @examples
get_burp <- function(years = c(1994:2008, 2010:2023), huc12) {

  site_ids <- vector()

  for(i in years) {

    url_sites <- paste0('https://www2.deq.idaho.gov/water/BurpViewer/Menu?Year=', i)

    req <- httr2::request(url_sites)

    req_body <- req %>% httr2::req_perform()

    # get ids
    ids <- readLines(req_body$url)

    keywords <- paste0(i,"[A-Z0-9]+")

    si <- na.omit(stringr::str_extract(ids, keywords))

    site_ids <- append(site_ids, si)


  }

  # get the ids

  site_ids_filtered <- stringr::str_unique(site_ids)

  burp_scrape <- site_ids_filtered %>%
    furrr::future_map(purrr::safely(~burp_func(.)))

  burp_final <- burp_scrape %>%
                purrr::keep(~length(.) != 0) %>%
                purrr::map(~.x[['result']]) %>%
                plyr::rbind.fill()

  #### now clean up and make a spatial copy

  burp_final_cleaned <- burp_final %>%
                        dplyr::mutate(across(everything(), ~ifelse(.x  %in% c('No Data', 'NA m', ' ', '', 'mg/l'), NA_real_, .x))) %>%
                        dplyr::mutate(across(c(air_temperature:total_reach_length, stream_order), ~readr::parse_number(.x))) %>%
                        dplyr::filter(latitude > 0,
                                           site_id != '2001SPOCA029',
                                           p_h < 15,
                                           p_h > 0)

  burp_final_cleaned_sf <- burp_final_cleaned %>% sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

  #now intersect with huc12

  huc_list <- sf::st_intersects(burp_final_cleaned_sf, sf::st_transform(huc12, crs = sf::st_crs(burp_final_cleaned_sf)))

  huc_list <- purrr::map_vec(huc_list, ~ifelse(is.null(.x), NA, .x))

  # now we'll have each site iwth a huc12

  burp_final_cleaned_sf$huc12 <- huc12[huc_list,]$huc12

  burp_final_cleaned_sf <- burp_final_cleaned_sf %>% dplyr::filter(!is.na(huc12))

}

#' Webscrape BURP
#'
#' @param id A character of a BURP id.
#'
#' @return A tibble with water quality information.
#'
burp_func <- function(id){

  url_streams <- paste0('https://www2.deq.idaho.gov/water/BurpViewer/BurpSite/Stream?BurpID=', id)

  req <- httr2::request(url_streams)

  req_body <- req %>% httr2::req_perform()

  stream_html <- req_body %>% httr2::resp_body_html()

  values <- stream_html %>%
    rvest::html_elements(xpath = "//span[@class='readOnly']/text()") %>%
    rvest::html_text2()

  labels <- stream_html %>%
    rvest::html_elements(xpath = "//div[@class='oneThird']/label/text()") %>%
    rvest::html_text2()

  meta_label <- stream_html %>%
    rvest::html_elements(xpath = "//div[@id='leftCol']//ul/li/text()") %>%
    rvest::html_text2()

  meta <- stream_html %>%
    rvest::html_elements(xpath = "//div[@id='leftCol']//ul/li/strong/text()") %>%
    rvest::html_text2()

  meta <- stream_html %>%
    rvest::html_elements(xpath = "//div[@id='leftCol']//ul/li/strong/text()") %>%
    rvest::html_text2()

  md <- dplyr::tibble(
    labels = meta_label[-2],
    value = meta
  )

  url_location <- paste0('https://www2.deq.idaho.gov/water/BurpViewer/BurpSite/Location?BurpID=', id)

  req <- httr2::request(url_location)

  req_body <- req %>% httr2::req_perform()

  location_html <- req_body %>% httr2::resp_body_html()

  values_loc <- location_html %>%
    rvest::html_elements(xpath = "//div[@class='half']//label/text()") %>%
    rvest::html_text2()

  labels_loc <- location_html %>%
    rvest::html_elements(xpath = "//span[@class='readOnly']/text()") %>%
    rvest::html_text2()

  site_info <- dplyr::tibble(labels = labels,
                      values = values,
                      sample_year = md[2,2]$value,
                      stream = md[3,2]$value,
                      assessment_unit = md[4,2]$value,
                      site_id = id)%>%
    tidyr::pivot_wider(names_from = labels, values_from = values) %>%
    janitor::clean_names()

  location_info <- dplyr::tibble(labels = labels_loc,
                          values = values_loc,
                          site_id = id) %>%
    tidyr::pivot_wider(names_from = values, values_from = labels) %>%
    janitor::clean_names()

  suppressMessages(dplyr::left_join(site_info, location_info))

}
