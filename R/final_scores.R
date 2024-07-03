#' Get Final AIS Risk Scores
#'
#' @param link_wqs_with_huc12 A previously created `link_wqs_with_huc12()` object.
#'
#' @return
#' @export
#'
get_final_scores <- function(link_wqs_with_huc12) {

  final_stream <- link_wqs_with_huc12[['flowline_final_together']]
  final_lakes <- link_wqs_with_huc12[['waterbodies_final_together']]

  social_and_habitat_final_final <- dplyr::bind_rows(final_stream, final_lakes) %>%
    dplyr::mutate(dplyr::across(c(waterbody_type:mussel_proximity, p_h_model:temperature_model), ~ifelse(is.na(.x), 0, .x))) %>%
    dplyr::mutate(
      summed_social = waterbody_position + waterbody_size_rec + waterbody_type + mussel_proximity,
      summed_habitat = temperature_model + p_h_model + calcium_model + hardness_model + do_model + conductivity_model
    ) %>%
    dplyr::mutate(
      final_score_social = dplyr::case_when(
        summed_social > 0 & summed_social <= 5 ~ 1,
        summed_social > 5 & summed_social <= 10 ~ 2,
        summed_social > 10 & summed_social <= 16 ~ 3,
        summed_social > 16 & summed_social <= 24 ~ 4,
      ),
      final_score_habitat = dplyr::case_when(
        summed_habitat > 0 & summed_habitat <= 5 ~ 1,
        summed_habitat > 5 & summed_habitat <= 10 ~2,
        summed_habitat > 10 & summed_habitat <= 16 ~ 3,
        summed_habitat > 16 & summed_habitat <= 24 ~ 4,
      )
    ) %>%
    dplyr::mutate(
      final_score = dplyr::case_when(
        final_score_social==4 & final_score_habitat==4 ~ 1,
        final_score_social==4 & final_score_habitat==3 ~ 2,
        final_score_social==3& final_score_habitat==4 ~ 2,
        final_score_social==3& final_score_habitat==3 ~ 3,
        final_score_social==4& final_score_habitat==2 ~ 4,
        final_score_social==4& final_score_habitat==1 ~ 4,
        final_score_social==2& final_score_habitat==4 ~ 4,
        final_score_social==1& final_score_habitat==4 ~ 4,
        final_score_social==3& final_score_habitat==NA_real_ ~ 5,
        final_score_social==NA_real_& final_score_habitat==3 ~ 5,
        final_score_social==3& final_score_habitat==2 ~ 6,
        final_score_social==2& final_score_habitat==3 ~ 6,
        final_score_social==3& final_score_habitat==1 ~ 7,
        final_score_social==1& final_score_habitat==3 ~ 7,
        final_score_social==2& final_score_habitat==2 ~ 8,
        final_score_social==2& final_score_habitat==1 ~ 9,
        final_score_social==1& final_score_habitat==2 ~ 9,
        final_score_social==1 &final_score_habitat==1 ~ 10,
        TRUE ~ NA_real_
      )
    )


  final_lakes <- final_lakes %>%
    dplyr::mutate(dplyr::across(c(waterbody_type:mussel_proximity, p_h_model:temperature_model), ~ifelse(is.na(.x), 0, .x))) %>%
    dplyr::mutate(
      summed_social = waterbody_position + waterbody_size_rec + waterbody_type + mussel_proximity,
      summed_habitat = temperature_model + p_h_model + calcium_model + hardness_model + do_model + conductivity_model
    ) %>%
    dplyr::mutate(
      final_score_social = dplyr::case_when(
        summed_social > 0 & summed_social <= 5 ~ 1,
        summed_social > 5 & summed_social <= 10 ~ 2,
        summed_social > 10 & summed_social <= 16 ~ 3,
        summed_social > 16 & summed_social <= 24 ~ 4,
      ),
      final_score_habitat = dplyr::case_when(
        summed_habitat > 0 & summed_habitat <= 5 ~ 1,
        summed_habitat > 5 & summed_habitat <= 10 ~ 2,
        summed_habitat > 10 & summed_habitat <= 16 ~ 3,
        summed_habitat > 16 & summed_habitat <= 24 ~ 4,
      )
    ) %>%
    dplyr::mutate(
      final_score = dplyr::case_when(
        final_score_social==4 & final_score_habitat==4 ~ 1,
        final_score_social==4 & final_score_habitat==3 ~ 2,
        final_score_social==3& final_score_habitat==4 ~ 2,
        final_score_social==3& final_score_habitat==3 ~ 3,
        final_score_social==4& final_score_habitat==2 ~ 4,
        final_score_social==4& final_score_habitat==1 ~ 4,
        final_score_social==2& final_score_habitat==4 ~ 4,
        final_score_social==1& final_score_habitat==4 ~ 4,
        final_score_social==3& final_score_habitat==NA_real_ ~ 5,
        final_score_social==NA_real_& final_score_habitat==3 ~ 5,
        final_score_social==3& final_score_habitat==2 ~ 6,
        final_score_social==2& final_score_habitat==3 ~ 6,
        final_score_social==3& final_score_habitat==1 ~ 7,
        final_score_social==1& final_score_habitat==3 ~ 7,
        final_score_social==2& final_score_habitat==2 ~ 8,
        final_score_social==2& final_score_habitat==1 ~ 9,
        final_score_social==1& final_score_habitat==2 ~ 9,
        final_score_social==1 &final_score_habitat==1 ~ 10,
        TRUE ~ NA_real_



      )
    )

  final_stream <- final_stream %>%
    dplyr::mutate(dplyr::across(c(waterbody_type:mussel_proximity, p_h_model:temperature_model), ~ifelse(is.na(.x), 0, .x))) %>%
    dplyr::mutate(
      summed_social = waterbody_position + waterbody_size_rec + waterbody_type + mussel_proximity,
      summed_habitat = temperature_model + p_h_model + calcium_model + hardness_model + do_model + conductivity_model
    ) %>%
    dplyr::mutate(
      final_score_social = dplyr::case_when(
        summed_social > 0 & summed_social <= 5 ~ 1,
        summed_social > 5 & summed_social <= 10 ~ 2,
        summed_social > 10 & summed_social <= 16 ~ 3,
        summed_social > 16 & summed_social <= 24 ~ 4,
      ),
      final_score_habitat = dplyr::case_when(
        summed_habitat > 0 & summed_habitat <= 5 ~ 1,
        summed_habitat > 5 & summed_habitat <= 10 ~ 2,
        summed_habitat > 10 & summed_habitat <= 16 ~ 3,
        summed_habitat > 16 & summed_habitat <= 24 ~ 4,
      )
    ) %>%
    dplyr::mutate(
      final_score = dplyr::case_when(
        final_score_social==4 & final_score_habitat==4 ~ 1,
        final_score_social==4 & final_score_habitat==3 ~ 2,
        final_score_social==3& final_score_habitat==4 ~ 2,
        final_score_social==3& final_score_habitat==3 ~ 3,
        final_score_social==4& final_score_habitat==2 ~ 4,
        final_score_social==4& final_score_habitat==1 ~ 4,
        final_score_social==2& final_score_habitat==4 ~ 4,
        final_score_social==1& final_score_habitat==4 ~ 4,
        final_score_social==3& final_score_habitat==NA_real_ ~ 5,
        final_score_social==NA_real_& final_score_habitat==3 ~ 5,
        final_score_social==3& final_score_habitat==2 ~ 6,
        final_score_social==2& final_score_habitat==3 ~ 6,
        final_score_social==3& final_score_habitat==1 ~ 7,
        final_score_social==1& final_score_habitat==3 ~ 7,
        final_score_social==2& final_score_habitat==2 ~ 8,
        final_score_social==2& final_score_habitat==1 ~ 9,
        final_score_social==1& final_score_habitat==2 ~ 9,
        final_score_social==1 &final_score_habitat==1 ~ 10,
        TRUE ~ NA_real_



      )
    )

  list(final_flowlines = final_stream %>% dplyr::mutate(final_score = 11-final_score),
       final_waterbodies = final_lakes%>% dplyr::mutate(final_score = 11-final_score),
       social_and_habitat_final =social_and_habitat_final_final%>% dplyr::mutate(final_score = 11-final_score))

}
