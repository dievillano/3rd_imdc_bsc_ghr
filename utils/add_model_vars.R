add_model_vars <- function(data) {
  data |>
    dplyr::mutate(
      koppen_id2 = koppen_id,
      koppen_id3 = koppen_id,
      koppen_id4 = koppen_id,
      koppen_id5 = koppen_id,
      koppen_id6 = koppen_id,
      koppen_id7 = koppen_id,
      koppen_id8 = koppen_id,
      
      v1 = tasan6.l1,
      v2 = spei3.l1,
      v3 = spei12.l3,
      v1v2 = v1 * v2,
      v1v3 = v1 * v3,
      v2v3 = v2 * v3,
      v1v2v3 = v1 * v2 * v3,
      
      period_id = dplyr::if_else(epiyear <= 2018L, 1L, 2L),
      state_id2 = state_id
    )
}