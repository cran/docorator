## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(docorator)

## -----------------------------------------------------------------------------
library(gt)
mytbl <- gtcars |>
   dplyr::slice_head(n = 10) |>
   dplyr::select(mfr, model, year, msrp, ctry_origin) |>
   gt(rowname_col = "mfr") |>
   tab_style(
     style = cell_borders(c("left","right")),
     locations = list(cells_body(), cells_stub(), cells_column_labels())
   )

mytbl

## -----------------------------------------------------------------------------
mytbl |>
  scale_gt(tbl_stub_pct = 0.4)

## -----------------------------------------------------------------------------
mytbl <- gtcars |>
   dplyr::slice_head(n = 10) |>
   dplyr::select(mfr, model, year, msrp, ctry_origin) |>
   gt(
      groupname_col = "ctry_origin", 
      rowname_col = "mfr",
      row_group_as_column = TRUE) |>
   tab_style(
     style = cell_borders(c("left","right")),
     locations = list(cells_body(), cells_stub(), cells_column_labels())
   )

mytbl

## -----------------------------------------------------------------------------
mytbl |>
  scale_gt(tbl_stub_pct = 0.4)

## ----eval = FALSE-------------------------------------------------------------
# mytbl |>
#   gt::cols_width(
#     msrp ~ pct(50)
#   ) |>
#   as_docorator(tbl_scale = FALSE)

## ----eval = FALSE-------------------------------------------------------------
# mytbl |>
#   gt::cols_width(
#     ctry_origin ~ pct(15),
#     mfr ~ pct(10),
#     model ~ pct(7),
#     year ~ pct(9),
#     msrp ~ pct(15)
#   ) |>
#   as_docorator(tbl_scale = FALSE)

