## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(docorator)

## -----------------------------------------------------------------------------
# fancyhead(
#   fancyrow(left = "My Study", center = NA, right = NA)
# )

## -----------------------------------------------------------------------------
# fancyhead(
#   fancyrow(left = "My Study", center = NA, right = "Date as of today"),
#   fancyrow(left = "My Population", center = NA, right = NA),
#   fancyrow(left = NA, center = "My Table", right = NA)
# )

## -----------------------------------------------------------------------------
# fancyfoot(
#   fancyrow(left = "Date as of today", center = NA, right = NA)
# )

## -----------------------------------------------------------------------------
# fancyfoot(
#   fancyrow(left = "General footer for the table", center = NA, right = NA),
#   fancyrow(left = "Date as of today", center = NA, right = "path/to/docname.pdf")
# )

## -----------------------------------------------------------------------------
# fancyhead(
#   fancyrow(left = "My study", center = NA, right = doc_pagenum())
# )

## -----------------------------------------------------------------------------
# fancyfoot(
#   fancyrow(left = doc_path(), center = NA, right = doc_datetime())
# )

## -----------------------------------------------------------------------------
# gt::gtcars |>
#   dplyr::slice_head(n = 10) |>
#   dplyr::select(mfr, model, year, msrp) |>
#   gt::gt(
#     groupname_col = "mfr",
#     row_group_as_column = TRUE
#   ) |>
#   as_docorator(
#     display_name = "mytbl",
#     header = fancyhead(
#       fancyrow(left = "My Study", center = NA, right = doc_pagenum()),
#       fancyrow(left = "My Population", center = NA, right = NA),
#       fancyrow(left = NA, center = "My Table", right = NA)
#     ),
#     footer = fancyfoot(
#       fancyrow(
#         left = "path/mytbl.pdf", center = NA, right = "Data as of 2025-01-01"
#       )
#     )
#   ) |>
#   render_pdf()

## -----------------------------------------------------------------------------
knitr::include_graphics("img/doc_example.png")

