# A results table with the look of the Differential tables: toolbar
# with column picker, page length, copy, CSV and Excel; a filter row on top.
# `filename` names the CSV/Excel export, without extension
# (download_name(<module>, NULL, <item>)).
# Render it with DT::renderDataTable(..., server = FALSE) so the exports have
# every row, not only the rows of the current page.
# `options` are added to the DT options (e.g. order, columnDefs), `...` go to
# DT::datatable() (e.g. rownames, colnames, escape).
results_datatable <- function(data, filename, options = list(), rownames = FALSE, ...){
  DT::datatable(
    data,
    extensions = "Buttons",
    filter = "top",
    rownames = rownames,
    class = "cell-border compact stripe hover order-column",
    options = c(list(
      paging = TRUE,
      searching = TRUE,
      ordering = TRUE,
      autoWidth = TRUE,
      dom = "Bfrtip",
      lengthMenu = c(10, 25, 50, 100, -1),
      buttons = list(
        list(extend = "colvis", text = "Show/Hide Columns"),
        "pageLength",
        "copy",
        list(extend = "csv", filename = filename),
        list(extend = "excel", filename = filename)
      )
    ), options),
    ...
  )
}
