# FUNCTIONS USED IN THE APP AND AS STANDALONE TOOLS

# Function to process time series with error handling
process_timeseries <- function(ts_data) {
  tryCatch({
    if (!is.numeric(ts_data)) {
      stop("Input must be numeric time series data")
    }

    myseries <- ts_data

    # Seasonal adjustment with X13 or Tramo-Seats
    spec_ts <- RJDemetra::tramoseats_spec(spec = "RSAfull")
    model_sa_ts <- RJDemetra::tramoseats(myseries, spec = spec_ts)

    # Creation of Workspace to save the model
    wk <- RJDemetra::new_workspace()
    mp <- RJDemetra::new_multiprocessing(wk, "sap1")
    RJDemetra::add_sa_item(wk, "sap1", model_sa_ts, "TramoSeats")
    RJDemetra::compute(wk) # It's important to compute the workspace before retrieving the SA model

    # Retrieving summary for computed models
    sa_item1 <- get_object(mp, 1)
    RJDemetra::get_model(sa_item1, wk) # To extract the model of the sa_item1: its the object sa_x13

    # To get all models from the multiprocessing mp:
    RJDemetra::get_model(mp, wk)

    components <- list(
      y_values = model_sa_ts$final$series[, "y"],
      sa_values = model_sa_ts$final$series[, "sa"],
      t_values = model_sa_ts$final$series[, "t"],
      s_values = model_sa_ts$final$series[, "s"],
      i_values = model_sa_ts$final$series[, "i"]
    )

    # Remove any NULL or NA components
    components <- components[!sapply(components, function(x) is.null(x) || all(is.na(x)))]
    return(components)

  }, error = function(e) {
    warning(paste("Error processing time series:", e$message))
    return(NULL)
  })
}
