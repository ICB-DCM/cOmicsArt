### general utility functions will be defined here


update_data <- function(data, updates, current_updates){
  # update data if updates is larger than current_updates
  # could force to always update
  if (updates() > current_updates){
    print("Updating data...")
    data <- res_tmp
  }
  return(data)
}


update_params <- function(params, updates, current_updates){
  # update data if updates is larger than current_updates
  # could force to always update
  if (updates() > current_updates){
    print("Updating parameters...")
    params <- par_tmp
  }
  return(params)
}
