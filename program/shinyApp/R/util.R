### general utility functions will be defined here


update_data <- function(data, updates, current_updates){
  # update data if updates is larger than current_updates
  if (updates() > current_updates & current_updates > 0){
    print("Updating data...")
    data <- res_tmp
  }
  return(data)
}