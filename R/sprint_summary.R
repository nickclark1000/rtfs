library(lubridate)

#' Get Sprint History
#'
#' Get the cumulative completed work item points on each day of a sprint.
#'
#' @param iteration_id An iteration ID. Use \code{\link{get_iteration_tree}} to acquire a list of iterations.
#' @param sprint_start_date First day of the sprint.
#' @param sprint_end_date Last day of the sprint.
#' @return Dataframe with five columns: \code{COMPLETED_POINTS}, \code{TOTAL_POINTS}, \code{COMPLETED_COUNT}, \code{TOTAL_COUNT} and \code{AS_OF} (indicating the date)
#' @examples
#' iteration_id <- '50'
#' sprint_start_date <- '2016-01-01'
#' sprint_end_date <- '2016-01-14'
#' get_sprint_history(iteration_id, sprint_start_date, sprint_end_date)
#' @export
get_sprint_history <- function(iteration_id, sprint_start_date, sprint_end_date){
  days <- seq(from = as.Date(sprint_start_date), to = as.Date(sprint_end_date), by = 'days')
  weekdays <- days[lubridate::wday(days) %in% c(2:6)]
  sprint_history <- data.frame(COMPLETED_POINTS = double())
  for (i in seq_along(weekdays)){
    #add 1 day for midnight calculation
    work_item_ids <- get_release_wi_ids(iteration_id, weekdays[i]+1)$content
    work_item_df <- get_release_wis(work_item_ids$workItems$id, weekdays[i]+1)
    done <- subset(work_item_df, System.State == 'Done' | System.State == 'Closed')
    done_as_of <- data.frame(COMPLETED_POINTS = sum(done$Microsoft.VSTS.Scheduling.Effort, na.rm = T),
                             TOTAL_POINTS = sum(work_item_df$Microsoft.VSTS.Scheduling.Effort, na.rm = T),
                             COMPLETED_COUNT = nrow(done),
                             TOTAL_COUNT = nrow(work_item_df),
                             AS_OF = weekdays[i])
    sprint_history <- bind_rows(sprint_history, done_as_of)
  }
  return(sprint_history)
}
