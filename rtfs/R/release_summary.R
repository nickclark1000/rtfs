#' Get Iteration Tree
#'
#' Get iteration tree for a particular TFS project.
#'
#' @return \code{api_get} class of iterations
#' @export
get_iteration_tree <- function() {
  if (tfs_collection == "" || tfs_project == "")
    stop("TFS Collection or Project is not defined", call. = FALSE)
  url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/_apis/wit/classificationNodes/iterations?$depth=3", sep = "")
  releases <- api_get(url)
  return(releases)
}

#' Get Current Release
#'
#' Computes the current release based on today's date.
#'
#' @return A list of current major (e.g., 3.0.0) and minor (e.g., 3.0.1) releases.
#' Current minor release will be \code{NULL} if there are no minor releases defined.
#' @export
get_current_release <- function() {
    releases <- get_iteration_tree()$content
    today <- format(Sys.Date())
    current_minor_release <- NULL

    for (i in 1:nrow(releases$children)) {
        child <- releases$children[i, ]
        if (!is.na(child$attributes$startDate) || !is.na(child$attributes$finishDate)) {
            cat("Release:", child$name, ", Start:", child$attributes$startDate, ", Finish:", child$attributes$finishDate,
                "\n")
            if (today > child$attributes$startDate && today < child$attributes$finishDate) {
                current_major_release <<- child
                target_release_date <<- child$attributes$finishDate
            }
        } else {
            cat("Release:", child$name, "is missing start or finish dates.\n")
        }
    }
    if (!exists("current_major_release"))
        stop("Today's date does not fall between any release start and end dates")

    cat("Current Major Release:", current_major_release$name, "\n")

    if (is.element("TRUE", current_major_release$children[[1]]$hasChildren)) {
        for (i in 1:nrow(current_major_release$children[[1]])) {
            grandchild <- current_major_release$children[[1]][i, ]
            if (grandchild$hasChildren) {
                if (!is.na(grandchild$attributes$startDate) || !is.na(grandchild$attributes$finishDate)) {
                  cat("Release:", grandchild$name, ", Start:", grandchild$attributes$startDate, ", Finish:", grandchild$attributes$finishDate,
                    "\n")
                  if (today > grandchild$attributes$startDate && today < grandchild$attributes$finishDate) {
                    current_minor_release <<- grandchild
                    target_release_date <<- grandchild$attributes$finishDate
                    cat("Current Minor Release:", current_minor_release$name, "\n")
                  }
                } else {
                  cat("Release:", grandchild$name, "is missing start or finish dates.\n")
                }
            }
        }
    } else {
        cat("No minor releases to define \n")

    }
    return(list(current_minor_release = current_minor_release, current_major_release = current_major_release))
}

#' Get Release Sprints
#'
#' Get a list of sprints in a particular release/iteration.
#'
#' @param release_name Name of the release/iteration as defined in TFS
#' @return A list of sprint iterations
#' @examples
#' get_release_sprints('Version 2.0')
#' @export
get_release_sprints <- function(release_name) {
    # Returns a list of sprints in the release.  Args: release: Release list.  Returns: sprints$children: List of child
    # sprints in the release.
    if (tfs_collection == "" || tfs_project == "")
        stop("TFS Collection or Project is not defined", call. = FALSE)
    url <- paste("/tfs/", tfs_collection, "/", tfs_project, "/_apis/wit/classificationNodes/iterations/", URLencode(release_name),
        "?$depth=1", sep = "")
    sprints <- api_get(url)
    return(sprints$children)

}

#' Get Release Work Item IDs
#'
#' Get a list of work item IDs for a particular release. Currently retrieves work items where:
#' \itemize{
#'    \item Work Item Type is Product Backlog Item, Bug or Work Order
#'    \item State is not equal to Removed or Closed
#' }
#' @param iteration_ids A comma-separated list of iteration IDs. Use \code{get_iteration_tree} to
#' acquire a list of iterations.
#' @param date The TFS API has the ability to fetch data as of a certain date. Passing a date object
#' will return a list of work item IDs
#' whose iteration path is under \code{iteration_ids} as of this date. Defaults to today's date.
#' @return \code{api_post} class of work item IDs
#' @examples
#' iteration_ids <- '50, 51, 52'
#' get_release_wi_ids(iteration_ids)
#' @export
get_release_wi_ids <- function(iteration_ids, date = format(Sys.Date())) {
    backlog_filter_string <- rtfs::get_team_backlog_filter_wiql()
    query <- paste("Select [System.Id] ",
                   "From WorkItems ",
                   "Where [System.WorkItemType] in ('Product Backlog Item', 'Bug', 'Work Order') ",
                      "AND [System.IterationId] in (", iteration_ids, ") ",
                      "AND (", backlog_filter_string, ") ",
                      "AND [System.State] <> 'Removed' ",
                      "ASOF '", date, "'",
                   sep = "")
    cat("Request Query:", query, "\n")
    url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/_apis/wit/wiql?api-version=1.0", sep = "")
    work_items <- api_post(url, query)
    return(work_items)
}

#' Get Release Work Items
#'
#' Get a list of work items for a particular team's release.
#'
#' @param wi_id_list A comma-separated list of work item IDs. Use \code{\link{get_release_wi_ids}} to acquire a list of work items.
#' @param date The TFS API has the ability to fetch data as of a certain date. Passing a date object will return a list of work items
#' as of this date. Defaults to today's date.
#' @return \code{api_post} class of work items
#' @examples
#' wi_id_list <- '50000, 51000, 52000'
#' get_release_wis(wi_id_list)
#' @export
get_release_wis <- function(wi_id_list, date = format(Sys.Date())) {
    remainder <- wi_id_list
    cat("Remainder length:", length(remainder), "\n")
    while (length(remainder) > 0) {
        first_200 <- head(remainder, 200)
        remainder <- remainder[-c(1:200)]
        cat("Remainder loop length:", length(remainder), "\n")

        id_list <- paste(as.character(first_200), collapse = ",")
        return_fields <- paste("System.Id",
                               "System.Title",
                               "System.WorkItemType",
                               "System.IterationPath",
                               "System.IterationId",
                               "System.State",
                               "System.CreatedDate",
                               "Microsoft.VSTS.Scheduling.Effort",
                               "Microsoft.VSTS.Common.Severity",
                               "TR.Elite.BugType",
                               "Microsoft.VSTS.Common.ClosedDate",
                               "System.AreaPath",
                               sep = ",")
        url <- paste("/tfs/", URLencode(tfs_collection), "/_apis/wit/workitems?ids=", id_list, "&asOf=", URLencode(as.character(date)), "&fields=", return_fields, "&api-version=1.0", sep = "")
        if (!exists("work_items")) {
            work_items <- api_get(url)$content$value$fields
        } else {
            work_items <- full_join(work_items, api_get(url)$content$value$fields)
        }
    }
    return(work_items)
}

#' Get Historical Backlog Size
#'
#' Get the total backlog size at different points in time.
#'
#' @param iteration_ids A comma-separated list of iteration IDs. Use \code{\link{get_iteration_tree}} to acquire a list of iterations.
#' @param dates A list of \code{date} objects representing the points in time to calculate the backlog size.
#' @return Dataframe with three columns: \code{TOTAL_RELEASE_POINTS}, \code{TOTAL_RELEASE_COUNT} and \code{AS_OF} (indicating the date)
#' @examples
#' iteration_ids <- '50, 51, 52'
#' dates <- list(Sys.Date(), Sys.Date() - 14, Sys.Date() - 28)
#' get_backlog_history(iteration_ids, dates)
#' @export
get_backlog_history <- function(iteration_ids, dates) {
    cat("Dates:", dates)
    backlog_history <- data.frame(TOTAL_RELEASE_POINTS = double(), AS_OF = character())
    for (i in 1:length(dates)) {
        work_item_ids <- get_release_wi_ids(iteration_ids, dates[i])$content
        if(length(work_item_ids$workItems) == 0){
          backlog_as_of <- data.frame(TOTAL_RELEASE_POINTS = 0,
                                      TOTAL_RELEASE_COUNT = 0,
                                      AS_OF = dates[i])
        } else {
          work_item_df <- get_release_wis(work_item_ids$workItems$id, dates[i])
          backlog_as_of <- data.frame(TOTAL_RELEASE_POINTS = sum(work_item_df$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE),
                                      TOTAL_RELEASE_COUNT = nrow(work_item_df),
                                      AS_OF = dates[i])
        }
        backlog_history <- bind_rows(backlog_history, backlog_as_of)
    }
    return(backlog_history)
}

#' Get Planned Velocity
#'
#' Get planned velocity for each sprint.
#'
#' @param iteration_id The iteration ID of the sprint whose planned velocity is desired.
#' @param date_time The date and time to calculate the planned velocity.
#' @return Dataframe with two columns: \code{PLANNED_VELOCITY} and \code{SPRINT_ITERATION_ID}
#' @examples
#' iteration_id <- '50'
#' date_time <- update(as_datetime('2016-09-01T00:00:00Z'), hour = 23, minute = 59)
#' get_planned_velocity(iteration_id, date_time)
#' @export
get_planned_velocity <- function(iteration_id, date_time) {
  work_item_ids <- get_release_wi_ids(iteration_id, date_time)$content
  if(length(work_item_ids$workItems)==0) {
    #Try 24hrs later
    work_item_ids <- get_release_wi_ids(iteration_id, lubridate::ymd_hms(date_time) + 86400)$content
  }
  cat("Work Item count:", length(work_item_ids$workItems), "\n")
  if(length(work_item_ids$workItems) == 0){
    planned_velocity <- data.frame(PLANNED_VELOCITY = 0,
                                   SPRINT_ITERATION_ID = iteration_id)
    return(planned_velocity)
  }
  work_item_df <- get_release_wis(work_item_ids$workItems$id, date_time)
  planned_velocity <- data.frame(PLANNED_VELOCITY = sum(work_item_df$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE),
                                 SPRINT_ITERATION_ID = iteration_id)
  return(planned_velocity)
}
