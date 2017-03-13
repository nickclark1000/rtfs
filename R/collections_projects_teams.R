get_collections <- function() {
#http://elite-tfsapp.elitecorp.com:8080/tfs/_apis/projectcollections/
}

#' Get TFS Projects
#'
#' Get a list of projects in a TFS Collection.
#'
#' @param tfs_collection TFS Collection
#' @return \code{api_get} class of TFS Projects
#' @examples
#' get_projects('FinancialReportingCollection')
#' @export
get_projects <- function(tfs_collection) {
    url <- paste("/tfs/", URLencode(tfs_collection), "/_apis/projects", sep = "")
    projects <- api_get(url)
    return(projects)
}

#' Get TFS Project teams
#'
#' Get a list of teams in a TFS Project.
#'
#' @param tfs_collection TFS Collection
#' @param tfs_project TFS Project that is in \code{tfs_collection}. Use \code{\link{get_projects}} for a list of projects in a collection.
#' @return \code{api_get} class of TFS Teams
#' @examples
#' get_teams('FinancialReportingCollection', 'FinancialReportingProjects')
#' @export
get_teams <- function(tfs_collection, tfs_project) {
    url <- paste("/tfs/", URLencode(tfs_collection), "/_apis/projects/", URLencode(tfs_project), "/teams", sep = "")
    teams <- api_get(url)
    return(teams)
}

#' Get Team Backlog Filter
#'
#' Gets a TFS Team's backlog filter type (TR.Elite.Team or System.AreaPath) and values.
#'
#' @return Response from call to TFS TeamFieldValues.
#' @examples
#' get_team_backlog_filter()
#' @export
get_team_backlog_filter <- function() {
  url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/", URLencode(tfs_team), "/_apis/Work/TeamSettings/TeamFieldValues",
               sep = "")
  response <- api_get(url)$content
  return(response)
}

#' Get Team Backlog Filter WIQL
#'
#' Gets the WIQL of the team's backlog filter.
#'
#' @return WIQL string to be included in WIQL calls to REST API.
#' @examples
#' get_team_backlog_filter_wiql()
#' @export
get_team_backlog_filter_wiql <- function() {
  query_string <- ''
  backlog_filter <- rtfs::get_team_backlog_filter()
  filter_values <- backlog_filter$values$value
  if(backlog_filter$field$referenceName == 'TR.Elite.Team'){
    for(i in 1:length(filter_values)){
      if(i==1){
        team_field <- paste("[TR.Elite.Team] = '", filter_values[1], "'", sep = "")
      } else {
        team_field <- paste("OR [TR.Elite.Team] = '", filter_values[i], "'", sep = "")
      }
      query_string <- paste(query_string, team_field)
    }
  } else {
    for(i in 1:length(filter_values)){
      if(i==1){
        ###This could be problematic if the top-most area path is selected for the team in a multi-team project
        area_path <- paste("[System.AreaPath] under '", filter_values[1], "'", sep = "")
      } else {
        area_path <- paste("OR [System.AreaPath] under '", filter_values[i], "'", sep = "")
      }
      query_string <- paste(query_string, area_path)
    }
  }
  return(query_string)
}
