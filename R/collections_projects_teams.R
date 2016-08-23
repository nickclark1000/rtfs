get_collections <- function() {

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

#' Get Default Area Path
#'
#' Get TFS Team's default area path.
#'
#' @return Default area path string
#' @examples
#' get_default_area_path()
#' @export
get_default_area_path <- function() {
    url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/", URLencode(tfs_team), "/_apis/Work/TeamSettings/TeamFieldValues",
        sep = "")
    default_area_path <- api_get(url)$content$defaultValue
    return(default_area_path)
}
