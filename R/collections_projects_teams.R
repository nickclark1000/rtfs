get_collections <- function() {

}

get_projects <- function(tfs_collection) {
    url <- paste("/tfs/", URLencode(tfs_collection), "/_apis/projects", sep = "")
    projects <- api_get(url)
    return(projects)
}

get_teams <- function(tfs_collection, tfs_project) {
    url <- paste("/tfs/", URLencode(tfs_collection), "/_apis/projects/", URLencode(tfs_project), "/teams", sep = "")
    teams <- api_get(url)
    return(teams)
}

get_default_area_path <- function() {
    url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/", URLencode(tfs_team), "/_apis/Work/TeamSettings/TeamFieldValues",
        sep = "")
    default_area_path <- api_get(url)$defaultValue
    return(default_area_path)
}
