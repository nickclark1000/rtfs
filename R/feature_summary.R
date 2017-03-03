
#' Get Release Feature IDs
#'
#' Get completion percentages for each feature marked with a particular iteration path / release.
#' Percentage is based on sum of child work items completed effort divided by total effort.
#'
#' @return Dataframe with 3 columns: \code{ID}, \code{FEATURE TITLE} and \code{PERCENT COMPLETE}
#' @export
get_release_feature_ids <- function(iteration_id) {
  # Returns list of work item IDs.
  area_path_string <- get_team_area_paths_string()
  query <- paste("Select [System.Id] ",
                 "From WorkItems ",
                 "Where [System.WorkItemType] = 'Feature' ",
                 "AND [System.IterationId] = '", iteration_id, "' ",
                 "AND (", area_path_string, ") ",
                 "AND [System.State] <> 'Removed' ",
                 sep = "")
  cat("Request Query:", query, "\n")
  url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/_apis/wit/wiql?api-version=1.0", sep = "")
  features <- api_post(url, query)
  return(features$content$workItems$id)
}

get_feature_children <- function(feature_id) {
  # Returns list of work item IDs.
  query <- paste("Select [System.Id], [System.Links.LinkType]",
                 "From WorkItemLinks ",
                 "Where [Source].[System.Id] = '", feature_id, "' ",
                 "AND [Target].[System.State] <> 'Removed' ",
                 "AND [System.Links.LinkType] = 'System.LinkTypes.Hierarchy-Forward' ",
                 "AND [Target].[System.WorkItemType] <> 'Feature' ",
                 "mode(MustContain)",
                 sep = "")
  cat("Request Query:", query, "\n")
  url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/_apis/wit/wiql?api-version=1.0", sep = "")
  features <- api_post(url, query)
  ###drop the first result, as that is the feature id for some reason
  children <- features$content$workItemRelations$target$id[-1]
  children_list <- paste(as.character(children), collapse=",")
  return(children_list)
}

get_feature_completion <- function(feature_id) {
  children_list <- get_feature_children(feature_id)
  children_df <- get_release_wis(children_list)
  completed <- sum(subset(children_df, System.State == 'Done')$Microsoft.VSTS.Scheduling.Effort)
  total <- sum(children_df$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)
  percent_complete <- round(completed/total*100)
  return(percent_complete)
}

get_release_feature_completion <- function(release_iteration_id) {
  feature_ids <- get_release_feature_ids(release_iteration_id)
  get_feature_completion(feature_ids[1])

}

