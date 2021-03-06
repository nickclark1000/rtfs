get_release_feature_ids <- function(iteration_id, backlog_filter_string, date = format(Sys.Date())) {
  query <- paste("Select [System.Id] ",
                 "From WorkItems ",
                 "Where [System.WorkItemType] = 'Feature' ",
                 "AND [System.IterationId] = '", iteration_id, "' ",
                 "AND (", backlog_filter_string, ") ",
                 "AND [System.State] <> 'Removed' ",
                 "ASOF '", date, "'",
                 sep = "")
  cat("get_release_feature_ids Request Query:", query, "\n \n")
  url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/_apis/wit/wiql?api-version=1.0", sep = "")
  features <- api_post(url, query)
  return(features$content$workItems$id)
}

get_feature_children <- function(feature_id, date) {
  # Returns list of work item IDs.
  query <- paste("Select [System.Id], [System.Links.LinkType]",
                 "From WorkItemLinks ",
                 "Where [Source].[System.Id] = '", feature_id, "' ",
                 "AND [Target].[System.State] <> 'Removed' ",
                 "AND [System.Links.LinkType] = 'System.LinkTypes.Hierarchy-Forward' ",
                 "AND [Target].[System.WorkItemType] <> 'Feature' ",
                 "ASOF '", date, "' ",
                 "mode(MustContain)",
                 sep = "")
  cat("get_feature_children Request Query:", query, "\n \n")
  url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/_apis/wit/wiql?api-version=1.0", sep = "")
  features <- api_post(url, query)
  ###drop the first result, as that is the feature id for some reason
  children <- features$content$workItemRelations$target$id
  cat("children linked work item list:", length(children), "\n \n")
  if(length(children) > 0) {
    children_list <- paste(as.character(children), collapse=",")
  } else {
    children_list <- NULL
  }
  return(children_list)
}

get_feature_completion <- function(feature_id, date) {
  link_list <- get_feature_children(feature_id, date)
  if(is.null(link_list)) {
    return(NULL)
  } else {
    cat("linked work item list:", is.null(link_list), "\n \n")
    link_df <- get_release_wis(link_list, date)
    feature_df <- subset(link_df, System.WorkItemType == 'Feature')
    children_df <- subset(link_df, System.WorkItemType != 'Feature')
    completed <- sum(subset(children_df, System.State == 'Done')$Microsoft.VSTS.Scheduling.Effort)
    total <- sum(children_df$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE)
    percent_complete <- round(completed/total*100)
    feature <- data.frame(ID = feature_df$System.Id,
                          TITLE = feature_df$System.Title,
                          PERCENT_COMPLETE = percent_complete,
                          TOTAL_POINTS = total,
                          COMPLETED_POINTS = completed,
                          AS_OF = date)
    return(feature)
  }
}

#' Get Release Feature Completion
#'
#' Get completion percentages for each feature over the course of a release.
#' Percentage is based on sum of child work items completed effort divided by total effort.
#'
#' @return Dataframe with 6 columns: \code{ID}, \code{TITLE}, \code{PERCENT_COMPLETE}, \code{TOTAL_POINTS}, \code{COMPLETED_POINTS} and \code{AS_OF}
#' @export
get_release_feature_completion <- function(release_iteration_id, dates) {
  backlog_filter_string <- rtfs::get_team_backlog_filter_wiql()
  features <- data.frame(ID = double(),
                        TITLE = character(),
                        PERCENT_COMPLETE = double(),
                        TOTAL_POINTS = double(),
                        COMPLETED_POINTS = double(),
                        AS_OF = character())
  feature_ids <- get_release_feature_ids(release_iteration_id, backlog_filter_string)
  if(!is.null(feature_ids)){
    for(j in 1:length(dates)){
      for(i in 1:length(feature_ids)){
        feature <- get_feature_completion(feature_ids[i], dates[j])
        features <- bind_rows(features, feature)
      }
    }
  }
  latest_titles <- subset(features, AS_OF == max(AS_OF))[1:2]
  for(i in 1:nrow(latest_titles)) {
    for(j in 1:nrow(features)) {
      if(latest_titles$ID[i] == features$ID[j]) {
         features$TITLE[j] <- latest_titles$TITLE[i]
      }
    }
  }
  return(features)
}
