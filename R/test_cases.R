#' Get Test Case IDs
#'
#' Get a list of test case IDs for a particular product.
#'
#' @param date The TFS API has the ability to fetch data as of a certain date. Passing a date object will return a
#' list of test case IDs whose area path is under \code{get_default_area_path} as of this date. Defaults to today's date.
#' @return \code{api_post} class of test case IDs
#' @examples
#' get_test_case_ids()
#' @export
get_test_case_ids <- function(date = format(Sys.Date())) {
  ##default_area_path <- rtfs::get_default_area_path()$content$defaultValue
  team_area_paths <- rtfs::get_team_area_paths()
  area_path_string <- ''
  for(i in 1:length(team_area_paths)){
    if(i==1){
      ###This could be problematic if the top-most area path is selected for the team in a multi-team project
      area_path <- paste("[System.AreaPath] under '", team_area_paths[1], "'", sep = "")
    } else {
      area_path <- paste("OR [System.AreaPath] under '", team_area_paths[i], "'", sep = "")
    }
    area_path_string <- paste(area_path_string, area_path)
  }
  # Returns list of work item IDs.
  query <- paste("Select [System.Id] ",
                 "From WorkItems ",
                 "Where [System.WorkItemType] = 'Test Case' ",
                 "AND (", area_path_string, ") ",
                 "AND [System.State] <> 'Closed' ",
                 "ASOF '", date, "'",
                 sep = "")
  cat("Request Query:", query, "\n")
  url <- paste("/tfs/", URLencode(tfs_collection), "/", URLencode(tfs_project), "/_apis/wit/wiql?api-version=1.0", sep = "")
  test_cases <- api_post(url, query)
  return(test_cases)
}

#' Get Test Cases
#'
#' Get a list of test cases for a particular product. Currently uses the team's default area path.
#'
#' @param test_case_id_list A comma-separated list of test case IDs. Use \code{\link{get_test_case_ids}} to acquire a list of test cases.
#' @param date The TFS API has the ability to fetch data as of a certain date. Passing a date object will return a list of work items
#' as of this date. Defaults to today's date.
#' @return \code{api_post} class of test cases
#' @examples
#' test_case_id_list <- '50000, 51000, 52000'
#' get_test_cases(wi_id_list)
#' @export
get_test_cases <- function(test_case_id_list, date = format(Sys.Date())) {
  remainder <- test_case_id_list

  while (length(remainder) > 0) {
    first_200 <- head(remainder, 200)
    remainder <- remainder[-c(1:200)]

    id_list <- paste(as.character(first_200), collapse = ",")
    return_fields <- paste("System.Id",
                           "System.Title",
                           "System.IterationPath",
                           "System.AreaPath",
                           "System.IterationId",
                           "System.State",
                           "System.CreatedDate",
                           "Microsoft.VSTS.TCM.AutomationStatus",
                           "Microsoft.VSTS.Common.Priority",
                           sep = ",")
    url <- paste("/tfs/", tfs_collection, "/_apis/wit/workitems?ids=", id_list, "&asOf=", date, "&fields=", return_fields, "&api-version=1.0", sep = "")
    if (!exists("work_items")) {
      work_items <- api_get(url)$content$value$fields
    } else {
      work_items <- full_join(work_items, api_get(url)$content$value$fields)
    }

  }
  return(work_items)
}

#' Get Test Case Automation History
#'
#' Gets the total number of test cases and the number of test cases that are automated on different dates.
#' Only considers test cases under the team's default area path.
#'
#' @param dates A list of \code{date} objects representing the points in time to calculate the backlog size.
#' @return Dataframe with three columns: \code{TEST_CASES_TOTAL}, \code{TEST_CASES_AUTOMATED} and \code{AS_OF} (indicating the date)
#' @examples
#' dates <- list(Sys.Date(), Sys.Date() - 14, Sys.Date() - 28)
#' get_tc_automation_history(dates)
#' @export
get_tc_automation_history <- function(dates) {
  tc_history <- data.frame(TEST_CASES_TOTAL = double(), TEST_CASES_AUTOMATED = double(), AS_OF = character())
  for (i in 1:length(dates)) {
    test_case_ids <- get_test_case_ids(dates[i])$content
    test_case_df <- get_test_cases(test_case_ids$workItems$id, dates[i])
    tc_as_of <- data.frame(TEST_CASES_TOTAL = nrow(test_case_df),
                          TEST_CASES_AUTOMATED = nrow(subset(test_case_df, Microsoft.VSTS.TCM.AutomationStatus == 'Automated')),
                          AS_OF = dates[[i]])
    tc_history <- bind_rows(tc_history, tc_as_of)
  }
  return(tc_history)
}
