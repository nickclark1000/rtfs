get_current_release <- function() {
    # Computes the current release based on today's date.  Args: None Returns: current_release: The current release as a list.
    if (tfs_collection == "" || tfs_project == "")
        stop("TFS Collection or Project is not defined", call. = FALSE)
    url <- paste("/tfs/", tfs_collection, "/", tfs_project, "/_apis/wit/classificationNodes/iterations?$depth=3", sep = "")
    releases <- api_get(url)
    today <- format(Sys.Date())

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
                    target.release.date <<- grandchild$attributes$finishDate
                    cat("Current Minor Release:", current_minor_release$name, "\n")
                  }
                } else {
                  cat("Release:", grandchild$name, "is missing start or finish dates.\n")
                }
            }
        }
    } else {
        cat("No minor releases to define \n")
        current_minor_release <- NULL
    }
    return(list(current_minor_release = current_minor_release, current_major_release = current_major_release))
}


get_release_sprints <- function(release) {
    # Returns a list of sprints in the release.  Args: release: Release list.  Returns: sprints$children: List of child
    # sprints in the release.
    if (tfs_collection == "" || tfs_project == "")
        stop("TFS Collection or Project is not defined", call. = FALSE)
    url <- paste("/tfs/", tfs_collection, "/", tfs_project, "/_apis/wit/classificationNodes/iterations/", URLencode(release$name),
        "?$depth=1", sep = "")
    sprints <- api_get(url)
    return(sprints$children)
}


get_release_wi_ids <- function(iteration_ids, date = format(Sys.Date())) {
    # Returns list of work item IDs.
    query <- paste("Select [System.Id] \n                 From WorkItems \n                 Where [System.WorkItemType] in ('Product Backlog Item', 'Bug', 'Work Order') \n                 AND [System.IterationId] in (",
        iteration_ids, ")\n                 AND [System.AreaPath] under '", team.default.area.path, "'\n                 AND [System.State] <> 'Removed'\n                 ASOF '",
        date, "'", sep = "")
    cat("Request Query:", query, "\n")
    url <- paste("/tfs/", tfs_collection, "/", tfs_project, "/_apis/wit/wiql?api-version=1.0", sep = "")
    work_items <- api_post(url, query)
    return(work_items)
}

get_release_wis <- function(work.item.id.list, date = format(Sys.Date())) {
    remainder <- work.item.id.list

    while (length(remainder) > 0) {
        w <- head(remainder, 200)
        remainder <- remainder[-c(1:200)]

        list <- paste(as.character(w), collapse = ",")
        return.fields <- "System.Id, \n                    System.Title, \n                    System.WorkItemType, \n                    System.IterationPath, \n                    System.IterationId, \n                    System.State,\n                    System.CreatedDate,\n                    Microsoft.VSTS.Scheduling.Effort,\n                    Microsoft.VSTS.Common.Severity,\n                    TR.Elite.BugType,\n                    Microsoft.VSTS.Common.ClosedDate,\n                    System.AreaPath"
        url <- paste("/tfs/", tfs_collection, "/_apis/wit/workitems?ids=", list, "&asOf=", date, "&fields=", gsub("[\n ]",
            "", return.fields), "&api-version=1.0", sep = "")
        if (!exists("work_items")) {
            work_items <- api_get(url)$value$fields
        } else {
            work_items <- full_join(work_items, api_get(url)$value$fields)
        }

    }
    return(work_items)
}

get_backlog_history <- function(iteration_ids, dates) {
    cat("Dates:", dates)
    backlog_history <- data.frame(TOTAL_RELEASE_POINTS = double(), AS_OF = character())
    for (i in 1:length(dates)) {
        work_item_ids <- get_release_wi_ids(iteration_ids, dates[i])
        work_item_df <- get_release_wis(work_item_ids$workItems$id, dates[i])
        backlog_as_of <- data.frame(TOTAL_RELEASE_POINTS = sum(work.item.df$Microsoft.VSTS.Scheduling.Effort, na.rm = TRUE),
            AS_OF = dates[i])
        backlog_history <- bind_rows(backlog_history, backlog_as_of)
    }
    return(backlog_history)
}
