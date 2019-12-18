#' Table
#' @description Tabulates count and/or statistics (STAT) of a Property and a numerical variable (y).
#'
#' @param node A dataframe
#' @param Property_Name A categorical column
#' @param ProjectID Specify TRUE or FALSE to include Project ID in count
#' @param UniqueProjectID Specify TRUE or FALSE to count number of Unique Project IDs - CURRENTLY DEPRACATED
#' @param y A numerical column
#' @param STAT Calculated average of numerical column known as y
#'
#' @examples
#' Calculate the statistics of numerical variable, y,  for Property, assay_kit_name, without ProjectID using a dataframe (aka node), QuantData.
#'
#' tabler(QuantData,
#' assay_kit_name,
#' STAT = TRUE,
#' ProjectID = FALSE,
#' y = molecular_concentration)
#'
#' @examples
#' Calculate the count of Property, assay_kit_name, with ProjectID = TRUE.
#'
#' tabler(QuantData,
#' assay_kit_name,
#' ProjectID = TRUE)
#'
#' @return Table as data.frame
#'
#' @export tabler
tabler <- function (node, Property, ProjectID = NULL,
                    y = NULL, STAT = NULL){

Property_Name <- dplyr::enquo(Property)
y <- dplyr::enquo(y)

    # 1. Table
#Makes a table of descriptive statistics of specified `y`.
#Tabulates the N, mean, min, median, max, and SD of variable y and Property_Name. Project_ID is either TRUE or FALSE

  if(!is.null(y) && isTRUE(STAT) && isFALSE(ProjectID)){
    TableMean <- node %>%
      select(!!Property_Name, !!y) %>%
      tidyr::drop_na(!!y) %>%
      dplyr::group_by(!!Property_Name) %>%
      dplyr::summarize(N=sum(!is.na(!!y)),
                       Mean=mean(!!y, na.rm=TRUE),
                       Min = min(!!y, na.rm = TRUE),
                       Median=median(!!y, na.rm = TRUE),
                       Max = max(!!y, na.rm = TRUE),
                       SD  = sd(!!y, na.rm = TRUE),
                       SE  = SD / sqrt(N)) %>%
      dplyr::arrange(desc(N)) %>%
      data.frame()

    return(TableMean)
  }

  else if(!is.null(y) && isTRUE(STAT) && isTRUE(ProjectID)){
    TableMean <- node %>%
      select(!!Property_Name, !!y, project_id) %>%
      tidyr::drop_na(!!y) %>%
      dplyr::group_by(!!Property_Name, project_id) %>%
      dplyr::summarize(N=sum(!is.na(!!y)),
                       Mean=mean(!!y, na.rm=TRUE),
                       Min = min(!!y, na.rm = TRUE),
                       Median=median(!!y, na.rm = TRUE),
                       Max = max(!!y, na.rm = TRUE),
                       SD  = sd(!!y, na.rm = TRUE),
                       SE  = SD / sqrt(N)) %>%
      dplyr::arrange(desc(N)) %>%
      data.frame()

    return(TableMean)
  }

  #2. Table
#Tabluates N (aka Count) of Property_Name. Project_ID is either TRUE or FALSE

  else if(isTRUE(ProjectID)){
    TableID <- node %>%
      dplyr::select(!!Property_Name, project_id) %>%
      dplyr::group_by(!!Property_Name, project_id) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::arrange(desc(Count)) %>%
      data.frame()
    N = length(unique(TableID$project_id))
    n = c("Number Unique Projects:", N)
    print(n, quote = FALSE)

    return(TableID)
  }

  else if (isFALSE(ProjectID)){
    Table <- node %>%
      dplyr::select(!!Property_Name) %>%
      dplyr::group_by(!!Property_Name) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::arrange(desc(Count)) %>%
      data.frame()

    return(Table)
  }

  else{
    print("Specify TRUE or FALSE for projectID. e.g. projectID = TRUE ")
  }
}
