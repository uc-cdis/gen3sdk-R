#' Pie plot and nested pie plot
#' @description Creates a pie plot (ProjectID = FALSE) or a nested pie plot (ProjectID = TRUE) to demonstrate proportion of property.
#'
#' @param node A dataframe
#' @param ... is a property or properties. Usually a categorical variable
#' @param ProjectID Specify TRUE or FALSE to include Project ID into pieplot
#'
#' @examples
#' Nested pie plot the proportion of Properties, blood_tube_type & tissue_type, when ProjectID = TRUE
#'
#' pieplotr(BiospecimenData,
#' blood_tube_type,tissue_type
#' ProjectID = TRUE)
#'
#' @examples
#' Common pie plot of the proportion of Property, disease_type, when ProjectID = FALSE. Note: multiple properties cannot be passed successfully when ProjectID = FALSE.
#'
#' pieplotr(BiospecimenData,
#' disease_type,
#' ProjectID = FALSE)
#'
#' @return Nested pie plot and common pie eplot
#'
#' @importFrom dplyr select
#' @importFrom stats aggregate median reorder sd
#'
#' @export pieplotr
pieplotr <- function (node, ..., ProjectID = NULL){

# Colors I wanted to be used. Can easily be replaced or removed.
colors <- c(  "#FFFFFF","#ffae42","#aa98a9", "#44AAAA", "#771155",
              "#ffa089","#1dacd6","#fddb6d","#6e5160", "#ffbf00", "#cd9575",
              "#ebc7df", "#91a3b0","#2b6cc4","#cc6666","#ceff1d","#5F7FC7",
              "#014411", "#ff43a4", "#6d9c79", "#77CCCC", "#117777",
              "#DDAA77", "#973b42", "#deaa88", "#e9d66b", "#98777b",
              "#a1caf1", "#114477", "#cd9575", "#512023", "#a2a2d0",
              "#EA6CC0", "#AA4488", "#777711", "#7d200b", "#344c3a",
              "#7AB446", "#00755e", "#D2781E", "#ff9966", "#771122",
              "#7b3036", "#e2725b", "#ffff66", "#1E78D2", "#b4464e",
              "#eb9886", "#CC99BB", "#CC99BB", "#DD7788", "#ecd540",
              "#464EB4", "#f2e88a", "#4477AA", "#77AADD", "#AA4455",
              "#66023c", "#cf3d1d", "#88CCAA", "#629c92", "#117777",
              "#a2add0", "#77AADD", "#9966cc", "#c5e384", "#537b5d",
              "#8f509d", "#5d8aa8", "#fc89ac", "#17806d", "#ffae42",
              "#cda4de", "#cc7c82", "#CBD588", "#18402c", "#f0f8ff",
              "#D21E2C")

# This allows users to add multiple Property_Names if necessary.
Property_Name <- dplyr::enquos(...)

# Data formatting step necessary for nested pieplot using function sunburstR()
PropertyName_ProjectID_Table <-  node %>% #Creates a dataframe that groups by Property_Name and ProjectID for usage in pcount below.
                                 dplyr::group_by(project_id,!!!Property_Name) %>%
                                 dplyr::summarise(Count = dplyr::n()) %>%
                                 dplyr::arrange(Count) %>%
                                 data.frame()

PropertyName_ProjectID_Table$project_id = gsub("-", "_", PropertyName_ProjectID_Table$project_id)

PropertyName_ProjectID_Table <- PropertyName_ProjectID_Table %>%
  dplyr::mutate(path = paste(project_id,!!!Property_Name, sep="-")) %>%
  dplyr::select(path, Count)

# 1. Nested pie plot with Property_Names and Project IDs

if(isTRUE(ProjectID)){

    sunny = sunburstR::sund2b(PropertyName_ProjectID_Table,
                            colors = colors)

    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(sunny), paste("PiePlot_", st, ".html", sep = ""))

    return(sunny)

}

# 1. Pie plot with Property_Names and NO project IDs

else if(isFALSE(ProjectID)){

  Pie <- rlang::eval_tidy(
    rlang::quo_squash(quo(
      plot_ly(node,
              labels = ~!!!Property_Name,
              type = "pie",
              sort = T,
              marker = list(line = list(color = '#FFFFFF',
                                        width = .5))
      ))))

  st=format(Sys.time(), "%Y-%m-%d_%H:%M")
  htmlwidgets::saveWidget(as_widget(Pie), paste("PiePlot_", st, ".html", sep = ""))

  return(Pie)
}

else {
  print("Review inputs")
   }
}
