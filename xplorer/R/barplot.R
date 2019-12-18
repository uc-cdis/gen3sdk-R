#' Bar plot
#' @description Creates barplots for count or mean with SE that can be interactive or not.
#'
#' @param node A data frame (required)
#' @param Property A categorical column (required)
#' @param y A numerical column.
#' @param MEAN Calculated average of numerical column known as y. Specificy TRUE or FALSE
#' @param Interactive An interactive html plot. Specificy TRUE or FALSE
#'
#' @examples
#'
#'
#'
#' @examples
#'
#'
#' @return Barplots of Count, MEAN
#'
#' @importFrom dplyr select
#' @importFrom stats aggregate median reorder sd
#'
#' @export barplotr
barplotr <- function (node, Property, y_variable = NULL, MEAN = NULL, Interactive = NULL){
#Function takes node (required), property (required), y_variable(optional), MEAN (TRUE or FALSE), Interactive (TRUE or FALSE)

#custom_final28 and colors are used to determine colors of barplots. ggplot2::scale_fill_manual(values = custom_final28)
custom_final28 = c("blueviolet", "#77AADD", "#117777", "gold1", "darkseagreen4",
                     "#77CCCC" ,"#771155", "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA",
                     "#771122", "#AA4455", "plum4", "slateblue", "violetred","#5F7FC7",
                     "#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
                     "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711","#E69F00")

colors <- c("#91a3b0","#537b5d","#6d9c79","#344c3a","#a1caf1","#98777b","#a2a2d0","#66023c","#00755e",
              "#9966cc","#ffbf00","#cd9575","#e9d66b","#ff9966","#e2725b","#464EB4","#ecd540","#aa98a9",
              "#44AAAA", "#629c92", "#77AADD", "#117777", "#18402c", "#eb9886","#77CCCC" ,"#771155",
              "#AA4488", "#EA6CC0", "#CC99BB","#88CCAA", "#771122", "#AA4455", "#cf3d1d", "#f2e88a",
              "#014411","#b4464e","#973b42","#7b3036","#7AB446","#512023","#cc7c82","#5d8aa8","#f0f8ff",
              "#7d200b","#5F7FC7","#D2781E", "#DDAA77","#CBD588", "#CC99BB", "#114477", "#4477AA",
              "#1E78D2", "#77AADD", "#117777","#D21E2C","#DD7788","#777711")

#Take variable assigned to Property and put it in quotes using enquo(). Enquo() looks at the argument, see what the user typed, and return that value as a quosure.
#Notice that I put quosures around the argument Property and assigned it to Property_Name. Property_Name will be used to create a data table and plot.
Property_Name <- dplyr::enquo(Property)

#y_variable <- dplyr::enquo(y_variable)


# if (!is.null(y_variable)){
#     y_variable <- dplyr::enquo(y_variable)
# }

#Creates a dataframe that counts Property_Name that will be used in ggplot2::annotate to specify text location on the y axis. y= PropertyName_Table$Count
PropertyName_Table <- node %>%
    dplyr::group_by(!!Property_Name) %>%
    dplyr::summarise(Count = dplyr::n())

#Creates a dataframe that groups by Property_Name and ProjectID for usage in pcount below.
PropertyName_ProjectID_Table <-  node %>%
    dplyr::group_by(!!Property_Name, project_id) %>%
    dplyr::summarise(Count = dplyr::n()) %>%
    dplyr::arrange(Count) %>%
    data.frame()

#Creates a dataframe using the p from above to calculate the number of unique projectIDs within each response variable. Also specify text location on x axis. x=ProjectID_Count$Property_Name.
ProjectID_Count <- PropertyName_ProjectID_Table %>%
    dplyr::group_by(!!Property_Name) %>%
    dplyr::summarise(pCount = dplyr::n())
  colnames(ProjectID_Count)[1] <- "Property_Name" #Renames the first column to a generic name for use in ggplot2::annotate


  # 1. Plot Y in a box & whisker plot


  if (!is.null(y_variable) && isFALSE(MEAN) && isFALSE(Interactive)) {
    # print(node)
    # print(y)

    print("1. Plotted below is a boxplot to show distribution of Y")

    graph <- ggplot2::ggplot(node, ggplot2::aes(x = !!Property_Name, !!y_variable)) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = !!Property_Name)) +
      ggplot2::theme_bw() +
      ggplot2::geom_jitter(position=ggplot2::position_jitter(0.0), size = 2) +
      ggplot2::theme(legend.position = "bottom") +
      #labs( x = "Property Name") +
      ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
                     legend.text = ggplot2::element_text(color = "black", size = 5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_fill_manual(values = custom_final28)

    return(graph)
  }

  else if (!is.null(y_variable) && isFALSE(MEAN) && isTRUE(Interactive)) {

    print("2. Plotted below is a boxplot to show distribution of Y")

    graph <- plotly::ggplotly(ggplot2::ggplot(node, ggplot2::aes(x = !!Property_Name, !!y_variable)) +
                                ggplot2::geom_boxplot(ggplot2::aes(fill = !!Property_Name)) +
                                ggplot2::theme_bw() +
                                ggplot2::geom_jitter(position=ggplot2::position_jitter(0.0), size = 3) +
                                ggplot2::theme(legend.position = "bottom") +
                                ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
                                               legend.text = ggplot2::element_text(color = "black", size = 5)) +
                                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                                ggplot2::scale_fill_manual(values = custom_final28))

    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(graph), paste("Boxplot_", st, ".html", sep = ""))

    return(graph)
  }

  # 2. barplor - Plot Count
#This function plots the count of Property_Name. Colors specify the Project IDs. The number of Project IDs within a response variable of Property_Name is written at the top of corresponding bar.

else if(is.null(y_variable) && isFALSE(MEAN) && isFALSE(Interactive)){

   graph <- ggplot2::ggplot(node, aes(x = reorder(!!Property_Name,!!Property_Name,function(x)-length(x)),
                            fill = project_id)) +
            ggplot2::geom_bar() +
            theme_light() +
            ggplot2::annotate("text", x=ProjectID_Count$Property_Name, y= PropertyName_Table$Count, na.rm = TRUE,
                              label=paste("# Projects: ", ProjectID_Count$pCount), size = 3,vjust = -1) +
            labs(x = Property_Name, y = "Counts") +
            ggplot2::theme(text = element_text(family = "Times")) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
                           axis.text.y = ggplot2::element_text(size = 15)) +
            ggplot2::theme(axis.title.x = ggplot2::element_text(size=20, face = "bold", vjust = 5),
                           axis.title.y = ggplot2::element_text(size=15, vjust = 2)) +
            ggplot2::theme(legend.text = ggplot2::element_text(color = "black", size = 12)) +
            ggplot2::theme(legend.position = "bottom") +
            ggplot2::theme(legend.background = ggplot2::element_rect(linetype="longdash",colour ="darkgrey")) +
            ggplot2::guides(fill = guide_legend(title = "Project IDs", title.position = "top", title.hjust = .5,
                            title.theme = element_text(size = 15, face = "bold"))) +
            ggplot2::scale_fill_manual(values = colors)

    return(graph)

}

else if(is.null(y_variable) && isFALSE(MEAN) && isTRUE(Interactive)) {

   graph <- plotly::ggplotly(ggplot2::ggplot(node, aes(x = reorder(!!Property_Name,!!Property_Name,function(x)-length(x)),
                                                     fill = project_id)) +
            ggplot2::geom_bar(width = .8) +
            theme_light() +
            ggplot2::annotate("text", x=ProjectID_Count$Property_Name, y= PropertyName_Table$Count, na.rm = TRUE,
                              label=paste("# Projects: ", ProjectID_Count$pCount), size = 3,vjust = -1) +
            labs(x = Property_Name, y = "Counts") +
            ggplot2::theme(text = element_text(family = "Times")) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
                           axis.text.y = ggplot2::element_text(size = 15)) +
            ggplot2::theme(axis.title.x = ggplot2::element_text(size=20, face = "bold", vjust = 5),
                           axis.title.y = ggplot2::element_text(size=15, vjust = 2)) +
            ggplot2::theme(legend.text = ggplot2::element_text(color = "black", size = 12)) +
            ggplot2::theme(legend.position = "bottom") +
            ggplot2::theme(legend.background = ggplot2::element_rect(linetype="longdash",colour ="darkgrey")) +
            ggplot2::guides(fill = guide_legend(title = "Project IDs", title.position = "top", title.hjust = .5,
                            title.theme = element_text(size = 15, face = "bold"))) +
            ggplot2::scale_fill_manual(values = colors))

            st=format(Sys.time(), "%Y-%m-%d_%H:%M")
            htmlwidgets::saveWidget(as_widget(graph), paste("Count_BarGraph_", st, ".html", sep = ""))

    return (graph)
}


  # 3. barplot - Plot Mean of Y with standard error bars

  else if (!is.null(y_variable) && isTRUE(MEAN) && isFALSE(Interactive)) {

    graph <- ggplot2::ggplot(node, ggplot2::aes(reorder(x = project_id, (!!-y_variable)), !!y_variable)) +
      ggplot2::theme_bw() +
      ggplot2::stat_summary(geom = "bar", fun.y = mean, na.rm = TRUE, ggplot2::aes(fill = !!Property_Name)) +
      ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se) +
      ggplot2::scale_fill_manual(values = custom_final28) +
      ggplot2::theme(legend.position = "bottom") +
      labs(x = "Project ID") +
      ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
                     legend.text = ggplot2::element_text(color = "black", size = 5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::theme(legend.background = ggplot2::element_rect(linetype="longdash",colour ="darkgrey")) +
      ggplot2::guides(fill = guide_legend(title = Property_Name, title.position = "top", title.hjust = .5,
                                          title.theme = element_text(size = 15, face = "bold")))
    # ggplot2::theme(axis.title.x=element_blank(),t.x=element_blank(),
    #                axis.ticks.x=element_blank())

    return(graph)
  }


  else if (!is.null(y_variable) && isTRUE(MEAN) && isTRUE(Interactive)) {

    graph <- plotly::ggplotly(ggplot2::ggplot(node, ggplot2::aes(reorder(x = !!Property_Name, -(!!y_variable)), !!y_variable)) +
                                ggplot2::theme_bw() +
                                ggplot2::stat_summary(geom = "bar", fun.y = mean, ggplot2::aes(fill = !!Property_Name)) +
                                ggplot2::stat_summary(geom = "errorbar", fun.data = mean_se) +
                                ggplot2::scale_fill_manual(values = custom_final28) +
                                ggplot2::theme(legend.position = "bottom") +
                                ggplot2::theme(legend.title = ggplot2::element_text(color = "black", size = 10),
                                               legend.text = ggplot2::element_text(color = "black", size = 5)) +
                                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                                ggplot2::theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()))

    st=format(Sys.time(), "%Y-%m-%d_%H:%M")
    htmlwidgets::saveWidget(as_widget(graph), paste("Mean_BarGraph_", st, ".html", sep = ""))

    return(graph)
  }


else { stop("Specify MEAN = FALSE or TRUE; Interactive = TRUE or FALSE")}
}

