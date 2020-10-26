library("plyr")
library("ggplot2")
#install.packages(c("sunburstR","plotly", "doBy"))
library("plotly")
library("sunburstR")
library("tidyr")
library("htmlwidgets")
library("doBy")

AnalyteData  <- read.csv("../MasterDataSet/master_analyte.csv", sep = ",")
AliquotData <- read.csv("../MasterDataSet/master_aliquot.csv", sep = ",")
BiospecimenData <- read.csv("../MasterDataSet/master_biospecimen.csv", sep = ",")
SampleData <- read.csv("../MasterDataSet/master_sample.csv", sep = ",")
QuantData <- read.csv("../MasterDataSet/master_quantification_assay.csv", sep = ",")

########### To remove the blanks from disease_type
BiospecimenData <- BiospecimenData[as.character(BiospecimenData$disease_type)!= "" ,]
########## Rename blanks from disease_type to "Unknown"
BiospecimenData$disease_type[BiospecimenData$disease_type == ""] <- "Unknown"

##Pieplotr()

#plot Property_Name(s) and ProjectID
xplorer::pieplotr(BiospecimenData,
         disease_type,
         ProjectID = TRUE)

#plot Property_Name only
xplorer::pieplotr(BiospecimenData,
                  disease_type,
                  ProjectID = FALSE)

##Barplotr()

attach(QuantData)

# 1. Plot Y in a boxplot
xplorer::barplotr(QuantData,
         assay_kit_name,
         y_variable =  molecular_concentration,
         MEAN = FALSE,
         Interactive = FALSE)

# 2. Plot Count -- Currently running the wrong function (plot# 1)
xplorer::barplotr(QuantData,
                  Property = assay_kit_name,
                  Interactive = FALSE,
                  MEAN = FALSE)

# 3. Plot Mean of Y
xplorer::barplotr(QuantData,
         assay_kit_name,
         y_variable =  molecular_concentration,
         MEAN = TRUE,
         Interactive = FALSE)

# Table

# 1. Tabulates descriptive statistics

xplorer::tabler(QuantData,
       assay_kit_name,
       STAT = TRUE,
       ProjectID = TRUE,
       y = molecular_concentration)

# 2. Tabluates N (aka Count) of Property_Name.
xplorer::tabler(QuantData,
                assay_kit_name,
                ProjectID = FALSE)

AnalyteData %>%
  group_by(days_to_assay, project_id) %>%
  summarize(N = sum(!is.na(days_to_assay)))
