#### BUDGETS- PUBLIC FILES ####

library(dplyr)
library(tidyr)
library(readxl)

##FY15##
FY15_Budget <- read.table("C:/Users/astrid.atienza/Desktop/R/Data/FY15_Budget.txt", sep="\t", header=TRUE, strip.white=TRUE, stringsAsFactors=FALSE)
FY15_Budget <- rename(FY15_Budget, School_Name=School)

##FY16##
FY16_Budget <- read.csv("C:/Users/astrid.atienza/Desktop/R/Data/FY16_Budget.csv", strip.white=TRUE, stringsAsFactors=FALSE)
FY16_Budget <- rename(FY16_Budget, School_Name=School.Name)

##FY17##
FY17_Budget <- read_excel("C:/Users/astrid.atienza/Desktop/R/Data/fy17_initial_allocations.xlsx", sheet="Initial Allocations (2.16.16)", skip=1)
FY17_Budget <- rename(FY17_Budget, School_Name=School Name)

##sample excel inputs: ?read_excel

###Example 7###
ID <- c("1","2")
Race <- c("Black","White")
Gender <- c("Male","Female")
Score <- c(92,84)
data <- data.frame(ID, Race, Gender, Score)
data_long <- gather(data, "Column_Names","Value", 2:4)
data_wide <- spread(data_long, Column_Names, Value)


##MERGE FY17 FY16 FY15##

##PREP FILE FOR MERGE##
FY15_Budget <- read.table("C:/Users/astrid.atienza/Desktop/R/Data/FY15_Budget.txt", sep="\t", header=TRUE, strip.white=TRUE, stringsAsFactors=FALSE)
FY16_Budget <- read.csv("C:/Users/astrid.atienza/Desktop/R/Data/FY16_Budget.csv", strip.white=TRUE, stringsAsFactors=FALSE)
FY17_Budget <- read_excel("C:/Users/astrid.atienza/Desktop/R/Data/fy17_initial_allocations.xlsx", sheet="Initial Allocations (2.16.16)", skip=1)

FY15_Budget <-filter(FY15_Budget, Revenue.Type=="Total")
FY15_Budget <- select(FY15_Budget, School, FY15.Projected.Enrollment, Amount)
FY15_Budget <- rename(FY15_Budget, School_Name=School,  FY15_Enrollment=FY15.Projected.Enrollment, FY15_Total_Budget=Amount)

FY16_Budget <- spread(FY16_Budget, Budget.Allocation.Category, Amount)
FY16_Budget <- select(FY16_Budget, School.Name, FY16.Budgeted.Enrollment, Total)
FY16_Budget <- rename(FY16_Budget, School_Name=School.Name,  FY16_Enrollment=FY16.Budgeted.Enrollment, FY16_Total_Budget=Total)

FY17_Budget <- select(SY17_Budget, School.Code)
FY17_Budget <- rename(FY17_Budget, School_Name=School.Name,  FY16_Enrollment=FY16.Budgeted.Enrollment, FY16_Total_Budget=Total)


"Budget_Data" <-left_join(FY17_Budget,FY16_Budget, by="School_Code")


