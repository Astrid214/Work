
library(dplyr)
library(tidyr)

##FY15##
FY15_Budget <- read.table("C:/Users/astrid.atienza/Desktop/R/Data/FY15_Budget.txt", sep="\t", header=TRUE, strip.white=TRUE, stringsAsFactors=FALSE)
FY15_Budget <- rename(FY15_Budget, School_Name=School)


##FY16##
FY16_Budget <- read.csv("C:/Users/astrid.atienza/Desktop/R/Data/FY16_Budget.csv", strip.white=TRUE, stringsAsFactors=FALSE)
FY16_Budget <- rename(FY16_Budget, School_Name=School.Name)


##FY17##
library(readxl)
FY17_Budget <- read_excel("C:/Users/astrid.atienza/Desktop/R/Data/fy17_initial_allocations.xlsx", sheet="Initial Allocations (2.16.16)", skip=1)
FY17_Budget <- rename(FY17_Budget, School_Name=School Name)


##sample excel inputs: ?read_excel

library(dplyr)
library(tidyr)


##MERGE FY17 FY16 FY15

"Budget_Data" <-left_join(FY17_Budget,FY16_Budget, by="School_Code")

###Example 7###
ID <- c("1","2")
Race <- c("Black","White")
Gender <- c("Male","Female")
Score <- c(92,84)
data <- data.frame(ID, Race, Gender, Score)
data_long <- gather(data, "Column_Names","Value", 2:4)
data_wide <- spread(data_long, Column_Names, Value)



###### Task 2 ######
library(readxl)
library(dplyr)
library(tidyr)

##FY15##
FY15_Budget <- read.table("C:/Users/astrid.atienza/Desktop/R/Data/FY15_Budget.txt", sep="\t", header=TRUE, strip.white=TRUE, stringsAsFactors=FALSE)
FY16_Budget <- read.csv("C:/Users/astrid.atienza/Desktop/R/Data/FY16_Budget.csv", strip.white=TRUE, stringsAsFactors=FALSE)
#FY17_Budget <- read_excel("C:/Users/astrid.atienza/Desktop/R/Data/fy17_initial_allocations.xlsx", sheet="Initial Allocations (2.16.16)", skip=1)
FY17_Budget <- read_excel("C:/Users/astrid.atienza/Desktop/R/Data/fy17_initial_allocations.xlsx", sheet="Initial Allocations (2.16.16)",skip=1,col_names=FALSE)


FY15_Budget <-filter(FY15_Budget, Revenue.Type=="Total")
FY15_Budget <- select(FY15_Budget, School, FY15.Projected.Enrollment, Amount)
FY15_Budget <- rename(FY15_Budget, School_Name=School, FY15_Enrollment=FY15.Projected.Enrollment, FY15_Total_Budget=Amount)

FY16_Budget <- spread(FY16_Budget, Budget.Allocation.Category, Amount)
FY16_Budget <- select(FY16_Budget, School.Name, FY16.Budgeted.Enrollment, Total)
FY16_Budget <- rename(FY16_Budget, School_Name=School.Name,  FY16_Enrollment=FY16.Budgeted.Enrollment, FY16_Total_Budget=Total)

FY17_Budget <- rename(FY17_Budget, School_Code=X0, School_Name=X1, School_Type=X2, Ward=X3, FY17_Enrollment=X4 ,FY17_Total_Budget=X105)
FY17_Budget <- slice(FY17_Budget, -1)
FY17_Budget <- select(FY17_Budget, School_Code, School_Name, School_Type, Ward, FY17_Enrollment, FY17_Total_Budget)

#FY17_Budget <- select(SY17_Budget, School Code)
#FY17_Budget <- rename(FY17_Budget, School_Name=School.Name,  FY1_Enrollment=FY16.Budgeted.Enrollment, FY16_Total_Budget=Total)

Budget_Data <- left_join(FY17_Budget, FY16_Budget, by="School_Name")
Budget_Data <- left_join(Budget_Data, FY15_Budget, by="School_Name")


Budget_Data <- Budget_Data %>%
  mutate(Total_Budget_3yr=FY17_Total_Budget+FY16_Total_Budget+FY15_Total_Budget)

##### Mutating Example ####

library(readxl)
library(dplyr)
library(tidyr)

##FY15##
FY15_Budget <- read.table("C:/Users/astrid.atienza/Desktop/R/Data/FY15_Budget.txt", sep="\t", header=TRUE, strip.white=TRUE, stringsAsFactors=FALSE)
FY16_Budget <- read.csv("C:/Users/astrid.atienza/Desktop/R/Data/FY16_Budget.csv", strip.white=TRUE, stringsAsFactors=FALSE)
#FY17_Budget <- read_excel("C:/Users/astrid.atienza/Desktop/R/Data/fy17_initial_allocations.xlsx", sheet="Initial Allocations (2.16.16)", skip=1)
FY17_Budget <- read_excel("C:/Users/astrid.atienza/Desktop/R/Data/fy17_initial_allocations.xlsx", sheet="Initial Allocations (2.16.16)",skip=1,col_names=FALSE)


FY15_Budget <-filter(FY15_Budget, Revenue.Type=="Total")
FY15_Budget <- select(FY15_Budget, School, FY15.Projected.Enrollment, Amount)
FY15_Budget <- rename(FY15_Budget, School_Name=School, FY15_Enrollment=FY15.Projected.Enrollment, FY15_Total_Budget=Amount)

FY16_Budget <- spread(FY16_Budget, Budget.Allocation.Category, Amount)
FY16_Budget <- select(FY16_Budget, School.Name, FY16.Budgeted.Enrollment, Total)
FY16_Budget <- rename(FY16_Budget, School_Name=School.Name,  FY16_Enrollment=FY16.Budgeted.Enrollment, FY16_Total_Budget=Total)

FY17_Budget <- rename(FY17_Budget, School_Code=X0, School_Name=X1, School_Type=X2, Ward=X3, FY17_Enrollment=X4 ,FY17_Total_Budget=X105)
FY17_Budget <- slice(FY17_Budget, -1)
FY17_Budget <- select(FY17_Budget, School_Code, School_Name, School_Type, Ward, FY17_Enrollment, FY17_Total_Budget)


FY15_Budget <- FY15_Budget %>%
  mutate(School_Name=ifelse(School_Name=="Brightwood EC", "Brightwood Education Campus", School_Name),
         School_Name=ifelse(School_Name=="Capitol Hill Montessori", "Cap Hill Montessori @ Logan", School_Name),
         School_Name=ifelse(School_Name=="CHOICE Academy", "Choice Academy", School_Name),
         School_Name=ifelse(School_Name=="Jefferson MS", "Jefferson Middle School Academy", School_Name),
         School_Name=ifelse(School_Name=="Johnson, John Hayden MS","Johnson MS", School_Name),
         School_Name=ifelse(School_Name=="Luke Moore HS", "Luke Moore Alternative HS", School_Name),
         School_Name=ifelse(School_Name=="Malcolm X ES", "Malcolm X ES @ Green", School_Name),
         School_Name=ifelse(School_Name=="Oyster-Adams Bilingual School", "Oyster-Adams Bilingual", School_Name),
         School_Name=ifelse(School_Name=="River Terrace Special Education Center","River Terrace SEC", School_Name),
         School_Name=ifelse(School_Name=="School Without Walls @ Francis-Stevens EC", "School Without Walls @ Francis-Stevens", School_Name),
         School_Name=ifelse(School_Name=="School-Within-School", "School Without Walls HS", School_Name),
         School_Name=ifelse(School_Name=="School-Within-School", "School-Within-School @ Goding", School_Name))


FY16_Budget <- FY16_Budget %>%
  mutate(School_Name=ifelse(School_Name=="Brightwood EC", "Brightwood Education Campus", School_Name),
         School_Name=ifelse(School_Name=="Capitol Hill Montessori", "Cap Hill Montessori @ Logan", School_Name),
         School_Name=ifelse(School_Name=="CHOICE Academy", "Choice Academy", School_Name),
         School_Name=ifelse(School_Name=="Jefferson MS", "Jefferson Middle School Academy", School_Name),
         School_Name=ifelse(School_Name=="Langdon ES", "Langdon EC", School_Name),
         School_Name=ifelse(School_Name=="Luke Moore HS", "Luke Moore Alternative HS", School_Name),
         School_Name=ifelse(School_Name=="Malcolm X ES", "Malcolm X ES @ Green", School_Name),
         School_Name=ifelse(School_Name=="Noyes EC", "Noyes ES", School_Name),
         School_Name=ifelse(School_Name=="Oyster-Adams Bilingual School", "Oyster-Adams Bilingual", School_Name),
         School_Name=ifelse(School_Name=="River Terrace Special Education Center","River Terrace SEC", School_Name),
         School_Name=ifelse(School_Name=="School Without Walls @ Francis-Stevens EC", "School Without Walls @ Francis-Stevens", School_Name),
         School_Name=ifelse(School_Name=="School-Within-School", "School-Within-School @ Goding", School_Name))

Budget_Data <- left_join(FY17_Budget, FY16_Budget, by="School_Name")
Budget_Data <- left_join(Budget_Data, FY15_Budget, by="School_Name")


library(stringr)
#remove symbols from FY15#
Budget_Data <- Budget_Data %>%
  mutate(FY16_Total_Budget=str_replace_all(FY16_Total_Budget, "\\,", ""),
         FY16_Total_Budget=str_replace_all(FY16_Total_Budget, "\\$", ""))

#converting data types#
Budget_Data <- Budget_Data %>%
  mutate(FY16_Total_Budget=as.numeric(FY16_Total_Budget),
        FY17_Total_Budget=as.numeric(FY17_Total_Budget))

#create three year budget variable#
Budget_Data <- Budget_Data %>%
  mutate(Total_Budget_3yr=FY17_Total_Budget+FY16_Total_Budget+FY15_Total_Budget)

#convert enrollment to numeric#
Budget_Data <- Budget_Data %>%
  mutate(FY15_Enrollment=as.numeric(FY15_Enrollment),
        FY16_Enrollment=as.numeric(FY16_Enrollment),
         FY17_Enrollment=as.numeric(FY17_Enrollment))

Budget_Data <- Budget_Data %>%
  mutate(FY15_Per_Pupil=FY15_Total_Budget/FY15_Enrollment,
        FY16_Per_Pupil=FY16_Total_Budget/FY16_Enrollment,
        FY17_Per_Pupil=FY17_Total_Budget/FY17_Enrollment)


#Aggregating data

Summary_Data <- Budget_Data %>%
  group_by(Ward) %>%
  summarize(Ward_Average=max(FY17_Per_Pupil))

Window_Data <- Budget_Data %>%
  group_by(Ward) %>%
  mutate(Ward_Average=max(FY17_Per_Pupil))







