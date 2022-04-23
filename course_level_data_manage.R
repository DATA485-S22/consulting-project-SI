# Data Management File
library(tidyverse)
library(Hmisc) # Enables %nin% notation for "not in"
library(readxl) # read Excel file



grades <- read_xlsx("data/Student Grade.xlsx")
names(grades) <- make.names(names(grades), unique=TRUE)
write.csv(grades, 'data/grades.csv', row.names = FALSE)

grades <- read.csv("data/grades.csv")

program <- read.csv("data/Student Program.csv")
program.clean <- filter(program, Term.Year >= 2016) %>% # SI classes start in 2016
  mutate(Term.Type = factor(Term.Type), .keep = "unused") %>%
  filter(Term.Type %nin% c("Summer", "Winter")) %>%
  select(Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
         IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
         Academic.Program, Major.1.STEM.Flag, Major.1.College,
         Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
         Academic.Standing.Status)
program.clean$Random.Student.ID<-factor(program.clean$Random.Student.ID)
# Student Profile Dataset
profile <- read.csv("data/Student Profile Metric.csv")
profile <- filter(profile, Cohort.Term.Year >= 2016) %>%
  mutate(Random.Student.ID = factor(Random.Student.ID), .keep = "unused") %>%
  select(Cohort.Term, Random.Student.ID, Degree.Term,
         Full.Time.Part.Time.Code, Cohort.Student.Enrollment.Type, HS.GPA.Group,
         HS.GPA, Transfer.GPA.Group, Transfer.GPA, Cohort.Time.to.Degree.Year,
         Student.Orientation.Flag)
profile$Random.Student.ID<-factor(profile$Random.Student.ID)
student_profiles <- left_join(program.clean, profile, by = "Random.Student.ID")
student_profiles <- rename(student_profiles, Enrollment.Term = Cohort.Term)

grades$Random.Student.ID<- factor(grades$Random.Student.ID)
student_data<-left_join(student_profiles, grades, by = "Random.Student.ID" )

student_data$URM<-factor(student_data$IPEDS.Ethnicity.URM.Non.URM)
student_data$First.Gen<-factor(student_data$First.Generation.Flag) 
student_data$First.Gen<-ifelse(student_data$First.Generation.Flag == "Y",1,0)

si_visit <- read.csv('data/SLC Appointment.csv')
si_visit$Random.Student.ID<-factor(si_visit$Random.Student.ID)
student_data<-left_join(student_data,si_visit, by = "Random.Student.ID" )
###Work in progress
###########################################################################################
# Course Level Data
###########################################################################################
course.level <- student_data%>% group_by(Random.Course.ID.x, Term.Year.x, Term.Type.x, URM) %>%
  summarise(class.size = n(),
            class.average = sum(Student.Class.Grade.Point.per.Unit)/class.size,
            dwf.rate = sum(Grade.DFW.Count)/class.size,
            HS.GPA.Ave = sum(HS.GPA)/class.size,
            First.Gen.Perc = sum(First.Gen)/class.size,
            SLC.visits = sum(SLC.Attended.Flag)/class.size
            )
si_visit <- read.csv('data/SLC Appointment.csv')

