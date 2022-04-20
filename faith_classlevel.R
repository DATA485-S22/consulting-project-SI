
library(tidyverse)
library(readxl)
library(magrittr) #Allows %<>% notation to update lhs object with resulting value
library(naniar)
library(Hmisc) # %nin%
#library(sjPlot)

courses<-read_csv("data/Course Detail.csv")
names(courses) <- make.names(names(courses), unique=TRUE)
courses <- courses %>%
  mutate(course_num_clean = str_extract(Course.Catalog.Number, "\\d+"))
courses$course_num_clean <- factor(courses$course_num_clean)
levels(courses$course_num_clean)

courses <- courses %>%
  filter(course_num_clean %nin% c(189, 289, 389, 489, 589, 689, #internship
                                  198, 298, 398, 498, 598, 698, #special topics
                                  199, 299, 399, 499, 599, #special problems
                                  696, 697, 699)) #independent study
courses$course_num_clean <- droplevels(courses$course_num_clean)

courses <- courses %>%
  filter(Term.Year >= 2016) # Start of SI


identical(courses[["Writing.Course.Flag.Description"]], courses[["Writing.or.Not.Writing.Class"]]) # TRUE
identical(courses[["GE.or.Program.Flag.Description"]], courses[["GE.or.Program.Major"]]) # TRUE

si_appt  <- read.csv("data/SLC Appointment.csv") %>% 
  select(-c(College.Year, Academic.Year, Term, Term.Type))
si_appt<- filter(si_appt, Term.Year>=2016)
  
courses$Random.Course.ID <- factor(courses$Random.Course.ID)
si_appt$Random.Course.ID <- factor(si_appt$Random.Course.ID)
si_appt$Random.Student.ID <- factor(si_appt$Random.Student.ID)

# Stores classes with an SI component
si_courses <- filter(courses, Random.Course.ID %in% levels(si_appt$Random.Course.ID))
si_attend<-left_join(si_appt, si_courses, by = "Random.Course.ID")

# Want student level 1 year retention as function of some variable for SI visits, student demographics, year fixed effects, course fixed effects?
# time to graduate by cohort 
# one year retention rates gets around COVID 
# could use if student dropped failed or withdrew 
# if course could use DFW rate 

#Currently plotting  years retention by cohort 
# glm --> adding resampling using tidymodels 

grades <- read_xlsx("data/Student Grade.xlsx")
names(grades) <- make.names(names(grades), unique=TRUE)
grades <- filter(grades, Term.Year >= 2016)

profile <- read.csv("data/Student Profile Metric.csv") 

profile<- profile %>%  filter(Cohort.Term.Year >= 2016, 	
        Cohort.Student.Enrollment.Type == "First-Time Freshman") %>%
        select(-c(Transfer.GPA,Transfer.GPA.Group, Transfer.GPA.Group.Code))
profile$Cohort.Academic.Year = factor(profile$Cohort.Academic.Year)
profile$Years.enrolled = rowSums(profile[,c(24,25,26,27,28)]) 
profile$Years.enrolled = factor(profile$Years.enrolled )
profile$Random.Student.ID<-factor(profile$Random.Student.ID)

program<-read.csv("data/Student Program.csv")
program<- filter(program, Term.Year >= 2016) %>%
  mutate(Term.Type = factor(Term.Type), .keep = "unused") %>%
  filter(Term.Type %nin% c("Summer", "Winter")) %>%
  select(Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
         IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
         Academic.Program, Major.1.STEM.Flag, Major.1.College,
         Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
         Academic.Standing.Status)

program$Random.Student.ID <- factor(program$Random.Student.ID)

d<-left_join(profile, si_attend, by = "Random.Student.ID" ) %>% 
  select(c(Random.Student.ID, Random.Course.ID),everything() )

d<-left_join(d, program, by = "Random.Student.ID")

#d<-subset(d, !is.na(Random.Course.ID)) # just SI classes

d$SLC.Attended.Flag<-ifelse(is.na(d$SLC.Attended.Flag),0,1)
d$SLC.Attended.Flag<-factor(d$SLC.Attended.Flag)
d$Instruction.Mode<-factor(d$Instruction.Mode)
d$Student.Orientation.Flag<-factor(d$Student.Orientation.Flag)
d$GE.Class.Flag<-factor(d$GE.Class.Flag)
d$One.Year.Retention<-factor(d$One.Year.Retention)
d$URM<-factor(d$IPEDS.Ethnicity.URM.Non.URM)
d$Gender.Code<-factor(d$Gender.Code)
d$Writing.Course.Flag<-factor(d$Writing.Course.Flag)
d$Major.1.STEM.Flag<-factor(d$Major.1.STEM.Flag)

d<-select(d, c(Random.Student.ID, Random.Course.ID, Cohort.Term.Year, Degree.Term, HS.GPA, One.Year.Retention,
               SLC.Attended.Flag, Random.Instructor.ID, Class.Level, Instruction.Mode, Writing.Course.Flag, 
               GE.Class.Flag, Gender.Code, IPEDS.Ethnicity, IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag,
               Major.1.STEM.Flag))
fit1<-glm(One.Year.Retention ~ SLC.Attended.Flag + HS.GPA + URM,
             family = binomial(link = "logit"), data = d)
summary(fit1)




