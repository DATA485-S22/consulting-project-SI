# Joseph Shifman's exploring file
library(tidyverse)
library(readxl)
library(magrittr) # Allows %<>% notation to update lhs object with resulting value
library(naniar)
library(Hmisc) # %nin%

program <- read.csv("data/Student Program.csv")

cols <- c("College.Year", "Academic.Year", "Term", "Term.Type", "Gender",
         "Gender.Code", "IPEDS.Ethnicity", "IPEDS.Ethnicity.URM.Non.URM",
         "First.Generation.Flag", "Academic.Level", "Academic.Level.Code",
         "Academic.Program", "Major.1.STEM.Flag", "Major.1.College",
         "Major.1.Department", "Major.2.STEM.Flag", "Major.2.College",
        "Major.2.Department", "Entry.Enrollment.Type..New.Continuing.Other.",
        "Entry.Enrollment.Type", "Academic.Standing.Status")
program %<>% mutate_at(cols, factor)

program <- group_by(program, Random.Student.ID) %>% # Student was enrolled at any time during/after 2016
  filter(any(Term.Year >= 2016))

levels(program$Academic.Year)
program %>%
  group_by(Academic.Year) %>%
  summarise(no_rows = length(Academic.Year))

program %>%
  group_by(Major.1.STEM.Flag) %>%
  summarise(no_rows = length(Major.1.STEM.Flag))

#########################################################
courses <- read.csv("data/Course Detail.csv")

length(unique(courses$Course.Subject.and.Number))

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
levels(courses$course_num_clean)

courses <- courses %>%
  filter(Term.Year >= 2016) # Start of SI

courses$Term.Type <- factor(courses$Term.Type)
levels(courses$Term.Type)
courses <- filter(courses, Term.Type %nin% c("Summer", "Winter"))
courses <- droplevels(courses)

identical(courses[["Writing.Course.Flag.Description"]], courses[["Writing.or.Not.Writing.Class"]]) # TRUE
identical(courses[["GE.or.Program.Flag.Description"]], courses[["GE.or.Program.Major"]]) # TRUE

cols2 <- c("Random.Course.ID", "Course.Subject.and.Number", "Academic.Subject", "Academic.Subject.Code",
           "Class.Level", "Class.Learning.Mode", "Instruction.Mode",
           "Instruction.Mode.Code", "Inst.MD.Persn.Chcoflx.Onl.Othr",
           "Course.Type", "Course.Fee.Exist.Flag", "Writing.Course.Flag",
           "GE.Class.Flag")
courses %<>% mutate_at(cols2, factor)


##############################################
si_appt <- read.csv("data/SLC Appointment.csv")

courses$Random.Course.ID <- factor(courses$Random.Course.ID)
si_appt$Random.Course.ID <- factor(si_appt$Random.Course.ID)
si_appt <- filter(si_appt, Random.Course.ID %nin% c(-1,-2))
si_courses <- filter(courses, Random.Course.ID %in% levels(si_appt$Random.Course.ID))

program.clean <- select(program, Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
                  IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
                  Academic.Program, Major.1.STEM.Flag, Major.1.College,
                  Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
                  Academic.Standing.Status)


profile <- read.csv("data/Student Profile Metric.csv")
profile <- filter(profile, Cohort.Term.Year >= 2016) %>%
  select(Cohort.Term, Random.Student.ID, Last.Term, Degree.Term,
         Full.Time.Part.Time.Code, Cohort.Student.Enrollment.Type, HS.GPA.Group,
         HS.GPA, Transfer.GPA.Group, Transfer.GPA, Cohort.Time.to.Degree.Year,
         Student.Orientation.Flag)
# Profile stores all students
# Add total SI visits
siii <- select(si_appt, Random.Student.ID, SLC.Attended.Flag, Term)
siii$Term <- factor(siii$Term)
siii <- siii[!duplicated(siii[,'Random.Student.ID']),]

grades <- read_xlsx("data/Student Grade.xlsx")
grades <- filter(grades, `Term Year` >= 2016)
grades$`Random Course ID` <- factor(grades$`Random Course ID`)
grades$`Term Type` <- factor(grades$`Term Type`)
si_grades <- filter(grades, `Term Type` %nin% c("Summer", "Winter"), `Random Course ID` %in% levels(si_appt$Random.Course.ID))

# Replace with all students who took SI classes
siii <- right_join(siii, profile, by = c("Random.Student.ID", "Term"))
siii$SLC.Attended.Flag[is.na(siii$SLC.Attended.Flag)] <- 0
ggplot(siii) +
  geom_bar(aes(x = SLC.Attended.Flag))
