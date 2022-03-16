# Joseph Shifman's exploring file
library(tidyverse)
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

cols2 <- c("Course.Subject.and.Number", "Academic.Subject", "Academic.Subject.Code",
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
courses <- filter(courses, Random.Course.ID %in% levels(si_appt$Random.Course.ID))

program.clean <- select(program, Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
                  IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
                  Academic.Program, Major.1.STEM.Flag, Major.1.College,
                  Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
                  Academic.Standing.Status)
