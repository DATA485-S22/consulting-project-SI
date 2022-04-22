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
si_appt$Random.Student.ID <- factor(si_appt$Random.Student.ID)
si_appt$Term <- factor(si_appt$Term)
# Stores classes with an SI component
si_courses <- filter(courses, Random.Course.ID %in% levels(si_appt$Random.Course.ID))

############################################################
program.clean <- filter(program, Term.Year >= 2016) %>%
  mutate(Term.Type = factor(Term.Type), .keep = "unused") %>%
  filter(Term.Type %nin% c("Summer", "Winter")) %>%
  select(Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
                  IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
                  Academic.Program, Major.1.STEM.Flag, Major.1.College,
                  Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
                  Academic.Standing.Status)
program.clean$Random.Student.ID <- factor(program.clean$Random.Student.ID)


profile <- read.csv("data/Student Profile Metric.csv")
profile <- filter(profile, Cohort.Term.Year >= 2016) %>%
  mutate(Random.Student.ID = factor(Random.Student.ID), .keep = "unused") %>%
  select(Cohort.Term, Random.Student.ID, Degree.Term,
         Full.Time.Part.Time.Code, Cohort.Student.Enrollment.Type, HS.GPA.Group,
         HS.GPA, Transfer.GPA.Group, Transfer.GPA, Cohort.Time.to.Degree.Year,
         Student.Orientation.Flag)

# Joined program to profile
# about 35% of students are not present in the profile dataset
# therefore, they have null values
student_profiles <- left_join(program.clean, profile, by = "Random.Student.ID")
student_profiles <- rename(student_profiles, Enrollment.Term = Cohort.Term)
write.csv(student_profiles, "data/student_profiles_clean.csv")

student_profiles <- read.csv("data/student_profiles_clean.csv")

##############################################################

# Contains number of visits in the term, and flag for at least one visit
clean_si_visit <- select(si_appt, Random.Student.ID, SLC.Attended.Flag, Term, Visit.Count..per.day.) %>%
  group_by(Random.Student.ID, Term) %>%
  summarise(attended.si = min(SLC.Attended.Flag), count.visits = sum(Visit.Count..per.day.))


grades <- read_xlsx("data/Student Grade.xlsx")
grades <- filter(grades, `Term Year` >= 2016)
grades$`Random Course ID` <- factor(grades$`Random Course ID`)
grades$`Term Type` <- factor(grades$`Term Type`)
si_grades <- filter(grades, `Term Type` %nin% c("Summer", "Winter"), `Random Course ID` %in% levels(si_appt$Random.Course.ID))
write.csv(si_grades, "data/si_grades.csv", row.names = FALSE)

# Contains student grades for all classes with an SI component
si_grades <- read.csv("data/si_grades.csv")

##############################################################

si_grades$Random.Student.ID <- factor(si_grades$Random.Student.ID)
si_student_profiles <- filter(student_profiles, Random.Student.ID %in% levels(si_grades$Random.Student.ID))

################################################################################
# Making Graphs
################################################################################

# GPA Spread for SI/Non-SI with First-gen facet

student_grades <- read.csv("data/grades.csv")
student_grades <- select(student_grades, Random.Course.ID,
                         Student.Class.Grade.Point.per.Unit, Random.Student.ID)
student_profiles <- read.csv("data/student_profiles_clean.csv")
student_profiles <- select(student_profiles, Random.Student.ID, First.Generation.Flag)
courses <- read.csv("data/course_level.csv")
courses <- select(courses, Random.Course.ID, dwf.rate, class.average, class.size, SI.Component.Flag)

si_grades_with_fg <- left_join(student_grades, courses, by = "Random.Course.ID")
si_grades_with_fg <- left_join(si_grades_with_fg, student_profiles, by = "Random.Student.ID")
si_grades_with_fg$First.Generation.Flag <- factor(si_grades_with_fg$First.Generation.Flag,
                                                  labels = c("Not First-Gen", "First-Gen Student"))
si_grades_with_fg$SI.Component.Flag <- factor(si_grades_with_fg$SI.Component.Flag)
si_grades_with_fg <- na.omit(si_grades_with_fg)

num_zeros <- filter(si_grades_with_fg, Student.Class.Grade.Point.per.Unit == 0) %>%
  group_by(Random.Course.ID, First.Generation.Flag) %>%
  summarise(count = n(), class.size) %>%
  mutate(zero.rate = count/class.size)
si_summary_grades <- group_by(si_grades_with_fg, Random.Course.ID, First.Generation.Flag) %>%
  summarise(avg_grade = mean(Student.Class.Grade.Point.per.Unit)) %>%
  left_join(select(num_zeros, Random.Course.ID, zero.rate), by = c("Random.Course.ID", "First.Generation.Flag"))
si_summary_grades$zero.rate <- coalesce(si_summary_grades$zero.rate, 0)
si_summary_grades <- left_join(unique(si_summary_grades),
                               select(courses, Random.Course.ID, SI.Component.Flag),
                               by = "Random.Course.ID")

# SI First Gen Student `Grade = Zero` rate
SI_FG_zero_rate <- filter(si_summary_grades, First.Generation.Flag == "First-Gen Student", SI.Component.Flag == 1)
SI_FG_zero_rate <- mean(SI_FG_zero_rate$zero.rate)
# Non-SI First Gen Student `Grade = Zero` rate
NSI_FG_zero_rate <- filter(si_summary_grades, First.Generation.Flag == "First-Gen Student", SI.Component.Flag == 0)
NSI_FG_zero_rate <- mean(NSI_FG_zero_rate$zero.rate)
# SI Non-First Gen Student `Grade = Zero` rate
SI_NFG_zero_rate <- filter(si_summary_grades, First.Generation.Flag == "Not First-Gen", SI.Component.Flag == 1)
SI_NFG_zero_rate <- mean(SI_NFG_zero_rate$zero.rate)
# Non-SI NonFirst Gen Student `Grade = Zero` rate
NSI_NFG_zero_rate <- filter(si_summary_grades, First.Generation.Flag == "Not First-Gen", SI.Component.Flag == 0)
NSI_NFG_zero_rate <- mean(NSI_NFG_zero_rate$zero.rate)

# Text for Facets
dat_text <- data.frame(
  label = c(paste("Zero Rate for Class w/ SI: ", round(SI_NFG_zero_rate,3), "\nZero Rate for Class w/o SI: ", round(NSI_NFG_zero_rate,3)),
            paste("Zero Rate for Class w/ SI: ", round(SI_FG_zero_rate,3), "\nZero Rate for Class w/o SI: ", round(NSI_FG_zero_rate,3))),
  First.Generation.Flag = c("Not First-Gen", "First-Gen Student"))

# Student Level
ggplot(data = si_grades_with_fg) +
  geom_density(aes(x = Student.Class.Grade.Point.per.Unit, fill = SI.Component.Flag), alpha = .3) +
  geom_text(data = dat_text, aes(x = 1.25, y = 1, label = label)) +
  facet_grid(rows = vars(First.Generation.Flag))

# Course level SI Spread

ggplot(data = si_grades_with_fg) +
  geom_density(aes(x = class.average, fill = SI.Component.Flag), alpha = .3)

