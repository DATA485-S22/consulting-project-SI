# Data Management File
library(tidyverse)
library(Hmisc) # Enables %nin% notation for "not in"
library(readxl) # read Excel file

###########################################################################################
# Create grades.csv 
###########################################################################################
# WARNING: THIS IS VERY LARGE, IT WILL TAKE ~5 MINS
grades <- read_xlsx("data/Student Grade.xlsx")

write.csv(grades, 'data/grades.csv')


###########################################################################################
# Course Level Data
###########################################################################################
grades <- read.csv("data/grades.csv")


course.level <- grades %>% group_by(Random.Course.ID, Term.Year, Term.Type) %>%
  summarise(class.size = n(),
            class.average = sum(Student.Class.Grade.Point.per.Unit)/class.size,
            dwf.rate = sum(Grade.DFW.Count)/class.size)
si_visit <- read.csv('data/SLC Appointment.csv')

temp3 <- si_visit %>% group_by(Random.Course.ID) %>%
  summarise(SI.Visit.Num = sum(SLC.Attended.Flag))

course.level <- course.level %>% left_join(temp3, by = 'Random.Course.ID')

# Add covariates from course detail file
course.detail <- read.csv("data/Course Detail.csv") 
course.level <- left_join(course.level, course.detail, by = 'Random.Course.ID')

# Create flag for SI component
course.level$SI.Visit.Num[is.na(course.level$SI.Visit.Num)] <- 0
course.level$SI.Component.Flag <- course.level$SI.Visit.Num
course.level$SI.Component.Flag[course.level$SI.Component.Flag > 0] <- 1
course.level$SI.Component.Flag <- factor(course.level$SI.Component.Flag)

# Remove repeated cols
course.level <- select(course.level, -Term.Year.x, -Term.Type.x, - Term.Year.y, -Term.Type.y)

write.csv(course.level, 'data/course_level.csv')

################################################################################
# Cleaned Student Profiles
################################################################################

# Student Program Dataset
program <- read.csv("data/Student Program.csv")
program.clean <- filter(program, Term.Year >= 2016) %>% # SI classes start in 2016
  mutate(Term.Type = factor(Term.Type), .keep = "unused") %>%
  filter(Term.Type %nin% c("Summer", "Winter")) %>%
  select(Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
         IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
         Academic.Program, Major.1.STEM.Flag, Major.1.College,
         Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
         Academic.Standing.Status)
program.clean$Random.Student.ID <- factor(program.clean$Random.Student.ID)
# Remove duplicate rows
program.clean <- program.clean[order(program.clean$Random.Student.ID),]
program.clean <- program.clean[!duplicated(program.clean$Random.Student.ID),]

# Student Profile Dataset
profile <- read.csv("data/Student Profile Metric.csv")
profile <- filter(profile, Cohort.Term.Year >= 2016) %>%
  mutate(Random.Student.ID = factor(Random.Student.ID), .keep = "unused") %>%
  select(Cohort.Term, Random.Student.ID, Degree.Term,
         Full.Time.Part.Time.Code, Cohort.Student.Enrollment.Type, HS.GPA.Group,
         HS.GPA, Transfer.GPA.Group, Transfer.GPA, Cohort.Time.to.Degree.Year,
         Student.Orientation.Flag)

# Student Programs and Profiles Joined together
# about 35% of students are not present in the profile dataset
# therefore they have null values
student_profiles <- left_join(program.clean, profile, by = "Random.Student.ID")
student_profiles <- rename(student_profiles, Enrollment.Term = Cohort.Term)
write.csv(student_profiles, "data/student_profiles_clean.csv")

################################################################################
# Grades Data
################################################################################

# Contains number of visits in the term, and flag for at least one visit
clean_si_visit <- select(si_appt, Random.Student.ID, SLC.Attended.Flag,
                         Term, Visit.Count..per.day.) %>%
  group_by(Random.Student.ID, Term) %>%
  summarise(attended.si = min(SLC.Attended.Flag),
            count.visits = sum(Visit.Count..per.day.))

# Import Grades Data, clean
grades <- filter(grades, `Term Year` >= 2016)
grades$`Random Course ID` <- factor(grades$`Random Course ID`)
grades$`Term Type` <- factor(grades$`Term Type`)

# Create Table for Grades of SI Classes
si_grades <- filter(grades, `Term Type` %nin% c("Summer", "Winter"),
                    `Random Course ID` %in% levels(si_appt$Random.Course.ID))
si_grades$Random.Student.ID <- factor(si_grades$Random.Student.ID)

# Contains Grades of all Students for All Courses With SI
write.csv(si_grades, "data/si_grades.csv", row.names = FALSE)

################################################################################
# Filtered Student Profiles (Have taken at least one SI Course)
################################################################################
si_student_profiles <- filter(student_profiles,
                              Random.Student.ID %in% levels(si_grades$Random.Student.ID))

#############################################################################################
#                         Create Grades data for only SI classes
#############################################################################################

si_students <- read.csv("data/SLC Appointment.csv")
grades <- read.csv("data/grades.csv")
profiles <- read.csv("data/Student Profile Metric.csv")
courses <- read.csv("data/course_level.csv") %>% filter(SI.Component.Flag == 1)

# Select grades from courses that have an SI component
grades <- grades %>% filter(Random.Course.ID %in% courses$Random.Course.ID)

si_students <- si_students %>% select(Term.Year, 
                                      Term.Type,
                                      Random.Course.ID,
                                      Random.Student.ID,
                                      SLC.Attended.Flag)

# Aggregate number of SI visits for each student in SLC dataset
si_count <- si_students %>% group_by(Random.Course.ID, Random.Student.ID) %>%
  dplyr::summarize(SI.Visit.Num = sum(SLC.Attended.Flag))

grades <- grades %>% left_join(si_count)

grades <- select(Term.Year, Term.Type, Random.Course.ID, Student.Class.Official.Grade,
                 Random.Studen.ID, SI.Visit.Num, SI.Attended)

# Create a flag for SI attended
grades$SI.Attended <- ifelse(grades$SI.Visit.Num > 0, 1, 0)

grades$SI.Attended[is.na(grades$SI.Attended)] <- 0

# Grades Data for only SI classes
write.csv(grades, "data/grades_SI_classes.csv")

#############################################################################################
#                                 Data for matchit
#############################################################################################

grades <- read.csv("data/grades_SI_classes.csv")  %>%
  select(c("Random.Course.ID",
           "Term",
           "Term.Year",
           "Term.Type",
           "Student.Class.Unit.Attempted",
           "Student.Class.Unit.Passed",
           "Student.Class.Grade.Point.per.Unit",
           "Student.Class.Official.Grade",
           "Random.Student.ID",
           "SI.Visit.Num",
           "SI.Attended"))
profiles <- read.csv("data/student_profiles_clean.csv") %>%
  select(c("Random.Student.ID",
           "IPEDS.Ethnicity",
           "IPEDS.Ethnicity.URM.Non.URM",
           "Gender.Code",
           "First.Generation.Flag",
           "Academic.Level",
           "Academic.Program",
           "Major.1.STEM.Flag",
           "Major.1.College",
           "Entry.Enrollment.Type",
           "Academic.Standing.Status",
           "Full.Time.Part.Time.Code",
           "HS.GPA",
           "Transfer.GPA", 
           "Student.Orientation.Flag"))
courses <- read.csv("data/Course Detail.csv") %>%
  select(c("Random.Course.ID",
           "Term",
           "Term.Year",
           "Term.Type",
           "Class.Subject.Number.Section",
           "Course.Subject.and.Number",
           "Academic.Subject",
           "Academic.Subject.Code",
           "Course.Catalog.Number",
           "Class.Learning.Mode",
           "Instruction.Mode",
           "Inst.MD.Persn.Chcoflx.Onl.Othr",
           "Inst.MD.Persn.Onl.Othr",
           "Course.Type",
           "Class.Status",
           "Course.Fee.Exist.Flag",
           "Combined.Section.Group.ID",
           "Writing.Course.Flag",
           "GE.Class.Flag",
           "GE.Foundation.Course",
           "GE.Foundation.Course.Code",
           "GE.Advanced.Course.Substitution.Flag",
           "Honors.Class.Flag"))

# Merge the data sets
data <- grades %>% left_join(profiles) %>% left_join(courses) %>% drop_na(c(HS.GPA, 
                                                                            Student.Orientation.Flag,
                                                                            Major.1.STEM.Flag,
                                                                            Full.Time.Part.Time.Code,
                                                                            Academic.Program))

write.csv(data, "data/CEM_full_dataset.csv")