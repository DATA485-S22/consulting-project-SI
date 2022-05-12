# Data Management File
#### Add flags for only creating new stuff
library(tidyverse)
library(Hmisc) # Enables %nin% notation for "not in"
library(magrittr) # Allows %<>% notation to update lhs object with resulting value
library(readxl) # read Excel file

###########################################################################################
# Create grades.csv (easier to work with)
###########################################################################################
# WARNING: THIS IS VERY LARGE, IT WILL TAKE ~5 MINS
grades <- read_xlsx("data/Student Grade.xlsx")

write.csv(grades, 'data/grades.csv', row.names = FALSE)


###########################################################################################
# Course Level Data
###########################################################################################
grades <- read.csv("data/grades.csv")

# Get class size, average, and dfw rate from the grades dataset
course.level <- grades %>% group_by(Random.Course.ID, Term.Year, Term.Type) %>%
  summarise(class.size = n(),
            class.average = sum(Student.Class.Grade.Point.per.Unit)/class.size,
            dwf.rate = sum(Grade.DFW.Count)/class.size)

# Add number of SI visits to course level data
si_visit <- read.csv('data/SLC Appointment.csv')
temp3 <- si_visit %>% group_by(Random.Course.ID) %>%
  summarise(SI.Visit.Num = sum(SLC.Attended.Flag), Total.visits = sum(Visit.Count..per.day.)) %>%
  filter(Total.visits >= 10) # Removed classes are considered non-SI classes
course.level <- course.level %>% left_join(temp3, by = 'Random.Course.ID')

# Add covariates from course detail file
course.detail <- read.csv("data/Course Detail.csv")

# Filter out courses from Summer or Winter and with special numbers or from before 2016
course.detail <- course.detail %>%
  mutate(course_num_clean = str_extract(Course.Catalog.Number, "\\d+"))
course.detail$course_num_clean <- factor(course.detail$course_num_clean)
course.detail <- course.detail %>%
  filter(course_num_clean %nin% c(189, 289, 389, 489, 589, 689, #internship
                                  198, 298, 398, 498, 598, 698, #special topics
                                  199, 299, 399, 499, 599, #special problems
                                  696, 697, 699)) #independent study
course.detail$course_num_clean <- droplevels(course.detail$course_num_clean)
course.detail <- course.detail %>%
  filter(Term.Year >= 2016, Term.Year <= 2019) # Start of SI, before the Pandemic
course.detail$Term.Type <- factor(course.detail$Term.Type)
course.detail <- filter(course.detail, Term.Type %nin% c("Summer", "Winter"))
course.detail <- droplevels(course.detail)
cols2 <- c("Random.Course.ID", "Course.Subject.and.Number", "Academic.Subject", "Academic.Subject.Code",
           "Class.Level", "Class.Learning.Mode", "Instruction.Mode",
           "Instruction.Mode.Code", "Inst.MD.Persn.Chcoflx.Onl.Othr",
           "Course.Type", "Course.Fee.Exist.Flag", "Writing.Course.Flag",
           "GE.Class.Flag")
course.detail %<>% mutate_at(cols2, factor)
course.level$Random.Course.ID <- factor(course.level$Random.Course.ID)

# Joining covariates
course.level <- inner_join(course.level, course.detail, by = 'Random.Course.ID')

# Create flag for SI component
course.level$SI.Visit.Num[is.na(course.level$SI.Visit.Num)] <- 0
course.level$SI.Component.Flag <- course.level$SI.Visit.Num
course.level$SI.Component.Flag[course.level$SI.Component.Flag > 0] <- 1
course.level$SI.Component.Flag <- factor(course.level$SI.Component.Flag)

# Remove repeated cols
course.level <- dplyr::select(course.level, -Term.Year.x, -Term.Type.x, - Term.Year.y, -Term.Type.y)

write.csv(course.level, 'data/course_level.csv', row.names = FALSE)

################################################################################
# Cleaned Student Profiles
################################################################################

# Student Program Dataset
program <- read.csv("data/Student Program.csv")
program.clean <- filter(program, Term.Year >= 2016) %>% # SI classes start in 2016
  mutate(Term.Type = factor(Term.Type), .keep = "unused") %>%
  filter(Term.Type %nin% c("Summer", "Winter")) %>%
  dplyr::select(Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
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
  dplyr::select(Cohort.Term, Random.Student.ID, Degree.Term,
         Full.Time.Part.Time.Code, Cohort.Student.Enrollment.Type, HS.GPA.Group,
         HS.GPA, Transfer.GPA.Group, Transfer.GPA, Cohort.Time.to.Degree.Year,
         Student.Orientation.Flag, Cohort.Term.Year, One.Year.Retention)

# Student Programs and Profiles Joined together
# about 35% of students are not present in the profile dataset
# therefore they have null values
student_profiles <- left_join(program.clean, profile, by = "Random.Student.ID")
student_profiles <- rename(student_profiles, Enrollment.Term = Cohort.Term)
write.csv(student_profiles, "data/student_profiles_clean.csv", row.names = FALSE)

################################################################################
# SI expanded grades Data
################################################################################

# Contains number of visits in the term per student per class, and flag for at least one visit
clean_si_visit <- dplyr::select(si_visit, Random.Course.ID, Random.Student.ID, SLC.Attended.Flag,
                         Term, Visit.Count..per.day.) %>%
  group_by(Random.Student.ID, Random.Course.ID) %>%
  summarise(attended.si = min(SLC.Attended.Flag),
            count.visits = sum(Visit.Count..per.day.))

# Import Grades Data, clean
grades$Random.Course.ID <- factor(grades$Random.Course.ID)
grades$Term.Type <- factor(grades$Term.Type)

# Create Table for Grades of SI Classes
clean_si_visit$Random.Course.ID <- factor(clean_si_visit$Random.Course.ID)
clean_si_visit$Random.Student.ID <- factor(clean_si_visit$Random.Student.ID)
si_grades <- filter(grades, Term.Type %nin% c("Summer", "Winter"),
                    Random.Course.ID %in% levels(clean_si_visit$Random.Course.ID))
si_grades$Random.Student.ID <- factor(si_grades$Random.Student.ID)
si_grades <- left_join(si_grades, clean_si_visit,
                       by = c("Random.Student.ID", "Random.Course.ID"))

si_grades$attended.si[is.na(si_grades$attended.si)] <- 0
si_grades$count.visits[is.na(si_grades$count.visits)] <- 0

# Contains Grades of all Students for all Courses With SI
write.csv(si_grades, "data/si_grades.csv", row.names = FALSE)

################################################################################
# Filtered Student Profiles (Have taken at least one SI Course)
################################################################################
si_student_profiles <- filter(student_profiles,
                              Random.Student.ID %in% levels(si_grades$Random.Student.ID))

#############################################################################################
#   Create Grades data for only SI classes
#############################################################################################
si_students <- si_visit %>% dplyr::select(Term.Year, 
                                          Term.Type,
                                          Random.Course.ID,
                                          Random.Student.ID,
                                          SLC.Attended.Flag,
                                          Visit.Count..per.day.)

# Aggregate number of SI visits for each student in SLC dataset
si_count <- si_students %>% group_by(Random.Course.ID, Random.Student.ID) %>%
  dplyr::summarize(SI.Visit.Num = sum(Visit.Count..per.day.))
si_count$Random.Course.ID <- factor(si_count$Random.Course.ID)
si_count$Random.Student.ID <- factor(si_count$Random.Student.ID)

si_grades <- si_grades %>% left_join(si_count)

si_grades <- dplyr::select(si_grades, Term.Year, Term.Type, Random.Course.ID, Student.Class.Official.Grade,
                           Random.Student.ID, SI.Visit.Num, Student.Class.Unit.Passed, Student.Class.Unit.Attempted)

# Create a flag for SI attended
si_grades$SI.Attended <- ifelse(si_grades$SI.Visit.Num > 0, 1, 0)
si_grades$SI.Attended[is.na(si_grades$SI.Attended)] <- 0
si_grades$SI.Visit.Num[is.na(si_grades$SI.Visit.Num)] <- 0

# Grades Data for only SI classes
write.csv(si_grades, "data/grades_SI_classes.csv", row.names = FALSE)

#############################################################################################
#                                 Data for matchit
#############################################################################################
grades <- read.csv("data/grades_SI_classes.csv")
profiles <- student_profiles %>%
  dplyr::select(c("Random.Student.ID",
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
  dplyr::select(c("Random.Course.ID",
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
grades$Random.Student.ID<-factor(grades$Random.Student.ID)
grades$Random.Course.ID <- factor(grades$Random.Course.ID)
courses$Random.Course.ID <- factor(courses$Random.Course.ID)
data <- grades %>% left_join(profiles) %>% left_join(courses) %>%
  drop_na(c(HS.GPA, Student.Orientation.Flag, Major.1.STEM.Flag, 
            Full.Time.Part.Time.Code, Academic.Program))

write.csv(data, "data/CEM_full_dataset.csv", row.names = FALSE)

################################################################################
#                       Student analysis Dataset
################################################################################
profile<- student_profiles %>% 
  dplyr::select(c("Random.Student.ID",
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
                  "Enrollment.Term",
                  "Cohort.Term.Year",
                  "One.Year.Retention",
                  "Student.Orientation.Flag"))

grades <- read.csv("data/grades_SI_classes.csv")
courses <- read.csv("data/Course Detail.csv") %>%
  dplyr::select(c("Random.Course.ID",
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

grades$Random.Student.ID<-factor(grades$Random.Student.ID)
grades$Random.Course.ID <- factor(grades$Random.Course.ID)
courses$Random.Course.ID <- factor(courses$Random.Course.ID)
# Merge the data sets
d <- grades %>% left_join(profile) %>% left_join(courses) %>%
  drop_na(c(HS.GPA, Student.Orientation.Flag, Major.1.STEM.Flag,
            Full.Time.Part.Time.Code, Academic.Program))

write.csv(d, "data/student_analysis_dataset.csv", row.names = FALSE)

################################################################################
#                       Course analysis Dataset
################################################################################
grades <- read.csv("data/grades.csv")
student_profiles <- read.csv("data/student_profiles_clean.csv")
student_profiles$Random.Student.ID <- factor(student_profiles$Random.Student.ID)
grades$Random.Student.ID<- factor(grades$Random.Student.ID)
student_data <- inner_join(student_profiles, grades, by = "Random.Student.ID" )

student_data$URM<-factor(student_data$IPEDS.Ethnicity.URM.Non.URM)
student_data$First.Gen<-factor(student_data$First.Generation.Flag) 
student_data$First.Gen<-ifelse(student_data$First.Generation.Flag == "Y",1,0)
student_data <- dplyr::select(student_data, Term.Year, Random.Course.ID,
                              URM, First.Gen, HS.GPA)

course.level <- read.csv("data/course_level.csv")
course.level$Random.Course.ID <- factor(course.level$Random.Course.ID)
student_data$Random.Course.ID <- factor(student_data$Random.Course.ID)

# More instances of lower division SI courses, and need substantial class size (20)
course.level <- filter(course.level, class.size >= 20, course_num_clean < 300) %>%
  dplyr::select(Random.Course.ID, class.size, class.average, dwf.rate, SI.Component.Flag)
course.level <- inner_join(course.level, student_data, by = "Random.Course.ID")
course.level <- drop_na(course.level, Term.Year)

# Modeling data
course.level <- group_by(course.level, Random.Course.ID) %>%
  summarise(HS.GPA.Ave = mean(HS.GPA, na.rm = TRUE),
            First.Gen.Perc = sum(First.Gen, na.rm = TRUE)/class.size,
            URM = sum(as.integer(URM)-1, na.rm = TRUE)/class.size,
            class.size, dwf.rate, class.average, SI.Component.Flag, Term.Year)
course.level <- course.level[!duplicated(course.level$Random.Course.ID), ]
course.level <- filter(course.level, Term.Year <= 2019)

write.csv(course.level, "data/course_analysis_dataset.csv", row.names = FALSE)

###################################################
# !!! Everything Finished !!!
###################################################