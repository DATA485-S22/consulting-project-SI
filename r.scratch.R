Lit Notes, in alphabetical order, 

Abraham and Telang - univeristy of texas
  Groups: SI attendees (attend 2 or more sessions), Non-SI attendees (0-1 sessions)
  No mentions of minority groups
Arrendale - Not relevant (1990s)
Bengesai - Not really relevant (2009-2011), South African University, equity gap-no financial aid nor prior academic advances
Blat - (2000s), First, SI focuses on high-risk courses, not on high- risk students.
Bonsangue(1) - fullerton
  URM - black,hispanic,native american or multiple races
Bonsangue(2) - Cal poly from (1990s)
  groups - minority students math,science,engineering.
  URM- underrepresented groups (Primarily Latinos)
Bonsangue(3) - 1990s
  Calculus, transfer students
Dawson - High risk course, Did not really specify URM
Eroy - SFSU, mostly talked about Latinos
Guarcello(1) - SDSU EOP first gen and low income from URM (black,af-am,latino,am-ind/nat-am)
Guarcello(2) - nothing on URM
Holek - Old article, Students of color at MWU, supports Af-am,latino,na.am
Impact of ROI - Defined SI student as attending 1 time, minimal impact to pay for SI
Malm - Does not explore equity gaps, just higher grades due to SI
Meiling - (2012) Study on retention for Latino students south Texas, says first gen engage more
Mitra - (2017) SI is more helpful for students identified as at risk than for those who are not (after controlling for background variables)
Okun - (2015) minority students may benefit more from SI than Euro-American students
Peterfreund - (2007) defined URM as pretty much not white. Concluded SI helped everyone especially URM
Rabitoy - (2015) Study on SI attendees, leaders and faculty. Demographic: Gender(female,male), Ethnicity, white or POC
Ramirez - (1997) 4 groups - SI/Non-SI of socioeconomic advantaged backgrounds, SI/Non-SI of academically underprepared (EOP, disabled student)
Rath(1) - mostresearch suggests that SI benefits all ethnic−racial groups equally although research at SFSU found URM students
seeming to benefit more from SI than their peers in some courses
Does not define their URM but compares URM vs Non-URM
Rath(2) - URM in biology - black, Hispanic, American Indian/Alaska Native, or from the Pacific Islands
Stanich -  POC, EOP, women, low-income individuals, and first-generation students
Van Sickle - POC, first-gen, and women
Wasinger - URM - POC 
Williams - biology and science - gender, focus on white non-hispanic,black non-hispanic, and hispanic 
Wilson - traditionally URM - including Hispanics, African Americans, and Native Americans
Yue - 1)underrepresented minority status 2) first-generation status, 3)Federal Pell Grant eligible status, and 4) English/ mathematics remedial status


library(dplyr)
# Data Management File
library(tidyverse)
library(Hmisc) # Enables %nin% notation
library(readxl) # read Excel file

###########################################################################################
# Create grades.csv 
###########################################################################################
grades <- read_xlsx("~/Downloads/Student Grade.xlsx")

write.csv(grades, '~/Downloads/grades.csv')


###########################################################################################
# Course Level Data
###########################################################################################
grades.clean <- read.csv("~/Downloads/grades.csv")


course.level <- grades.clean %>% group_by(Random.Course.ID, Term.Year, Term.Type) %>%
  summarise(class.size = n(),
            class.average = sum(Student.Class.Grade.Point.per.Unit)/class.size,
            dwf.rate = sum(Grade.DFW.Count)/class.size)
si_visit <- read.csv('~/Downloads/SLC Appointment.csv')

temp3 <- si_visit %>% group_by(Random.Course.ID) %>%
  summarise(SI.Visit.Num = sum(SLC.Attended.Flag))

course.level <- course.level %>% left_join(temp3, by = 'Random.Course.ID')

# Add covariates from course detail file
course.detail <- read.csv("~/Downloads/Course Detail.csv") 
course.level <- left_join(course.level, course.detail, by = 'Random.Course.ID')

# Create flag for SI component
course.level$SI.Visit.Num[is.na(course.level$SI.Visit.Num)] <- 0
course.level$SI.Component.Flag <- course.level$SI.Visit.Num
course.level$SI.Component.Flag[course.level$SI.Component.Flag > 0] <- 1
course.level$SI.Component.Flag <- factor(course.level$SI.Component.Flag)

# Remove repeated cols
course.level <- select(course.level, -Term.Year.x, -Term.Type.x, - Term.Year.y, -Term.Type.y)

write.csv(course.level, '~/Downloads/course_level.csv')

################################################################################
# Cleaned Student Profiles
################################################################################

# Student Program Dataset
program <- read.csv("~/Downloads/Student Program.csv")
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
profile <- read.csv("~/Downloads/Student Profile Metric.csv")
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
write.csv(student_profiles, "~/Downloads/student_profiles_clean.csv")

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
grades <- filter(grades, `Term.Year` >= 2016)
grades$`Random.Course.ID` <- factor(grades$`Random.Course.ID`)
grades$`Term.Type` <- factor(grades$`Term.Type`)

# Create Table for Grades of SI Classes
si_grades <- filter(grades, `Term.Type` %nin% c("Summer", "Winter"),
                    `Random.Course.ID` %in% levels(si_appt$Random.Course.ID))
si_grades$Random.Student.ID <- factor(si_grades$Random.Student.ID)

# Contains Grades of all Students for All Courses With SI
write.csv(si_grades, "~/Downloads/si_grades.csv", row.names = FALSE)

################################################################################
# Filtered Student Profiles (Have taken at least one SI Course)
################################################################################
si_student_profiles <- filter(student_profiles,
                              Random.Student.ID %in% levels(si_grades$Random.Student.ID))


courses<-read_csv("~/Downloads/Course Detail.csv")
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

si_appt  <- read.csv("~/Downloads/SLC Appointment.csv") %>% 
  select(-c(College.Year, Academic.Year, Term, Term.Type))
si_appt<- filter(si_appt, Term.Year>=2016)

courses$Random.Course.ID <- factor(courses$Random.Course.ID)
si_appt$Random.Course.ID <- factor(si_appt$Random.Course.ID)
si_appt$Random.Student.ID <- factor(si_appt$Random.Student.ID)

# Stores classes with an SI component
si_courses <- filter(courses, Random.Course.ID %in% levels(si_appt$Random.Course.ID))
#Joining SI Classes with SI visit data 
si_attend<-left_join(si_appt, si_courses, by = "Random.Course.ID")

profile <- read.csv("~/Downloads/Student Profile Metric.csv") 

profile<- profile %>%  filter(Cohort.Term.Year >= 2016, 	
                              Cohort.Student.Enrollment.Type == "First-Time Freshman") %>%
  select(-c(Transfer.GPA,Transfer.GPA.Group, Transfer.GPA.Group.Code))
profile$Cohort.Academic.Year = factor(profile$Cohort.Academic.Year)
profile$Years.enrolled = rowSums(profile[,c(24,25,26,27,28)]) 
profile$Years.enrolled = factor(profile$Years.enrolled )
profile$Random.Student.ID<-factor(profile$Random.Student.ID)

program<-read.csv("~/Downloads/Student Program.csv") 
program.clean <- filter(program, Term.Year >= 2016) %>% # SI classes start in 2016
  mutate(Term.Type = factor(Term.Type), .keep = "unused") %>%
  filter(Term.Type %nin% c("Summer", "Winter")) %>%
  select(Term, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
         IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
         Academic.Program, Major.1.STEM.Flag, Major.1.College,
         Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
         Academic.Standing.Status)
program.clean$Random.Student.ID<-factor(program.clean$Random.Student.ID)

grades <- read_xlsx("~/Downloads/Student Grade.xlsx")
names(grades) <- make.names(names(grades), unique=TRUE)
write.csv(grades, '~/Downloads/grades.csv', row.names = FALSE)

grades <- read.csv("~/Downloads/grades.csv")

# Student Profile Dataset
profile <- read.csv("~/Downloads/Student Profile Metric.csv")
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

si_visit <- read.csv('~/Downloads/SLC Appointment.csv')
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

str(course.level)

si_visit$Random.Course.ID <- factor(si_visit$Random.Course.ID)
si_visit <- filter(si_visit, Term.Year>=2016)

si_courses <- filter(courses, Random.Course.ID %in% levels(si_visit$Random.Course.ID))
si_courses$Random.Course.ID <- factor(si_courses$Random.Course.ID)
course.level <- rename(course.level, Random.Course.ID=Random.Course.ID.x)
course.level$Random.Course.ID <- factor(course.level$Random.Course.ID)
si_visit$Random.Course.ID <- factor(si_visit$Random.Course.ID )

si_attend<-left_join(si_visit, si_courses, by = "Random.Course.ID")


temp3 <- si_visit %>% group_by(Random.Course.ID) %>%
  summarise(SI.Visit.Num = sum(SLC.Attended.Flag))

temp3$Random.Course.ID <- factor(temp3$Random.Course.ID)
str(temp3)
course.level <- course.level %>% left_join(temp3, by = 'Random.Course.ID')
tes <- course.level %>% left_join(temp3, by = 'Random.Course.ID')
tes
test <- si_visit %>% group_by(Random.Course.ID) %>%
  summarise(SI.Visit.Num = sum(SLC.Attended.Flag))
# Add covariates from course detail file
course.detail <- read.csv("~/Downloads/Course Detail.csv") 
course.detail$Random.Course.ID <- factor(course.detail$Random.Course.ID )
course.level <- left_join(course.level, course.detail, by = 'Random.Course.ID')

course.detail$Random.Course.ID <- factor(course.detail$Random.Course.ID)
# Create flag for SI component
course.level$SI.Visit.Num[is.na(course.level$SI.Visit.Num)] <- 0
course.level$SI.Component.Flag <- course.level$SI.Visit.Num
course.level$SI.Component.Flag[course.level$SI.Component.Flag > 0] <- 1
course.level$SI.Component.Flag <- factor(course.level$SI.Component.Flag)

# Remove repeated cols
course.level <- select(course.level, -Term.Year.x, -Term.Type.x, - Term.Year.y, -Term.Type.y)

str(course.level)

course.level <- select(course.level, c("Random.Course.ID", "Term.Year.x.x", "Term.Type.x.x", "URM", "class.size", "class.average", "dwf.rate",
                            "HS.GPA.Ave", "First.Gen.Perc", "SLC.visits", "SI.Visit.Num", "SI.Component.Flag"))

dwf.equity <- group_by(course.level, URM)
dwf.equity <- summarise(dwf.equity, dwf.sum = sum(dwf.rate))
dwf.equity

```{r eval=FALSE, include=FALSE}
#DATA WORK
# Student Program Dataset
program <- read.csv("~/Downloads/Student Program.csv")
program.clean <- filter(program, Term.Year >= 2016) %>% # SI classes start in 2016
  mutate(Term.Type = factor(Term.Type), .keep = "unused") %>%
  filter(Term.Type %nin% c("Summer", "Winter")) %>%
  select(Term.Year, Random.Student.ID, Gender.Code, IPEDS.Ethnicity,
         IPEDS.Ethnicity.URM.Non.URM, First.Generation.Flag, Academic.Level,
         Academic.Program, Major.1.STEM.Flag, Major.1.College,
         Major.2.STEM.Flag, Major.2.College, Entry.Enrollment.Type,
         Academic.Standing.Status)

program.clean$Random.Student.ID <- factor(program.clean$Random.Student.ID)
# Remove duplicate rows
program.clean <- program.clean[order(program.clean$Random.Student.ID),]
program.clean <- program.clean[!duplicated(program.clean$Random.Student.ID),]

# Student Profile Dataset
profile <- read.csv("~/Downloads/Student Profile Metric.csv")
profile <- filter(profile, Cohort.Term.Year >= 2016) %>%
  mutate(Random.Student.ID = factor(Random.Student.ID), .keep = "unused") %>%
  filter(Cohort.Student.Enrollment.Type == "First-Time Freshman")%>%
  select(Cohort.Term, Cohort.Term.Year, Random.Student.ID, Degree.Term,
         Full.Time.Part.Time.Code, Cohort.Student.Enrollment.Type, HS.GPA.Group,
         HS.GPA, Transfer.GPA.Group, Transfer.GPA, Cohort.Time.to.Degree.Year,One.Year.Retention,
         Student.Orientation.Flag)
program.clean<-filter(program.clean, Random.Student.ID %in% levels(profile$Random.Student.ID))

student_profiles <- left_join(program.clean, profile, by = "Random.Student.ID")
student_profiles <- rename(student_profiles, Enrollment.Term = Cohort.Term)

profile<- student_profiles %>% 
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
           "Enrollment.Term",
           "Cohort.Term.Year",
           "One.Year.Retention",
           "Student.Orientation.Flag"))

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
           "Grade.DFW.Count",
           "SI.Visit.Num",
           "SI.Attended"))

courses <- read.csv("~/Downloads/Course Detail.csv") %>%
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

grades$Random.Student.ID<-factor(grades$Random.Student.ID)
# Merge the data sets
d <- grades %>% left_join(profile) %>% left_join(courses) %>% drop_na(c(HS.GPA, 
                                                                        Student.Orientation.Flag,
                                                                        Major.1.STEM.Flag,
                                                                        Full.Time.Part.Time.Code,
                                                                        Academic.Program))

d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)], as.factor)
d$Term.Year<-factor(d$Term.Year)
d$Cohort.Term.Year<-factor(d$Cohort.Term.Year)
d<-rename(d, URM = IPEDS.Ethnicity.URM.Non.URM)
d$URM.First.Gen<-factor(ifelse(d$URM=="URM" & d$First.Generation.Flag == "Y", 1, 0 )) #new var, student who is urm and first gen
d$One.Year.Retention<-factor(d$One.Year.Retention)
dod_data <- d %>% group_by(URM, SI.Attended)
str(d)
