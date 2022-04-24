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


what it is, why do we care, what we did
distribution with SI and n SI
data aggregated to course level.
create class level measure of gap. get equity gap as your outcome
dfw rate a vs b calcualte at class level (group by class group by first gen calcualte by dfw rate) and then subtract the 2 and get measure of a gap 
at class level and now have class level data set percent of first gen maybe average gpa and measure gap as a continuous
goal is inference 
resampling, instead of accuracy, no a beta for coefficient for si on the gap. beta and a distribution
fit a linear regression model
proportion that went to SI
distrbution of dfw rates with classes with SI and not SI
)




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

# Ignore Below

#str(si_student_profiles)
#si_student_profiles <- transform(si_student_profiles, IPEDS.Ethnicity.URM.Non.URM=as.factor(IPEDS.Ethnicity.URM.Non.URM))
#si_student_profiles$First.Generation.Flag<-ifelse(si_student_profiles$First.Generation.Flag=="Y",1,0)
#si_student_profiles <- transform(si_student_profiles, First.Generation.Flag=as.factor(First.Generation.Flag))
#si_student_profiles$IPEDS.Ethnicity <- as.factor(si_student_profiles$IPEDS.Ethnicity)
#si_student_profiles$Gender.Code <- as.factor(si_student_profiles$Gender.Code)
#str(si_student_profiles)

#m4 <- glm(Gender.Code ~ IPEDS.Ethnicity, data=si_student_profiles, family=binomial(link = "logit"))

#summary(m1) # URM or Not URM on First Gen
# exp(1.02490) - odds of a student being first gen is 1.78???

# summary(m2) # URM or Not URM on First Gen and Gender
# exp(1.01590) - odds of student being first gen is 1.76??

#m9 <- glm(First.Generation.Flag ~ IPEDS.Ethnicity.URM.Non.URM, data=si_student_profiles, family=binomial(link = "logit"))
#summary(m9) # First Gen on URM or Non URM
# exp(1.02490) - odds of student being First Gen on URM or Non URM is 1.78????

#summary(m3) # Gender on First Gen 
#exp(-0.21486 ) - 
#summary(m4)

#str(course.level)
#m5 <- lm(dwf.rate ~ SI.Component.Flag, data = course.level) # dwfrate lm
#ggplot(course.level, aes(dwf.rate)) + geom_histogram() # Right skewed
#ggplot(course.level, aes(log(course.level$dwf.rate+1))) + geom_histogram()
#m7 <- glm(dwf.rate ~ SI.Component.Flag, data = course.level) # dwfrate glm
#m8 <- glm(dwf.rate ~ SI.Component.Flag + class.size, data = course.level)




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

d$SLC.Attended.Flag[is.na(d$SLC.Attended.Flag)] <- 0
d$SLC.Attended.Flag<-factor(d$SLC.Attended.Flag)
d$Instruction.Mode<-factor(d$Instruction.Mode)
d$Student.Orientation.Flag<-factor(d$Student.Orientation.Flag)
d$GE.Class.Flag<-factor(d$GE.Class.Flag)
d$One.Year.Retention<-factor(d$One.Year.Retention)
d$URM<-factor(d$IPEDS.Ethnicity.URM.Non.URM)
d$Gender.Code<-factor(d$Gender.Code)
d$Writing.Course.Flag<-factor(d$Writing.Course.Flag) # no writing classes only one level so omitted from model
d$Major.1.STEM.Flag<-factor(d$Major.1.STEM.Flag)
d$Random.Instructor.ID<-factor(d$Random.Instructor.ID)
d$Class.Level<-factor(d$Class.Level)
d$Academic.Standing.Status<-factor(d$Academic.Standing.Status)
d$Cohort.Term.Year<-factor(d$Cohort.Term.Year)
d$First.Generation.Flag<-factor(d$First.Generation.Flag)
d$Term.Year<-factor(d$Term.Year.x)

q<-select(d, c(Random.Student.ID, Random.Course.ID, Cohort.Term.Year, Term.Year, Degree.Term, HS.GPA, One.Year.Retention,Student.Orientation.Flag, Years.enrolled, SLC.Attended.Flag, Random.Instructor.ID, Class.Level, Instruction.Mode, Writing.Course.Flag,GE.Class.Flag, Gender.Code, IPEDS.Ethnicity, IPEDS.Ethnicity.URM.Non.URM, URM, First.Generation.Flag, Major.1.STEM.Flag, Major.1.College, Academic.Standing.Status)) 

summary(d$Random.Course.ID)
summary(course.level$Random.Course.ID)
str(course.level)
str(d)
course.level$Random.Course.ID <- as.factor(course.level$Random.Course.ID)
str(course.level)

a <-left_join(q, course.level, by = "Random.Course.ID")
p <- group_by(a, URM)
p <- summarise(p, dwf.mean = mean(dwf.rate))

unique(a)
m <- a[!duplicated(a), ]
m <- select(m, -Random.Student.ID)
m <- m[!is.na(m$Random.Course.ID),]
d <- group_by(m, URM)
d <- summarise(d, dwf.mean = mean(dwf.rate))
summary(m$dwf.rate)




grades <- read_xlsx("~/Downloads/Student Grade.xlsx")
names(grades) <- make.names(names(grades), unique=TRUE)
write.csv(grades, '~/Downloads/grades.csv', row.names = FALSE)

grades <- read.csv("~/Downloads/grades.csv")

program <- read.csv("~/Downloads/Student Program.csv")
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

si_visit <- read.csv('~/Downloads/SLC Appointment.csv')
si_visit <- filter(si_visit, Term.Year>=2016)
si_visit$Random.Course.ID <- factor(si_visit$Random.Course.ID)

si_courses <- filter(courses, Random.Course.ID %in% levels(si_visit$Random.Course.ID))
si_courses$Random.Course.ID <- factor(si_courses$Random.Course.ID)
course.level <- rename(course.level, Random.Course.ID=Random.Course.ID.x)
course.level$Random.Course.ID <- factor(course.level$Random.Course.ID)

si_attend<-left_join(si_visit, si_courses, by = "Random.Course.ID")

a <- left_join(profile, si_attend, by = "Random.Student.ID" ) %>% 
  select(c(Random.Student.ID, Random.Course.ID),everything() )
si_attend$Random.Student.ID <- factor(si_visit$Random.Student.ID)

b <-left_join(course.level, program, by = "Random.Course.ID")
str(program)


temp3 <- si_visit %>% group_by(Random.Course.ID) %>%
  summarise(SI.Visit.Num = sum(SLC.Attended.Flag))

course.level <- course.level %>% left_join(temp3, by = 'Random.Course.ID')

# Add covariates from course detail file
course.detail <- read.csv("~/Downloads/Course Detail.csv") 
course.level <- left_join(course.level, course.detail, by = 'Random.Course.ID')

str(course.detail)
course.detail$Random.Course.ID <- factor(course.detail$Random.Course.ID)
# Create flag for SI component
course.level$SI.Visit.Num[is.na(course.level$SI.Visit.Num)] <- 0
course.level$SI.Component.Flag <- course.level$SI.Visit.Num
course.level$SI.Component.Flag[course.level$SI.Component.Flag > 0] <- 1
course.level$SI.Component.Flag <- factor(course.level$SI.Component.Flag)

# Remove repeated cols
course.level <- select(course.level, -Term.Year.x, -Term.Type.x, - Term.Year.y, -Term.Type.y)
