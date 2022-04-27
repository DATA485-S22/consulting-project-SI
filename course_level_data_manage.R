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




si_visit <- filter(si_visit, Term.Year>=2016)
si_visit$Random.Student.ID<-factor(si_visit$Random.Student.ID)
student_data<-left_join(student_data,si_visit, by = "Random.Student.ID" )

course.level <- student_data%>% group_by(Random.Course.ID.x, Term.Year.x, Term.Type.x, URM) %>%
  summarise(class.size = n(),
            class.average = sum(Student.Class.Grade.Point.per.Unit)/class.size,
            dwf.rate = sum(Grade.DFW.Count)/class.size,
            HS.GPA.Ave = sum(HS.GPA)/class.size,
            First.Gen.Perc = sum(First.Gen)/class.size,
            SLC.visits = sum(SLC.Attended.Flag)/class.size
  )

course.level <- rename(course.level, Random.Course.ID=Random.Course.ID.x)
course.level$Random.Course.ID <- factor(course.level$Random.Course.ID)
temp3 <- si_visit %>% group_by(Random.Course.ID) %>%
  summarise(SI.Visit.Num = sum(SLC.Attended.Flag))
temp3$Random.Course.ID <- factor(temp3$Random.Course.ID)
course.level <- course.level %>% left_join(temp3, by = 'Random.Course.ID')

# Add covariates from course detail file
course.detail <- read.csv("data/Course Detail.csv") 
course.detail$Random.Course.ID <- factor(course.detail$Random.Course.ID)
course.level <- left_join(course.level, course.detail, by = 'Random.Course.ID')

course.detail$Random.Course.ID <- factor(course.detail$Random.Course.ID)
# Create flag for SI component
course.level$SI.Visit.Num[is.na(course.level$SI.Visit.Num)] <- 0
course.level$SI.Component.Flag <- course.level$SI.Visit.Num
course.level$SI.Component.Flag[course.level$SI.Component.Flag > 0] <- 1
course.level$SI.Component.Flag <- factor(course.level$SI.Component.Flag)

course.level <- select(course.level, c("Random.Course.ID", "Term.Year.x.x", "Term.Type.x.x", "URM", "class.size", "class.average", "dwf.rate",
                                       "HS.GPA.Ave", "First.Gen.Perc", "SLC.visits", "SI.Visit.Num", "SI.Component.Flag"))



m <- lm(dwf.rate ~ SI.Component.Flag + class.size + URM + First.Gen.Perc, data=course.level)

m1 <- lm(dwf.rate ~ URM, data = course.level)
m2 <- lm(dwf.rate ~ SI.Component.Flag, data = course.level)
m3 <- lm(dwf.rate ~ URM + SI.Component.Flag, data = course.level)
m4 <- lm(dwf.rate ~ SI.Component.Flag + First.Gen.Perc + HS.GPA.Ave + class.average + class.size + URM, data = course.level)
summary(m3)

#Increase in SI, class size negative, URM and First Gen positive
# URM positive
# SI positive
# Both significant and positive
# Class average and size both negative and significant, URM is positive
dwf.equity <- group_by(course.level, URM)
dwf.equity <- summarise(dwf.equity, dwf.sum = sum(dwf.rate))
dwf.equity

dod <- course.level %>% group_by(URM, SI.Component.Flag)
summary(lm(dwf.rate ~ SI.Component.Flag + URM, data=dod))

ggplot(dod, aes(SI.Component.Flag, dwf.rate, color=URM)) + geom_point() + geom_smooth() + xlab("Attended SI") + ylab("DWF Rate")
ggplot(dod, aes(dwf.rate, color=URM)) + geom_density(alpha = 0.2) + theme_bw(base_size = 10) + xlab("DWF Rate")
ggplot(dod, aes(SI.Component.Flag, dwf.rate, color=URM)) + geom_boxplot() + xlab("Attended SI") + ylab("DWF Rate") 

ggplot(dod, aes(URM, dwf.rate)) + geom_boxplot() + facet_wrap(~SI.Component.Flag) # Use
ggplot(dod, aes(First.Gen.Perc, dwf.rate)) + geom_point(alpha = 0.5) + facet_wrap(~SI.Component.Flag) # Use



write.csv(course.level, "~/Downloads/course_analysis_dataset.csv")

m2 <- lm(dwf.rate ~ SI.Component.Flag + First.Gen.Perc + HS.GPA.Ave + class.average + class.size + URM, data = course.level)
summary(m2)

m1 <- lm(dwf.rate ~ URM + SI.Component.Flag, data = course.level)
summary(m1)

m <- lm(dwf.rate ~ SI.Component.Flag + class.size + URM + First.Gen.Perc, data=course.level)
summary(m)

plot(m)
library(broom)
ggplot(augment(m1), aes(URM, dwf.rate, color=SI.Component.Flag) + geom_boxplot() + xlab("Attended SI") + ylab("DWF Rate"))

ggplot(dod, aes(URM, dwf.rate, color=SI.Component.Flag)) + geom_point() + geom_smooth(method='lm') + xlab("Attended SI") + ylab("DWF Rate")


ggplot(course.level, aes(dwf.rate, class.average, color=URM)) + geom_point(alpha=0.1) + geom_smooth(method=lm)

ggplot(course.level, aes(dwf.rate, First.Gen.Perc)) + geom_point()

ggplot(dod, aes(dwf.rate, SI.Component.Flag, color=URM)) +geom_smooth() + geom_density(alpha = 0.2) + theme_bw(base_size = 10) + xlab("DWF Rate") 

ggplot(course.level, aes(URM, First.Gen.Perc, color=SI.Component.Flag)) + geom_boxplot() # First gen

ggplot(dod, aes(URM, dwf.rate)) + geom_boxplot(alpha = 0.5) + facet_wrap(~SI.Component.Flag) + geom_smooth(method=lm)

*Course level displays the DWF rates of a course at Chico State and we address the impact SI has on DFW rates. We consider the question,
"Will SI have an impact on reducing equity gaps in the overall pass rate for a course" 

Figure 1 compares DWF Rate of URM and Non-URM students that did not attend SI (column 0) and students
that did attend SI (column 1). Figure 2 compares DWF Rate of the percentage of students that 
are First Generation who did not attend SI (column 0) and students that did attend SI (column 1). Both figures
show SI reduces the DWF rates of equity gaps.
ggplot(dod, aes(URM, dwf.rate)) + geom_boxplot() + facet_wrap(~SI.Component.Flag) + ylab("DWF Rate")
ggplot(dod, aes(First.Gen.Perc, dwf.rate)) + geom_point(alpha = 0.5) + facet_wrap(~SI.Component.Flag) +
  xlab("Percentage of First Generation") + ylab("DWF Rate")




lm1 <- lm(dwf.rate ~ URM + First.Gen.Perc + SI.Component.Flag, data=course.level)
summary(lm1)
model1 <- tidy(lm1) %>%
  filter(term == "URMURM" | term == "First.Gen.Perc" | term == "SI.Component.Flag1")
dwplot(i) + geom_vline(xintercept = 0, colour = "black",linetype = 2) + ggtitle("Model Results DWF Rate and Equity Gaps") + xlab("Confidence Interval") + theme_few() +
  theme(legend.position = "none", axis.title.x = element_text(size = 20), title = element_text(size = 32))

The model results show URM, First Generation, and SI is positively related to DWF rates # May have interpreted this wrong??

