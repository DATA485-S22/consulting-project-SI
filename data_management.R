# Data Management File

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
  summarise(n = sum(SLC.Attended.Flag))

course.level <- course.level %>% left_join(temp3, by = 'Random.Course.ID')

# Add covariates from course detail file
course.detail <- read.csv("data/Course Detail.csv") 
course.level <- left_join(course.level, course.detail, by = 'Random.Course.ID')

# Create flag for SI component
course.level$n[is.na(course.level$n)] <- 0
course.level$SI.Component.Flag <- course.level$n
course.level$SI.Component.Flag[course.level$SI.Component.Flag > 0] <- 1
course.level$SI.Component.Flag <- as.factor(course.level$SI.Component.Flag)

write.csv(course.level, 'data/course_level.csv')
