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



library(readr)
library(kableExtra)
library(broom)
library(dotwhisker)
library(dplyr)
library(ggthemes)
course.level <- read_csv("data/course_analysis_dataset.csv")
str(course.level)

course.level$Term.Year<-factor(course.level$Term.Year)
course.level$SI.Component.Flag<-factor(course.level$SI.Component.Flag)


#lm1 <- lm(dwf.rate ~ URM*SI.Component.Flag + First.Gen.Perc*SI.Component.Flag + class.size + Term.Year + HS.GPA.Ave, data=course.level, na.action = na.omit)
summary(lm1)
model1 <- tidy(lm1)
dwplot(model1) + geom_vline(xintercept = 0, colour = "black",linetype = 2) + ggtitle("Model Results DWF Rate and Equity Gaps") + xlab("Confidence Interval") + theme_few() +
  theme(legend.position = "none", axis.title.x = element_text(size = 20), title = element_text(size = 32))
d1 <- data.frame(model1)
kable(d1)


# First gen is only statistically significant
lm2 <- lm(dwf.rate ~ SI.Component.Flag + URM + First.Gen.Perc + Term.Year, data = course.level)
summary(lm2)
model2 <- tidy(lm2)
dwplot(model2) + geom_vline(xintercept = 0, colour = "black",linetype = 2) + ggtitle("Model Results DWF Rate and Equity Gaps") + xlab("Confidence Interval") + theme_few() +
  theme(legend.position = "none", axis.title.x = element_text(size = 20), title = element_text(size = 32))
d2 <- data.frame(model2)
kable(d2)

#First gen is still statistically significant 
lm3 <- lm(dwf.rate ~ SI.Component.Flag + URM + First.Gen.Perc + Term.Year + HS.GPA.Ave + URM*SI.Component.Flag + First.Gen.Perc*SI.Component.Flag, data=course.level, na.action = na.omit)
summary(lm3)
model3 <- tidy(lm3)
dwplot(model3) + geom_vline(xintercept = 0, colour = "black",linetype = 2) + ggtitle("Model Results DWF Rate and Equity Gaps") + xlab("Confidence Interval") + theme_few() +
  theme(legend.position = "none", axis.title.x = element_text(size = 20), title = element_text(size = 32))
d3 <- data.frame(model3)
kable(d3)

#lm4 <- lm(dwf.rate ~ URM + SI.Component.Flag + First.Gen.Perc + URM*SI.Component.Flag + First.Gen.Perc*SI.Component.Flag, data=course.level, na.action = na.omit)
summary (lm4)

lm5 <- lm(dwf.rate ~ SI.Component.Flag*First.Gen.Perc, data = course.level)
summary(lm5)

lm6 <- lm(dwf.rate ~ SI.Component.Flag + URM + First.Gen.Perc + Term.Year + HS.GPA.Ave + class.average, data = course.level)
summary(lm6)

library(ggplot2)
#ggplot(course.level, aes(dwf.rate, URM, group = 1)) + facet_wrap(~Term.Year) + geom_boxplot() 
#ggplot(course.level, aes(dwf.rate, First.Gen.Perc, group = 1)) + facet_wrap(~Term.Year) + geom_boxplot()

ggplot(course.level, aes(Term.Year, dwf.rate, color = URM)) + 
  geom_line() +
  scale_y_continuous(limits = c(0, max(course$dwf.rate)))
ggplot(course.level, aes(Term.Year, dwf.rate, color = First.Gen.Perc)) + 
  geom_line() +
  scale_y_continuous(limits = c(0, max(course.level$dwf.rate)))

# These graphs show dwf of the percentages of equity gaps in a course in each year
ggplot(course.level, aes(URM, dwf.rate, color = URM)) + geom_point() + facet_wrap(. ~Term.Year) + xlab("Percent URM") + ylab("DWF Rate") + theme(legend.position="none") + geom_smooth(se=F)
ggplot(course.level, aes(First.Gen.Perc, dwf.rate, color = First.Gen.Perc)) + geom_point() + facet_wrap(. ~Term.Year) + xlab("Percent First Gen") + ylab("DWF Rate") + theme(legend.position="none") + geom_smooth(se=F)
