library(readxl)
program <- read_excel("code_books/studen_program_codebook.xlsx")
profile <- read_excel("code_books/student_profile_codebook.xlsx")
detail <- read_excel("code_books/course_detail_codebook.xlsx")
detail

program$'Variable Name'
detail$Notes
detail$Question
sum(is.na(detail$Question)) #131
sum(is.na(detail$Label)) #121
sum(is.na(detail$'Qualtrics Variables')) #147
sum(is.na(detail$'Paper Survey Number')) #148
sum(is.na(detail$Values)) #13
detail.naa <- filter(detail, is.na(Values))
detail.naa

# Breif Corhort - a group of students
profile <- profile %>%
  slice(-c(7,10))
profile

program$Values 

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




