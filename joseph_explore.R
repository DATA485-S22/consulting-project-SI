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
  filter(Term.Year >= 2016)

courses$Term.Type <- factor(courses$Term.Type)
levels(courses$Term.Type)
courses <- courses %>%
  filter(Term.Type %nin% c("Summer", "Winter"))
courses$Term.type <- droplevels(courses$Term.Type)
levels(courses$Term.Type)

