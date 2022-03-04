# Joseph Shifman's exploring file
library(tidyverse)
library(magrittr) # Allows %<>% notation to update lhs object with resulting value
library(naniar)

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

program$Major.1.STEM.Flag <- replace(program$Major.1.STEM.Flag, program$Major.1.STEM.Flag == "", "N")

ggplot(data = program) +
  geom_bar(aes(x = Academic.Level.Code))

program %>%
  group_by(Random.Student.ID) %>%
  summarise(no_rows = length(Random.Student.ID))

# Keep only most recent term record per student
# Consider re-doing using `Term`
program$College.Year <- factor(program$College.Year,
                               levels = c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16",
                                          "2016-17", "2017-18", "2018-19", "2019-20", "2020-21",
                                          "2021-22"),
                               ordered = is.ordered(program$College.Year))
program$Term.Type <- factor(program$Term.Type,
                       levels = c("Intersession", "Summer", "Fall", "Winter", "Spring"),
                       ordered = is.ordered(program$Term.Type))
program <- program %>%
  group_by(Random.Student.ID) %>%
  arrange(College.Year, Term.Type) %>%
  filter(row_number()==1)

program %>%
  group_by(Random.Student.ID) %>%
  summarise(no_rows = length(Random.Student.ID))
