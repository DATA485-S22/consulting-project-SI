# The impact of Supplemental Instruction on Equity Gaps at Chico State

## Introduction
* Supplemental Instruction (**SI**) is designed to increase student success in historically difficult courses by utilizing peer-assisted study sessions. The SI leader has performed well in the course in a previous semester.

* An **equity gap** refers to differences in educational outcomes and student success across race/ethnicity, socioeconomic status, gender, physical or mental abilities, and other demographic traits and intersectionalities.

## Workflow
Before running any files, you must first download the data files from the secure Box folder and put them into a folder called "data" in the main project repository on your local system. This data is private and must not be seen by eyes not working on the project. After downloading and moving data, run the entire file called "data_management.R". This will create many csv files (described below) and many data sets in your local environment. After it is done running, you can click the broomstick to clear your environment.

_Data Sets Created:_

- **grades.csv**
  - Copy of the grades Excel file to be a more light-weight format
- **course_level.csv**
  - Course information (PER SECTION, see codebook for more info)
  - Year, Term (only Fall or Spring), subject name and number
  - Section class size, GPA average, DWF rate of section (derived from grades data)
  - SI component flag, total SI visits in semester
  - Other course info from original courses dataset
  - **Notes:** Sections with less than 10 visits to SI during the semester do not have the SI component flag set, these sections will be considered non-SI component sections. This dataset contains all classes that are not specially numbered (see CSU number reference) and within the range [Spring 2016 - Fall 2019]. This range is selected because SI first started in 2016, and students had the option to drop classes during the COVID-19 pandemic without incurring penalties.
- **student_profiles_clean.csv**
  - Clean mutating join of student profiles and student program datasets
  - (PER STUDENT, see codebook for more info) Gender, IPEDS Ethnicity, First-Generation Student Flag, academic level, major(s)
  - Enrollment term, degree term, full-time/part-time, highschool/transfer GPA, one year retention flag
  - **Notes:** Only profiles and programs with a date after 2016 are used. The student programs dataset contained multiple rows per student, an attempt was made to select the most recent program record. Aproximately 35% of students in the programs dataset are not present in the profiles dataset. These entries have NA values for columns [15-26].
- **si_grades.csv**

-----

### References 
* Building your zombie apocalypse team: https://www.bamboohr.com/blog/6-employee-types-for-your-zombie-apocalypse-team/

* Github issues https://docs.github.com/en/issues/tracking-your-work-with-issues/about-issues
* Github project boards: https://docs.github.com/en/issues/organizing-your-work-with-project-boards/managing-project-boards/about-project-boards 
  Tracking projects: https://docs.github.com/en/issues/organizing-your-work-with-project-boards/tracking-work-with-project-boards/adding-issues-and-pull-requests-to-a-project-board
