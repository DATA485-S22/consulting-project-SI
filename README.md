# The impact of Supplemental Instruction on Equity Gaps at Chico State

## Introduction
* Supplemental Instruction (**SI**) is designed to increase student success in historically difficult courses by utilizing peer-assisted study sessions. The SI leader has performed well in the course in a previous semester.

* An **equity gap** refers to differences in educational outcomes and student success across race/ethnicity, socioeconomic status, gender, physical or mental abilities, and other demographic traits and intersectionalities.

* **Underrepresented Minority (URM)** - is defined as a U.S. citizen who identifies as Black/African American, Hispanic/Latino, or American Indian. All other Race/Ethnicity categories or Non-U.S. citizens are considered as a **Non-Underrepresented Minority (Non-URM)**.

* **First generation** - is defined as a student who reported both parents as not receiving a baccalaureate degree. All other students are considered as **Not First Generation**.

[Definition Source](https://www.calstate.edu/data-center/institutional-research-analyses/Pages/Glossary.aspx)

## Workflow
Before running any files, you must first download the data files from the secure Box folder and put them into a folder called "data" in the main project repository on your local system. This data is private and must not be seen by eyes not working on the project. After downloading and moving data, run the entire file called "data_management.R". This will create many csv files (described below) and many data sets in your local environment. After it is done running, you can click the broomstick to clear your environment.

### Data Sets Created:
*Please carefully read the caveats of each dataset!*

- **grades.csv**
  - Copy of the grades Excel file to be a more light-weight format
- **course_level.csv**
  - Course information (PER SECTION, see codebook for more info)
  - Year, Term (only Fall or Spring), subject name and number
  - Section class size, GPA average, DWF rate of section (derived from grades data)
  - SI component flag, total SI visits in semester
  - Other course info from original courses dataset
  - **Notes:**
    - Sections with less than 10 visits to SI during the semester do not have the SI component flag set, these sections will be considered non-SI component sections.
    - This dataset contains all classes that do not have a special number ([see CSU Chico Numbering Policy](https://www.csuchico.edu/pres/em/2017/17-012.shtml))
    - All records are within the range [Spring 2016 - Fall 2019]. This range is selected because SI first started in 2016, and students had the option to drop classes during the COVID-19 pandemic without incurring penalties.
- **student_profiles_clean.csv**
  - Clean mutating join of student profiles and student program datasets
  - (PER STUDENT, see codebook for more info) Gender, IPEDS Ethnicity, First-Generation Student Flag, academic level, major(s)
  - Enrollment term, degree term, full-time/part-time, high school/transfer GPA, one year retention flag
  - **Notes:**
    - Only profiles and programs with a date after 2016 are used.
    - The student programs dataset contained multiple rows per student, an attempt was made to select the most recent program record (see data_management.R lines 91-93).
    - Approximately 35% of students in the programs dataset are not present in the profiles dataset. These entries have NA values for columns [15-26].
- **grades_SI_classes.csv**
  - Contains grades of all students for all courses with SI
  - Includes a flag for at least one SI attendance number of SI visits in the semester
- **CEM_full_dataset.csv**
  - Merging of cleaned profiles, grades, and course level information
  - **Notes:**
    - Records with missing values in the following columns are dropped due to not enough information for matching. [HS.GPA, Student.Orientation.Flag, Major.1.STEM.Flag, Full.Time.Part.Time.Code, Academic.Program]
    - This dataset was built using **course_level.csv** and **student_profiles_clean.csv**, so it contains the same caveats as them both.
- **student_analysis_dataset.csv**
  - Used in the 2022 NSC poster code files to create logistic models predicting one year retention
  - Merging of cleaned profiles, grades, and original course (not course level) information
  - **Notes:**
    - Records with missing values in the following columns are dropped. [HS.GPA, Student.Orientation.Flag, Major.1.STEM.Flag, Full.Time.Part.Time.Code, Academic.Program]
    - This dataset was built using **student_profiles_clean.csv**, so it contains the same caveats.
- **course_analysis_dataset.csv**
  - Used in the 2022 NSC poster code files to create a linear regression model predicting course section DWF rate
  - (PER SECTION, see _codebook_[need to create codebook] for more info) Class size, high school GPA average of students, percentage of first generation students, percentage of under-represented minority students, DWF rate, SI component flag, term year
  - **Notes:**
    - A number of student records contain missing values for high school GPA, URM status, or first generation status. These students are not included in the calculation of average GPA and are considered non-URM or not first generation, respectfully.
    - Only course records between 2016 and 2019, inclusive, are represented.
    - Since a miniscule proportion of courses with SI are upper division, the decision was made to only include courses with a number less than 300.
    - In an attempt to remove high DWF outliers, only course sections with a class size 20 or greater are considered.
    - This dataset was created using **course_level.csv** and **student_profiles_clean.csv**, so it contains the same caveats as them both.

-----

### References 
* Building your zombie apocalypse team: https://www.bamboohr.com/blog/6-employee-types-for-your-zombie-apocalypse-team/

* Github issues https://docs.github.com/en/issues/tracking-your-work-with-issues/about-issues
* Github project boards: https://docs.github.com/en/issues/organizing-your-work-with-project-boards/managing-project-boards/about-project-boards 
  Tracking projects: https://docs.github.com/en/issues/organizing-your-work-with-project-boards/tracking-work-with-project-boards/adding-issues-and-pull-requests-to-a-project-board
