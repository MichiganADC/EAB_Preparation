# EAB_Preparation_2019_OR_Core.R

# **************
# Libraries ----

# Database
library(DBI)
# Munging
library(dplyr)
library(readr)
library(stringr)




# ********************
# Config / Helpers ----

source("~/Box Sync/Documents/R_helpers/config.R")
source("~/Box Sync/Documents/R_helpers/helpers.R")


# ***************************************
# Connect to dumped MiNDSet Registry ----

df_ms <- 
  readRDS("~/Box Sync/Documents/MADC_gen/MADC_Data_Dump/2019-07-31/df_ms.Rds") %>% 
  # filter(!is.na(exam_date)) %>% 
  distinct(subject_id, exam_date, .keep_all = TRUE)


# ********************************************
# Connect to dumped `UM MAP - UDS 3` data ----

df_u3 <-
  readRDS("~/Box Sync/Documents/MADC_gen/MADC_Data_Dump/2019-07-31/df_u3.Rds") %>% 
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  filter(!is.na(form_date)) %>% 
  distinct(ptid, form_date, .keep_all = TRUE)

ids_u3 <-
  df_u3 %>% 
  distinct(ptid) %>% 
  pull %>% 
  sort

# *********************************************************
# Left join df_u3 + df_m2 matching IDs and visit dates ----

# df_u3_ms <-
#   left_join(df_u3, df_ms, 
#             by = c("ptid" = "subject_id", "form_date" = "exam_date"))
df_u3_ms <-
  readRDS("2019/df_u3_ms.Rds")

# ********************************
# Connect to Studies Database ----

df_st <-
  readRDS("~/Box Sync/Documents/MADC_gen/MADC_Data_Dump/2019-07-31/df_st.Rds")


# # *********************************************
# # Connect to `madc_integ` DB with UDS data ----
# 
# con <- 
#   dbConnect(RPostgres::Postgres(),
#             service = "madcbrain pgsql madc_integ", # ~/.pg_service.conf
#             user     = rstudioapi::askForPassword("PostgreSQL Username:"),
#             password = rstudioapi::askForPassword("PostgreSQL Password:"))


# **************
# Questions ----


# *****************
# _ Question 1 ----
# Q: Total # of participants in the MiNDSet Registry?
# A: 3395 (total)
# A: 1117 (RVF date >= 2015-01-01)

df_ms %>% 
  filter(!is.na(date)) %>% 
  filter(date >= "2015-01-01") %>% 
  distinct(subject_id) %>% 
  nrow


# *****************
# _ Question 2 ----
# Q: Total # of participants in the UM-MAP Cohort?
# A: 407

df_u3_ms %>% 
  distinct(ptid) %>% 
  nrow


# *****************
# _ Question 3 ----
# Q: Recruitment Source - "How did you hear about the MADC?" on RVF
# A: [seet tables below]

#' `owndoc`         ~ `Clinic`
#' `madc_website`   ~ `Marketing`
#' `madcnewsletter` ~ `Marketing`
#' `alz_assoc`      ~ `Recruitment Event`
#' `radio_announc`  ~ `Marketing`
#' `tv`             ~ `Marketing`
#' `event`          ~ `Recruitment Event`
#' `health_fair`    ~ `Recruitment Event`
#' `referred_from`  ~ `Research Study`
#' `other`          ~ `Other`            
#'   + `referred_ifother`

fields_referral <-
  c(
    "owndoc"
    , "madc_website"
    , "madcnewsletter"
    , "alz_assoc"
    , "radio_announc"
    , "tv"
    , "event"
    , "health_fair"
    , "referred_from"
    , "other"
    , "referred_ifother"
  )

df_ms_ref <-
  df_ms %>%
  filter(subject_id %in% ids_u3) %>% 
  select(subject_id
         , exam_date
         , fields_referral) %>% 
  get_nonempty_records(fields_referral)

if (sum(duplicated(df_ms_ref$subject_id)) == 0) {
  df_ms_ref_mut <- 
    df_ms_ref %>% 
    mutate(recr_source = case_when(
      owndoc == 1             ~ "Clinic",
      madc_website == 1       ~ "Marketing",
      madcnewsletter == 1     ~ "Marketing",
      alz_assoc == 1          ~ "Recruitment Event",
      radio_announc == 1      ~ "Marketing",
      tv == 1                 ~ "Marketing",
      event == 1              ~ "Recruitment Event",
      health_fair == 1        ~ "Recruitment Event",
      !(is.na(referred_from)) ~ "Research Study",
      other == 1              ~ "Other",
      TRUE                    ~ NA_character_
    )) %>% 
    filter(!is.na(recr_source))
}

df_recr_source <-
  df_ms_ref_mut %>% 
  group_by(recr_source) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

df_recr_source_other <-
  df_ms_ref_mut %>% 
  filter(recr_source == "Other") %>% 
  filter(!is.na(referred_ifother)) %>% 
  arrange(referred_ifother) %>% 
  group_by(referred_ifother) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


# *****************
# _ Question 4 ----
# Q: # of people referred from the registry to our supported studies?
# A: 771

ids_ms <- 
  df_ms %>% 
  filter(!is.na(date)) %>% 
  filter(date >= "2015-01-01") %>% 
  distinct(subject_id) %>% 
  pull %>% 
  sort

df_st %>% 
  filter(subject_id %in% ids_ms) %>% 
  distinct(subject_id) %>% 
  nrow


# *****************
# _ Question 5 ----

# ******************
# _ _ Question 5a ----
# Q: Of those referred to our supported studies, how many were enrolled?
# A: 595

df_st %>% 
  filter(subject_id %in% ids_ms) %>% 
  filter(enrolled == 1) %>% 
  distinct(subject_id) %>% 
  nrow

# ******************
# _ _ Question 5b ----
# Q: Of those referred to our supported studies, how many were enrolled and who
#    are a minority?
# A: 255

df_ms_race <-
  df_ms %>% 
  select(subject_id, exam_date, race_value) %>% 
  arrange(subject_id, exam_date) %>% 
  filter(!duplicated(.$subject_id)) %>% 
  select(-exam_date) %>% 
  filter(!is.na(race_value))

df_st_race <-
  left_join(df_st, df_ms_race, by = c("subject_id" = "subject_id"))

df_st_race %>% 
  filter(subject_id %in% ids_ms) %>% 
  filter(enrolled == 1) %>% 
  filter(race_value != 1) %>% 
  distinct(subject_id) %>% 
  nrow


# *****************
# _ Question 6 ----
# (Answer available via MADC Dashboard)

# ********************
# _ _ Question 6a ----
# Q: Of total # of participants in the UM-MAP cohort, how many agreed to brain 
#    donation?
# A: 174

# ********************
# _ _ Question 6b ----
# Q: Of total # of participants in the UM-MAP cohort, how many are considering 
#    brain donation?
# A: 221

# ********************
# _ _ Question 6c ----
# Q: Of total # of participants in the UM-MAP cohort, how many refused brain 
#    donation?
# A: 5

# We don't have autopsy consent data for 7 participants.


# *****************
# _ Question 7 ----
# Q: Number of African Americans in UM-MAP?
# (Answer available via MADC Dashboard)
# A: 157


# *****************
# _ Question 8 ----
# Q: Numer of African Americans referred to a supported study from the registry?
# A: 243

df_st_race %>% 
  filter(subject_id %in% ids_ms) %>% 
  filter(enrolled == 1) %>% 
  filter(race_value == 2) %>% 
  distinct(subject_id) %>% 
  nrow



# ***********************
# Disconnect from DB ----

dbDisconnect(con)



