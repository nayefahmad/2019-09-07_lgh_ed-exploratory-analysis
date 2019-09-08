

#'--- 
#' title: "LGH ED - Wait times analysis"
#' author: "Nayef Ahmad"
#' date: "2019-09-07"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---

#+ lib, include = FALSE 
library(tidyverse)
library(lubridate)
library(denodoExtractor)
library(DT)


setup_denodo()

#+ rest
# pull ED data: ------------

df1.ed_data <- 
  vw_eddata %>% 
  filter(facility_name == "LGH Lions Gate", 
         start_date_id >= 20170101, 
         start_date_id < 20190906) %>% 
  select(start_date_id, 
         start_to_admit_elapsed_time_id, 
         start_to_admit_elapsed_time_minutes, 
         start_to_left_ed_elapsed_time_id, 
         start_to_left_ed_elapsed_time_minutes, 
         is_admitted, 
         patient_svc_cd, 
         patient_svc_desc) %>% 
  collect()

# str(df1.ed_data)
# summary(df1.ed_data)

# data wrangling: ------------
df2.ed_data_modified <- 
  df1.ed_data %>% 
  mutate(start_date = ymd(start_date_id), 
         start_year = year(start_date) %>% as.factor, 
         patient_svc_cd = as.factor(patient_svc_cd) ,
         is_admitted = as.factor(is_admitted)) %>% 
  mutate(is_admitted = fct_recode(is_admitted, 
                                  not_admitted = "0", 
                                  admitted = "1")) %>% 
  rename(ed_los = start_to_left_ed_elapsed_time_minutes, 
         admit_los = start_to_admit_elapsed_time_minutes) %>% 
  select(start_date, 
         start_date_id,
         start_year, 
         is_admitted, 
         ed_los,
         admit_los)


str(df2.ed_data_modified)  
summary(df2.ed_data_modified)  


#' What's with the NAs in the time fields? 
# data quality checks: -------------
df2.ed_data_modified %>% 
  filter(is.na(ed_los)) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


df2.ed_data_modified %>% 
  filter(is.na(admit_los), 
         is_admitted == "admitted") %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' Unreasonably large values: 
df2.ed_data_modified %>% 
  filter(admit_los > 5000 | 
           ed_los > 5000) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))



#+ distributions
# densities: ---------------
df2.ed_data_modified %>% 
  select(ed_los, 
         start_year, 
         is_admitted) %>% 
  filter(ed_los < quantile(ed_los, .99, na.rm = TRUE)) %>% 
  
  ggplot(aes(x = ed_los)) +
  geom_density() + 
  facet_wrap(~start_year + is_admitted) + 
  geom_vline(xintercept = 600, col = "blue")


#' Things to investigate:
#'
#' 1. Any change in dist for the not_admitted encounters from one year to the
#' next?
#'
#' 2. Among admitted encntrs, there's clearly several different distributions
#' smooshed together. Can we separate them by CTAS? Using variable `is_ed_admit_p4p_definition`? 