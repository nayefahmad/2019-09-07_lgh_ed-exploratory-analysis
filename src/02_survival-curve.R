
#'--- 
#' title: "LGH ED - Survival curve"
#' author: "Nayef Ahmad"
#' date: "2019-09-09"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 

#+ lib, include = FALSE 
library(tidyverse)
library(lubridate)
library(denodoExtractor)
library(DT)
library(survival)
library(ggfortify)

setup_denodo()

#+ rest
# pull ED data: ------------
# today_date <- "20190901"
today_date <- Sys.Date() %>% as.character() %>% gsub("-", "", .)

vw_ed_real_time <<- dplyr::tbl(cnx, dbplyr::in_schema("publish", 
                                                      "emergency_nrt"))

df1.ed_data <- 
  vw_ed_real_time %>% 
  filter(facility_name == "LGH Lions Gate", 
         start_date_id == today_date) %>% 
  select(start_date_id, 
         start_dt_tm, 
         start_to_left_ed_elapsed_time_minutes, 
         first_triage_acuity_cd, 
         age_at_start_date) %>% 
  collect()

# df1.ed_data
# str(df1.ed_data)
# summary(df1.ed_data)

df2.ed_modified <- 
  df1.ed_data %>% 
  rename(ed_los = start_to_left_ed_elapsed_time_minutes) %>% 
  
  mutate(time_now = ymd_hms(Sys.time()), 
         ed_los_filled = case_when(is.na(ed_los) ~ as.numeric(time_now - start_dt_tm, 
                                                              units = "hours"), 
                                   TRUE          ~ as.numeric(ed_los/60)), 
         left_ed = ifelse(is.na(ed_los), 0, 1)) %>% 
  
  select(start_dt_tm,
         ed_los, 
         ed_los_filled, 
         left_ed)

# df2.ed_modified



# survival model: -------
km1 <- survfit(Surv(ed_los_filled, left_ed) ~ 1, 
               data = df2.ed_modified)



p1.survival <- autoplot(km1,
                        conf.int = FALSE, 
                        firsty = 1, 
                        surv.colour = "skyblue4",
                        surv.size = 1.5, 
                        censor.size = 5, 
                        censor.colour = "firebrick") + 
  scale_y_continuous(limits = c(0,1), 
                     expand = c(0, 0), 
                     breaks = seq(0, 1, .1)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(0, 50000, 1)) + 
  geom_vline(xintercept = 10, 
             col = "grey70") + 
  labs(x = "Time from start in ED (hours)",
       y = "Probability of staying longer than specified time", 
       title = sprintf("LGH ED - Patients arriving on %s", 
                       ymd(today_date)), 
       subtitle = sprintf("%i patients arrived at the ED so far today", 
                          nrow(df2.ed_modified))) + 
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey95"), 
        panel.grid.major = element_line(colour="grey95")); p1.survival


#' ## How to read this curve
#'
#' [This paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3932959/) is a good
#' general discussion on Kaplan-Meier curves. Here are some specifics related to
#' the ED LOS context:
#'
#' * The x-axis is time, representing ED LOS. On this graph, the exact start
#' time of a patient doesn't matter, only the ED LOS does.
#'
#' * The blue line shows the pattern of patient discharges, according to their
#' ED LOS. Whenever the line dips downward, that means that a patient was
#' discharged after spending that amount of time in the ED. This pattern allows
#' us to choose any LOS cutoff and see how many patients were achieving that
#' goal. For example, we can ask, *"What proportion of patients had ED LOS < 4
#' hours"* The answer is given by `1 - y`, where `y` is the reading on the
#' y-axis of the graph
#'
#' * Red points represent patients **still in ED**. Although we don't know their
#' final ED LOS yet, we can track how long they have already been in ED, and use
#' the blue curve to try to predict how much longer they may be there.

