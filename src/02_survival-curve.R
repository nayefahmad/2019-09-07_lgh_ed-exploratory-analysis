
#'--- 
#' title: "LGH ED - Visualizing ED LOS with survival curves"
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
         age_at_start_date, 
         is_admitted) %>% 
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
         left_ed = ifelse(is.na(ed_los), 0, 1), 
         is_admitted = as.factor(is_admitted) %>% fct_recode(non_admit = "0", 
                                                             admit = "1")) %>% 
  
  select(start_dt_tm,
         ed_los, 
         ed_los_filled, 
         left_ed, 
         is_admitted)

# df2.ed_modified


#' ## All ED visits 
#+ model1, message = FALSE
# survival model 1: -------
km1 <- survfit(Surv(ed_los_filled, left_ed) ~ 1, 
               data = df2.ed_modified)

num_still_in_ed <- 
  df2.ed_modified %>% 
  filter(left_ed == 0) %>% 
  pull %>% 
  as.numeric() %>% 
  sum
  

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
       subtitle = sprintf("%i patients arrived at the ED so far today \n%i patients admitted \n%i still in ED", 
                          nrow(df2.ed_modified), 
                          df2.ed_modified %>% filter(is_admitted == "admit") %>% nrow(), 
                          num_still_in_ed), 
       caption = sprintf("\n\nReport created at %s", 
                         Sys.time())) + 
  theme_light() + 
  theme(panel.grid.minor = element_line(colour="grey95"), 
        panel.grid.major = element_line(colour="grey95")); p1.survival



#' ## ED admits vs non-admits 
#+ model2, message = FALSE
# survival model 2: -----------
km2 <- survfit(Surv(ed_los_filled, left_ed) ~ is_admitted, 
               data = df2.ed_modified)

num_admits <- 
  df2.ed_modified %>% 
  filter(is_admitted == "admit") %>% 
  nrow()

if (num_admits < 1){
  print("No admits so far today")
  
} else {
  p2.survival <- autoplot(km2,
                          conf.int = FALSE, 
                          firsty = 1, 
                          #surv.colour = "skyblue4",
                          surv.size = 1.5, 
                          censor.size = 5, 
                          censor.colour = "black") + 
    scale_y_continuous(limits = c(0,1), 
                       expand = c(0, 0), 
                       breaks = seq(0, 1, .1)) + 
    scale_x_continuous(expand = c(0, 0), 
                       breaks = seq(0, 50000, 1)) + 
    geom_vline(xintercept = 4, 
               col = "grey70") + 
    geom_vline(xintercept = 10, 
               col = "grey70") + 
    labs(x = "Time from start in ED (hours)",
         y = "Probability of staying longer than specified time", 
         title = sprintf("LGH ED - Patients arriving on %s", 
                         ymd(today_date)), 
         subtitle = sprintf("%i patients arrived at the ED so far today \n%i patients admitted \n%i still in ED", 
                            nrow(df2.ed_modified), 
                            df2.ed_modified %>% filter(is_admitted == "admit") %>% nrow(), 
                            num_still_in_ed),
         caption = sprintf("\n\nReport created at %s", 
                           Sys.time())) + 
    theme_light() + 
    theme(panel.grid.minor = element_line(colour="grey95"), 
          panel.grid.major = element_line(colour="grey95"), 
          legend.position = "bottom"); p2.survival
  
  
}




#' ## How to read these curves
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
#' * Red/black points represent patients **still in ED**. Although we don't know
#' their final ED LOS yet, we can track how long they have already been in ED,
#' and use the blue curve to try to predict how much longer they may be there.
#'
#'
#' ## Advantages of the survival curve
#'
#' You could get a similar viz by taking all ED LOS, and showing as a bar chart,
#' arranged from lowest to max LOS. You could use colour to highlight the bars
#' that are patients still in ED
#'
#' I think the survival curve is better because:
#'
#' 1. Much easier to compare across days, sites, different timescales, because
#' the y-axis always standardized to be between 0 and 1.
#'
#' 2. Easy to read off percentiles from the y-axis.
#'
#' 3. Shape of the graph is not distorted by the censored observations (pts
#' still in ED). This correctly reflects the fact that we do not yet know their
#' ED LOS.
#'
#' 4. Opens the door for more advanced methods: we can easily get confidence
#' intervals, log-rank tests to test whether 2 different sites/time periods are
#' significantly different in terms of their LOS patterns.


# output: -----
ggsave(here::here("results", 
                  "dst", 
                  sprintf("%s_lgh_ed-los-survival-curves.pdf", 
                          Sys.time() %>% gsub(":", ".", .))), 
       p2.survival)
             


