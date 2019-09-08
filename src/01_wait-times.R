

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
#'     toc_depth: 4
#' ---

#+ lib, include = FALSE 
library(tidyverse)
library(lubridate)
library(denodoExtractor)
library(DT)
library(broom)
# library(fitdistrplus)

setup_denodo()

#+ rest
# pull ED data: ------------

df1.ed_data <- 
  vw_eddata %>% 
  filter(facility_name == "LGH Lions Gate", 
         start_date_id >= 20170101, 
         start_date_id < 20190906) %>% 
  select(start_date_id, 
         start_dt_tm, 
         start_to_admit_elapsed_time_id, 
         start_to_admit_elapsed_time_minutes, 
         start_to_left_ed_elapsed_time_id, 
         start_to_left_ed_elapsed_time_minutes, 
         is_admitted, 
         patient_svc_cd, 
         patient_svc_desc, 
         is_ed_admit_p4p_definition, 
         first_triage_acuity_cd, 
         age_at_start_date) %>% 
  collect()

# str(df1.ed_data)
# summary(df1.ed_data)

# data wrangling: ------------
df2.ed_data_modified <- 
  df1.ed_data %>% 
  mutate(start_date = ymd(start_date_id), 
         start_hour = ymd_hms(start_dt_tm) %>% hour(), 
         start_year = year(start_date) %>% as.factor, 
         patient_svc_cd = as.factor(patient_svc_cd) ,
         is_admitted = as.factor(is_admitted), 
         is_ed_admit_p4p_definition = as.factor(is_ed_admit_p4p_definition), 
         first_triage_acuity_cd = as.factor(first_triage_acuity_cd)) %>% 
  
  # recode is_admitted
  mutate(is_admitted = fct_recode(is_admitted, 
                                  not_admitted = "0", 
                                  admitted = "1")) %>% 
  
  rename(ed_los = start_to_left_ed_elapsed_time_minutes, 
         admit_los = start_to_admit_elapsed_time_minutes, 
         ctas = first_triage_acuity_cd, 
         is_within_p4p = is_ed_admit_p4p_definition) %>% 
  
  select(start_date, 
         start_hour, 
         start_dt_tm, 
         start_date_id,
         start_year, 
         is_admitted, 
         ed_los,
         admit_los, 
         is_within_p4p, 
         ctas, 
         age_at_start_date)


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


#' ## Wait time distributions 
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
  geom_vline(xintercept = 600, col = "blue") + 
  labs(title = "LGH ED: Start to disp times in minutes")

# only nonadmit
df2.ed_data_modified %>% 
  select(ed_los, 
         start_year, 
         is_admitted) %>% 
  filter(ed_los < quantile(ed_los, .99, na.rm = TRUE), 
         is_admitted == "not_admitted") %>% 
  
  ggplot(aes(x = ed_los)) +
  geom_density() + 
  facet_wrap(~start_year + is_admitted) + 
  geom_vline(xintercept = 600, col = "blue") + 
  labs(title = "LGH ED: Start to disp times in minutes")


#' Things to investigate:
#'
#' 1. Any change in dist for the not_admitted encounters from one year to the
#' next?
#'
#' 2. Among admitted encntrs, there's clearly several different distributions
#' smooshed together. Can we separate them by CTAS? Using variable `is_ed_admit_p4p_definition`? 
#' 
#' 
#' 

#' ### Nonadmits 
#' #### Are distributions lognormal and constant across years?
#' Reference: https://miningthedetails.com/blog/r/non-parametric-tests/
#'
#' Note that we convert from the mean on lognormal scale to the untransformed
#' mean using `mean_minutes = exp(logged_mean + logged_variance/2)`

#' To get from the mean on the lognormal scale to the **median** on the
#' untransformed scale, we use `median_minutes = exp(logged_mean)`

# lognormals -----
v1.nonadmit_2017 <- 
  df2.ed_data_modified %>% 
  filter(start_year == "2017", 
         is_admitted == "not_admitted") %>% 
  select(ed_los) %>% 
  na.omit() %>% 
  pull(ed_los) # %>% density %>% plot

v2.nonadmit_2018 <- 
  df2.ed_data_modified %>% 
  filter(start_year == "2018", 
         ed_los < 5000, 
         is_admitted == "not_admitted") %>%
  select(ed_los) %>% 
  na.omit() %>% 
  pull(ed_los) # %>% density %>% plot

v3.nonadmit_2019 <- 
  df2.ed_data_modified %>% 
  filter(start_year == "2019", 
         ed_los < 50000, 
         is_admitted == "not_admitted") %>%
  select(ed_los) %>% 
  na.omit() %>% 
  pull(ed_los) # %>% density %>% plot


stats::ks.test(v1.nonadmit_2017, 
               v2.nonadmit_2018, 
               alternative = "two.sided") %>% tidy()
stats::ks.test(v2.nonadmit_2018, 
               v3.nonadmit_2019, 
               alternative = "two.sided") %>% tidy()


fit_v1 <- (v1.nonadmit_2017 + 1) %>% fitdistrplus::fitdist("lnorm")
plot(fit_v1)

fit_v2 <- (v2.nonadmit_2018 + 1) %>% fitdistrplus::fitdist("lnorm")
plot(fit_v2)

fit_v3 <- (v3.nonadmit_2019+ 1) %>% fitdistrplus::fitdist("lnorm")
plot(fit_v3)



# summary(fit_v1)
# summary(fit_v2)
# summary(fit_v3)

#' In practical terms, these are probably equivalent distributions. Reference
#' above says that the D-stat will be between 0 and 1. When close to 1, very
#' likely they're different distributions. When close to 0, the difference is
#' very small.

data.frame(x = v1.nonadmit_2017) %>% 
  ggplot(aes(x = x)) + 
  stat_ecdf(geom = "step") + 
  geom_vline(xintercept = 600, col = "blue")


#' Estimates for 2018 ED LOS distribution: 
#' 
data.frame(year = 2018, 
           distribution = "lognormal", 
           mean_log_scale = fit_v2$estimate[1] %>% unname, 
           sd_log_scale = fit_v2$estimate[2] %>% unname, 
           mean_minutes = exp(fit_v2$estimate[1] + (fit_v2$estimate[2]^2) * 1/2), 
           median_minutes = exp(fit_v2$estimate[1])) %>% 
  gather() %>% 
  datatable()




# plot on log scale: 
df2.ed_data_modified %>% 
  select(ed_los, 
         start_year, 
         is_admitted) %>% 
  filter(ed_los < quantile(ed_los, .99, na.rm = TRUE), 
         is_admitted == "not_admitted") %>% 
  
  ggplot(aes(x = log(ed_los + 1))) +
  geom_density() + 
  facet_wrap(~start_year + is_admitted) + 
  labs(title = "LGH ED: Start to disp times in log(minutes)")


#' Testing whether 2018 data could have come from the fitted lognormal: 
#' 
stats::ks.test(v2.nonadmit_2018, 
               "plnorm", 
               meanlog = 5.15, 
               sdlog = 0.68, 
               alternative = "two.sided")
#' Given very small D-stat, this seems good enough. 

#' 
#' ### Conclusion

#' There have been minimal changes in this distribution across years. It's
#' described pretty well as a lognormal.
#'
#' **Proposal**: 
#' 
#' * Every month, use the past month's data as a sample, and find
#' a confidence interval for the population mean, rather than just a point
#' estimate.
#'
#' * If the CI contains the reference value for the population mean (estimated
#' by fitting a dist to past year data), then conclude that any differences are
#' just sampling variability
#' 
#' * CI for lognormal dist mean: https://amstat.tandfonline.com/doi/full/10.1080/10691898.2005.11910638#.XXTHtZNKiUk
#' 


#' ### Admits
#' #### Breakdown by CTAS 

df2.ed_data_modified %>% 
  select(ed_los, 
         start_year, 
         is_admitted, 
         ctas, 
         is_within_p4p) %>% 
  filter(ed_los < quantile(ed_los, .99, na.rm = TRUE), 
         is_admitted == "admitted", 
         start_year == "2018") %>% 
  
  ggplot(aes(x = ed_los)) +
  geom_density() + 
  facet_wrap(~ctas + is_within_p4p) + 
  geom_vline(xintercept = 600, col = "blue") + 
  labs(title = "LGH ED: Start to disp times in minutes", 
       subtitle = "Admitted patients, by CTAS and 'is_within_p4p', in 2018")



#' Does it matter what time you come to ED? 
#' 
df2.ed_data_modified %>% 
  filter(ed_los < quantile(ed_los, .99, na.rm = TRUE), 
         is_admitted == "admitted", 
         start_year == "2019") %>% 
  
  ggplot(aes(x = start_hour, 
             y = ed_los)) + 
  geom_point(alpha = .2) +
  geom_smooth() + 
  geom_hline(yintercept = 600, col = "red") + 
  facet_wrap(~ctas)

#' Here again we see that mixture of 2 distributions for every hour after 8:00 AM  
#' There's a group of patients who make the 10hr target, and a separate one who don't. 
#' The distribution of EDLOS for those who don't shifts downwards from 8:00 AM to 
#' midnight, as the ED becomes less busy. 


#' **Is this just the difference between patients who had to wait for a bed vs those who didn't? ** 





