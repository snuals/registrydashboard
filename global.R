# Dashboard: The SNU ALS registry 
# https://github.com/snuals/registrydashboard

# Date of registry updated
date_update_registry = Sys.Date()

# Load packages 
library(tidyverse)
library(pier)
library(plotly)
library(survival)
library(survminer)
library(RColorBrewer)

## Designate files 
# list_files = list.files("./data", pattern="*.csv")
# dx_file = paste("./data", list_files[grep("Dx", list_files)], sep = "/")
# fu_file = paste("./data", list_files[grep("Follow", list_files)], sep = "/")
# event_file = paste("./data", list_files[grep("Event", list_files)], sep = "/")
# close_file = paste("./data", list_files[grep("Close", list_files)], sep = "/")
# Biobank1_file = paste("./data", list_files[grep("Biobank1", list_files)], sep = "/")
# Biobank2_file = paste("./data", list_files[grep("Biobank2", list_files)], sep = "/")
# 
# base = read.csv(dx_file, na.strings = c(NA, ""))
# all_na_cols = apply(base, 2, function(x){all(is.na(x))})
# all_na_rows = apply(base, 1, function(x){all(is.na(x))})
# base = base[,!(all_na_cols)]
# base = base[!(all_na_rows),]


library(googlesheets4)
base = read_sheet("https://docs.google.com/spreadsheets/d/1j1sFQdk9g3NvqqCO6V3mXtf0AoUXNCBmPPXF-WKj3Sk/edit?usp=sharing")
fu = read_sheet("https://docs.google.com/spreadsheets/d/1ONU-QmIXBHV2AdHkXTBsavlF_y-5EomIXOD686LpGHE/edit?usp=sharing")
event = read_sheet("https://docs.google.com/spreadsheets/d/1UE6xgj2wn4bs77NiJduMoPJGr_v2p6pNfaQW1GUr-ik/edit?usp=sharing")
close = read_sheet("https://docs.google.com/spreadsheets/d/1oEosSmRXCRr5gVxmpIIwGLH7G-1UyKTds9MtxA2p9S0/edit?usp=sharing")
biobank1 = read_sheet("https://docs.google.com/spreadsheets/d/1s_hc50zUIa9htn9CWXUMfkGf41XlNqLUqU8vMexSlF4/edit?usp=sharing")
biobank2 = read_sheet("https://docs.google.com/spreadsheets/d/1KKYsXyDcYk14XWdg-oBENa970-DKYdWO7cqmBsBy2D0/edit?usp=sharing")

# Base 
base = within(base, {
  Date_birth = as.Date(Date_birth, format = "%Y-%m-%d")
  Date_onset = as.Date(Date_onset, format = "%Y-%m-%d")
  Date_dx = as.Date(Date_dx, format = "%Y-%m-%d")
  Date_enrollment = as.Date(Date_enrollment, format = "%Y-%m-%d")
})


# Date of diagnosis: distribution 
# plot_dx_month = base %>%
#   mutate(month_dx = as.Date(cut(Date_dx, breaks = "month"))) %>%
#   ggplot(aes(month_dx)) + 
#   geom_bar(fill = "blue") +
#   scale_x_date(date_labels = "%Y-%m") + 
#   xlab("Month of diagnosis") + 
#   ylab("Number of patients") + 
#   theme(axis.text.x = 
#           element_text(angle = 45, vjust = 0.5, hjust = 1)) 
# ggplotly(plot_dx_month)
# 
# # Date of enrollment: distribution 
# plot_enrollment_month = base %>%
#   mutate(month_enrollment = as.Date(cut(Date_enrollment, 
#                                         breaks = "month"))) %>%
#   ggplot(aes(month_enrollment)) + 
#   geom_bar(fill = "#99D594") +
#   scale_x_date(date_labels = "%Y-%m") + 
#   xlab("Month of enrollment") + 
#   ylab("Number of patients enrolled") + 
#   theme(axis.text.x = 
#           element_text(angle = 45, vjust = 0.5, hjust = 1)) 
# ggplotly(plot_enrollment_month)

# Exclude cases diagnosed before 2016-01-01 (1 year before the official start of registry)
# Exclude cases enrolled before 2017-01-01 
baseline = base %>%
  filter(Date_dx > "2016-01-01") %>%
  filter(Date_enrollment > "2017-01-01")

# Date of diagnosis: distribution 
# plot_dx_month = baseline %>%
#   mutate(month_dx = as.Date(cut(Date_dx, breaks = "month"))) %>%
#   ggplot(aes(month_dx)) + 
#   geom_bar(fill = "blue") +
#   scale_x_date(date_labels = "%Y-%m") + 
#   xlab("Month of diagnosis") + 
#   ylab("Number of patients") + 
#   theme(axis.text.x = 
#           element_text(angle = 45, vjust = 0.5, hjust = 1)) 
# ggplotly(plot_dx_month)


# Date of enrollment: distribution 
plot_enrollment_month = baseline %>%
  mutate(month_enrollment = as.Date(cut(Date_enrollment, 
                                        breaks = "month"))) %>%
  ggplot(aes(month_enrollment)) + 
  geom_bar(fill = "#99D594") +
  scale_x_date(date_labels = "%Y-%m") + 
  xlab("Month of enrollment") + 
  ylab("Number of patients enrolled") + 
  theme(axis.text.x = 
          element_text(angle = 45, vjust = 0.5, hjust = 1)) 
ggplotly(plot_enrollment_month)

# Diagnosis composition
dx_df = baseline %>%
  filter(!is.na(Dx)) %>%
  group_by(Dx) %>%
  count()  

data_dx <- data.frame(label = dx_df$Dx,
                      value = dx_df$n,
                      color = brewer.pal(dim(dx_df)[1], 'Spectral'))

plot_mnd_type = data_dx %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='MND', location='pie-center') %>%
  pie.subtitle(text='by Type') %>%
  pie.footer(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = 'bottom-left') %>%
  pie.tooltips()

plot_mnd_type

# Dx: missing, others, PMA, PLS, and ALS variants such as PBP, FAS, FLS
dx_uk = baseline %>%
  filter(is.na(Dx))
dim(dx_uk) # 20 patients with missing dx ? 

dx_others = baseline %>%
  filter(Dx == "Others")
dim(dx_others) # 48 patients with dx == Others ? 
# PMA or ALS?  
# PBP? FLS? FAS? 
# PLS? 

dx_PBP= baseline %>%
  filter(Dx == "PBP")
dim(dx_PBP)

# fu 
fu = within(fu, {
  Date_visit = as.Date(Date_visit, format = "%Y-%m-%d")
})

# event 
event$Date_event = as.Date(event$Date_event, format = "%Y-%m-%d")

# close 
close$Date_close = as.Date(close$Date_close, format = "%Y-%m-%d")

# biobank

########################  ALS ##########################

als <- baseline %>% # from dx table 
  filter(Dx == "ALS")

fu_als = fu %>% 
  filter(Study_ID %in% als$Study_ID) 

event_als = event %>%
  filter(Study_ID %in% als$Study_ID)

close_als = close %>%
  filter(Study_ID %in% als$Study_ID)

# Sex composition 
sex_df <- als %>% 
  group_by(Sex) %>% 
  count() 

col2 = brewer.pal(3, 'Spectral')

data_sex <- data.frame(label = sex_df$Sex,
                   value = sex_df$n,
                   color = c("#FC8D59","#99D594"))

plot_sex = data_sex %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='ALS', location='pie-center') %>%
  pie.subtitle(text='by Sex') %>%
  pie.footer(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = 'bottom-left') %>%
  pie.tooltips()

plot_sex  

# Age (at dx) and sex distribution 
als = als %>%
  mutate(age = floor(as.numeric(Date_dx - Date_birth)/365)) %>%
  mutate(age_gr = cut(age, breaks = seq(20,90, by=10), 
                      include.lowest = T, right = F))

age.sex.df = als %>%
  count(Sex, age_gr)

age_median = median(als$age, na.rm = T)

plot_age = ggplot(age.sex.df, aes(age_gr, n)) + 
  geom_bar(stat = "identity", aes(fill = Sex), 
           position = "dodge") +
  ylab("Number of patients") + 
  theme_bw() + 
  geom_text(x = 2, y = 40, 
            label = paste("Median age", age_median, sep=" = "))

plot_age

# Create dataframe on time variables 
# from als (dx table), fu_als (fu table), event and close 

# onset, dx, enrollment
temp1 = als %>%
  select(Study_ID, Date_onset, Date_dx, Date_enrollment)

# first visit, latest visit, fu duration 
temp2 = fu_als %>% 
  group_by(Study_ID) %>%
  summarise(firstVisit = min(Date_visit), 
            latestVisit = max(Date_visit), 
            fu_duration = round(as.numeric(difftime(latestVisit, firstVisit, units = "days"))/365*12, 1))

# date_gastro, date_niv, date_tracheo, date_death, 
# date_refer, date_lost2fu
close_als = close_als %>%
  rename(Date_event = Date_close, Event = Close_reason) 

event_close_als = rbind(event_als, close_als)

temp3 = spread(event_close_als, key = "Event", 
               value = "Date_event")
temp3 = temp3 %>%
  rename(Date_gastro = Gastrostomy, 
         Date_niv = NIV, 
         Date_tracheo = Tracheostomy,
         Date_lost2fu = `Lost to f/u`, 
         Date_refer = Refer, 
         Date_death = Death)

time_df = temp1 %>%
  left_join(temp2, by = "Study_ID") %>%
  left_join(temp3, by = "Study_ID")

# Onset to diagnosis (months) distribution
# Diagnosis to enrollment (months) distribution
# Enrollment to the first visit (months) distribution
time_df_initial <- time_df %>%
  mutate(onset2dx = round(as.numeric(Date_dx - Date_onset)/365*12, 1), 
         dx2enroll = round(as.numeric(Date_enrollment - Date_dx)/365*12, 1), 
         enroll2firstVisit = round(as.numeric(firstVisit - Date_enrollment)/365*12,1)
         )

time_df_initial_long = time_df_initial %>%
  select(Study_ID, onset2dx, dx2enroll, enroll2firstVisit) %>%
  gather(key = "period", value = "months", onset2dx:enroll2firstVisit) %>%
  mutate(period = factor(period, levels = c("onset2dx", 
                                            "dx2enroll",
                                            "enroll2firstVisit"), 
                         labels = c("Onset to Dx", "Dx to Enroll", "Enroll to Visit (1st)")))

plot_time_initial = ggplot(time_df_initial_long, 
                           aes(months, fill = period)) + 
  geom_histogram(color = "gray") + 
  ylab("Number of patients") + 
  xlab("Time (months)") + 
  facet_wrap(.~ period, scales = "free") + 
  theme(legend.position = "none")

plot_time_initial

# check weird (I)
time_df_initial %>%
  filter(onset2dx < 0|dx2enroll < 0|enroll2firstVisit <0) %>%
  select(Study_ID, onset2dx, dx2enroll, enroll2firstVisit, 
         Date_onset, Date_dx, Date_enrollment, 
         firstVisit)

# check weird (II)
# onset2dx > 90 percentile value 
time_df_initial %>%
  filter(onset2dx > quantile(time_df_initial$onset2dx, 
                             probs = seq(0,1,0.1))[10]) %>%
  select(Study_ID, onset2dx, Date_onset, Date_dx)

# dx2enroll > 90 percentile value 
time_df_initial %>%
  filter(dx2enroll > quantile(time_df_initial$dx2enroll, 
                             probs = seq(0,1,0.1))[10]) %>%
  select(Study_ID, dx2enroll, Date_dx, Date_enrollment)

# enroll2firstVisit > 0 or NA 
time_df_initial %>%
  filter(is.na(enroll2firstVisit)|enroll2firstVisit>0) %>%
  select(Study_ID, enroll2firstVisit, Date_enrollment, firstVisit)

# ALS onset region composition 
onset_region_df <- als %>% 
  group_by(Onset_region) %>% 
  count()

data_onset_region <- data.frame(label = onset_region_df$Onset_region,
                        value = onset_region_df$n,
                        color = brewer.pal(dim(onset_region_df)[1], 'Spectral'))

plot_onset_region = data_onset_region %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='ALS', location='pie-center') %>%
  pie.subtitle(text='by onset region') %>%
  pie.header(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = 'top-left') %>%
  pie.tooltips()

plot_onset_region

# Follow-up duration diatribution (I)
# from the first vist to the latest visit (in fu table)
median_fu_mo = median(time_df_initial$fu_duration, na.rm = T)

plot_fu_duration = ggplot(time_df_initial, 
                          aes(fu_duration)) + 
  geom_histogram(col = "dark grey", fill = "violet", 
                 binwidth = 1) + 
  ylab("Number of patients") + 
  xlab("Time from entry to the latest visit (months)") + 
  theme_bw() + 
  geom_text(x=20, y=50, label = paste("Median (months)", 
                                      median_fu_mo, 
                                      sep = " = "))
plot_fu_duration

# Follow-up duration diatribution (II)
fu_als_duration_gr = time_df_initial %>%
  mutate(fu_duration_gr = cut(fu_duration, 
                              breaks = c(0,3,6,12,Inf), 
                              include.lowest = T, right = F))

levels(fu_als_duration_gr$fu_duration_gr) = c(
  "Less than 3 mo", "3 to 6 mo", "6 to 12 mo", "Longer than 12 mo"
)

temp = fu_als_duration_gr %>% 
  group_by(fu_duration_gr) %>% 
  count()

data_fu_duration_gr <- data.frame(label = temp$fu_duration_gr,
                                value = temp$n,
                                color = brewer.pal(dim(temp)[1], 'Spectral'))

plot_fu_duration_pie = data_fu_duration_gr %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='FU duration', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.header(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = "top-left") %>%
  pie.footer(text = "Undefined: enrolled, but no visit record in fu table", 
             location = "bottom-left") %>%
  pie.tooltips()

plot_fu_duration_pie

# Temporal change of ALSFRS-R 
# exclude patients with only one visit  
fu_alsfrs = fu_als %>%
  filter(!is.na(ALSFRS)) %>%
  group_by(Study_ID) %>%
  mutate(cnt = n()) %>%
  filter(cnt > 1)

temp = fu_alsfrs %>%
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  mutate(fu_duration = as.numeric(Date_visit - min(Date_visit)), 
         fu_duration = round(fu_duration/365*12,1)) %>%
  select(Study_ID, fu_duration, ALSFRS, Date_visit)

temp_slope = temp %>%
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  summarise(slope = round((last(ALSFRS)-first(ALSFRS))/
                            max(fu_duration), 1))

q4 = quantile(temp_slope$slope, 
              probs = c(0,0.25,0.75,1), na.rm = T)

temp_slope$slope_gr = cut(temp_slope$slope, 
                            breaks = q4, 
                            include.lowest = T, 
                            right = F)
temp_slope_alsfrs = temp %>%
  left_join(temp_slope, by = "Study_ID")

plot_alsfrs = ggplot(temp_slope_alsfrs, 
       aes(fu_duration, ALSFRS, 
           col=slope_gr, group=factor(Study_ID))) +
  geom_point() + geom_line() +  theme_bw() +
  xlab("Time from baseline (months)") +
  ylab("ALSFRS-R") + 
  facet_wrap(.~slope_gr) + 
  labs(col = "Slope (delta/mo)")

plot_alsfrs
ggplotly(plot_alsfrs)

# Temporal change of weight
fu_wt = fu_als %>%
  filter(!is.na(Wt)) %>%
  group_by(Study_ID) %>%
  mutate(cnt = n()) %>%
  filter(cnt > 1) 

temp_wt = fu_wt %>%
  group_by(Study_ID) %>% 
  mutate(fu_duration = as.numeric(Date_visit - min(Date_visit)), 
       fu_duration = round(fu_duration/365*12,1)) %>%
  select(Study_ID, fu_duration, Wt, Date_visit)

slope_wt = temp_wt %>%
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  summarise(slope = round((last(Wt)-first(Wt))/
                            max(fu_duration), 1))

q4 = quantile(slope_wt$slope, probs = c(0,0.25,0.75,1), na.rm = T)
temp = merge(temp_wt, slope_wt, by="Study_ID")

temp$slope_gr = cut(temp$slope, breaks = q4, include.lowest = T, 
                    right = F)

plot_wt = ggplot(temp, 
                     aes(fu_duration, Wt, 
                         col=slope_gr, group=factor(Study_ID))) +
  geom_point() + geom_line() +  theme_bw() +
  xlab("Time from baseline (months)") +
  ylab("Wt") + 
  facet_wrap(.~slope_gr) + 
  labs(col = "Slope (delta/mo)")

plot_wt
ggplotly(plot_wt)

# Temporal change of FVC   
fu_FVC = fu_als %>%
  filter(!is.na(FVC)) %>%
  group_by(Study_ID) %>%
  mutate(cnt = n()) %>%
  filter(cnt > 1)

temp_FVC = fu_FVC %>%
  group_by(Study_ID) %>%
  mutate(fu_duration = as.numeric(Date_visit - min(Date_visit)), 
         fu_duration = round(fu_duration/365*12,1)) %>%
  select(Study_ID, fu_duration, FVC, Date_visit)

slope_FVC = temp_FVC %>%
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  summarise(slope = round((last(FVC)-first(FVC))/
                            max(fu_duration), 1))

q4 = quantile(slope_FVC$slope, probs = c(0,0.25,0.75,1))
temp = merge(temp_FVC, slope_FVC, by="Study_ID")

temp$slope_gr = cut(temp$slope, breaks = q4, 
                    include.lowest = T, 
                    right = F)

plot_FVC_percent = ggplot(temp, 
                 aes(fu_duration, FVC, 
                     col=slope_gr, group=factor(Study_ID))) +
  geom_point() + geom_line() +  theme_bw() +
  xlab("Time from baseline (months)") +
  ylab("FVC (% of predicted)") + 
  facet_wrap(.~slope_gr) + 
  labs(col = "Slope (delta/mo)")

plot_FVC_percent

# Proportion: event, close 
event_df = event_als %>%
  count(Event) %>%
  mutate(Prop = round(n/length(als$Study_ID)*100, 1))

event_df

close_df = close_als %>%
  count(Event) %>%
  mutate(Prop = round(n/length(als$Study_ID)*100, 1))

close_df

# Distribution; onset to gastrostomy and NIV (months)
event_als_fu1 = event_als %>%
  filter(Event %in% c("Gastrostomy", "NIV")) %>%
  inner_join(als, by = "Study_ID") %>% 
  group_by(Study_ID) %>%
  select(Study_ID, Event, Date_onset, Date_event) %>%
  mutate(onset2event = round(
    as.numeric(
      difftime(Date_event, Date_onset, units = "days")
    )/365*12
  )) 

max_onset2event1 <- max(event_als_fu1$onset2event)

plot_event1 = ggplot(event_als_fu1, aes(onset2event)) + 
  geom_histogram(col = "dark grey", fill = "blue", 
                 binwidth = 3) + 
  ylab("Number of patients") + 
  xlab("Time (months)") + 
  scale_x_continuous(limits = c(0,max_onset2event1), oob = scales::squish) + 
  facet_wrap(.~factor(Event)) + 
  theme_bw() 

plot_event1 

# distribution: enroll2death_tracheo
# latestVisit2lost2fu, latestVisit2refer; should be zero 
time_df_close = event_close_als %>%
  filter(Event %in% c("Death", "Tracheostomy")) %>%
  group_by(Study_ID) %>%
  summarise(Date_death_tracheo = min(Date_event)) %>%
  right_join(subset(time_df, select = c("Study_ID", 
                                        "Date_enrollment", 
                                        "latestVisit", 
                                        "Date_lost2fu",
                                        "Date_refer")), 
             by = "Study_ID")

# Death or Tracheostomy 
# KM curve for death (or tracheostomy)
# from enrollment 
# caluculate enroll2death_tracheo, enroll2latestVisit 

time_df_close = time_df_close %>%
  mutate(enroll2death_tracheo = 
           round(as.numeric(Date_death_tracheo - Date_enrollment)/365*12, 1), 
         enroll2latestVisit = 
           round(as.numeric(latestVisit - Date_enrollment)/365*12, 1))

# Death table 
KM = time_df_close %>%
  mutate(Status = ifelse(is.na(Date_death_tracheo), 0, 1)) %>%
  mutate(Time = ifelse(is.na(enroll2death_tracheo), 
                       enroll2latestVisit, enroll2death_tracheo)) %>%
  select(Study_ID, Status, Time) 

dim(KM) # enrolled, but no time record (eg, no latestVisit)

fit <- survfit(Surv(Time, Status) ~ 1, data = KM)

plot_survival = ggsurvplot(fit, data = KM, 
           conf.int = T, 
           risk.table = T, 
           break.time.by = 12, 
           title = "KM survival curve (death or tracheostomy)", 
           ggtheme = theme_bw(),
           surv.median.line = "hv", 
           legend = "none", 
           xlab = "Months from enrollment"
           )
plot_survival

# Close vs. Not-closed 
# Close refers to the end of all tracking attempts, which ...
# includes death, refer, and lost2fu
# Note that this does not apply to the following... 
# refer, but tracking is ongoing... 
# Note that tracking is ongoing (eg, by phone), do not close with lost2fu

# Distribution of time from the latest visit.. 
# in not-closed cases 
time_df_close2 = time_df_close %>%
  filter(!(Study_ID %in% close_als$Study_ID)) %>%
  mutate(latestVisit2now = 
           round(as.numeric(date_update_registry - latestVisit)/365*12, 1)) %>%
  select(Study_ID, latestVisit2now, latestVisit)

dim(time_df_close2)
time_df_close2 %>% 
  filter(latestVisit2now > 6)

plot_latestVisit2now = time_df_close2 %>%
  ggplot(aes(latestVisit2now)) + 
  geom_histogram(col = "dark grey", fill = "blue") + 
  xlab("Time from the latest visit (months)") + 
  ylab("Number of patients")

plot_latestVisit2now

time_df_close2$time_gr = cut(time_df_close2$latestVisit2now, 
                             breaks = c(0,3,6,Inf), 
                             include.lowest = T, 
                             right = F)
temp = time_df_close2 %>%
  filter(!is.na(time_gr)) %>%
  group_by(time_gr) %>%
  count()

levels(temp$time_gr) = list(less_than_3mo = "[0,3)", 
                             btw_3mo_6mo = "[3,6)", 
                             longer_than_6mo = "[6,Inf]")

data_time <- data.frame(label = temp$time_gr, 
                       value = temp$n, 
                       color = brewer.pal(dim(temp)[1], 'Spectral'))

plot_latestVisit2now_gr = data_time %>%
  pier() %>%
  pie.size(inner=50, outer=70, width = 250, height = 300)  %>%
  pie.header(text='Time from the latest visit (mo)', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.tooltips()

plot_latestVisit2now_gr

# Biobank

biobank = merge(biobank1, biobank2, by="Provider_Ocode")
biobank_als = biobank %>%
  filter(Study_ID %in% als$Study_ID)
biobank_als %>%
  select(Study_ID, Provider_Ocode, Sample_Bcode) -> biobank_als
csf = biobank_als[grep("CSF", biobank_als$Sample_Bcode),]
ser = biobank_als[grep("SER", biobank_als$Sample_Bcode),]
buf = biobank_als[grep("BUF", biobank_als$Sample_Bcode),]
pla = biobank_als[grep("PLA", biobank_als$Sample_Bcode),]

# FU samples: number of patients according to the number of FU sample

# serum  
ser$visit_no = regmatches(ser$Sample_Bcode, regexpr("SER..", ser$Sample_Bcode))
ser$visit_no = regmatches(ser$visit_no, regexpr("[0-9][0-9]",ser$visit_no))
ser$visit_no = as.integer(ser$visit_no)
ser$sample_no = regmatches(ser$Sample_Bcode, regexpr("...$", ser$Sample_Bcode))
ser$sample_no = as.integer(ser$sample_no)
ser %>%
  group_by(Study_ID) %>%
  mutate(max_fu_no = max(visit_no)) %>%
  select(Study_ID, max_fu_no) %>%
  unique() -> ser.temp
ser.temp$max_fu_gr = cut(ser.temp$max_fu_no, breaks = c(1,2,3,Inf), 
                     include.lowest = T, 
                     right = F)
levels(ser.temp$max_fu_gr) = c("1", "2", "3+")
temp = ser.temp %>%
  group_by(max_fu_gr) %>%
  count()
data_ser <- data.frame(label = temp$max_fu_gr, 
                       value = temp$n, 
                       color = RColorBrewer::brewer.pal(dim(temp)[1], 'Spectral'))
plot_ser = data_ser %>%
  pier() %>%
  pie.size(inner=50, outer=70, width = 250, height = 300)  %>%
  pie.header(text='Serum', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.tooltips()
plot_ser

# Plasma
pla$visit_no = regmatches(pla$Sample_Bcode, regexpr("PLA..", pla$Sample_Bcode))
pla$visit_no = regmatches(pla$visit_no, regexpr("[0-9][0-9]",pla$visit_no))
pla$visit_no = as.integer(pla$visit_no)
pla$sample_no = regmatches(pla$Sample_Bcode, regexpr("...$", pla$Sample_Bcode))
pla$sample_no = as.integer(pla$sample_no)
pla %>%
  group_by(Study_ID) %>%
  mutate(max_fu_no = max(visit_no)) %>%
  select(Study_ID, max_fu_no) %>%
  unique() -> pla.temp
pla.temp$max_fu_gr = cut(pla.temp$max_fu_no, breaks = c(1,2,3,Inf), 
                         include.lowest = T, 
                         right = F)
levels(pla.temp$max_fu_gr) = c("1", "2", "3+")
temp = pla.temp %>%
  group_by(max_fu_gr) %>%
  count()
data_pla <- data.frame(label = temp$max_fu_gr, 
                       value = temp$n, 
                       color = RColorBrewer::brewer.pal(dim(temp)[1], 'Spectral'))
plot_pla = data_pla %>%
  pier() %>%
  pie.size(inner=50, outer=70, width = 250, height = 300)  %>%
  pie.header(text='Plasma', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.tooltips()
plot_pla

# CSF
csf$visit_no = regmatches(csf$Sample_Bcode, regexpr("CSF..", csf$Sample_Bcode))
csf$visit_no = regmatches(csf$visit_no, regexpr("[0-9][0-9]",csf$visit_no))
csf$visit_no = as.integer(csf$visit_no)
csf$sample_no = regmatches(csf$Sample_Bcode, regexpr("...$", csf$Sample_Bcode))
csf$sample_no = as.integer(csf$sample_no)
csf %>%
  group_by(Study_ID) %>%
  mutate(max_fu_no = max(visit_no)) %>%
  select(Study_ID, max_fu_no) %>%
  unique() -> csf.temp
csf.temp$max_fu_gr = cut(csf.temp$max_fu_no, breaks = c(1,2,3,Inf), 
                         include.lowest = T, 
                         right = F)
levels(csf.temp$max_fu_gr) = c("1", "2", "3+")
temp = csf.temp %>%
  group_by(max_fu_gr) %>%
  count()
colors_csf = RColorBrewer::brewer.pal(dim(temp)[1], 'Spectral')
data_csf <- data.frame(label = temp$max_fu_gr, 
                       value = temp$n, 
                       color = colors_csf[1:dim(temp)[1]])
plot_csf = data_csf %>%
  pier() %>%
  pie.size(inner=50, outer=70, width = 250, height = 300) %>%
  pie.header(text='CSF', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.tooltips()
plot_csf

# Buffy coat
buf$visit_no = regmatches(buf$Sample_Bcode, regexpr("BUF..", buf$Sample_Bcode))
buf$visit_no = regmatches(buf$visit_no, regexpr("[0-9][0-9]",buf$visit_no))
buf$visit_no = as.integer(buf$visit_no)
buf$sample_no = regmatches(buf$Sample_Bcode, regexpr("...$", buf$Sample_Bcode))
buf$sample_no = as.integer(buf$sample_no)
buf %>%
  group_by(Study_ID) %>%
  mutate(max_fu_no = max(visit_no)) %>%
  select(Study_ID, max_fu_no) %>%
  unique() -> buf.temp
buf.temp$max_fu_gr = cut(buf.temp$max_fu_no, breaks = c(1,2,3,Inf), 
                         include.lowest = T, 
                         right = F)
levels(buf.temp$max_fu_gr) = c("1", "2", "3+")
temp = buf.temp %>%
  group_by(max_fu_gr) %>%
  count()
colors_buf = RColorBrewer::brewer.pal(dim(temp)[1], 'Spectral')
data_buf <- data.frame(label = temp$max_fu_gr, 
                       value = temp$n, 
                       color = colors_buf[1:dim(temp)[1]])
plot_buf = data_buf %>%
  pier() %>%
  pie.size(inner=50, outer=70, width = 250, height = 300) %>%
  pie.header(text='Buffy coat', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.tooltips()
plot_buf

