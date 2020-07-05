# Dashboard: The SNU ALS registry 
# https://github.com/snuals/registrydashboard

library(dplyr)
library(ggplot2)
library(pier)
library(plotly)
library(survival)
library(survminer)

## Designate files 
list_files = list.files("./data", pattern="*.csv")
dx_file = paste("./data", list_files[grep("Dx", list_files)], sep = "/")
fu_file = paste("./data", list_files[grep("Follow", list_files)], sep = "/")
event_file = paste("./data", list_files[grep("Event", list_files)], sep = "/")
close_file = paste("./data", list_files[grep("Close", list_files)], sep = "/")
Biobank1_file = paste("./data", list_files[grep("Biobank1", list_files)], sep = "/")
Biobank2_file = paste("./data", list_files[grep("Biobank2", list_files)], sep = "/")

base = read.csv(dx_file, na.strings = c(NA, ""))
all_na_cols = apply(base, 2, function(x){all(is.na(x))})
all_na_rows = apply(base, 1, function(x){all(is.na(x))})
base = base[,!(all_na_cols)]
base = base[!(all_na_rows),]

base$Date_birth = as.Date(base$Date_birth, format = "%Y.%m.%d")
base$Date_onset = as.Date(base$Date_onset, format = "%Y.%m.%d")
base$Date_dx = as.Date(base$Date_dx, format = "%Y.%m.%d")
base$Date_enrollment = as.Date(base$Date_enrollment, format = "%Y.%m.%d")

# Date of diagnosis: distribution 
month_dx = as.Date(cut(base$Date_dx, 
                               breaks = "month"))
month_dx_df = as.data.frame(table(month_dx))
month_dx_df$month_dx = 
  as.Date(month_dx_df$month_dx)

plot_dx_month = ggplot(month_dx_df, aes(month_dx, Freq)) + 
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_date(date_labels = "%Y-%m") + 
  xlab("Month of diagnosis") + 
  ylab("Number of patients") + 
  theme(axis.text.x = 
          element_text(angle = 45, vjust = 0.5, hjust = 1)) 
ggplotly(plot_dx_month)

# Date of enrollment: distribution 
month_enrollment = as.Date(cut(base$Date_enrollment, 
                               breaks = "month"))
month_enrollment_df = as.data.frame(table(month_enrollment))
month_enrollment_df$month_enrollment = 
  as.Date(month_enrollment_df$month_enrollment)
plot_enrollment_month = ggplot(month_enrollment_df, aes(month_enrollment, Freq)) + 
  geom_bar(stat = "identity", fill = "#99D594") +
  scale_x_date(date_labels = "%Y-%m") + 
  xlab("Month of enrollment") + 
  ylab("Number of patients enrolled") + 
  theme(axis.text.x = 
          element_text(angle = 45, vjust = 0.5, hjust = 1)) 
ggplotly(plot_enrollment_month)

# filter base data
# diagnosis after 2016-01-01 (1 year before the official start of registry)
# enrollment after 2017-01-01 
baseline = base %>%
  filter(Date_dx > "2016-01-01") %>%
  filter(Date_enrollment > "2017-01-01")

month_dx = as.Date(cut(baseline$Date_dx, 
                       breaks = "month"))
month_dx_df = as.data.frame(table(month_dx))
month_dx_df$month_dx = 
  as.Date(month_dx_df$month_dx)

# Date of diagnosis: distribution 
plot_dx_month = ggplot(month_dx_df, aes(month_dx, Freq)) + 
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_date(date_labels = "%Y-%m") + 
  xlab("Month of diagnosis") + 
  ylab("Number of patients") + 
  theme(axis.text.x = 
          element_text(angle = 45, vjust = 0.5, hjust = 1)) 
ggplotly(plot_dx_month)

# Date of enrollment: distribution 
month_enrollment = as.Date(cut(baseline$Date_enrollment, 
                       breaks = "month"))
month_enrollment_df = as.data.frame(table(month_enrollment))
month_enrollment_df$month_enrollment = 
  as.Date(month_enrollment_df$month_enrollment)

plot_enrollment_month = ggplot(month_enrollment_df, aes(month_enrollment, Freq)) + 
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_date(date_labels = "%Y-%m") + 
  xlab("Month of diagnosis") + 
  ylab("Number of patients") + 
  theme(axis.text.x = 
          element_text(angle = 45, vjust = 0.5, hjust = 1)) 
ggplotly(plot_enrollment_month)

# fu 
fu = read.csv(fu_file, na.strings = c(NA, ""))
all_na_rows = apply(fu, 1, function(x){all(is.na(x))}) 
all_na_cols = apply(fu, 2, function(x){all(is.na(x))}) 
fu = fu[!(all_na_rows), !(all_na_cols)]
fu$Date_visit = as.Date(fu$Date_visit, format = "%Y.%m.%d")
fu$Mitos = factor(fu$Mitos)

# event 
event = read.csv(event_file, na.strings = c("", NA))
all_na_cols = apply(event, 2, function(x){all(is.na(x))})
all_na_rows = apply(event, 1, function(x){all(is.na(x))})
event = event[,!(all_na_cols)]
event = event[!(all_na_rows),]
event$Date_event = as.Date(event$Date_event, format = "%Y.%m.%d")

# close 
close = read.csv(close_file, na.strings = c("",NA))
all_na_cols = apply(close, 2, function(x){all(is.na(x))})
all_na_rows = apply(close, 1, function(x){all(is.na(x))})
close = close[!all_na_rows, !all_na_cols]
close$Date_close = as.Date(close$Date_close, format = "%Y.%m.%d")

# biobank
biobank1 = read.csv(Biobank1_file, na.strings = c("", NA))
biobank2 = read.csv(Biobank2_file, na.strings = c("", NA))

# Date of registry updated
date_update = strsplit(dx_file, split = "_")[[1]][3]
date_update_registry = as.Date(date_update, format = "%Y%m%d")

# Dx: ALS 
als <- baseline %>%
  filter(Dx == "ALS")

# plot: mnd by type composition
dx_df = baseline %>%
  filter(!is.na(Dx)) %>%
  group_by(Dx) %>%
  count()  
data_dx <- data.frame(label = dx_df$Dx,
                   value = dx_df$n,
                   color = RColorBrewer::brewer.pal(dim(dx_df)[1], 'Spectral'))
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

# Plot: ALS sex composition 
sex_df <- als %>% 
  group_by(Sex) %>% 
  count() 
col2 = RColorBrewer::brewer.pal(3, 'Spectral')
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

# Plot: ALS Age (at dx) and sex distribution 
als = als %>%
  mutate(age = floor(as.numeric(Date_dx-Date_birth)/365)) %>%
  mutate(age_gr = cut(age, breaks = seq(20,90, by=10), 
                      include.lowest = T, right = F))
age.sex.df = as.data.frame(table(als$Sex, als$age_gr))
colnames(age.sex.df) = c("Sex", "Age", "Freq")
age_median = median(als$age, na.rm = T)
plot_age = ggplot(age.sex.df, aes(Age, Freq)) + 
  geom_bar(stat = "identity", aes(fill = Sex), 
           position = "dodge") +
  ggtitle("Age at diagnosis") + 
  ylab("Number of patients") + 
  theme_bw() + 
  geom_text(x = 2, y = 40, 
            label = paste("Median age", age_median, sep=" = "))
plot_age

# Plot: ALS onset to diagnosis (months) distribution
als <- als %>%
  mutate(onset2dx = round(as.numeric(Date_dx - Date_onset)/30))
onset2dx_median = median(als$onset2dx, na.rm = T)
plot_onset2dx = ggplot(data = als, aes(x=onset2dx)) + 
  geom_histogram(col = "dark grey", fill = "purple", binwidth = 3) + 
  ggtitle("Onset to diagnosis (months)") + 
  ylab("Number of patients") + 
  xlab("Time from onset to diagnosis (months)") + 
  theme_bw() + 
  geom_text(x=100, y=50, label = paste("Median (months)", 
                                       onset2dx_median, 
                                       sep = " = "))
plot_onset2dx

# Plot: ALS onset region composition 
onset_region_df <- als %>% 
  group_by(Onset_region) %>% 
  count()
data_onset_region <- data.frame(label = onset_region_df$Onset_region,
                        value = onset_region_df$n,
                        color = RColorBrewer::brewer.pal(dim(onset_region_df)[1], 'Spectral'))
plot_onset_region = data_onset_region %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='ALS', location='pie-center') %>%
  pie.subtitle(text='by onset region') %>%
  pie.footer(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = 'bottom-left') %>%
  pie.tooltips()
plot_onset_region

# Follow-up: ALS patients 
fu_als = fu %>%
  filter(Study_ID %in% als$Study_ID) %>%
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  mutate(Visit_no = rank(Date_visit)) %>%
  mutate(Visit_interval_mo = round(as.numeric(difftime(Date_visit, Date_visit[1], 
                                   units = "days"))/30), 1)

# Plot: follow-up duration distribution 
fu_als_duration = fu_als %>%
  group_by(Study_ID) %>%
  summarize(FU_duration = max(Visit_interval_mo))
median_fu_mo = median(fu_als_duration$FU_duration)
plot_fu_duration = ggplot(fu_als_duration, 
                          aes(FU_duration)) + 
  geom_histogram(col = "dark grey", fill = "violet", 
                 binwidth = 1) + 
  ggtitle("FU duration (months)") + 
  ylab("Number of patients") + 
  xlab("Time from entry to the latest visit (months)") + 
  theme_bw() + 
  geom_text(x=20, y=50, label = paste("Median (months)", 
                                      median_fu_mo, 
                                      sep = " = "))
plot_fu_duration

# Time from entry to the latest visit 
# Number of patients followed longer than 3, 6, 12 mo 
fu_als_duration_gr = fu_als_duration %>%
  mutate(FU_duration_gr = cut(FU_duration, 
                              breaks = c(0,3,6,12,Inf), 
                              include.lowest = T, right = F))
levels(fu_als_duration_gr$FU_duration_gr) = c(
  "Less than 3 mo", "3 to 6 mo", "6 to 12 mo", "Longer than 12 mo"
)
temp = fu_als_duration_gr %>% 
  group_by(FU_duration_gr) %>% 
  count()
data_fu_duration_gr <- data.frame(label = temp$FU_duration_gr,
                                value = temp$n,
                                color = RColorBrewer::brewer.pal(dim(temp)[1], 'Spectral'))
plot_fu_duration_pie = data_fu_duration_gr %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='FU duration', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.footer(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = 'bottom-left') %>%
  pie.tooltips()
plot_fu_duration_pie

# Temporal change of ALSFRS-R 
# only in patients with 2 or more visit ALSFRS 
fu_alsfrs = fu_als %>%
  filter(!is.na(ALSFRS)) %>%
  group_by(Study_ID) %>%
  mutate(Max_visit_no = max(Visit_no)) %>%
  mutate(FU_duration = max(Visit_interval_mo)) %>%
  filter(FU_duration > 0) 
slope_alsfrs = fu_alsfrs %>%
  group_by(Study_ID) %>%
  summarize(Slope = (last(ALSFRS)-first(ALSFRS))/
           last(Visit_interval_mo))
q4 = quantile(slope_alsfrs$Slope, probs = c(0,0.25,0.75,1))
temp = merge(fu_alsfrs, slope_alsfrs, by="Study_ID")
temp$slope_gr = cut(temp$Slope, breaks = q4, include.lowest = T, 
                    right = F)
plot_alsfrs = ggplot(temp, 
       aes(Visit_interval_mo, ALSFRS, 
           col=slope_gr, group=factor(Study_ID))) +
  geom_point() + geom_line() +  theme_bw() +
  xlab("Time from baseline (months)") +
  ylab("ALSFRS-R") + 
#  facet_wrap(.~slope_gr) + 
  labs(col = "Slope (delta/mo)")
plot_alsfrs

# Temporal change of weight
fu_wt = fu_als %>%
  filter(!is.na(Wt)) %>%
  group_by(Study_ID) %>%
  mutate(Max_visit_no = max(Visit_no)) %>%
  mutate(FU_duration = max(Visit_interval_mo)) %>%
  filter(FU_duration > 0) 
slope_wt = fu_wt %>%
  group_by(Study_ID) %>%
  summarize(Slope = (last(Wt)-first(Wt))/
              last(Visit_interval_mo))
q4 = quantile(slope_wt$Slope, probs = c(0,0.25,0.75,1))
temp = merge(fu_wt, slope_wt, by="Study_ID")
temp$slope_gr = cut(temp$Slope, breaks = q4, include.lowest = T, 
                    right = F)
plot_wt = ggplot(temp, 
                     aes(Visit_interval_mo, Wt, 
                         col=slope_gr, group=factor(Study_ID))) +
  geom_point() + geom_line() +  theme_bw() +
  xlab("Time from baseline (months)") +
  ylab("Wt") + 
  #  facet_wrap(.~slope_gr) + 
  labs(col = "Slope (delta/mo)")
plot_wt

# Temporal change of FVC   
fu_FVC_percent = fu_als %>%
  filter(!is.na(FVC_percent)) %>%
  group_by(Study_ID) %>%
  mutate(Max_visit_no = max(Visit_no)) %>%
  mutate(FU_duration = max(Visit_interval_mo)) %>%
  filter(FU_duration > 0) 
slope_FVC_percent = fu_FVC_percent %>%
  group_by(Study_ID) %>%
  summarize(Slope = (last(FVC_percent)-first(FVC_percent))/
              last(Visit_interval_mo))
q3 = quantile(slope_FVC_percent$Slope, probs = c(0,0.25,0.75,1))
temp = merge(fu_FVC_percent, slope_FVC_percent, by="Study_ID")
temp$slope_gr = cut(temp$Slope, breaks = q3, include.lowest = T, 
                    right = F)
plot_FVC_percent = ggplot(temp, 
                 aes(Visit_interval_mo, FVC_percent, 
                     col=slope_gr, group=factor(Study_ID))) +
  geom_point() + geom_line() +  theme_bw() +
  xlab("Time from baseline (months)") +
  ylab("FVC (% of predicted)") + 
  #  facet_wrap(.~slope_gr) + 
  labs(col = "Slope (delta/mo)")
plot_FVC_percent

# Event
event_als = event %>%
  filter(Study_ID %in% als$Study_ID)

# Gastrostomy 
gastrostomy_als = event_als %>%
  filter(Event == "Gastrostomy")
gastrostromy_pct = round(dim(gastrostomy_als)[1]/dim(als)[1]*100,1)
temp1 = merge(als, gastrostomy_als, by="Study_ID", all.x = T)
fu_als_latest = fu_als %>%
  group_by(Study_ID) %>%
  filter(Visit_no == max(Visit_no))
temp2 = merge(temp1, fu_als_latest, by="Study_ID")
gastrostomy_als_fu = temp2 %>% 
  select(Study_ID, Event, Date_dx, Date_event, Date_visit) %>%
  mutate(dx2event = round(
    as.numeric(
      difftime(Date_event, Date_dx, units = "days")
    )/30
  )) %>%
  mutate(dx2latestVisit = round(
    as.numeric(
      difftime(Date_visit, Date_dx, units = "days")
    )/30
  ))

plot_gastostomy = ggplot(gastrostomy_als_fu, aes(dx2event)) + 
  geom_histogram(col = "dark grey", fill = "blue", 
                 binwidth = 3) + 
  ggtitle("Time from diagnosis to gastrostomy (months)") + 
  ylab("Number of patients") + 
  xlab("Time (months)") + 
  theme_bw() 
plot_gastostomy

# NIV 
NIV_als = event_als %>%
  filter(Event == "NIV")
NIV_pct = round((dim(NIV_als)[1]/dim(als)[1])*100, 1)
temp1 = merge(als, NIV_als, by="Study_ID", all.x = T)
temp2 = merge(temp1, fu_als_latest, by="Study_ID")
NIV_als_fu = temp2 %>% 
  select(Study_ID, Event, Date_dx, Date_event, Date_visit) %>%
  mutate(dx2event = round(
    as.numeric(
      difftime(Date_event, Date_dx, units = "days")
    )/30
  )) %>%
  mutate(dx2latestVisit = round(
    as.numeric(
      difftime(Date_visit, Date_dx, units = "days")
    )/30
  ))

plot_NIV = ggplot(NIV_als_fu, aes(dx2event)) + 
  geom_histogram(col = "dark grey", fill = "blue", 
                 binwidth = 3) + 
  ggtitle("Time from diagnosis to NIV (months)") + 
  ylab("Number of patients") + 
  xlab("Time (months)") + 
  theme_bw() 
plot_NIV

# Tracheostomy
Tracheostomy_als = event_als %>%
  filter(Event == "Tracheostomy")
Tracheostomy_pct = round((dim(Tracheostomy_als)[1]/dim(als)[1])*100, 1)
temp1 = merge(als, Tracheostomy_als, by="Study_ID", all.x = T)
temp2 = merge(temp1, fu_als_latest, by="Study_ID")
Tracheostomy_als_fu = temp2 %>% 
  select(Study_ID, Event, Date_dx, Date_event, Date_visit) %>%
  mutate(dx2event = round(
    as.numeric(
      difftime(Date_event, Date_dx, units = "days")
    )/30
  )) %>%
  mutate(dx2latestVisit = round(
    as.numeric(
      difftime(Date_visit, Date_dx, units = "days")
    )/30
  ))

plot_tracheostomy = ggplot(Tracheostomy_als_fu, aes(dx2event)) + 
  geom_histogram(col = "dark grey", fill = "blue", binwidth = 3) + 
  ggtitle("Time from diagnosis to Tracheostomy (months)") + 
  ylab("Number of patients") + 
  xlab("Time (months)") + 
  theme_bw() 
plot_tracheostomy

# Death 
death_als = close %>%
  filter(Close_reason == "Death")
death_pct = round((dim(death_als)[1]/dim(als)[1])*100, 1)
temp1 = merge(als, death_als, by="Study_ID", all.x = T)
temp2 = merge(temp1, fu_als_latest, by="Study_ID")
death_als_fu = temp2 %>% 
  select(Study_ID, Close_reason, Date_dx, Date_close, Date_visit) %>%
  mutate(dx2death = round(
    as.numeric(
      difftime(Date_close, Date_dx, units = "days")
    )/30
  )) %>%
  mutate(dx2latestVisit = round(
    as.numeric(
      difftime(Date_visit, Date_dx, units = "days")
    )/30
  ))

plot_death = ggplot(death_als_fu, aes(dx2death)) + 
  geom_histogram(col = "dark grey", fill = "blue", binwidth = 3) + 
  ggtitle("Time from diagnosis to death (months)") + 
  ylab("Number of patients") + 
  xlab("Time (months)") + 
  theme_bw() 
plot_death

# KM curve for event (including gastrostomy, NIV, tracheostomy and death) 
KM_death = death_als_fu %>%
  mutate(Outcome = "Death") %>%
  mutate(Status = ifelse(is.na(Close_reason), 0, 1)) %>%
  mutate(Time = ifelse(is.na(dx2death), dx2latestVisit, dx2death)) %>%
  select(Study_ID, Outcome, Status, Time, Date_close)
colnames(KM_death)[5] = "Date"

KM_gastrostomy = gastrostomy_als_fu %>%
  mutate(Outcome = "Gastrostomy") %>%
  mutate(Status = ifelse(is.na(Event), 0, 1)) %>%
  mutate(Time = ifelse(is.na(dx2event), dx2latestVisit, dx2event)) %>%
  select(Study_ID, Outcome, Status, Time, Date_event)
colnames(KM_gastrostomy)[5] = "Date"

KM_NIV = NIV_als_fu %>%
  mutate(Outcome = "NIV") %>%
  mutate(Status = ifelse(is.na(Event), 0, 1)) %>%
  mutate(Time = ifelse(is.na(dx2event), dx2latestVisit, dx2event)) %>%
  select(Study_ID, Outcome, Status, Time, Date_event)
colnames(KM_NIV)[5] = "Date"

KM_tracheostomy = Tracheostomy_als_fu %>%
  mutate(Outcome = "Tracheostomy") %>%
  mutate(Status = ifelse(is.na(Event), 0, 1)) %>%
  mutate(Time = ifelse(is.na(dx2event), dx2latestVisit, dx2event)) %>%
  select(Study_ID, Outcome, Status, Time, Date_event)
colnames(KM_tracheostomy)[5] = "Date"

KM = rbind(KM_death, KM_gastrostomy, KM_NIV, KM_tracheostomy)
KM$Outcome = factor(KM$Outcome)
levels(KM$Outcome) = list(
  Gastrostomy = "Gastrostomy", 
  NIV = "NIV", 
  Death_or_tracheostomy = c("Death", "Trachestomy"))
KM_5years = KM %>%
  filter(Time < 70)

fit <- survfit(Surv(Time, Status) ~ Outcome, data = KM_5years)

plot_survival = ggsurvplot(fit, data = KM_5years, 
           conf.int = T, 
           risk.table = T, 
           xlim = c(0,70), 
           break.time.by = 12, 
           legend = c(0.25, 0.25),
           legend.labs = c("Gastrostomy", 
                           "NIV",
                           "Death_or_tracheostomy"), 
           title = "KM estimates survival curve", 
           ggtheme = theme_bw())
plot_survival

# plot: fu status, pie chart 
data_death_tracheostomy = KM %>%
  filter(Outcome == "Death_or_tracheostomy") %>%
  filter(Status == 1) %>%
  droplevels() %>%
  select(Study_ID, Outcome, Date)
data_refer = close %>%
  filter(Close_reason == "Refer") %>%
  droplevels() %>%
  select(Study_ID, Close_reason, Date_close)
colnames(data_refer)[c(2,3)] = c("Outcome", "Date")
data_lost2fu = close %>%
  filter(Close_reason == "Lost to f/u") %>%
  droplevels() %>%
  select(Study_ID, Close_reason, Date_close)
colnames(data_lost2fu)[c(2,3)] = c("Outcome", "Date")
data_outcome = rbind(data_death_tracheostomy, 
                     data_refer, 
                     data_lost2fu)
data_latest_fu = fu_als_latest %>%
  select(Study_ID, Date_visit)
colnames(data_latest_fu)[2] = "Date"
data_fu_outcome = merge(data_latest_fu, 
                        data_outcome, 
                        by="Study_ID", all.x = T)
data_fu_outcome = data_fu_outcome %>%
  select(Study_ID, Outcome, Date.x, Date.y)
colnames(data_fu_outcome)[c(3,4)] = c("Date_latestVisit", 
                                      "Date_outcome")
temp = data_fu_outcome %>%
  group_by(Outcome) %>%
  count()
count_fu_outcome <- data.frame(label = temp$Outcome,
                          value = temp$n,
                          color = RColorBrewer::brewer.pal(dim(temp)[1], 'Spectral'))
plot_fu_outcome = count_fu_outcome %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='FU status', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.footer(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = 'bottom-left') %>%
  pie.tooltips()
plot_fu_outcome

# plot: fu status, undefined  
# time between the latest visit to registry update date (months)
undefined_fu_outcome = data_fu_outcome %>% 
  filter(is.na(Outcome)) %>%
  mutate(duration_missing_mo = round(
    as.numeric(
      difftime(
        as.Date(date_update_registry, format = "%Y-%m-%d"),
        Date_latestVisit, 
               units = "days"))/30))
undefined_fu_outcome$missing_dur_gr = cut(undefined_fu_outcome$duration_missing_mo, 
             breaks = c(0,3,6,Inf), include.lowest = T, right = F)
levels(undefined_fu_outcome$missing_dur_gr) = c("Less than 3 mo", 
                                               "3 to 6 mo", 
                                               "Longer than 6 mo")
temp = undefined_fu_outcome %>%
  group_by(missing_dur_gr) %>%
  count()
data_undefined_fu_outcome <- data.frame(label = temp$missing_dur_gr,
                               value = temp$n,
                               color = RColorBrewer::brewer.pal(dim(temp)[1], 'Spectral'))
plot_undefined_fu_duration = data_undefined_fu_outcome %>%
  pier() %>%
  pie.size(inner=60, outer=80, width = 500, height = 400) %>%
  pie.header(text='Time from the latest visit', location='pie-center') %>%
  pie.subtitle(text='ALS') %>%
  pie.footer(text=paste('The SNU ALS/MND Registry', 
                        date_update_registry, sep = " "),
             location = 'bottom-left') %>%
  pie.tooltips()
plot_undefined_fu_duration

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
