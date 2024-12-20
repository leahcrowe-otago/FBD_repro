library(odbc);library(dplyr);library(DBI);library(ggplot2);library(lubridate)

#reproductive assignment for available females

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")%>%filter(ID_NAME != "CULL")
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist
nrow(photo_analysis_calfyear_sql)

ind_lh<-lifehist%>%
  filter(LAST_YEAR != "<NA>")%>%
  tidyr::pivot_longer(cols = c(14:48), names_to = "CALFYEAR", values_to = "Ageclass")%>%
  filter(CALFYEAR > 2004)%>% # 2008 has missing data in Doubtful
  #filter(NAME != "COMMON")%>%
  dplyr::select(POD, NAME, SEX, FIRST_CALF, BIRTH_YEAR, FIRST_YEAR, LAST_YEAR, CALFYEAR, Ageclass)

ind_lh$CALFYEAR<-as.numeric(ind_lh$CALFYEAR)
ind_lh$FIRST_CALF = as.numeric(ind_lh$FIRST_CALF)

###

# this is used later for the timeline so keep year filter here
PA_filter<-photo_analysis_calfyear_sql%>%
  filter(CALFYEAR > 2004 & CALFYEAR < 2024)
unique(PA_filter$SURVEY_AREA)
##
#only DOUBTFUL and DUSKY SA
PA_filter_SA<-photo_analysis_calfyear_sql%>%
  filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY")%>% #only looking at doubtful and dusky complexes
  filter(CALFYEAR > 2004 & CALFYEAR < 2024)
unique(PA_filter_SA$SURVEY_AREA)

##########

avg_primo_age

mothers<-lifehist%>%distinct(MOM)

calves<-lifehist%>%
  filter(!is.na(MOM) & MOM != "")%>%
  dplyr::select(calf = NAME, SEX, MOM, BIRTH_YEAR, POD)

yr_season_code<-data.frame(CALFYEAR = c(rep(1990:2024, each = 3)),
                         season_code = c(rep(c(0,0.33,0.67))))

###

calves_mom_list<-split(calves, calves$MOM)
length(calves_mom_list)
calves_mom_list[[33]]

n = length(calves_mom_list)
mom_wean_list = list()
mom_wean_list = vector("list", length = n)

##

photo_ID_season_code<-photo_analysis_calfyear_sql%>%
  mutate(month = month(DATE))%>%
  mutate(season_code = case_when(
    month >= 9 & month <= 12 ~ 0,
    month >= 1 & month <= 4 ~ 0.33,
    month >= 5 & month <= 8 ~ 0.67))

for (j in 1:length(calves_mom_list)){

  x = as.data.frame(calves_mom_list[[j]])
  print(x)
  print(nrow(x))
  print("skip")

  n = nrow(x)
  wean_season_list = list()
  wean_season_list = vector("list", length = n)
  
  for (i in 1:nrow(x)){

    #searches for mother/offspring in same photo
      wean<-photo_ID_season_code%>%
        # mutate(SEASON = case_when(
        #   SEASON == "SPRING" | SEASON == "SUMMER" ~ "SPRING/SUMMER",
        #   SEASON == "AUTUMN" | SEASON == "WINTER" ~ "AUTUMN/WINTER"))%>%
        filter(ID_NAME == x$calf[i] | ID_NAME == x$MOM[i])%>%
        distinct(SURVEY_AREA, TRIP, DATETIME, ID_NAME, PHOTOGRAPHER, CALFYEAR, season_code)%>%
        group_by(TRIP, DATETIME, PHOTOGRAPHER)%>%
        mutate(n_time = n())%>%
        arrange(DATETIME,ID_NAME)%>%
        ungroup()
      
      wean_seconds<-wean%>%
        filter(n_time == 1)%>%
        mutate(lag_name = lag(ID_NAME),
               lead_name = lead(ID_NAME))%>%
        filter((ID_NAME != lag_name) | (ID_NAME != lead_name))%>%
        mutate(lag_time = ymd_hms(DATETIME) - lag(ymd_hms(DATETIME)),
               lead_time = lead(ymd_hms(DATETIME)) - ymd_hms(DATETIME))%>%
        filter(lag_time <= seconds(60) | lead_time <= seconds(60))%>%
        distinct(CALFYEAR, season_code)
      
      wean_season<-wean%>%
        filter(n_time > 1)%>%
        distinct(CALFYEAR, season_code)%>%
        bind_rows(wean_seconds)%>%
        distinct(CALFYEAR, season_code)%>%
        mutate(mom = x$MOM[i],
               calf = x$calf[i],
               wean_season = "W")
  
  if (i != nrow(x)){
    wean_season<-wean_season%>%
      filter(CALFYEAR < x$BIRTH_YEAR[i+1])
    }

      #wean_season$i <- i  # maybe you want to keep track of which iteration produced it?
      wean_season_list[[i]] <- wean_season  
      mom_wean_list[[j]]<-data.table::rbindlist(wean_season_list)
      
  print(wean_season)
  
}
}

all_wean_ls<-as.data.frame(data.table::rbindlist(mom_wean_list))%>%
  left_join(yr_season_code, by = c("CALFYEAR","season_code"))%>%
  dplyr::rename("NAME" = "mom")%>%
  arrange(NAME, CALFYEAR, season_code)

dbDisconnect(dbListConnections(drv=RMySQL::MySQL())[[1]])

all_wean_females<-all_wean_ls%>%
  # life hist relative to reproductive females
  left_join(lifehist, by = c("NAME"))%>%
  mutate(first_season = case_when(
    month(ymd(FIRST_DATE)) >= 9 & month(ymd(FIRST_DATE)) <= 12 ~ paste0(as.numeric(year(FIRST_DATE))+1),
    month(ymd(FIRST_DATE)) >= 1 & month(ymd(FIRST_DATE)) <= 4 ~ paste0(as.numeric(year(FIRST_DATE))+0.33),
    month(ymd(FIRST_DATE)) >= 5 & month(ymd(FIRST_DATE)) <= 8 ~ paste0(as.numeric(year(FIRST_DATE))+0.67),
    TRUE ~ '999909'
  ))%>%
  mutate(last_season = case_when(
    month(ymd(LAST_DATE)) >= 9 & month(ymd(LAST_DATE)) <= 12 ~ paste0(as.numeric(year(LAST_DATE))+1),
    month(ymd(LAST_DATE)) >= 1 & month(ymd(LAST_DATE)) <= 4 ~ paste0(as.numeric(year(LAST_DATE))+0.33),
    month(ymd(LAST_DATE)) >= 5 & month(ymd(LAST_DATE)) <= 8 ~ paste0(as.numeric(year(LAST_DATE))+0.67),
    TRUE ~ '999909'
  ))%>%
  dplyr::select(CALFYEAR, season_code, POD, NAME, SEX, calf, wean_season, FIRST_CALF, BIRTH_YEAR, first_season, DEATH_YEAR, last_season)

#filter(life_stage %in% c("A", "F", "P","W","M"))

ggplot(all_wean_females%>%filter(CALFYEAR > 2004))+
  geom_point(aes(x = as.numeric(last_season), y = NAME), size = 3)+
  geom_point(aes(x = as.numeric(first_season), y = NAME), size = 3, color = "green")+
  #geom_point(aes(x = as.numeric(BIRTH_YEAR), y = NAME), size = 3, color = "red")+
  geom_point(aes(x = as.numeric(paste0(CALFYEAR+season_code)), y = NAME))+
  facet_wrap(~POD, scales = "free")

###

## preparing cap history relative to reproduction
# all females known to be reproductively active at some point

obs_female_wean<-all_wean_females%>%
  filter(CALFYEAR > 2004 & CALFYEAR < 2024)%>%
  mutate(FIRST_CALF = as.numeric(FIRST_CALF))
head(obs_female_wean)

## photo timeline ----
yday("2017-05-01")

dates_eff<-function(x){x%>%
    left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
    filter(POD == "DOUBTFUL" | POD == "DUSKY")%>%
    distinct(DATE, CALFYEAR, SEASON, SURVEY_AREA, POD)%>%
    mutate(Ordinal = yday(DATE))%>%
    mutate(season_ordinal = Ordinal-244)%>%
    mutate(season_ordinal = case_when(
      season_ordinal < 0 ~ 365 + season_ordinal,
      TRUE ~ season_ordinal
    ))
}

PA_dates<-dates_eff(PA_filter)
PA_dates_SA<-dates_eff(PA_filter_SA)

PA_dates%>%filter(POD == "DOUBTFUL" & CALFYEAR == 2023)

PA_timeline<-ggplot()+
  geom_point(PA_dates%>%filter(SURVEY_AREA != "DOUBTFUL" & SURVEY_AREA != "DUSKY"), mapping = aes(x = season_ordinal, y = as.numeric(CALFYEAR)), color = "red", size = 2, shape = "square", alpha = 0.8)+
  geom_point(PA_dates%>%filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY"), mapping = aes(x = season_ordinal, y = as.numeric(CALFYEAR)), size = 1, shape = "square")+
  scale_x_continuous(breaks = c(0,30,61,91,122,153,181,212,242,273,303,334,366),
                     labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",""), limits = c(0,366))+
  annotate("rect", xmin = 242, xmax = 366, ymin = 2005, ymax = 2023,
           alpha = .1,fill = "blue")+
  annotate("rect", xmin = 1, xmax = 122, ymin = 2005, ymax = 2023,
           alpha = .1,fill = "orange")+
  scale_y_continuous(breaks = c(2005:2023))+
  theme_bw()+
  xlab("")+
  ylab("Dolphin year (01Sep_{y-1}–31Aug_{y})")+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")+
  facet_wrap(~POD)

PA_timeline

ggsave('./figures/PA_timeline.png', PA_timeline, dpi = 320, width = 200, height = 100, units = 'mm')

#########

all_photo<-photo_ID_season_code
SA_photo<-photo_ID_season_code%>%
  filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY")

#everyone, all data / only in survey area

final_ch<-function(x){
  x%>%
  distinct(ID_NAME, CALFYEAR, month, season_code)%>%
    left_join(lifehist, by = c("ID_NAME"="NAME"))%>%
    filter(POD == "DOUBTFUL" | POD == "DUSKY")%>%
    mutate(first_season = case_when(
         month(ymd(FIRST_DATE)) >= 9 & month(ymd(FIRST_DATE)) <= 12 ~ paste0(as.numeric(year(FIRST_DATE))+1),
         month(ymd(FIRST_DATE)) >= 1 & month(ymd(FIRST_DATE)) <= 4 ~ paste0(as.numeric(year(FIRST_DATE))+0.33),
         month(ymd(FIRST_DATE)) >= 5 & month(ymd(FIRST_DATE)) <= 8 ~ paste0(as.numeric(year(FIRST_DATE))+0.67),
         TRUE ~ '999909'
       ))%>%
    mutate(last_season = case_when(
         month(ymd(LAST_DATE)) >= 9 & month(ymd(LAST_DATE)) <= 12 ~ paste0(as.numeric(year(LAST_DATE))+1),
         month(ymd(LAST_DATE)) >= 1 & month(ymd(LAST_DATE)) <= 4 ~ paste0(as.numeric(year(LAST_DATE))+0.33),
         month(ymd(LAST_DATE)) >= 5 & month(ymd(LAST_DATE)) <= 8 ~ paste0(as.numeric(year(LAST_DATE))+0.67),
         TRUE ~ '999909'
       ))%>%
    dplyr::select(CALFYEAR, season_code, POD, NAME = ID_NAME, SEX, FIRST_CALF, BIRTH_YEAR, first_season, DEATH_YEAR, last_season)%>%
  mutate(year_season_code = CALFYEAR + season_code)%>%
  mutate(age = case_when(
    BIRTH_YEAR > 1 ~ year_season_code - as.numeric(BIRTH_YEAR),
    TRUE ~ year_season_code - as.numeric(first_season)))%>%
  mutate(ageclass = case_when(
    age >= as.integer(avg_primo_age) ~ "A",
    BIRTH_YEAR > 1 & age < as.integer(avg_primo_age) ~ "N",
    TRUE ~ "U"
  ))%>%
  filter(year_season_code >= 2005 & year_season_code < 2024)%>%
  distinct()
}

everyone<-final_ch(all_photo)
everyone_SA<-final_ch(SA_photo)

everyone_last<-everyone%>%distinct(NAME, last_season, POD, first_season)%>%mutate(time = as.numeric(last_season) - as.numeric(first_season))%>%
  mutate(sig_hist = case_when(
    time < 6 ~ "_<6",
    time >=6 & time < 10 ~ "_>=6 & <10",
    time >= 9 ~ ">=10"
  ))

ggplot(everyone_last)+
  geom_histogram(aes(x = as.factor(last_season), fill = sig_hist), stat = "count")+
  facet_wrap(~POD)+
  ylim(c(0,10))+
  theme_bw()+
  #scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

nrow(everyone)
e<-everyone%>%filter(year_season_code >= 2021)%>%group_by(year_season_code)%>%tally()
e<-everyone_SA%>%filter(year_season_code >= 2021)%>%group_by(year_season_code)%>%tally()

nrow(everyone_SA)

everyone$FIRST_CALF<-as.numeric(everyone$FIRST_CALF)
everyone_SA$FIRST_CALF<-as.numeric(everyone_SA$FIRST_CALF)

ch<-function(x){
  x%>%
  dplyr::select(-age)%>%
  arrange(year_season_code)%>%
  distinct(POD, NAME, SEX, year_season_code)%>%
  mutate(ch = 1)%>%
  tidyr::pivot_wider(names_from = year_season_code, values_from = ch)%>%
  arrange(POD, SEX)%>%
  group_by(POD)%>%
  mutate(
  ind = 1:n(),  
  pod_ch = case_when(
    POD == "DOUBTFUL" ~ 1,
    POD == "DUSKY" ~ 2))%>%
  mutate(sex_ch = case_when(
    SEX == "F" ~ 1,
    SEX == "M" ~ 2,
    SEX == "X" ~ 2
  ))%>%
  ungroup()
}

everyone_ch<-ch(everyone)
everyone_SA_ch<-ch(everyone_SA)

summary(everyone_ch)
summary(everyone_SA_ch)

# Go to phi_all*.R
saveRDS(everyone_ch, "./data/everyone_everywhere.RDS")
# Go to phi_SA*.R
saveRDS(everyone_SA_ch, "./data/everyone_SA.RDS")

### 
## Effort ----

photo_days<-function(x){
  x%>%
  filter(CALFYEAR >= 2005 & CALFYEAR < 2024)%>%
  mutate(month = month(DATE))%>%
  mutate(season_code = case_when(
    month >= 9 & month <= 12 ~ 0,
    month >= 1 & month <= 4 ~ 0.33,
    month >= 5 & month <= 8 ~ 0.67))%>%
  mutate(year_season_code = CALFYEAR+season_code)%>%
  distinct(POD, year_season_code, DATE)%>%
  group_by(POD, year_season_code)%>%
  tally()%>%
  as.data.frame()
}

photo_days_all<-photo_days(PA_dates)
photo_days_SA<-photo_days(PA_dates_SA)

IDperday_fxn<-function(x,y){

  ID_sampling<-x%>%
    distinct(POD, NAME, year_season_code)%>%
    group_by(POD, year_season_code)%>%
    tally()
  
  occasions<-yr_season_code%>%
    filter(CALFYEAR >= 2005 & CALFYEAR < 2024)%>%
    mutate(year_season_code = CALFYEAR + season_code)
  
  occasions_Dbt<-occasions%>%
    mutate(POD = "DOUBTFUL")
  
  occasions_Dus<-occasions%>%
    mutate(POD = "DUSKY")
  
  occ_POD<-occasions_Dbt%>%
    full_join(occasions_Dus, by = c("year_season_code","POD","CALFYEAR","season_code"))
  
  ID_per_day<-occ_POD%>%
    left_join(y, by = c("year_season_code","POD"))%>%
    left_join(ID_sampling, by = c("POD", "year_season_code"))%>%
    mutate(IDperDay = n.y/n.x)%>%
    mutate(season = case_when(
      grepl(".33", year_season_code) ~ "Summer",
      grepl(".67", year_season_code) ~ "Winter",
      TRUE ~ "Spring"
    ))
  
  ID_per_day[is.na(ID_per_day)]<-0
  
  ID_per_day
}

ID_per_day_all<-IDperday_fxn(everyone,photo_days_all)%>%mutate(area = "All areas")
saveRDS(ID_per_day_all, "./data/ID_per_day_all.RDS") 

ID_per_day_SA<-IDperday_fxn(everyone_SA,photo_days_SA)%>%mutate(area = "Complexes only")
saveRDS(ID_per_day_SA, "./data/ID_per_day_SA.RDS") 

##num individuals with effort days, not finished
ggplot()+
  geom_col(everyone%>%distinct(POD, NAME, year_season_code)%>%group_by(POD, year_season_code)%>%tally(), mapping = aes(x = year_season_code, y = n))+
  geom_point(photo_days_all, mapping = aes(x=year_season_code, y = n*10), color = "red")+
  facet_wrap(~POD)

ggsave('./figures/eff_days_photos.png', dpi = 300, width = 300, height = 175, units = "mm")

###

ID_per_day_together<-ID_per_day_all%>%
  bind_rows(ID_per_day_SA)

## point graph of # IDs/effort days in each sampling period
ID_eff_plot<-ggplot(ID_per_day_together)+
  geom_point(aes(x = year_season_code, y = IDperDay, shape = as.factor(season), color = area))+
  geom_path(aes(x = year_season_code, y = IDperDay, color = area))+
  facet_wrap(~POD)+
  theme_bw()+
  xlab("Sampling period")+
  ylab("# individuals identified/day")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

ID_eff_plot

ggsave('./figures/ID_eff_plot.png', ID_eff_plot, dpi = 320, width = 200, height = 100, units = 'mm')

count_ID_samp<-ggplot(ID_per_day_together)+
  geom_point(aes(x = year_season_code, y = n.y, shape = as.factor(season), color = area))+
  geom_path(aes(x = year_season_code, y = n.y, color = area))+
  facet_wrap(~POD)+
  theme_bw()+
  xlab("Sampling period")+
  ylab("# individuals identified")+
  scale_x_continuous(breaks = c(2005:2024))+
  theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  scale_color_manual(values = c("All areas" = "red", "Complexes only" = "black"))

count_ID_samp

ggsave('./figures/count_ID_samp.png', count_ID_samp, dpi = 320, width = 200, height = 100, units = 'mm')

##

long_samp<-ID_per_day_all%>%
  dplyr::select(year_season_code, POD, n.x)%>%
  filter(year_season_code != 2007.67)%>%
  tidyr::pivot_wider(names_from = year_season_code, values_from = n.x)

saveRDS(long_samp, "./data/long_samp.RDS") 

long_samp_SA<-ID_per_day_SA%>%
  dplyr::select(year_season_code, POD, n.x)%>%
  filter(year_season_code != 2007.67)%>%
  tidyr::pivot_wider(names_from = year_season_code, values_from = n.x)

saveRDS(long_samp_SA, "./data/long_samp_SA.RDS") 

# merge everyone with the reproductive females
everyone_repro<-everyone%>%
  left_join(obs_female_wean, by = c("CALFYEAR", "season_code", "POD", "NAME", "SEX", "FIRST_CALF", "BIRTH_YEAR", "first_season", "DEATH_YEAR", "last_season"))

