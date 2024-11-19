library(odbc);library(dplyr);library(DBI);library(ggplot2);library(lubridate)

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value

photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")%>%filter(ID_NAME != "CULL")
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist


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

#only DOUBTFUL and DUSKY SA
PA_filter_SA<-photo_analysis_calfyear_sql%>%
  filter(SURVEY_AREA == "DOUBTFUL" | SURVEY_AREA == "DUSKY")%>% #only looking at doubtful and dusky complexes
  filter(CALFYEAR > 2004 & CALFYEAR < 2024)

PA_long<-PA_filter%>%
  distinct(SURVEY_AREA, ID_NAME, CALFYEAR, SEASON)%>%
  mutate(ch = 1)%>%
  mutate(season_code = case_when(
    SEASON == "SPRING" | SEASON == "SUMMER" ~ 0,
    SEASON == "AUTUMN" | SEASON == "WINTER" ~ 5))%>%
  mutate(SEASON = case_when(
      SEASON == "SPRING" | SEASON == "SUMMER" ~ "SPRING/SUMMER",
      SEASON == "AUTUMN" | SEASON == "WINTER" ~ "AUTUMN/WINTER"))%>%
  arrange(CALFYEAR, season_code, ID_NAME)%>%
  left_join(ind_lh, by = c("ID_NAME" = "NAME", "CALFYEAR"))%>%
  mutate(age = case_when(
    BIRTH_YEAR > 0 ~ CALFYEAR-as.numeric(BIRTH_YEAR),
    BIRTH_YEAR == "" ~ CALFYEAR-as.numeric(FIRST_YEAR) #min_age
  ))%>%
  mutate(age_value = case_when(
    BIRTH_YEAR > 0 ~ "actual", #birth year known
    BIRTH_YEAR == "" ~ "est" #birth year not known
  ))%>%
  mutate(age = case_when( ## for some reproductive females, we can infer an older age based on calving history
    age_value == "est" & as.numeric(FIRST_CALF) > 0 & (as.numeric(FIRST_CALF) - as.integer(avg_primo_age)) < as.numeric(FIRST_YEAR) ~ age + (as.integer(avg_primo_age) - (as.numeric(FIRST_CALF) - as.numeric(FIRST_YEAR))),
    TRUE ~ age
  ))%>%
  mutate(sex_ch = case_when(
         SEX == "F" ~ 1,
         SEX == "M" ~ 2))%>%
  left_join(obs_female_wean, by = c("ID_NAME" = "NAME", "POD", "CALFYEAR","SEASON","season_code","SEX","FIRST_CALF","BIRTH_YEAR"))%>%
  arrange(POD, ID_NAME, CALFYEAR, season_code)%>%
  dplyr::select(-SURVEY_AREA)%>%
  distinct()%>%
  mutate(repro_ch = case_when(
    wean_season == "W" ~ 2,
    TRUE ~ 1
  ))%>%
  mutate(pod_ch = case_when(
    POD == "DOUBTFUL" ~ 1,
    POD == "DUSKY" ~ 2
  ))

all_occasions<-data.frame(CALFYEAR = c(rep(2005:2023, each = 2)),
                         season_code = c(rep(c(0,5))),
                         SEASON = c(rep(c("SPRING/SUMMER","AUTUMN/WINTER"))))

obs_ch<-all_occasions%>%
  left_join(PA_long, by = c("CALFYEAR","season_code","SEASON"))%>%
  filter(Ageclass == "A" | Ageclass == "U")%>% # can consider allowing the year before first weaning to mean adult
  mutate(calfyr_season = CALFYEAR + (season_code/10))%>%
  ungroup()%>%
  dplyr::select(ID_NAME, POD, pod_ch, SEX, sex_ch, calfyr_season, repro_ch)%>%
  filter(POD == "DOUBTFUL" | POD == "DUSKY")%>%
  tidyr::pivot_wider(names_from = calfyr_season, values_from = repro_ch)%>%
  arrange(POD, ID_NAME)%>%
  mutate(ind = 1:n())

nrow(obs_ch)

#saveRDS(obs_ch, file = paste0("./data/SA_obs_ch_",Sys.Date(),".rds"))
saveRDS(obs_ch, file = paste0("./data/obs_ch_",Sys.Date(),".rds"))

## photo timeline
yday("2014-09-01")

PA_dates<-PA_filter%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  filter(POD == "DOUBTFUL" | POD == "DUSKY")%>%
  distinct(DATE, CALFYEAR, SEASON, SURVEY_AREA, POD)%>%
  mutate(Ordinal = yday(DATE))%>%
  mutate(season_ordinal = Ordinal-244)%>%
  mutate(season_ordinal = case_when(
    season_ordinal < 0 ~ 365 + season_ordinal,
    TRUE ~ season_ordinal
  ))

PA_dates%>%filter(is.na(POD))

PA_timeline<-ggplot(PA_dates)+
  geom_point(aes(x = season_ordinal, y = as.factor(CALFYEAR), color = SURVEY_AREA), size = 0.2)+
  scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335,366),
                     labels = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",""), limits = c(1,366))+
  annotate("rect", xmin = 182, xmax = 366, ymin = as.factor(2005), ymax = as.factor(2023),
           alpha = .1,fill = "blue")+
  annotate("rect", xmin = 1, xmax = 181, ymin = as.factor(2005), ymax = as.factor(2023),
           alpha = .1,fill = "orange")+
  #scale_y_continuous(breaks = seq(min(data$Year), max(data$Year), by = 1))+
  theme_bw()+
  xlab("")+
  ylab("Calendar year")+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom")+
  facet_wrap(~POD)

PA_timeline

ggsave('./figures/PA_timeline.png', PA_timeline, dpi = 320, width = 200, height = 100, units = 'mm')

########################################

#############
## old ###
##########

obs_ch_F<-CH_LH_long_F%>%
  mutate(SEX == "F")%>%
  dplyr::select(ID_NAME, POD, CALFYEAR, ch)%>%
  mutate(SEX = 1)%>%
  filter(CALFYEAR > 2007 & CALFYEAR < 2024)%>%
  tidyr::pivot_wider(names_from = CALFYEAR, values_from = ch)%>%
  group_by(POD)%>%
  #mutate(ind = 1:n())%>%
  ungroup()%>%
  replace(is.na(.), 0) # 0 for not sighted


## everybody + females
CH_LH_long_NF<-photo_analysis_calfyear_sql%>%
  mutate(ch = 1)%>%
  distinct(ID_NAME, CALFYEAR, ch)%>%
  left_join(lifehist, by = c("ID_NAME" = "NAME"))%>%
  arrange(POD, ID_NAME, CALFYEAR)%>%
  mutate(age = case_when(
    BIRTH_YEAR > 0 ~ CALFYEAR-as.numeric(BIRTH_YEAR),
    BIRTH_YEAR == "" ~ CALFYEAR-as.numeric(FIRST_YEAR) #min_age
  ))%>%
  mutate(age_value = case_when(
    BIRTH_YEAR > 0 ~ "actual", #birth year known
    BIRTH_YEAR == "" ~ "est" #birth year not known
  ))%>%
  filter(SEX != "F",
         POD == "DOUBTFUL" | POD == "DUSKY")%>%
  filter(CALFYEAR > 2004 & CALFYEAR < 2024,
         !(POD == "DUSKY" & CALFYEAR < 2008))

obs_ch_NF<-CH_LH_long_NF%>%
  dplyr::select(ID_NAME, POD, SEX, CALFYEAR, 31:ncol(.)-1, -age,-CALFYEAR)%>%
  mutate(SEX = case_when(SEX == "M" ~ 2,
                         SEX == "X" ~ 2))%>%
  distinct()

obs_ch_NF[obs_ch_NF == "D"] <- 0
obs_ch_NF[obs_ch_NF == "C"] <- 0
obs_ch_NF[obs_ch_NF == "J"] <- 0
obs_ch_NF[obs_ch_NF == "S-A"] <- 0
obs_ch_NF[obs_ch_NF == "NA"] <- 0
obs_ch_NF[obs_ch_NF == "A"] <- 1
obs_ch_NF[obs_ch_NF == "U"] <- 1

obs_ch_NF<-obs_ch_NF%>%
  dplyr::select(ID_NAME, POD, SEX, `2008`:`2023`)%>%
  mutate(across(4:ncol(.), as.numeric))%>%
  replace(is.na(.), 0) %>%
  mutate(sum = rowSums(.[4:ncol(.)]))%>%
  filter(sum>0)%>%
  dplyr::select(-sum)

obs_ch<-obs_ch_NF%>%
  bind_rows(obs_ch_F)%>%
  group_by(POD)%>%
  arrange(ID_NAME)%>%
  mutate(ind = 1:n())%>%
  ungroup()



# capture history by observation state
obs_state_ch<-CH_LH_long%>%
  mutate(obs_state = case_when(
    ch == 0 ~ 3, # not sighted
    ch == 1 & life_stage == "A" ~ 1 ,# sighted and not known to be pregnant and no calf/offspring
    ch == 1 & life_stage == "P" ~ 2, # sighted and known to be pregnant
    ch == 1 & Phase == "Weaning" ~ 2, # sighted and in close association with calf/offspring
    ch == 1 & life_stage == "U" ~ 3 # sighted but unsure if a breeding female at this time need to estimate this
  ))%>%
  dplyr::select(ID_NAME, POD, CALFYEAR, obs_state)%>%
  tidyr::pivot_wider(names_from = CALFYEAR, values_from = obs_state)%>%
  group_by(POD)%>%
  mutate(ind = 1:n())%>%
  ungroup()%>%
  replace(is.na(.), 4) # 4 for not sighted

#capture history by age
age_state_ch<-CH_LH_long%>%
  mutate(age_ch = case_when(
    life_stage == "U" ~ 1, # unknown is repro age
    TRUE ~ 2 # known to be old enough, or have a calving history
  ))%>%
  dplyr::select(ID_NAME, POD, CALFYEAR, age_ch)%>%
  tidyr::pivot_wider(names_from = CALFYEAR, values_from = age_ch)%>% 
  group_by(POD)%>%
  mutate(ind = 1:n())%>%
  ungroup()%>%
  replace(is.na(.), 0)

#fixed effects, all female

fixed_effects<-CH_LH_long%>%
  distinct(ID_NAME, POD, age_value)%>% 
  group_by(POD)%>%
  mutate(ind = 1:n())

