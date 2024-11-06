library(odbc);library(dplyr);library(DBI);library(ggplot2)

#reproductive assignment for available females

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")%>%filter(ID_NAME != "CULL")
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist
nrow(photo_analysis_calfyear_sql)

mothers<-lifehist%>%distinct(MOM)

calves<-lifehist%>%
  filter(!is.na(MOM) & MOM != "")%>%
  dplyr::select(calf = NAME, SEX, MOM, BIRTH_YEAR, POD)

# yr_season_code<-data.frame(CALFYEAR = c(rep(1990:2024, each = 4)),
#            season_code = c(rep(1:4)),
#            SEASON = c(rep(c("SPRING","SUMMER","AUTUMN","WINTER"))))

yr_season_code<-data.frame(CALFYEAR = c(rep(1990:2024, each = 2)),
                         season_code = c(rep(1:2)),
                         SEASON = c(rep(c("SPRING/SUMMER","AUTUMN/WINTER"))))

###

calves_mom_list<-split(calves, calves$MOM)
length(calves_mom_list)
calves_mom_list[[33]]

n = length(calves_mom_list)
mom_wean_list = list()
mom_wean_list = vector("list", length = n)

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
      wean<-photo_analysis_calfyear_sql%>%
        mutate(SEASON = case_when(
          SEASON == "SPRING" | SEASON == "SUMMER" ~ "SPRING/SUMMER",
          SEASON == "AUTUMN" | SEASON == "WINTER" ~ "AUTUMN/WINTER"))%>%
        filter(ID_NAME == x$calf[i] | ID_NAME == x$MOM[i])%>%
        distinct(SURVEY_AREA, TRIP, DATETIME, ID_NAME, PHOTOGRAPHER, CALFYEAR, SEASON)%>%
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
        distinct(CALFYEAR, SEASON)
      
      wean_season<-wean%>%
        filter(n_time > 1)%>%
        distinct(CALFYEAR, SEASON)%>%
        bind_rows(wean_seconds)%>%
        distinct(CALFYEAR, SEASON)%>%
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
  left_join(yr_season_code, by = c("CALFYEAR","SEASON"))%>%
  dplyr::rename("NAME" = "mom")%>%
  arrange(NAME, CALFYEAR, season_code)

dbDisconnect(dbListConnections(drv=RMySQL::MySQL())[[1]])

all_wean_females<-all_wean_ls%>%
  # life hist relative to reproductive females
  left_join(lifehist, by = c("NAME"))%>%
  mutate(first_season = case_when(
    month(ymd(FIRST_DATE)) >= 9 & month(ymd(FIRST_DATE)) <= 11 ~ paste0(as.numeric(year(FIRST_DATE))+1,"1"),
    month(ymd(FIRST_DATE)) == 12 ~ paste0(as.numeric(year(FIRST_DATE))+1,"1"),
    month(ymd(FIRST_DATE)) <= 2 ~ paste0(as.numeric(year(FIRST_DATE)),"1"),
    month(ymd(FIRST_DATE)) >= 3 & month(ymd(FIRST_DATE)) <= 5 ~ paste0(as.numeric(year(FIRST_DATE)),"2"),
    month(ymd(FIRST_DATE)) >= 6 & month(ymd(FIRST_DATE)) <= 8 ~ paste0(as.numeric(year(FIRST_DATE)),"2"),
    TRUE ~ '999909'
  ))%>%
  mutate(last_season = case_when(
    month(ymd(LAST_DATE)) >= 9 & month(ymd(LAST_DATE)) <= 11 ~ paste0(as.numeric(year(LAST_DATE))+1,"1"),
    month(ymd(LAST_DATE)) == 12 ~ paste0(as.numeric(year(LAST_DATE))+1,"1"),
    month(ymd(LAST_DATE)) <= 2 ~ paste0(as.numeric(year(LAST_DATE)),"1"),
    month(ymd(LAST_DATE)) >= 3 & month(ymd(LAST_DATE)) <= 5 ~ paste0(as.numeric(year(LAST_DATE)),"2"),
    month(ymd(LAST_DATE)) >= 6 & month(ymd(LAST_DATE)) <= 8 ~ paste0(as.numeric(year(LAST_DATE)),"2"),
    TRUE ~ '999909'
  ))%>%
  dplyr::select(CALFYEAR, SEASON, season_code, POD, NAME, SEX, calf, wean_season, FIRST_CALF, BIRTH_YEAR, first_season, DEATH_YEAR, last_season)

#filter(life_stage %in% c("A", "F", "P","W","M"))

ggplot(all_wean_females%>%filter(CALFYEAR > 2004))+
  geom_point(aes(x = as.numeric(last_season), y = NAME), size = 3)+
  geom_point(aes(x = as.numeric(first_season), y = NAME), size = 3, color = "green")+
  #geom_point(aes(x = as.numeric(BIRTH_YEAR), y = NAME), size = 3, color = "red")+
  geom_point(aes(x = as.numeric(paste0(CALFYEAR,season_code)), y = NAME))+
  facet_wrap(~POD, scales = "free")

###

## preparing cap history relative to reproduction
# all females known to be reproductively active at some point

obs_female_wean<-all_wean_females%>%
  filter(CALFYEAR > 2003 & CALFYEAR < 2024,
         !(POD == "DUSKY" & CALFYEAR < 2007))%>%
  mutate(FIRST_CALF = as.numeric(FIRST_CALF))



#####
##old ##
########


repro_avail<-obs_females%>%
  filter(YEAR > 2004 & YEAR < 2024,
         !life_stage %in% c("C","J","S-A"),
         !(POD == "DUSKY" & YEAR < 2008),
         !is.na(Phase))

repro_avail$Phase<-factor(repro_avail$Phase, levels = c("Unknown", "Available", "Weaning","Pregnant","Prima-pregnant"))

tally<-readRDS("~/git-otago/Fiordland_reporting/data/tally_2024-10-29.rds")%>%
  filter(!(POD == "DUSKY" & CALFYEAR < 2008))

repro_avail_tally<-repro_avail%>%
  group_by(POD, YEAR, Phase)%>%
  mutate(Phase_n = n())%>%
  left_join(tally, by = c("POD", "YEAR" = "CALFYEAR"))%>%
  distinct(POD, YEAR, Phase, Phase_n)%>%
  group_by(POD, YEAR)%>%
  mutate(year_total = sum(Phase_n))

#i don't think the below makes sense
# fecund<-repro_avail_tally%>%filter(Phase == 'Reproductive')%>%
#   mutate(fecundity_repro_participation = Phase_n/year_total)

ggplot(repro_avail%>%filter(life_stage != 0))+
  geom_histogram(aes(x = YEAR, fill = Phase), color = "black", binwidth = 1, alpha = 0.6)+
  geom_point(tally, mapping = aes(x = CALFYEAR, y = calfyear_ng/2))+
  geom_line(tally, mapping = aes(x = CALFYEAR, y = calfyear_ng/2))+
  #geom_point(fecund, mapping = aes(x = YEAR, y = fecundity*30))+
  #geom_line(fecund, mapping = aes(x = YEAR, y = fecundity*30))+
  facet_wrap(~POD)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_y_continuous(name = "# Adult-ish females", sec.axis = sec_axis(~.*2, name = "Population size"))

ggsave('./figures/count_avail.png', dpi = 600, width = 300, height = 230, units = "mm")

ggplot(fecund)+
  geom_point(mapping = aes(x = fecundity_repro_participation, y = Phase_n, color = as.factor(YEAR)))+
  geom_smooth(mapping = aes(x = fecundity_repro_participation, y = Phase_n), method = 'lm')+
  facet_wrap(~POD, scales = "free")
