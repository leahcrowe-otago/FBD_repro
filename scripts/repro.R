library(odbc);library(dplyr);library(DBI);library(ggplot2)

#reproductive assignment for available females

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
photo_analysis_calfyear_sql<-dbReadTable(con, "photo_analysis_calfyear")%>%filter(ID_NAME != "CULL")
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist
nrow(photo_analysis_calfyear_sql)

calves<-lifehist%>%
  filter(!is.na(MOM) & MOM != "")%>%
  dplyr::select(calf = NAME, SEX, MOM, BIRTH_YEAR, POD)

birth_calves<-calves%>%
  dplyr::select(-SEX, -calf)%>%
  tidyr::pivot_longer(cols = c(2), names_to = "year_type", values_to = "Year")%>%
  mutate(year_type = "CALF_BIRTH")%>%
  dplyr::rename("NAME" = "MOM")

head(birth_calves)

females<-lifehist%>%
  filter(SEX == "F")%>%
  filter(NAME != "COMMON")%>%
  dplyr::select(-SEX)%>%
  dplyr::select(POD, NAME, FIRST_CALF, BIRTH_YEAR, FIRST_YEAR, LAST_YEAR)%>%
  filter(LAST_YEAR != "<NA>")

females_long<-females%>%
  tidyr::pivot_longer(cols = c(3:6), names_to = "year_type", values_to = "Year")%>%
  filter(Year != "" & !is.na(Year))

females_tl<-females_long%>%
  filter(year_type != "LAST_YEAR")%>%
  filter(year_type != "FIRST_YEAR")%>%
  filter(year_type != "BIRTH_YEAR")%>%
  bind_rows(birth_calves)%>%
  arrange(NAME, Year)

head(females_tl)
female_first_last<-lifehist%>%
  filter(SEX == "F")%>%
  dplyr::select(NAME, BIRTH_YEAR, FIRST_YEAR, LAST_YEAR)

lh_long<-lifehist%>%
  filter(SEX == "F")%>%
  tidyr::pivot_longer(cols = 14:ncol(lifehist), names_to = "YEAR", values_to = "life_stage")%>%
  dplyr::select(NAME, POD, YEAR, life_stage)

lh_ls_tl<-lh_long%>%
  left_join(females_tl, by = c("NAME" = "NAME", "YEAR" = "Year", "POD" = "POD"))%>%
  mutate(life_stage = case_when(
    year_type == "FIRST_CALF" ~ "F",
    #year_type == "BIRTH_YEAR" ~ "C",
    year_type == "CALF_BIRTH" ~ "M",
    TRUE ~ life_stage
  ))%>%
  group_by(NAME, YEAR)%>%
  mutate(n = n())%>%
  #filter(NAME == "2-SCALLOPS")%>%
  ungroup()%>%
  arrange(NAME, YEAR, year_type)%>%
  filter(!(n == 2 & year_type == "CALF_BIRTH"))%>%
  mutate(life_stage_lag_lead = case_when(
    #lag(year_type) == "CALF_BIRTH" | lag(year_type) == "FIRST_CALF" ~ "W",
    lead(year_type) == "CALF_BIRTH" | lead(year_type) == "FIRST_CALF" ~ "P",
    TRUE ~ NA
  ))%>%
  mutate(life_stage = case_when(
    !is.na(life_stage_lag_lead) ~ life_stage_lag_lead,
    TRUE ~ life_stage))%>%
  as.data.frame()


###

calves_mom_list<-split(calves, calves$MOM)
length(calves_mom_list)
calves_mom_list[[33]]

for (j in 1:length(calves_mom_list)){
  x = as.data.frame(calves_mom_list[[j]])
  print(x)
  print(nrow(x))
  print("skip")

  for (i in 1:nrow(x)){
    print(i)
      wean_years<-photo_analysis_calfyear_sql%>%
        filter(ID_NAME == x$calf[i] | ID_NAME == x$MOM[i])%>%
        distinct(SURVEY_AREA, TRIP, DATETIME, ID_NAME, PHOTOGRAPHER, CALFYEAR)%>%
        group_by(TRIP, DATETIME, PHOTOGRAPHER)%>%
        mutate(n_time = n())%>%
        #filter(CALFYEAR == 2018)%>%
        arrange(DATETIME)%>%
        filter(n_time > 1)%>%
        ungroup()%>%
        distinct(CALFYEAR)%>%
        mutate(mom = x$MOM[i],
               wean_year = "W")
  
  if (i != nrow(x)){
    wean_years<-wean_years%>%
      filter(CALFYEAR < x$BIRTH_YEAR[i+1])
  }

  print(wean_years)
  lh_ls_tl$YEAR<-as.numeric(lh_ls_tl$YEAR)
  
  lh_ls_tl<-lh_ls_tl%>%
    left_join(wean_years, by = c("NAME" = "mom", "YEAR" = "CALFYEAR"))%>%
    mutate(life_stage = case_when(
      wean_year == "W" & life_stage == "A" ~ "W",
      TRUE ~ life_stage
    ))%>%
    dplyr::select(-wean_year)
  
  print(lh_ls_tl%>%group_by(life_stage)%>%tally())
  
}
}

lh_ls_tl%>%
  filter(NAME == "PAUA")

lh_ls_fix<-lh_ls_tl%>%
  arrange(NAME, YEAR)%>%
  group_by(NAME)%>%
  mutate(life_stage = case_when(
    (NAME == "GAP" ) &
      (YEAR == "2006") ~ "W",
    TRUE ~ life_stage
  ))%>%
  #get rid of false weaning by later times when mom/offspring seen together
  mutate(lag_ls = lag(life_stage))%>%
  mutate(life_stage = case_when(
    life_stage == "W" & lag_ls == "A" ~ "A",
    TRUE ~ life_stage
  ))%>% 
  #gets rid of second time false weaning by later times when mom/offspring seen together
  #most I've seen is 
  mutate(lag_ls = lag(life_stage))%>%
  mutate(life_stage = case_when(
    life_stage == "W" & lag_ls == "A" ~ "A",
    TRUE ~ life_stage
  ))%>%
  as.data.frame()

females_lifestage<-lh_ls_fix%>%
  filter(YEAR != 1990)%>% #rima and barcode both assigned W in 1990 for some reason
  left_join(female_first_last, by = "NAME")%>%
  filter(!is.na(LAST_YEAR))%>%
  filter(YEAR <= LAST_YEAR)%>%
  filter(life_stage != "<NA>")%>%
  filter(life_stage != "NA")%>%
  arrange(NAME, YEAR)%>%
  group_by(NAME)%>%
  mutate(min_first = as.numeric(min(FIRST_YEAR)),
         min_year = as.numeric(min(YEAR)))%>%
  ungroup()%>%
  mutate(FIRST_YEAR = case_when(
    min_year < min_first ~ min_year,
    TRUE ~ min_first
  ))%>%
  mutate(age = case_when(
    BIRTH_YEAR != "" ~ as.numeric(YEAR) - as.numeric(BIRTH_YEAR),
    BIRTH_YEAR == "" | is.na(BIRTH_YEAR) ~ as.numeric(YEAR) - as.numeric(FIRST_YEAR)
  ))

unique(females_lifestage$BIRTH_YEAR)

ls_tally<-females_lifestage%>%
  mutate(agebin = case_when(
    age >= 25 ~ "25+",
    age >= 20 & age < 25 ~ "20-25",
    age >= 15 & age < 20 ~ "15-20",
    age >= 10 & age < 15 ~ "10-15",
    age <= 20 ~ "<10"
  ))%>%
  mutate(life_stage2 = case_when(
    BIRTH_YEAR != "" ~  paste0(life_stage,"_",agebin),
    TRUE ~ paste0("U_",agebin)))%>%
  group_by(YEAR, age, POD, life_stage,life_stage2)%>%
  tally()%>%
  as.data.frame()

females_lifestage%>%
  filter(life_stage == "M")%>%
  filter(age > 25)

available_f<-ls_tally%>%filter(life_stage == "A" | life_stage == "U")%>%filter(YEAR != 2024)

calves_plot<-calves%>%
  group_by(POD,BIRTH_YEAR)%>%
  tally%>%
  ungroup()%>%
  add_row(POD = "DOUBTFUL",
              BIRTH_YEAR = "2015",
              n = 0)

ggplot(available_f)+
  geom_col(aes(x = YEAR, y = n, fill = life_stage2))+
  geom_point(data = calves_plot, mapping = aes(x = as.numeric(BIRTH_YEAR), y = n), size = 4, alpha = 0.5)+
  geom_line(data = calves_plot, mapping = aes(x = as.numeric(BIRTH_YEAR), y = n), alpha = 0.5)+
  facet_wrap(~POD)+
  xlim(c(2004,2023))+
  theme_bw()


females_lifestage%>%
  filter(life_stage == "A" & YEAR == 2017)%>%
  arrange(POD)%>%
  dplyr::select(NAME)


wean_years<-photo_analysis_calfyear_sql%>%
  filter(ID_NAME == "BACKSCRATCH" | ID_NAME == "BORAT")%>%
  distinct(SURVEY_AREA, TRIP, DATETIME, ID_NAME, PHOTOGRAPHER, CALFYEAR)%>%
  group_by(TRIP, DATETIME, PHOTOGRAPHER)%>%
  mutate(n_time = n())%>%
  #filter(CALFYEAR == 2018)%>%
  arrange(DATETIME)%>%
  filter(n_time > 1)%>%
  ungroup()%>%
  distinct(CALFYEAR)%>%
  mutate(mom = "2-SCALLOPS",
         wean_year = "W")

dbDisconnect(dbListConnections(drv=RMySQL::MySQL())[[1]])

