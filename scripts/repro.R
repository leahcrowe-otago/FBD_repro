#reproductive assignment for available females

source('~/git-otago/Fiordland_reporting/scripts/connect to MySQL.R', local = TRUE)$value
source('~/git-otago/Fiordland_reporting/scripts/life_history_ageclass update.R', local = TRUE)$value
lifehist<-lifehist

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
  dplyr::select(-SEX)%>%
  dplyr::select(POD, NAME, FIRST_CALF, BIRTH_YEAR, FIRST_YEAR, LAST_YEAR)

females_long<-females%>%
  tidyr::pivot_longer(cols = c(3:6), names_to = "year_type", values_to = "Year")%>%
  filter(Year != "" & !is.na(Year))

females_tl<-females_long%>%
  bind_rows(birth_calves)%>%
  arrange(NAME, Year)

head(females_tl)
female_last<-females_tl%>%
  filter(year_type == "LAST_YEAR")%>%
  dplyr::select(NAME, "last_year" = Year)

lh_long<-lifehist%>%
  filter(SEX == "F")%>%
  tidyr::pivot_longer(cols = 14:ncol(lifehist), names_to = "YEAR", values_to = "life_stage")%>%
  dplyr::select(NAME, POD, YEAR, life_stage)

lh_ls_tl<-lh_long%>%
  left_join(females_tl, by = c("NAME" = "NAME", "YEAR" = "Year", "POD" = "POD"))%>%
  mutate(life_stage = case_when(
    year_type == "FIRST_CALF" ~ "F",
    year_type == "BIRTH_YEAR" ~ "C",
    year_type == "CALF_BIRTH" ~ "M",
    TRUE ~ life_stage
  ))%>%
  group_by(NAME, YEAR)%>%
  mutate(n = n())%>%
  filter(!(n == 2 & year_type == "CALF_BIRTH"))%>%
  ungroup()%>%
  mutate(life_stage_lag_lead = case_when(
    lag(year_type) == "CALF_BIRTH" | lag(year_type) == "FIRST_CALF" ~ "W",
    lead(year_type) == "CALF_BIRTH" | lead(year_type) == "FIRST_CALF" ~ "P",
    TRUE ~ NA
  ))%>%
  mutate(life_stage = case_when(
    !is.na(life_stage_lag_lead) ~ life_stage_lag_lead,
    TRUE ~ life_stage))%>%
  as.data.frame()

females_lifestage<-lh_ls_tl%>%
  left_join(female_last, by = "NAME")%>%
  filter(!is.na(last_year))%>%
  filter(YEAR <= last_year)%>%
  filter(life_stage != "<NA>")%>%
  filter(life_stage != "NA")%>%
  group_by(YEAR, POD, life_stage)%>%
  tally()%>%
  as.data.frame()


ggplot(females_lifestage%>%filter(life_stage == "A" | life_stage == "U"))+
  geom_col(aes(x = YEAR, y = n, fill = life_stage))+
  facet_wrap(~POD)
