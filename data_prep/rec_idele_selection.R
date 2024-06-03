## ----setup----------------------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false

library(tidyverse)
library(readxl)
library(ggplot2)


## ----path-data-littoral-normand-------------------------------------------------------------------------
#| echo: false
path_to_data <- "data/"


## ----milk-recording-data--------------------------------------------------------------------------------
path_to_file <- "data/controle_elem.txt"
rec <- read.csv2(path_to_file)


## ----milk-recording-data-glimpse------------------------------------------------------------------------
glimpse(rec)


## ----milk-recording-formatting--------------------------------------------------------------------------
rec <- rec |> 
  rename(anim_id = ANIM,
         parity = NULACT,
         ctrl_date = DAPAUL,
         milk = LAI24H,
         fat = TBLACO,
         prot = TPLACO,
         scc = COLELC) |> 
  select(anim_id, ctrl_date, parity, milk, fat, prot, scc) |> 
  mutate(ctrl_date = as.Date(ctrl_date),
         milk = milk / 10)


## ----movement-data--------------------------------------------------------------------------------------
path_to_file <- "data/mvt.txt"
mvt <- read.csv2(path_to_file)


## ----mvt-glimpse----------------------------------------------------------------------------------------
glimpse(mvt)


## ----mvt-formatting-------------------------------------------------------------------------------------
mvt <- mvt |> 
  rename(anim_id = ANIM,
         birth_herd = NUM_EXP_NAIS,
         death_date = DAT_MORT,
         slaughter_date = DAT_ABAT,
         render_date = DAT_EQUAR,
         in_date = DAENCH,
         herd_id = NUM_EXP_DET,
         in_cause = CAENCH,
         out_date = DASORT,
         out_cause = CASORT) |> 
  mutate(death_date = as.Date(death_date),
         slaughter_date = as.Date(slaughter_date),
         render_date = as.Date(render_date),
         in_date = as.Date(in_date),
         out_date = as.Date(out_date)
         ) |> 
  select(anim_id, herd_id, in_date, in_cause, out_date, out_cause, birth_herd, death_date, slaughter_date, render_date)


## ----rec-mvt-join---------------------------------------------------------------------------------------
rec1 <- left_join(rec,
                  mvt |> 
                    select(anim_id, herd_id, in_date, out_date, out_cause)
  ) |> 
  filter(ctrl_date >= in_date & (ctrl_date <= out_date | is.na(out_date)))


## ----missing-herdid-milk-recording----------------------------------------------------------------------
rec2 <- anti_join(rec, rec1)

anim_missing_rec <- rec2 |> 
  group_by(anim_id) |> 
  summarise(
    rec_first = min(ctrl_date),
    rec_last = max(ctrl_date)
  )

anim_missing_mvt <- mvt |> 
  filter(anim_id %in% unique(anim_missing_rec$anim_id)) |> 
  group_by(anim_id) |> 
  summarise(mvt_first = min(in_date),
           mvt_last = max(out_date))

anim_missing <- full_join(anim_missing_rec, anim_missing_mvt)


## ----rec-final------------------------------------------------------------------------------------------
rec <- rec1 |> 
  select(herd_id, ctrl_date, anim_id, parity, milk, fat, prot, scc, out_cause, out_date) |> 
  arrange(herd_id, ctrl_date)


## ----rec-herd-year--------------------------------------------------------------------------------------
test_day <- rec |> 
  group_by(herd_id, ctrl_date) |> 
  summarise(
    n_cows = length(unique(anim_id))
  )


## ----number-cows-test-day-------------------------------------------------------------------------------
hist(test_day$n_cows[test_day$n_cows < 50], breaks = 1:50)


## -------------------------------------------------------------------------------------------------------
rec_herd_year <- test_day |>
  filter(n_cows > 10) |> 
  mutate(rec_year = format(as.Date(ctrl_date), "%Y")) |> 
  group_by(herd_id, rec_year) |> 
  summarise(
    n_rec = length(unique(ctrl_date)),
    n_cows_med = median(n_cows)
  )


## ----ncows-rec-year-dist--------------------------------------------------------------------------------
ggplot(rec_herd_year, aes(x = n_rec)) +
  geom_bar()


## ----herd-selection-nrec-year---------------------------------------------------------------------------
herd_year_sel <- rec_herd_year |> 
  filter(n_rec %in% 11:12)


## ----selected-milk-records------------------------------------------------------------------------------
test_days_sel <- left_join(herd_year_sel, test_day |> mutate(rec_year = format(as.Date(ctrl_date), "%Y")))

# hist(test_days_sel$n_cows)


## ----final-milk-recording-data--------------------------------------------------------------------------
rec_final <- left_join(test_days_sel |> 
                   select(herd_id, ctrl_date),
                 rec)


## ----formatage------------------------------------------------------------------------------------------

rec_final <- rec_final |> 
  mutate(ctrl_date = as.Date(ctrl_date))


## ----prd-chargment--------------------------------------------------------------------------------------
path_to_file <- paste0(path_to_data, "production.txt")

prd <- read.csv2(path_to_file)

colnames(prd) <- tolower(colnames(prd))

#modification du nom des colonnes pour simplifier
prd <- prd |> 
  rename(parity = nulact,
         anim_id = anim,
         calv_date = dadela,
         total_milk  = latolad,
         total_MG = mgtolad,
         total_MP = mptolad,
         total_tb = tbtolad,
         total_tp = tptolad)


## -------------------------------------------------------------------------------------------------------
rec_final <- inner_join(rec_final,
                  prd |> 
                    select(anim_id, parity, calv_date),
                  join_by(anim_id, parity))
 


## -------------------------------------------------------------------------------------------------------
rec_final <- rec_final |> 
  mutate(calv_date = as.Date(calv_date), ctrl_date = as.Date(ctrl_date))


rec_final <- rec_final |> 
  mutate(dim = as.integer(ctrl_date - calv_date))

rec_final <- rec_final |> 
  mutate(herd_id = as.character(herd_id))


## -------------------------------------------------------------------------------------------------------
rec_final <- rec_final |> 
  filter(dim <= 350)


## ----selection des mois de controle---------------------------------------------------------------------
rec_final <- rec_final |> 
  mutate(ctrl_months = format(as.Date(ctrl_date), "%Y-%m"))


## ----saving-final-milk-recording-data-------------------------------------------------------------------
write.csv2(rec_final, "generated_datasets/rec_final.csv")

