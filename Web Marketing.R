options(scipen=999)
set.seed(123456)

#### LIBRARIES ####

#update.packages(ask = FALSE, checkBuilt = TRUE)
#tinytex::reinstall_tinytex()

install.packages("DT")
install.packages("kableExtra")
install.packages('RQuantLib')
install.packages('tidyverse')
install.packages('wesanderson')
install.packages('rfm')
install.packages('rpart.plot')
install.packages('e1071')
install.packages('tree')
install.packages("funModeling")



library(car)
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib) 
library(tidyverse)
library(wesanderson)
library(caret)
library(rpart.plot)
library(randomForest)
library(rfm)
library(party)
library(e1071)
library(tree)
library(doBy)
library(kableExtra)
library(DT)
library(funModeling)

working_dir = "C:/Users/Ezio/Desktop/DIGITALM"
data_dir = "C:/Users/Ezio/Desktop/DIGITALM"
setwd(working_dir)




df_1_cli_fid <- read.csv2(
  file.path(data_dir,"raw_1_cli_fid.csv")
  , na.strings = c("NA", "")
)

#### INGESTION df_2 customers accounts details ####
df_2_cli_account <- read.csv2(
  file.path(data_dir,"raw_2_cli_account.csv")
  , na.strings = c("NA", "")
)

#### INGESTION df_3 customers addresses ####
df_3_cli_address <- read.csv2(
  file.path(data_dir,"raw_3_cli_address.csv")
  , na.strings = c("")
)

#### INGESTION df_4 customers privacy data ####
df_4_cli_privacy <- read.csv2(
  file.path(data_dir,"raw_4_cli_privacy.csv")
  , na.strings = c("NA", "")
)

#### INGESTION df_5 email campaign descriptions ####
df_5_camp_cat <- read.csv2(
  file.path(data_dir,"raw_5_camp_cat.csv")
  , na.strings = c("NA", "")
)

#### INGESTION df_6 email events ####
df_6_camp_event <- read.csv2(
  file.path(data_dir,"raw_6_camp_event.csv")
  , na.strings = c("NA", "")
)

#### INGESTION df_7 purchase tickets ####
df_7_tic <- read.csv2(
  file.path(data_dir,"raw_7_tic.csv")
  , na.strings = c("NA", "")
  , stringsAsFactors = FALSE
)


str(df_1_cli_fid)
summary(df_1_cli_fid)


### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates for combination CLI-FID !!!#

#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####
str(df_1_cli_fid_clean)
## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarise(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
  )
?n_distinct
tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#

## let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 3)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

#### EXPLORE COLUMNS of df_1 ####

### variable LAST_COD_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

#### TO DO df_1 ####
# EXPLORE the remaining df_1_cli_fid_clean relevant variables

### variable NUM_FIDS ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(NUM_FIDs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=NUM_FIDs, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid


### variable LAST_DT_ACTIVE ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(substring(LAST_DT_ACTIVE,1,4)) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(LAST_DT_ACTIVE, 1, 4)`)

df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

### variable FIRST_DT_ACTIVE ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(substring(FIRST_DT_ACTIVE,1,4)) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(FIRST_DT_ACTIVE, 1, 4)`)

df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

### variable LAST_STATUS_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_STATUS_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))


df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_STATUS_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)


#________________________________________________________________________________

#### FIRST LOOK of df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#### START CLEANING df_2 ####

df_2_cli_account_clean <- df_2_cli_account

#### CLEANING DUPLICATE VALUES in df_2 ####

## check for duplicates
df_2_cli_account_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_2 ####

## format boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## format numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

#### CLEANING MISSING VALUES in df_2 ####

## MISSING VALUES mapped as natural values ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

## MISSING VALUES mapped as new level in categorical columns ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! NOTE: all ID_CLI in df_1 are also in df_2 and vice-versa !!!#

#### EXPLORE COLUMNS of df_2 ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_emailprovider

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)

tot_emailproviders

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#

#### TO DO df_2 ####
# COMPUTE THE DISTRIBUTION for the remaining df_2_cli_fid_clean variables

#### RESHAPING df_2 ####

## keep the most frequent EMAIL_PROVIDER values and add a common factor level "OTHER" for the remaining ##
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)

## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

## add clean EMAIL_PROVIDER ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_emailproviderclean

## plot distribution
plot_df2_dist_emailproviderclean <- (
  ggplot(data=df2_dist_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_dist_emailproviderclean

#### TO DO df_2 ####
# EXPLORE the remaining df_2_cli_account_clean relevant variables

### W_PHONE variable

df2_dist_wphone <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_wphone

## plot distribution
plot_df2_dist_wphone <- (
  ggplot(data=df2_dist_wphone
         , aes(x=W_PHONE, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_dist_wphone

### ID_ADDRESS variable forse NO SENSE 

df2_dist_IDaddress <- df_2_cli_account_clean %>%
  group_by(ID_ADDRESS) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_IDaddress

## plot distribution
plot_df2_dist_IDaddress <- (
  ggplot(data=df2_dist_IDaddress
         , aes(x=ID_ADDRESS, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_dist_IDaddress

### TYP_CLI_ACCOUNT variable

df2_dist_TYP_CLI_ACCOUNT <- df_2_cli_account_clean %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_TYP_CLI_ACCOUNT

## plot distribution
plot_df2_dist_TYP_CLI_ACCOUNT <- (
  ggplot(data=df2_dist_TYP_CLI_ACCOUNT
         , aes(x=TYP_CLI_ACCOUNT, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_dist_TYP_CLI_ACCOUNT

### TYP_JOB variable

df2_dist_TYP_JOB <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_TYP_JOB

## plot distribution
plot_df2_dist_TYP_JOB <- (
  ggplot(data=df2_dist_TYP_JOB
         , aes(x=TYP_JOB, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_dist_TYP_JOB



#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)

#_____________________________________________________________________________

#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarise(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))


#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(is.na(PRV) & !is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarise(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### EXPLORE COLUMNS of df_3 ####

#### TO DO df_3 ####
# EXPLORE the df_3_cli_address_clean relevant variables

# COMPUTE THE DISTRIBUTION OF REGION

## compute distribution
df_3_cli_address_clean_region_distrib <- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarise(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_cli_address_clean_region_distrib

## plot distribution
plot_df_3_cli_address_clean_region_distrib <- (
  ggplot(data=df_3_cli_address_clean_region_distrib
         , aes(x=REGION, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_cli_address_clean_region_distrib

# COMPUTE THE DISTRIBUTION OF PRV

## compute distribution
df_3_cli_address_clean_prv_distrib <- df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarise(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_cli_address_clean_prv_distrib

## plot distribution
plot_df_3_cli_address_clean_prv_distrib <- (
  ggplot(data=df_3_cli_address_clean_prv_distrib
         , aes(x=PRV, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_cli_address_clean_prv_distrib

# COMPUTE THE DISTRIBUTION OF CAPs - NOT MEANINGFUL AS IT CALCULATES THE PERCENTAGE OF PEOPLE LIVING IN THE SAME CITY - TOO SMALL NUMBERS

## compute distribution
df_3_cli_address_clean_cap_distrib <- df_3_cli_address_clean %>%
  group_by(CAP) %>%
  summarise(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_cli_address_clean_cap_distrib

## plot distribution
plot_df_3_cli_address_clean_cap_distrib <- (
  ggplot(data=df_3_cli_address_clean_cap_distrib
         , aes(x=CAP, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_cli_address_clean_cap_distrib

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)

#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

#### TO DO df_4 ####
# EXPLORE the df_4_cli_privacy_clean relevant variables

# COMPUTE THE DISTRIBUTION OF FLAG_PRIVACY_1

## compute distribution
df_4_cli_privacy_clean_flag1_distrib <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarise(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_cli_privacy_clean_flag1_distrib

## plot distribution
plot_df_4_cli_privacy_clean_flag1_distrib <- (
  ggplot(data=df_4_cli_privacy_clean_flag1_distrib
         , aes(x=FLAG_PRIVACY_1, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_cli_privacy_clean_flag1_distrib

# COMPUTE THE DISTRIBUTION OF FLAG_PRIVACY_2

## compute distribution
df_4_cli_privacy_clean_flag2_distrib <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarise(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_cli_privacy_clean_flag2_distrib

## plot distribution
plot_df_4_cli_privacy_clean_flag2_distrib <- (
  ggplot(data=df_4_cli_privacy_clean_flag2_distrib
         , aes(x=FLAG_PRIVACY_2, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_cli_privacy_clean_flag2_distrib

# COMPUTE THE DISTRIBUTION OF FLAG_DIRECT_MKT

## compute distribution
df_4_cli_privacy_clean_flag_mkt_distrib <- df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarise(TOT_IDs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_4_cli_privacy_clean_flag_mkt_distrib

## plot distribution
plot_df_4_cli_privacy_clean_flag_mkt_distrib <- (
  ggplot(data=df_4_cli_privacy_clean_flag_mkt_distrib
         , aes(x=FLAG_DIRECT_MKT, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_4_cli_privacy_clean_flag_mkt_distrib


#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)

#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

#### EXPLORE COLUMNS of df_5 ####

#### TO DO df_5 ####
# EXPLORE the df_5_camp_cat_clean relevant variables

# COMPUTE THE DISTRIBUTION OF TYP_CAMP

## compute distribution
df_5_camp_cat_clean_TYP_CAMP <- df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarise(TOT_IDs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_5_camp_cat_clean_TYP_CAMP

## plot distribution
plot_df_5_camp_cat_clean_TYP_CAMP <- (
  ggplot(data=df_5_camp_cat_clean_TYP_CAMP
         , aes(x=TYP_CAMP, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_5_camp_cat_clean_TYP_CAMP

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)


#________________________________________________________________________________


#### FIRST LOOK of df_6 ####

str(df_6_camp_event)
summary(df_6_camp_event)

#### START CLEANING df_6 ####

df_6_camp_event_clean <- df_6_camp_event

#### CLEANING DATA TYPES in df_6 ####

## formatting dates and times ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATETIME = as.POSIXct(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EVENT_HOUR = hour(EVENT_DATETIME)) %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATETIME))

#### CONSISTENCY CHECK ID_CLI in df_1/df_6 ####

cons_idcli_df1_df6 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

#!!! NOTE: all ID_CLI in df_6 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_6 !!!#  

#### CONSISTENCY CHECK ID_CAMP in df_5/df_6 ####

cons_idcamp_df5_df6 <- df_5_camp_cat_clean %>%
  select(ID_CAMP) %>%
  distinct() %>%
  mutate(is_in_df_5 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CAMP) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CAMP"
  ) %>%
  group_by(is_in_df_5, is_in_df_6) %>%
  summarise(NUM_ID_CAMPs = n_distinct(ID_CAMP)) %>%
  as.data.frame()

cons_idcamp_df5_df6

#!!! NOTE: all ID_CAMP in df_6 are mapped in df_5, but not all ID_CAMP in df_5 are mapped in df_6 !!!#

#### RESHAPING df_6 ####

## remapping TYPE_EVENT values "E" [ERROR] and "B" [BOUNCE] into a level "F" [FAILURE] ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## adding type from df_5 ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

## organize the data adding to each sending event the corresponding opens/clicks/fails

# sends
df_sends <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT, ID_CLI, ID_CAMP, TYP_CAMP, ID_DELIVERY, SEND_DATE = EVENT_DATE) %>% as.data.frame()

# opens
# there could be multiple opens of the same communication
# 1- count the open events
# 2- consider explicitely only the first open

df_opens_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , OPEN_DATETIME = EVENT_DATETIME
         , OPEN_DATE = EVENT_DATE)

total_opens <- df_opens_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarise(NUM_OPENs = n_distinct(ID_EVENT_O))

df_opens <- df_opens_prep %>%
  left_join(total_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(OPEN_DATETIME == min(OPEN_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# clicks
# there could be multiple clicks of the same communication
# 1- count the click events
# 2- consider explicitely only the first click

df_clicks_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , CLICK_DATETIME = EVENT_DATETIME
         , CLICK_DATE = EVENT_DATE)

total_clicks <- df_clicks_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarise(NUM_CLICKs = n_distinct(ID_EVENT_C))

df_clicks <- df_clicks_prep %>%
  left_join(total_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(CLICK_DATETIME == min(CLICK_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# fails
df_fails <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , FAIL_DATETIME = EVENT_DATETIME
         , FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATETIME == min(FAIL_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# combine sends opens clicks and fails
df_6_camp_event_clean_final <- df_sends %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | OPEN_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(OPENED = !is.na(ID_EVENT_O)) %>%
  mutate(CLICKED = !is.na(ID_EVENT_C)) %>%
  mutate(FAILED = !is.na(ID_EVENT_F)) %>%
  mutate(DAYS_TO_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  select(ID_EVENT_S
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE
         
         , OPENED
         , OPEN_DATE
         , DAYS_TO_OPEN
         , NUM_OPENs
         
         , CLICKED
         , CLICK_DATE
         , NUM_CLICKs
         
         , FAILED
  )

#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview <- df_6_camp_event_clean_final %>% 
  summarise(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overview

### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_overviewbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP) %>%
  summarise(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overviewbytyp

## plot aggregate
plot_df6_overviewbytyp <- (
  ggplot(data=df6_overviewbytyp
         , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_overviewbytyp

### Variable OPENED ###

## compute aggregate
df6_dist_opened <- df_6_camp_event_clean_final %>%
  group_by(OPENED) %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_opened

## plot aggregate
plot_df6_dist_opened <- (
  ggplot(data=df6_dist_opened
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
)

plot_df6_dist_opened

### Variable OPENED by TYP_CAMP ###

## compute aggregate
df6_dist_openedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , OPENED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_openedbytyp

## plot aggregate
plot_df6_dist_openedbytyp <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp

## plot aggregate percent
plot_df6_dist_openedbytyp_percent <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp_percent

### Variable DAYS_TO_OPEN

## compute aggregate
df6_dist_daystoopen <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarise(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_daystoopen

## plot aggregate
plot_df6_dist_daystoopen <- (
  ggplot(data=df6_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_daystoopen

### DAYS_TO_OPEN vs CUMULATE PERCENT ###

## compute aggregate
df6_dist_daystoopen_vs_cumulate <- df6_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_dist_daystoopen_vs_cumulate <- (
  ggplot(data=df6_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    theme_minimal()
)

plot_df6_dist_daystoopen_vs_cumulate

#### TO DO df_6 ####
# EXPLORE the following relevant variables in df_6_camp_event_clean_final:

# - CLICKED/CLICKED by TYP_CAMP
## compute aggregate
df6_dist_clickedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, CLICKED)  %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , CLICKED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_clickedbytyp

## plot aggregate
plot_df6_dist_clickedbytyp <- (
  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_clickedbytyp

## plot aggregate percent
plot_df6_dist_clickedbytyp_percent <- (
  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_clickedbytyp_percent

# - FAILED/FAILED by TYP_CAP

## compute aggregate
df6_dist_failedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, FAILED)  %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , FAILED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_failedbytyp

## plot aggregate
plot_df6_dist_failedbytyp <- (
  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_failedbytyp

## plot aggregate percent
plot_df6_dist_failedbytyp_percent <- (
  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_failedbytyp_percent

# - NUM_OPENs

# compute aggregate
df6_dist_num_opens <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarise(AVG_OPENs = floor(mean(NUM_OPENs))) %>%
  ungroup() %>%
  group_by(AVG_OPENs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_num_opens 

## plot 
plot_df6_dist_num_opens <- (
  ggplot(data=df6_dist_num_opens
         , aes(x=AVG_OPENs, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_num_opens



# - NUM_CLICKs

# compute aggregate
df6_num_clicks <- df_6_camp_event_clean_final %>%
  filter(CLICKED) %>%
  group_by(ID_CLI) %>%
  summarise(AVG_CLICKs = floor(sum(NUM_CLICKs))) %>%
  ungroup() 
df6_dist_num_clicks <- df6_num_clicks %>%
  group_by(AVG_CLICKs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_num_clicks


## plot 
plot_df6_dist_num_clicks <- (
  ggplot(data=df6_dist_num_clicks
         , aes(x=AVG_CLICKs, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_num_opens


#### FINAL REVIEW df_6_clean ####

str(df_6_camp_event_clean_final)
summary(df_6_camp_event_clean_final)


#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

tickets_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
tickets_clean <- tickets_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME,
                                   format = "%Y-%m-%dT%H%M%S")) %>% #-- Date Time
  mutate(TIC_HOUR = hour(TIC_DATETIME))                         %>% #-- In Hours
  mutate(TIC_DATE = as.Date(TIC_DATETIME))                      %>% #-- In Date
  select(-DATETIME)                                             %>% #-- Removed DATETIME
  mutate(DIREZIONE = as.factor(DIREZIONE))                      %>% #-- Factorization
  mutate(COD_REPARTO = as.factor(COD_REPARTO))                      #-- Factorization





#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(tickets_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

tickets_clean <- tickets_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- tickets_clean %>% 
  summarise(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- tickets_clean %>%
  group_by(DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- tickets_clean %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- tickets_clean %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- tickets_clean %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarise(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- tickets_clean %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarise(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### TO DO df_7 ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO

## compute aggregate
df7_dist_importosconto_reparto <-tickets_clean %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_reparto <- df7_dist_importosconto_reparto %>%
  group_by(DIREZIONE) %>%
  summarise(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_reparto



#_________________________________________________________________
## plot aggregate
plot_df7_dist_importo_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo_reparto

## plot aggregate
plot_df7_dist_sconto_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_reparto

#________________________________________________________________

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

# number of tics per articolo
df7_dist_tics_articolo <-tickets_clean %>%
  group_by(ID_ARTICOLO) %>%
  summarise(NUM_TICs = sum(n_distinct(ID_SCONTRINO))) %>%
  ungroup()

df7_dist_tics_articolo



plot_df7_dist_tics_articolo <- (
  ggplot(data=df7_dist_tics_articolo %>%
           filter()
         , aes(color=ID_ARTICOLO, x=NUM_TICs)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_tics_articolo

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

## compute aggregate
df7_dist_importosconto_cli <- tickets_clean %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cli <- df7_dist_importosconto_cli %>%
  group_by(DIREZIONE) %>%
  summarise(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_cli



#_________________________________________________________________
## plot aggregate
plot_df7_dist_importo_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo_cli

## plot aggregate
plot_df7_dist_sconto_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_cli

#_____________________________________________________


# compute the distribution of customers by number of purchases (as described in the slides)

df7_dist_total_purch <-tickets_clean %>%
  filter(DIREZIONE == 1)                             %>% 
  group_by(ID_CLI)                                   %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>% 
  arrange(desc(TOT_PURCHASE))                           

df7_dist_total_purch




# compute the days for next purchase curve (as described in the slides)
#TROPPO TEMPO A RUNNARE

#data_for_next_purchase <- tickets_clean %>%
 # filter(DIREZIONE == 1) %>% #-- Only Purchases
  #select(ID_CLI,
   #      ID_ARTICOLO,
    #     TIC_DATE,
    #     DIREZIONE)      %>% #-- Variable Selection
  #arrange(ID_CLI)
#data_for_next_purchase

#df_np <- data_for_next_purchase %>%
 # group_by(ID_CLI) %>%
  #mutate(Diff = TIC_DATE - lag(TIC_DATE))

#data_for_next_purchase


#df_days_curve <- as.data.frame(table(df_date_diff$Days_difference))
#colnames(df_days_curve) <- c("Days_diff","Freq")
#df_days_curve <- df_days_curve[-1, ]
#df_days_curve$Perc <- df_days_curve$Freq/sum(df_days_curve$Freq)

#df_days_curve


#### FINAL REVIEW df_7_clean ####

str(tickets_clean)
summary(tickets_clean)

tickets_clean1 <- tickets_clean
########### MODELLO RFM #############


rfm_study_period <- tickets_clean %>%
  filter(TIC_DATE > as.Date("01/01/2018",
                            format = "%d/%m/%Y")) #-- Active Clients

#RECENCY

rfm_recency <- rfm_study_period %>%
  group_by(ID_CLI)       %>% #-- Grouped By Clients
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))


rfm_recency$RECENCY <- difftime(as.Date("30/04/2019",
                                        format = "%d/%m/%Y"),        #-- Recency
                                rfm_recency$LAST_PURCHASE_DATE,
                                units = "days")


#MONETARY

rfm_monetary <- rfm_study_period            %>%
  filter(DIREZIONE == 1)                    %>% #-- Only Purchases
  group_by(ID_CLI)                          %>% #-- Grouped By Clients
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = IMPORTO_LORDO - SCONTO) %>%
  ungroup()                                 %>%
  as.data.frame()                           %>%
  arrange(desc(IMPORTO_LORDO))

#FREQUENCY
rfm_frequency <- rfm_study_period                    %>%
  filter(DIREZIONE == 1)                             %>% #-- Only Purchases
  group_by(ID_CLI)                                   %>% #-- Grouped By Clients
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))


#MERGE

rfm <- merge(rfm_frequency, #-- Frequency
             rfm_monetary,  #-- Monetary
             by = "ID_CLI") #-- Key for Merge
rfm <- merge(rfm,           #-- Frequency + Monetary
             rfm_recency,   #-- Recency
             by = "ID_CLI") #-- Key for Merge

tickets_clean<- rfm





df<-tickets_clean[, c("ID_CLI", "LAST_PURCHASE_DATE","TOT_PURCHASE","IMPORTO_LORDO")]
colnames(df)[c(1,2,4)]<-c("tIDColName","tDateColName","tAmountColName")



getDataFrame <- function(df,startDate,endDate,tIDColName="tIDColName",tDateColName="tDateColName",tAmountColName="tAmountColName"){
  
    
    #order the dataframe by date descendingly
    df <- df[order(df[,tDateColName],decreasing = TRUE),]
    
    #remove the record before the start data and after the end Date
    df <- df[df[,tDateColName]>= startDate,]
    df <- df[df[,tDateColName]<= endDate,]
    
    #remove the rows with the duplicated IDs, and assign the df to a new df.
    newdf <- df[!duplicated(df[,tIDColName]),]
    
    # caculate the Recency(days) to the endDate, the smaller days value means more recent
    Recency<-as.numeric(difftime(endDate,newdf[,tDateColName],units="days"))
    
    # add the Days column to the newdf data frame
    newdf <-cbind(newdf,Recency)
    
    #order the dataframe by ID to fit the return order of table() and tapply()
    newdf <- newdf[order(newdf[,tIDColName]),]
    
    # caculate the frequency
    fre <- as.data.frame(table(df[,tIDColName]))
    Frequency <- fre[,2]
    newdf <- cbind(newdf,Frequency)
    
    #caculate the Money per deal
    m <- as.data.frame(tapply(df[,tAmountColName],df[,tIDColName],sum))
    Monetary <- m[,1]/Frequency
    newdf <- cbind(newdf,Monetary)
    
    return(newdf)
    
  } # end of function getDataFrame






getIndependentScore <- function(df,r=5,f=5,m=5) {
  
  if (r<=0 || f<=0 || m<=0) return
  
  #order and the score
  df <- df[order(df$Recency,-df$Frequency,-df$Monetary),]
  R_Score <- scoring(df,"Recency",r)
  df <- cbind(df, R_Score)
  
  df <- df[order(-df$Frequency,df$Recency,-df$Monetary),]
  F_Score <- scoring(df,"Frequency",f)
  df <- cbind(df, F_Score)
  
  df <- df[order(-df$Monetary,df$Recency,-df$Frequency),]
  M_Score <- scoring(df,"Monetary",m)
  df <- cbind(df, M_Score)
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  
  # caculate the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return (df)
  
} # end of function getIndependentScore



#Funzione per lo score

scoring <- function (df,column,r=5){
  
  #get the length of rows of df
  len <- dim(df)[1]
  
  score <- rep(0,times=len)
  
  # get the quantity of rows per 1/r e.g. 1/5
  nr <- round(len / r)
  if (nr > 0){
    
    # seperate the rows by r aliquots
    rStart <-0
    rEnd <- 0
    for (i in 1:r){
      
      #set the start row number and end row number
      rStart = rEnd+1
      
      #skip one "i" if the rStart is already in the i+1 or i+2 or ...scope.
      if (rStart> i*nr) next
      
      if (i == r){
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      
      # set the Recency score
      score[rStart:rEnd]<- r-i+1
      
      # make sure the customer who have the same recency have the same score
      s <- rEnd+1
      if(i<r & s <= len){
        for(u in s: len){
          if(df[rEnd,column]==df[u,column]){
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
    }
    
  }
  return(score)
} #end of function Scoring

# set the start and the end date of the analysis
startDate<-as.Date('2018-01-09')
endDate<-as.Date('2019-04-30')
df <- getDataFrame(df,startDate,endDate)

df1 <-getIndependentScore(df)

df1 <- df1[,-4]

##Draw the histograms in the R, F, and M dimensions so that we can see the distribution of customers in each RFM cell.

drawHistograms <- function(df,r=5,f=5,m=5){
  
  #set the layout plot window
  par(mfrow = c(f,r))
  
  names <-rep("",times=m)
  for(i in 1:m) names[i]<-paste("M",i)
  
  
  for (i in 1:f){
    for (j in 1:r){
      c <- rep(0,times=m)
      for(k in 1:m){
        tmpdf <-df[df$R_Score==j & df$F_Score==i & df$M_Score==k,]
        c[k]<- dim(tmpdf)[1]
        
      }
      if (i==1 & j==1) 
        barplot(c,col="lightblue",names.arg=names)
      else
        barplot(c,col="lightblue")
      if (j==1) title(ylab=paste("F",i))	
      if (i==1) title(main=paste("R",j))	
      
    }
    
  }
  
  par(mfrow = c(1,1))
  
} # end of drawHistograms function

#drawHistograms(df1)


par(mfrow = c(1,3))

hist(df$Recency)

hist(df$Frequency)

hist(df$Monetary)

S500<-df1[df1$Total_Score>500,]

dim(S500)

df_1<-df1[, c("tIDColName","TOT_PURCHASE","Monetary","Recency","tDateColName")]
colnames(df_1)[c(1,2,3,4,5)] <- c("customer_id","number_of_orders","revenue","recency_days","analysis_date")
rfm_data_customer<-df_1

analysis_date <- lubridate::as_date('2018-04-30')
rfm_result<- rfm_table_customer(rfm_data_customer, customer_id, number_of_orders, revenue,recency_days, analysis_date)


rfm_heatmap(rfm_result)
rfm_bar_chart(rfm_result)
rfm_order_dist(rfm_result)

# Associamo ad ogni categoria il valore minimo-massimo che deve avere in ogni categoria (recency freq etc) per essere tale. Vedi tabella sulla pagina

install.packages("DT")
install.packages("kableExtra")
library(kableExtra)
library(DT)


#imposto i segmenti della clientela
segment <- c(
"Champions", "Loyal Customers", "Potential Loyalist",
"New Customers", "Promising", "Need Attention",
"About To Sleep", "At Risk", "Can't Lose Them", "Hibernating",
"Lost"
)
description <- c(
  "Bought recently, buy often and spend the most",
  "Spend good money. Responsive to promotions",
  "Recent customers, spent good amount, bought more than once",
  "Bought more recently, but not often",
  "Recent shoppers, but haven't spent much",
  "Above average recency, frequency & monetary values",
  "Below average recency, frequency & monetary values",
  "Spent big money, purchased often but long time ago",
  "Made big purchases and often, but long time ago",
  "Low spenders, low frequency, purchased long time ago",
  "Lowest recency, frequency & monetary scores"
)
recency <- c("4 - 5", "2 - 5", "3 - 5", "4 - 5", "3 - 4", "2 - 3", "2 - 3", "<= 2", "<= 1", "1 - 2", "<= 2")
frequency <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")
monetary <- c("4 - 5", "3 - 5", "1 - 3", "<= 1", "<= 1", "2 - 3", "<= 2", "2 - 5", "4 - 5", "1 - 2", "<= 2")
segments <- tibble(
  Segment = segment, Description = description,
  R = recency, `F` = frequency, M = monetary
)

segments %>%
  kable() %>%
  kable_styling(full_width = TRUE, font_size = 12)

segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segments <- rfm_segment(rfm_result, segment_names, recency_lower, recency_upper, frequency_lower, frequency_upper, monetary_lower, monetary_upper)

segments<- rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
                       frequency_lower, frequency_upper, monetary_lower, monetary_upper)


segments<- segments[,-6]
# use datatable
segments %>%
  datatable(
    filter = "top",
    options = list(pageLength = 9, autoWidth = TRUE),
    colnames = c(
      "Index","Customer", "Segment", "RFM",
      "Orders",  "Total Spend","R","F","M"
    )
    
  )


#verifichiamo

segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)


rfm_plot_median_recency(segments)
rfm_plot_median_frequency(segments)



#MODELLO CHURN###

tickets_clean <- tickets_clean1

churn_study_period <- tickets_clean %>%
  filter(TIC_DATE < as.Date("1/1/2019",
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/10/2018",
                            format = "%d/%m/%Y"))



churn_recency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))
churn_recency$RECENCY <- difftime(as.Date("01/01/2019",
                                          format = "%d/%m/%Y"),          #-- Recency
                                  churn_recency$LAST_PURCHASE_DATE,
                                  units = "days")


#HOLDOUT
churn_holdout <- tickets_clean %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("28/02/2019",
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/01/2019",
                            format = "%d/%m/%Y"))
no_churner <- unique(churn_holdout$ID_CLI)


#-- Frequency to Merge
churn_frequency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))

#-- Monetary to Merge
churn_monetary <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = (IMPORTO_LORDO - SCONTO)) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))
churn <- merge(churn_recency, churn_frequency, by = "ID_CLI")
churn <- merge(churn, churn_monetary, by = "ID_CLI") %>%
  select(ID_CLI,
         RECENCY,
         SPESA, 
         TOT_PURCHASE)


# Assegnare a ciascun cliente una variabile target 0/1 in modo tale che 1 venga assegnato ai clienti che hanno agitato nel periodo di holout:

churn$CHURN <- 1
for (i in c(1:nrow(churn))){
  if (churn$ID_CLI[i] %in% no_churner) churn$CHURN[i] <- 0
}
churn$CHURN <- as.factor(churn$CHURN)
table(churn$CHURN)

#Defining a set of potentially relevant predictors variables to be computed within the lookback period:

# RECENCY;
# SPESA;
# TOT_PURCHASE;
# REGION;
# LAST_COD_FID;
# TYP_JOB.


churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB")], by = "ID_CLI")  #-- Add Type Job
churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI") #-- Add Type of Fidelity Card
region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")], by = "ID_ADDRESS") #-- Add Region
churn <- left_join(churn, region, by = "ID_CLI")



#trai test split
colnames(churn) <- c("ID_CLI", "RECENCY", "SPESA", "TOT_PURCHASE", "CHURN","TYP_JOB","LAST_COD_FID","REGION","ID_ADDRES")

churn <- na.omit(churn)
train_index <- createDataPartition(churn$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)


train <- churn[train_index,]
test <- churn[-train_index,]
table(train$CHURN)

#Recursive Partitioning And Regression Trees (`tree`);
# Random Forest (`tree_rf`);
# Logistic Regression (`logistic`);

#REGRESSION TREE

tree <- rpart(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
              data = train)
rpart.plot(tree, extra = "auto")
summary(tree) #-- num di acquisti è la variabile più importante
printcp(tree) #-- complexity parameter

#RANDOM FOREST

memory.limit(100000)
tree_rf <- randomForest(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
                        data = train, ntree = 100)
print(tree_rf)

#LOGISTIC REGRESSION
logistic <- train(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
                  data = train,
                  method = "glm")


summary(logistic)

#PREDICTION

# Prediction Treee
pred <- predict(tree, test[, -5], type = "class")
p1 <- unlist(pred)
confusionMatrix(p1, test$CHURN)


# Prediction RF
pred_rf <- predict(tree_rf, test[,-5], type = "class")
confusionMatrix(pred_rf, test$CHURN)


#Prediction Logistic
pred_logistic <- predict(logistic, test[, -5], type = "raw")
confusionMatrix(pred_logistic, test$CHURN)

# Accuracy CHURN
accuracy <- as.data.frame(t(cbind(confusionMatrix(pred_logistic, test$CHURN)$overall[1],
                                  confusionMatrix(pred_rf, test$CHURN)$overall[1],
                                  confusionMatrix(pred, test$CHURN)$overall[1])))
accuracy <- as.data.frame(cbind(c( "Logistic","Random Forest","Tree"),
                                accuracy))

colnames(accuracy) <- c("Models", "Accuracy")
ggplot(data = accuracy,
       aes(x = Models,
           y = Accuracy,
           fill = Models)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0.681, 0.693)) +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "Accuracy",
       x = "Models",
       y = " ") +
  scale_fill_manual(values = c("#FF1053","#6C6EA0","#66C7F4","#C1CAD6")) +
  theme(plot.title = element_text(hjust = 0.5)) + #-- Centering Title
  plot(accuracy$Accuracy)



## Assessment

#-- Probability
p_tree = predict(tree, test[,-5], "prob")[,1]
p_rf = predict(tree_rf, test[,-5], "prob")[,1]
p_log = predict(logistic, test[,-5], "prob")[,1]

#-- Data Frame
data_class = as.data.frame(cbind(p_tree, p_rf, p_log))
data_class = cbind(data_class, test$CHURN)
colnames(data_class) <- c("p_tree", "p_rf", "p_log", "churn")
head(data_class)
#-- Lift
lift_tree = gain_lift(data = data_class, score = 'p_tree', target = 'churn')
lift_rf = gain_lift(data = data_class, score = 'p_rf', target = 'churn')
lift_log = gain_lift(data = data_class, score = 'p_log', target = 'churn')

