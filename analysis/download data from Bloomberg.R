library("Rblpapi") 
library("tidyverse") 
library("psych")
library("visdat")
library("plm")
library("ggplot2")
library("DescTools")
library("stargazer")
blpConnect()

#create the vector of tickers to be retrieved
tickers <- c("QNBK QD Equity",
             "FAB UH Equity",
             "RJHI AB Equity",
             "NCB AB Equity",
             "SAMBA AB Equity",
             "EIB UH Equity",
             "RIBL AB Equity",
             "EMIRATES UH Equity",
             "SABB AB Equity",
             "ADCB UH Equity",
             "BSFR AB Equity",
             "QIBK QD Equity",
             "DIB UH Equity",
             "ALINMA AB Equity",
             "MARK QD Equity",
             "ARNB AB Equity",
             "CBQK QD Equity",
             "ALAWWAL AB Equity",
             "ALBI AB Equity",
             "ADIB UH Equity",
             "MASQ UH Equity",
             "SIBC AB Equity",
             "UNB UH Equity",
             "BJAZ AB Equity",
             "CBD UH Equity",
             "QIIK QD Equity",
             "DHBK QD Equity",
             "RAKBANK UH Equity",
             "NBF UH Equity",
             "ABQK QD Equity",
             "AUB BI Equity",
             "NBK KK Equity",
             "NBQ UH Equity",
             "KCBK QD Equity",
             "INVESTB UH Equity",
             "KFH KK Equity",
             "NBS UH Equity",
             "UAB UH Equity",
             "BOS UH Equity",
             "AJMANBAN UH Equity",
             "BOUBYAN KK Equity",
             "ABC BI Equity",
             "CBI UH Equity",
             "CBK KK Equity",
             "QFBQ QD Equity",
             "NBB BI Equity",
             "GBK KK Equity",
             "BURG KK Equity",
             "ALMUTAHE KK Equity",
             "FH UH Equity",
             "ABK KK Equity",
             "BBK BI Equity",
             "BARKA BI Equity",
             "EIBANK UH Equity",
             "ITHMR BI Equity",
             "KIB KK Equity",
             "WARBABAN KK Equity",
             "SALAM BI Equity",
             "BISB BI Equity",
             "KHCB BI Equity",
             "FACIL KK Equity",
             "OSOUL KK Equity",
             "AMAR KK Equity",
             "ALMADINA KK Equity",
             "GIH KK Equity"
)

#create vector data to store our fields of interest
data <- c(
  #BALANCE SHEET FIELDS
  "BS_CASH_NEAR_CASH_ITEM",
  "INTERBANKING_ASSETS",
  "ST_AND_LT_INVEST",
  "BS_TOT_LOAN",
  "BS_RSRV_LOAN_LOSS",
  "BS_LOAN_MTG",
  "BS_NON_PERFORM_ASSET",
  "BS_TOT_ASSET",
  "BS_CUSTOMER_DEPOSITS",
  "ST_BORROW_&_SECS_SOLD_UND_REPO",
  "BS_LT_BORROW",
  "BS_TOT_LIAB2",
  "TOTAL_EQUITY",
  #INCOME STATEMENT FIELDS
  "NET_REV",
  "NET_INT_INC",
  "INTEREST_INCOME",
  "IS_INTEREST_EXPENSES",
  "IS_TOTAL_NON-INTEREST_INCOME",
  "IS_PROV_FOR_LOAN_LOSS",
  "NON_INT_EXP",
  "EARN_FOR_COMMON",
  #ADEQUACY NUMBERS
  "ARDR_TIER1_COMMON_EQUITY",
  "BS_TIER1_CAP_RATIO",
  #MARKET CAP, RETURN/RISK Numbers (sharpe ratio, returns, vol, beta)
  "CUR_MKT_CAP"
)

bank_type_field <- c("IS_ISLAMIC") #Field for islamic/non-islamic bank in Bloomberg
bank_country_field <- c("COUNTRY") #Field for country of bank in Bloomberg

bank_type <- bdp(tickers, bank_type_field) #download bank type
bank_country <- bdp(tickers, bank_country_field) #download bank country

#We add Ticker columns to our data frame in order to join them with the panel by using the Ticker column
bank_type$Ticker <- rownames(bank_type) #add a Ticker column based on the row names
bank_country$Ticker <- rownames(bank_country) #add a Ticker column based on the row names 


#create a vector opt for options that we will pass to the bdh() function 
opt <- c("USD","YEARLY")
names(opt) <- c("currency","periodicitySelection") #Set the charactr vector name to currency

#call bdh() function to download data from Bloomberg and store data in BanksData list 
BanksData <- bdh(tickers, data, start.date = "FY 2006", end.date = "FY 2019", options=opt)

#merge data frames in the BanksData list into a single dataframe 
panel <- BanksData %>% map_df(.id="Ticker",rbind)


#join the panel table to the bank_country table
join_panel <- full_join(panel, bank_country, by="Ticker")

#join the panel table to the bank_type table
join_panel <- full_join(join_panel, bank_type, by="Ticker")