cat("\014")
rm(list = ls())

library("Rblpapi")
library("tidyverse")
library("psych")
library("visdat")
library("plm")
library("ggplot2")
library("DescTools")
library("stargazer")
blpConnect()

#----------------------------------------- Download data from Bloomberg and prepare it--------------------------------#
source("Research/analysis/download data from Bloomberg.R")

#drop the tier1 ratio, and RWA variables from the join_panel dataframe and all NA observations 
clean_panel <- join_panel %>% select(c(-24,-25)) %>% drop_na()

#renaming data frame variables
names(clean_panel)[3] <- "Cash"
names(clean_panel)[4] <- "Interbanking Assets"
names(clean_panel)[5] <- "Investments"
names(clean_panel)[6] <- "Total Loans"
names(clean_panel)[7] <- "Loan Loss Reserve"
names(clean_panel)[8] <- "Net Loans"
names(clean_panel)[9] <- "Non Performing Assets"
names(clean_panel)[10] <- "Total Assets"
names(clean_panel)[11] <- "Deposits"
names(clean_panel)[12] <- "Short Term Borrowings" 
names(clean_panel)[13] <- "Long Term Borrowing"
names(clean_panel)[14] <- "Total Liabilities"
names(clean_panel)[15] <- "Total Equity"
names(clean_panel)[16] <- "Revenue"
names(clean_panel)[17] <- "Net Interest Income"
names(clean_panel)[18] <- "Interest Income"
names(clean_panel)[19] <- "Interest Expense"
names(clean_panel)[20] <- "NonInterest Income"
names(clean_panel)[21] <- "Loan Loss Provision"
names(clean_panel)[22] <- "Non Interest Expense"
names(clean_panel)[23] <- "Net Income"
names(clean_panel)[24] <- "Market Cap"
names(clean_panel)[25] <- "Country"
names(clean_panel)[26] <- "Type"

####change value in Type colum from TRUE/FALSE to Islamic/Conventional
clean_panel$Type[clean_panel$Type == "FALSE"] <- "Conventional"
clean_panel$Type[clean_panel$Type == "TRUE"] <- "Islamic"

####set the Type column as a factor 
clean_panel$Type <- as.factor(clean_panel$Type)

#summary stats for raw data 
capture.output(stargazer(clean_panel[3:23], type = "html"), file="Research/Tables/raw_data_summary.html")

#winsorize the clean_panel dataframe
clean_panel[3:23] <- lapply(clean_panel[3:23], Winsorize, na.rm=TRUE) 

#summary stats of the winsorized data frame 
capture.output(stargazer(clean_panel[3:24],type="html"),file="Research/Tables/winsorized_data_summary.html")



#----------------------------------------------create study variables--------------------------------------------#

                                          ##dependant variables##
clean_panel$ROE <- clean_panel$`Net Income`/clean_panel$`Total Equity`
clean_panel$ROA <- clean_panel$`Net Income`/clean_panel$`Total Assets`
  
                                          ##Independant variables##

####Business model and orientation####

clean_panel$`Loans/Deposits` <- clean_panel$`Net Loans`/clean_panel$Deposits
clean_panel$`Fee Based Income Ratio` <- clean_panel$`NonInterest Income` / (clean_panel$`Interest Income` + clean_panel$`NonInterest Income`)
clean_panel$`Non Deposit Funding` <- (clean_panel$`Short Term Borrowings` + clean_panel$`Long Term Borrowing`) / (clean_panel$`Short Term Borrowings` + clean_panel$`Long Term Borrowing` + clean_panel$Deposits)

####Bank Efficiency Indicators####

clean_panel$`Overhead Cost Ratio` <- clean_panel$`Non Interest Expense` / clean_panel$`Total Assets`
clean_panel$`Cost to Income Ratio` <- clean_panel$`Non Interest Expense` / clean_panel$Revenue 
clean_panel$`Efficiency Ratio` <- clean_panel$`Non Interest Expense` / clean_panel$`Net Income`

####Asset Quality Indicators####

clean_panel$`Loan Loss Provisions Ratio` <- clean_panel$`Loan Loss Provision` /  clean_panel$`Total Loans` #loan loss provision ratio
clean_panel$`Loan Loss Reserves Ratio` <- clean_panel$`Loan Loss Reserve` / clean_panel$`Total Loans`      #loan loss reserves ratio
clean_panel$`Non Performing Assets Ratio` <- clean_panel$`Non Performing Assets` / clean_panel$`Total Loans`   #Non Performing Assets Ratio

####Bank Stability Indicators#### 

clean_panel$`Liquidity Ratio` <- (clean_panel$Cash + clean_panel$`Interbanking Assets` + clean_panel$Investments) / (clean_panel$Deposits + clean_panel$`Short Term Borrowings`)
clean_panel$`Capital Asset Ratio` <- (clean_panel$`Total Equity`/clean_panel$`Total Assets`)


#### Other Bank Specific Indicators ####

clean_panel$size <- log(clean_panel$`Total Assets`)
clean_panel$`Net Interest Margin` <- clean_panel$`Net Interest Income` / clean_panel$`Total Loans` #net interest margin

#summary stats of the study variables 
capture.output(stargazer(clean_panel[27:41], type="html"), file="Research/Tables/variables summary.html")

#----------------------------------------------------------Create Study Subsets-----------------------------------#


study_variables_panel <- clean_panel %>% select(1:2,24:41) 
study_variables_panel_islamic <- study_variables_panel %>% filter(Type=="Islamic")
study_variables_panel_conventional <- study_variables_panel %>% filter(Type=="Conventional")


#--------------------------------Create historical charts and scatterplots (optional)------------------#
source("Research/analysis/historical charts scatterplots script.R")


#----------------------------------------------------- Pre-prep for Analysis ---------------------------------------#

#Create new dataframes to set as a panel for the PLM regressions  

study_variables_panel_pdataframe <- pdata.frame(study_variables_panel, index =  c("Ticker","date"), drop.index = FALSE)
study_variables_panel_islamic_pdataframe <- pdata.frame(study_variables_panel_islamic, index =  c("Ticker","date"), drop.index = FALSE)
study_variables_panel_conventional_pdataframe <- pdata.frame(study_variables_panel_conventional, index =  c("Ticker","date"), drop.index = FALSE)


#----------------------------------------------------- Fit regressions---------------------------------------#

                                      #---------------------Full date frame ------------------------------# 

#------------ROE Regressions-----------#
##Pooled Regression
model.roe <- lm(study_variables_panel_pdataframe$ROE
                ~study_variables_panel_pdataframe$`Loans/Deposits`
                +study_variables_panel_pdataframe$`Fee Based Income Ratio`
                +study_variables_panel_pdataframe$`Non Deposit Funding`
                +study_variables_panel_pdataframe$`Overhead Cost Ratio`
                +study_variables_panel_pdataframe$`Cost to Income Ratio`
                +study_variables_panel_pdataframe$`Efficiency Ratio`
                +study_variables_panel_pdataframe$`Loan Loss Provisions Ratio`
                +study_variables_panel_pdataframe$`Loan Loss Reserves Ratio`
                +study_variables_panel_pdataframe$`Non Performing Assets Ratio`
                +study_variables_panel_pdataframe$`Liquidity Ratio`
                +study_variables_panel_pdataframe$`Capital Asset Ratio`
                +study_variables_panel_pdataframe$`Net Interest Margin`)
summary(model.roe)

##Panel Regression with Fixed Effects
model.roe.fe <- plm(study_variables_panel_pdataframe$ROE
                    ~study_variables_panel_pdataframe$`Loans/Deposits`
                    +study_variables_panel_pdataframe$`Fee Based Income Ratio`
                    +study_variables_panel_pdataframe$`Non Deposit Funding`
                    +study_variables_panel_pdataframe$`Overhead Cost Ratio`
                    +study_variables_panel_pdataframe$`Cost to Income Ratio`
                    +study_variables_panel_pdataframe$`Efficiency Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Provisions Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Reserves Ratio`
                    +study_variables_panel_pdataframe$`Non Performing Assets Ratio`
                    +study_variables_panel_pdataframe$`Liquidity Ratio`
                    +study_variables_panel_pdataframe$`Capital Asset Ratio`
                    +study_variables_panel_pdataframe$`Net Interest Margin`
                    ,study_variables_panel_pdataframe, model = "within")
summary(model.roe.fe)


##Panel Regression with Random Effects
model.roe.re <- plm(study_variables_panel_pdataframe$ROE
                    ~study_variables_panel_pdataframe$`Loans/Deposits`
                    +study_variables_panel_pdataframe$`Fee Based Income Ratio`
                    +study_variables_panel_pdataframe$`Non Deposit Funding`
                    +study_variables_panel_pdataframe$`Overhead Cost Ratio`
                    +study_variables_panel_pdataframe$`Cost to Income Ratio`
                    +study_variables_panel_pdataframe$`Efficiency Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Provisions Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Reserves Ratio`
                    +study_variables_panel_pdataframe$`Non Performing Assets Ratio`
                    +study_variables_panel_pdataframe$`Liquidity Ratio`
                    +study_variables_panel_pdataframe$`Capital Asset Ratio`
                    +study_variables_panel_pdataframe$`Net Interest Margin`
                    ,study_variables_panel_pdataframe, model = "random")
summary(model.roe.re)

#Run hausman test to determine which of fixed or random effect models is most appropriate 
phtest(model.roe.fe,model.roe.re) #Fixed effect holds at the 10% 

#save the ROE regression tables to an html 
capture.output(stargazer(model.roe,model.roe.fe,model.roe.re,type="html"),
               file="Research/Tables/roe.regressions.html")

#------------ROA Regressions-----------#
##Pooled Regression
model.roa <- lm(study_variables_panel_pdataframe$ROA
                ~study_variables_panel_pdataframe$`Loans/Deposits`
                +study_variables_panel_pdataframe$`Fee Based Income Ratio`
                +study_variables_panel_pdataframe$`Non Deposit Funding`
                +study_variables_panel_pdataframe$`Overhead Cost Ratio`
                +study_variables_panel_pdataframe$`Cost to Income Ratio`
                +study_variables_panel_pdataframe$`Efficiency Ratio`
                +study_variables_panel_pdataframe$`Loan Loss Provisions Ratio`
                +study_variables_panel_pdataframe$`Loan Loss Reserves Ratio`
                +study_variables_panel_pdataframe$`Non Performing Assets Ratio`
                +study_variables_panel_pdataframe$`Liquidity Ratio`
                +study_variables_panel_pdataframe$`Capital Asset Ratio`
                +study_variables_panel_pdataframe$`Net Interest Margin`)

summary(model.roa)

##Panel Regression with Fixed Effects
model.roa.fe <- plm(study_variables_panel_pdataframe$ROA
                    ~study_variables_panel_pdataframe$`Loans/Deposits`
                    +study_variables_panel_pdataframe$`Fee Based Income Ratio`
                    +study_variables_panel_pdataframe$`Non Deposit Funding`
                    +study_variables_panel_pdataframe$`Overhead Cost Ratio`
                    +study_variables_panel_pdataframe$`Cost to Income Ratio`
                    +study_variables_panel_pdataframe$`Efficiency Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Provisions Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Reserves Ratio`
                    +study_variables_panel_pdataframe$`Non Performing Assets Ratio`
                    +study_variables_panel_pdataframe$`Liquidity Ratio`
                    +study_variables_panel_pdataframe$`Capital Asset Ratio`
                    +study_variables_panel_pdataframe$`Net Interest Margin`
                    ,study_variables_panel_pdataframe ,model = "within")
summary(model.roa.fe)


##Panel Regression with Random Effects
model.roa.re <- plm(study_variables_panel_pdataframe$ROA
                    ~study_variables_panel_pdataframe$`Loans/Deposits`
                    +study_variables_panel_pdataframe$`Fee Based Income Ratio`
                    +study_variables_panel_pdataframe$`Non Deposit Funding`
                    +study_variables_panel_pdataframe$`Overhead Cost Ratio`
                    +study_variables_panel_pdataframe$`Cost to Income Ratio`
                    +study_variables_panel_pdataframe$`Efficiency Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Provisions Ratio`
                    +study_variables_panel_pdataframe$`Loan Loss Reserves Ratio`
                    +study_variables_panel_pdataframe$`Non Performing Assets Ratio`
                    +study_variables_panel_pdataframe$`Liquidity Ratio`
                    +study_variables_panel_pdataframe$`Capital Asset Ratio`
                    +study_variables_panel_pdataframe$`Net Interest Margin`
                    ,study_variables_panel_pdataframe, model = "random")
summary(model.roa.re)

#Run hausman test to determine which of fixed or random effect models is most appropriate 
phtest(model.roa.fe,model.roa.re) #fixed effects is appropriate 

#save the ROA regression tables to an html 
capture.output(stargazer(model.roa,model.roa.fe,model.roa.re, type="html"),
               file="Research/Tables/roa.regressions.html") 




#--------------------------------------------Islamic date frame ----------------------------------------------------#
#------------ROA Regressions-----------#
##Pooled Regression
model.roa.islamic <- lm(study_variables_panel_islamic_pdataframe$ROA
                   ~study_variables_panel_islamic_pdataframe$`Loans/Deposits`
                   +study_variables_panel_islamic_pdataframe$`Fee Based Income Ratio`
                   +study_variables_panel_islamic_pdataframe$`Non Deposit Funding`
                   +study_variables_panel_islamic_pdataframe$`Overhead Cost Ratio`
                   +study_variables_panel_islamic_pdataframe$`Cost to Income Ratio`
                   +study_variables_panel_islamic_pdataframe$`Efficiency Ratio`
                   +study_variables_panel_islamic_pdataframe$`Loan Loss Provisions Ratio`
                   +study_variables_panel_islamic_pdataframe$`Loan Loss Reserves Ratio`
                   +study_variables_panel_islamic_pdataframe$`Non Performing Assets Ratio`
                   +study_variables_panel_islamic_pdataframe$`Liquidity Ratio`
                   +study_variables_panel_islamic_pdataframe$`Capital Asset Ratio`
                   +study_variables_panel_islamic_pdataframe$`Net Interest Margin`)
summary(model.roa.islamic)

##Panel Regression with Fixed Effects
model.roa.fe.islamic <- plm(study_variables_panel_islamic_pdataframe$ROA
                       ~study_variables_panel_islamic_pdataframe$`Loans/Deposits`
                       +study_variables_panel_islamic_pdataframe$`Fee Based Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Deposit Funding`
                       +study_variables_panel_islamic_pdataframe$`Overhead Cost Ratio`
                       +study_variables_panel_islamic_pdataframe$`Cost to Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Efficiency Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Provisions Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Reserves Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Performing Assets Ratio`
                       +study_variables_panel_islamic_pdataframe$`Liquidity Ratio`
                       +study_variables_panel_islamic_pdataframe$`Capital Asset Ratio`
                       +study_variables_panel_islamic_pdataframe$`Net Interest Margin`
                       ,study_variables_panel_islamic_pdataframe ,model = "within")
summary(model.roa.fe.islamic)


##Panel Regression with Random Effects
model.roa.re.islamic <- plm(study_variables_panel_islamic_pdataframe$ROA
                       ~study_variables_panel_islamic_pdataframe$`Loans/Deposits`
                       +study_variables_panel_islamic_pdataframe$`Fee Based Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Deposit Funding`
                       +study_variables_panel_islamic_pdataframe$`Overhead Cost Ratio`
                       +study_variables_panel_islamic_pdataframe$`Cost to Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Efficiency Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Provisions Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Reserves Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Performing Assets Ratio`
                       +study_variables_panel_islamic_pdataframe$`Liquidity Ratio`
                       +study_variables_panel_islamic_pdataframe$`Capital Asset Ratio`
                       +study_variables_panel_islamic_pdataframe$`Net Interest Margin`
                       ,study_variables_panel_islamic_pdataframe, model = "random")
summary(model.roa.re.islamic)

#Run hausman test to determine which of fixed or random effect models is most appropriate 
phtest(model.roa.fe.islamic,model.roa.re.islamic) #fixed effects is appropriate 

#save the ROA regression tables to an html 
capture.output(stargazer(model.roa.islamic,model.roa.fe.islamic,model.roa.re.islamic, type="html"),
               file="Research/Tables/roa.regressions.islamic.html") 

#------------ROE Regressions-----------#
##Pooled Regression
model.roe.islamic <- lm(study_variables_panel_islamic_pdataframe$ROE
                   ~study_variables_panel_islamic_pdataframe$`Loans/Deposits`
                   +study_variables_panel_islamic_pdataframe$`Fee Based Income Ratio`
                   +study_variables_panel_islamic_pdataframe$`Non Deposit Funding`
                   +study_variables_panel_islamic_pdataframe$`Overhead Cost Ratio`
                   +study_variables_panel_islamic_pdataframe$`Cost to Income Ratio`
                   +study_variables_panel_islamic_pdataframe$`Efficiency Ratio`
                   +study_variables_panel_islamic_pdataframe$`Loan Loss Provisions Ratio`
                   +study_variables_panel_islamic_pdataframe$`Loan Loss Reserves Ratio`
                   +study_variables_panel_islamic_pdataframe$`Non Performing Assets Ratio`
                   +study_variables_panel_islamic_pdataframe$`Liquidity Ratio`
                   +study_variables_panel_islamic_pdataframe$`Capital Asset Ratio`
                   +study_variables_panel_islamic_pdataframe$`Net Interest Margin`)
summary(model.roe.islamic)


##Panel Regression with Fixed Effects
model.roe.fe.islamic <- plm(study_variables_panel_islamic_pdataframe$ROE
                       ~study_variables_panel_islamic_pdataframe$`Loans/Deposits`
                       +study_variables_panel_islamic_pdataframe$`Fee Based Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Deposit Funding`
                       +study_variables_panel_islamic_pdataframe$`Overhead Cost Ratio`
                       +study_variables_panel_islamic_pdataframe$`Cost to Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Efficiency Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Provisions Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Reserves Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Performing Assets Ratio`
                       +study_variables_panel_islamic_pdataframe$`Liquidity Ratio`
                       +study_variables_panel_islamic_pdataframe$`Capital Asset Ratio`
                       +study_variables_panel_islamic_pdataframe$`Net Interest Margin`
                       ,study_variables_panel_islamic_pdataframe, model = "within")
summary(model.roe.fe.islamic)


##Panel Regression with Random Effects
model.roe.re.islamic <- plm(study_variables_panel_islamic_pdataframe$ROE
                       ~study_variables_panel_islamic_pdataframe$`Loans/Deposits`
                       +study_variables_panel_islamic_pdataframe$`Fee Based Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Deposit Funding`
                       +study_variables_panel_islamic_pdataframe$`Overhead Cost Ratio`
                       +study_variables_panel_islamic_pdataframe$`Cost to Income Ratio`
                       +study_variables_panel_islamic_pdataframe$`Efficiency Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Provisions Ratio`
                       +study_variables_panel_islamic_pdataframe$`Loan Loss Reserves Ratio`
                       +study_variables_panel_islamic_pdataframe$`Non Performing Assets Ratio`
                       +study_variables_panel_islamic_pdataframe$`Liquidity Ratio`
                       +study_variables_panel_islamic_pdataframe$`Capital Asset Ratio`
                       +study_variables_panel_islamic_pdataframe$`Net Interest Margin`
                       ,study_variables_panel_islamic_pdataframe, model = "random")
summary(model.roe.re.islamic)

#Run hausman test to determine which of fixed or random effect models is most appropriate 
phtest(model.roe.fe.islamic,model.roe.re.islamic) #fixed effects is appropriate 

#save the ROE regression tables to an html 
capture.output(stargazer(model.roe.islamic,model.roe.fe.islamic,model.roe.re.islamic, type="html"),
               file = "Research/Tables/roe.regressions.islamic.html")

#--------------------------------------------Conventional date frame ----------------------------------------------------#
#------------ROA Regressions-----------#
##Pooled Regression
model.roa.conventional <- lm(study_variables_panel_conventional_pdataframe$ROA
                        ~study_variables_panel_conventional_pdataframe$`Loans/Deposits`
                        +study_variables_panel_conventional_pdataframe$`Fee Based Income Ratio`
                        +study_variables_panel_conventional_pdataframe$`Non Deposit Funding`
                        +study_variables_panel_conventional_pdataframe$`Overhead Cost Ratio`
                        +study_variables_panel_conventional_pdataframe$`Cost to Income Ratio`
                        +study_variables_panel_conventional_pdataframe$`Efficiency Ratio`
                        +study_variables_panel_conventional_pdataframe$`Loan Loss Provisions Ratio`
                        +study_variables_panel_conventional_pdataframe$`Loan Loss Reserves Ratio`
                        +study_variables_panel_conventional_pdataframe$`Non Performing Assets Ratio`
                        +study_variables_panel_conventional_pdataframe$`Liquidity Ratio`
                        +study_variables_panel_conventional_pdataframe$`Capital Asset Ratio`
                        +study_variables_panel_conventional_pdataframe$`Net Interest Margin`)
summary(model.roa.conventional)

##Panel Regression with Fixed Effects
model.roa.fe.conventional <- plm(study_variables_panel_conventional_pdataframe$ROA
                            ~study_variables_panel_conventional_pdataframe$`Loans/Deposits`
                            +study_variables_panel_conventional_pdataframe$`Fee Based Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Deposit Funding`
                            +study_variables_panel_conventional_pdataframe$`Overhead Cost Ratio`
                            +study_variables_panel_conventional_pdataframe$`Cost to Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Efficiency Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Provisions Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Reserves Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Performing Assets Ratio`
                            +study_variables_panel_conventional_pdataframe$`Liquidity Ratio`
                            +study_variables_panel_conventional_pdataframe$`Capital Asset Ratio`
                            +study_variables_panel_conventional_pdataframe$`Net Interest Margin`
                            ,study_variables_panel_conventional_pdataframe ,model = "within")
summary(model.roa.fe.conventional)


##Panel Regression with Random Effects
model.roa.re.conventional <- plm(study_variables_panel_conventional_pdataframe$ROA
                            ~study_variables_panel_conventional_pdataframe$`Loans/Deposits`
                            +study_variables_panel_conventional_pdataframe$`Fee Based Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Deposit Funding`
                            +study_variables_panel_conventional_pdataframe$`Overhead Cost Ratio`
                            +study_variables_panel_conventional_pdataframe$`Cost to Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Efficiency Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Provisions Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Reserves Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Performing Assets Ratio`
                            +study_variables_panel_conventional_pdataframe$`Liquidity Ratio`
                            +study_variables_panel_conventional_pdataframe$`Capital Asset Ratio`
                            +study_variables_panel_conventional_pdataframe$`Net Interest Margin`
                            ,study_variables_panel_conventional_pdataframe, model = "random")
summary(model.roa.re.conventional)

#Run hausman test to determine which of fixed or random effect models is most appropriate 
phtest(model.roa.fe.conventional,model.roa.re.conventional) #fixed effects is appropriate 

#save the ROA regression tables to an html 
capture.output(stargazer(model.roa.conventional,model.roa.fe.conventional,model.roa.re.conventional, type="html"),
               file="Research/Tables/roa.regressions.conventional.html") 

#------------ROE Regressions-----------#
##Pooled Regression
model.roe.conventional <- lm(study_variables_panel_conventional_pdataframe$ROE
                        ~study_variables_panel_conventional_pdataframe$`Loans/Deposits`
                        +study_variables_panel_conventional_pdataframe$`Fee Based Income Ratio`
                        +study_variables_panel_conventional_pdataframe$`Non Deposit Funding`
                        +study_variables_panel_conventional_pdataframe$`Overhead Cost Ratio`
                        +study_variables_panel_conventional_pdataframe$`Cost to Income Ratio`
                        +study_variables_panel_conventional_pdataframe$`Efficiency Ratio`
                        +study_variables_panel_conventional_pdataframe$`Loan Loss Provisions Ratio`
                        +study_variables_panel_conventional_pdataframe$`Loan Loss Reserves Ratio`
                        +study_variables_panel_conventional_pdataframe$`Non Performing Assets Ratio`
                        +study_variables_panel_conventional_pdataframe$`Liquidity Ratio`
                        +study_variables_panel_conventional_pdataframe$`Capital Asset Ratio`
                        +study_variables_panel_conventional_pdataframe$`Net Interest Margin`)
summary(model.roe.conventional)


##Panel Regression with Fixed Effects
model.roe.fe.conventional <- plm(study_variables_panel_conventional_pdataframe$ROE
                            ~study_variables_panel_conventional_pdataframe$`Loans/Deposits`
                            +study_variables_panel_conventional_pdataframe$`Fee Based Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Deposit Funding`
                            +study_variables_panel_conventional_pdataframe$`Overhead Cost Ratio`
                            +study_variables_panel_conventional_pdataframe$`Cost to Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Efficiency Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Provisions Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Reserves Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Performing Assets Ratio`
                            +study_variables_panel_conventional_pdataframe$`Liquidity Ratio`
                            +study_variables_panel_conventional_pdataframe$`Capital Asset Ratio`
                            +study_variables_panel_conventional_pdataframe$`Net Interest Margin`
                            ,study_variables_panel_conventional_pdataframe, model = "within")
summary(model.roe.fe.conventional)


##Panel Regression with Random Effects
model.roe.re.conventional <- plm(study_variables_panel_conventional_pdataframe$ROE
                            ~study_variables_panel_conventional_pdataframe$`Loans/Deposits`
                            +study_variables_panel_conventional_pdataframe$`Fee Based Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Deposit Funding`
                            +study_variables_panel_conventional_pdataframe$`Overhead Cost Ratio`
                            +study_variables_panel_conventional_pdataframe$`Cost to Income Ratio`
                            +study_variables_panel_conventional_pdataframe$`Efficiency Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Provisions Ratio`
                            +study_variables_panel_conventional_pdataframe$`Loan Loss Reserves Ratio`
                            +study_variables_panel_conventional_pdataframe$`Non Performing Assets Ratio`
                            +study_variables_panel_conventional_pdataframe$`Liquidity Ratio`
                            +study_variables_panel_conventional_pdataframe$`Capital Asset Ratio`
                            +study_variables_panel_conventional_pdataframe$`Net Interest Margin`
                            ,study_variables_panel_conventional_pdataframe, model = "random")
summary(model.roe.re.conventional)

#Run hausman test to determine which of fixed or random effect models is most appropriate 
phtest(model.roe.fe.conventional,model.roe.re.conventional) #fixed effects is appropriate 

#save the ROE regression tables to an html 
capture.output(stargazer(model.roe.conventional,model.roe.fe.conventional,model.roe.re.conventional, type="html"),
               file = "Research/Tables/roe.regressions.conventional.html")


