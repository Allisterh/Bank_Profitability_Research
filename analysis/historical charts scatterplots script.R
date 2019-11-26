#---------------------------------------- historical averages graphs of ROE/ROA and covariants --------------------------------#

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(ROE=mean(ROE)) %>% 
  ggplot(aes(date, ROE, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/ROE by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(ROA=mean(ROA)) %>% 
  ggplot(aes(date, ROA, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/ROA by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Loans/Deposits`=mean(`Loans/Deposits`)) %>% 
  ggplot(aes(date, `Loans/Deposits`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Loans to Deposits by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Fee Based Income Ratio`=mean(`Fee Based Income Ratio`)) %>% 
  ggplot(aes(date,`Fee Based Income Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Fee Based Income by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Non Deposit Funding`=mean(`Non Deposit Funding`)) %>% 
  ggplot(aes(date,`Non Deposit Funding`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Non deposit funding by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Overhead Cost Ratio`=mean(`Overhead Cost Ratio`)) %>% 
  ggplot(aes(date,`Overhead Cost Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Overhead Cost by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Cost to Income Ratio`=mean(`Cost to Income Ratio`)) %>% 
  ggplot(aes(date,`Cost to Income Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Cost to Income by Country.svg", width = 10, height = 5.4)


study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Efficiency Ratio`=mean(`Efficiency Ratio`)) %>% 
  ggplot(aes(date,`Efficiency Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Efficiency Ratio by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Loan Loss Provisions Ratio`=mean(`Loan Loss Provisions Ratio`)) %>% 
  ggplot(aes(date,`Loan Loss Provisions Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Loan Loss Provisions by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Loan Loss Reserves Ratio`=mean(`Loan Loss Reserves Ratio`)) %>% 
  ggplot(aes(date,`Loan Loss Reserves Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Loan Loss Reserves by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Non Performing Assets Ratio`=mean(`Non Performing Assets Ratio`)) %>% 
  ggplot(aes(date,`Non Performing Assets Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Non Performing Assets by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Liquidity Ratio`=mean(`Liquidity Ratio`)) %>% 
  ggplot(aes(date,`Liquidity Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Liquidity Ratio by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Capital Asset Ratio`=mean(`Capital Asset Ratio`)) %>% 
  ggplot(aes(date,`Capital Asset Ratio`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Captial Asset Ratio by Country.svg", width = 10, height = 5.4)

study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(`Net Interest Margin`=mean(`Net Interest Margin`)) %>% 
  ggplot(aes(date,`Net Interest Margin`, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Net Interest Margin by Country.svg", width = 10, height = 5.4)


study_variables_panel %>% 
  group_by(Country, Type, date) %>% 
  summarise(size=mean(size)) %>% 
  ggplot(aes(date,size, color=Type)) + geom_line() + facet_wrap(~Country, scale="free") + theme_minimal()
ggsave("Tables and figures/Figures/Size by Country.svg", width = 10, height = 5.4)


#-----------------------------------------------------ROE & ROA vs Covariants by Type ---------------------------------------#


#ROE and Covariants Scatterplots by Type
study_variables_panel %>% ggplot(aes(`Loans/Deposits`, ROE)) +
  geom_point() + geom_smooth(method="lm") +facet_wrap(~Type) + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Loans to Deposit by Type.svg", width = 10, height = 5.4)


study_variables_panel %>% ggplot(aes(`Non Deposit Funding`, ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type) + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Non Deposit Funding by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Overhead Cost Ratio`, ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Overhead Cost by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Cost to Income Ratio`, ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Cost to Income by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Efficiency Ratio`, ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Efficiency Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Loan Loss Provisions Ratio`, ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Loan Loss Provisions by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Loan Loss Reserves Ratio`, ROE)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Loan Loss Reserves by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Non Performing Assets Ratio`,ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Non Performing Assets by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Liquidity Ratio`, ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Liquidity Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Capital Asset Ratio`, ROE)) +
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Capital Asset Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Net Interest Margin`, ROE)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROE vs Net Interest Margin by Type.svg", width = 10, height = 5.4)



#ROA and Covariants Scatterplots by Type
study_variables_panel %>% ggplot(aes(`Loans/Deposits`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type) + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Loans to Deposit by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Non Deposit Funding`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type) + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Non Deposit Funding Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Overhead Cost Ratio`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Overhead Cost Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Cost to Income Ratio`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Cost to Income Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Efficiency Ratio`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Efficiency Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Loan Loss Provisions Ratio`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Loan Loss Provisions Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Loan Loss Reserves Ratio`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Loan Loss Reserves by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Non Performing Assets Ratio`,ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Non Performing Assets by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Liquidity Ratio`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Liquidity Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Capital Asset Ratio`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Capital Asset Ratio by Type.svg", width = 10, height = 5.4)

study_variables_panel %>% ggplot(aes(`Net Interest Margin`, ROA)) + 
  geom_point() + geom_smooth(method="lm") + facet_wrap(~Type)  + theme_minimal()
ggsave("Tables and figures/Figures/ROA vs Net Interest Margin by Type.svg", width = 10, height = 5.4)