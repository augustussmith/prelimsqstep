#Extra democracy data
#Countries: Belize, Bulgaria, Cape Verde, Chile, Cyprus, Estonia, Latvia, Lithuania,
#Mongolia, Namibia, Panama, Poland, Romania, Slovenia, South Africa, Taiwan, Vanuatu 
library(tidyverse)

qog_data %>%
  group_by(cname) %>%
  filter(ccodealp == "BLZ"| ccodealp == "BGR" | ccodealp == "CPV" | ccodealp == "CHL" | ccodealp == "CYP" | ccodealp == "EST" | ccodealp == "LVA" | ccodealp == "LTU" | ccodealp == "MNG" | ccodealp == "NAM" | ccodealp == "PAN" | ccodealp == "POL" | ccodealp == "ROU" | ccodealp == "SVN" | ccodealp == "ZAF" | ccodealp == "TWN" | ccodealp == "VUT") %>%
  summarise(gol_enep = gol_enep[which(!is.na(gol_enep))[1]])

#No epp data for Namibia and South Africa
mean_eff_parties_new_democracies <- c(2.06, 4.12, 1.81, 7.05, 3.62, 8.72, 6.21, 4.58, 2.71, NA, 4.71, 13.9, 2.21, 8.34, NA, 3.14, 2.56)

#Data from Gallagher Electoral Disproportionality Data, 121 Countries, 1945-2014, v2, Christopher Gandrud

disproportionality_data %>%
  group_by(country) %>%
  filter(iso2c == "BZ"| iso2c == "BG" | iso2c == "CV" | iso2c == "CL" | iso2c == "CY" | iso2c == "EE" | iso2c == "LV" | iso2c == "LT" | iso2c == "MN" | iso2c == "NA" | iso2c == "PA" | iso2c == "PL" | iso2c == "RO" | iso2c == "SI" | iso2c == "ZA" | iso2c == "TW" | country == "Vanuatu") %>%
  summarise(mean_disproportionality = disproportionality[which(!is.na(disproportionality))[1]])

#No disproportionality data for Vanuatu
mean_disproportionality <- c(19.7, 5.37, 7, 6.3, 17.1, 7.23, 4.14, 9.61, 27.4, 0.93, 7.87, 3.62, 0.9, 2.94, 0.36, 7.05, NA)

#Next variable is minimal winning cabinets
#Take a mean of number of minimal winning cabinets and one party cabinets
#Type is 1 and 2 in QoG  cpds_tg

qog_data_new_democracies = qog_data %>%
  group_by(cname) %>%
  filter(ccodealp == "BLZ"| ccodealp == "BGR" | ccodealp == "CPV" | ccodealp == "CHL" | ccodealp == "CYP" | ccodealp == "EST" | ccodealp == "LVA" | ccodealp == "LTU" | ccodealp == "MNG" | ccodealp == "NAM" | ccodealp == "PAN" | ccodealp == "POL" | ccodealp == "ROU" | ccodealp == "SVN" | ccodealp == "ZAF" | ccodealp == "TWN" | ccodealp == "VUT") %>%
  
#Code block for finding specific '1' or '2' values
qog_data %>%
  group_by(cname) %>%
  filter(ccodealp == "SVN") %>%
  filter(cpds_tg == 4) %>%
  nrow()

#Code block for finding total number of data points
qog_data %>%
  group_by(cname) %>%
  filter(ccodealp == "SVN") %>%
  filter(!is.na(cpds_tg)) %>%
  select(cpds_tg) %>%
  nrow()


#Note - one party is majority single party
#Minority is one party minority
bulgaria_onepartycabinet = 8
bulgaria_minimalcabinet = 1 #No minority
bulgaria_totalcabinet = 28
((8 / 28 * 100) + (9 / 28 * 100)) / 2 #30.35714

cyprus_onepartycabinet = 40 #No minority
cyprus_totalcabinet = 40
((40 / 40 * 100) + (40 / 40 * 100)) / 2 #100

estonia_onepartycabinet = 0
estonia_minimalcabinet = 20
estonia_minorityonepartycabinet = 3
estonia_totalcabinet = 26
((3 / 26 * 100) + (20 / 26 * 100)) / 2 #44.23077

latvia_oneparty = 0
latvia_minimalcabient = 9 #No minority
latvia_totalcabinet = 25
((0 / 25 * 100) + (9 / 25 * 100)) / 2 #18

lithuania_onepartycabinet = 5
lithuania_minimalcabinet = 9 #No minority
lithuania_totalcabinet = 26
((5 / 26 * 100) + (14 / 26 * 100)) / 2 #36.53846

poland_onepartycabinet = 2
poland_minimalcabinet = 16
poland_minoritycabinet = 2
poland_totalcabinet = 27
((4 / 27 * 100) + (18 / 27 * 100)) / 2 #40.74074

romania_onepartycabinet = 6
romania_minimalcabinet = 4
romania_minoritycabinet = 6
romania_totalcabinet = 28
((12 / 28 * 100) + (10 / 28 * 100)) / 2 #39.28571

#No single party
slovenia_minimalcabinet = 17
slovenia_totalcabinet = 25
((0 / 25 * 100) + (17 / 25 * 100)) / 2 #34


#No data for Belize, Cape Verde, Chile, Mongolia, Namibia, Panama, South Africa, Taiwan, Vanuatu

#This code block is for checking which countries we don't have data for
qog_data %>%
  group_by(cname) %>%
  filter(ccodealp == "ZAF" | ccodealp == "TWN" | ccodealp == "VUT") %>%
  select(cpds_tg) %>%
  filter(cpds_tg == 1 | cpds_tg ==2) 



#Disproportionality. Elections: 1991-2012 [in line with other data time frame. Gives 7 elections]
#I do 7 largest parties
#V-S^2
#No data for 2004. Excluded

vanuatu_disproportionality = mean(sqrt(0.5 * ((30.6-41)^2 + (22.6-21.7)^2 + (20.4-10/46)^2 + (15.4-4/46)^2 + (4.6-1/46)^2 + (2.9-1/46)^2 + (1.9-1/46)^2)), sqrt(0.5 * ((31.4-20/50)^2 + (27.4-17/50)^2 + (23.4-9/50)^2 + (2.7-1/50)^2 + (2.3-0)^2 + (1.8-1/50)^2 + (1.6-0)^2)), sqrt(0.5 * ((21 - 18/52)^2 + (20.1-12/52)^2 + (15.9-11/52)^2 + (14-6/52)^2 + (7.6-1/52)^2 + (2.2-2/52)^2 + (1.1-0)^2)), sqrt(0.5 * ((17-14/52)^2 + (15.1-15/52)^2 + (13.6-8/52)^2 + (7.1-3/52)^2 + (6.2-3/52)^2 + (5.1-1/52)^2 + (4.7-2/52)^2), sqrt(0.5 * ((24.23-11/52)^2 + (15.66-8/52)^2 + (13.26-7/52)^2 + (11.63 -7/52)^2 + (4.92-4/52)^2 + (3.44 - 2/52)^2 + (3.44 - 1/52)^2)), sqrt(0.5 * ((12.19-5/52)^2 + (11.29-8/52)^2 + (8 - 6/52)^2 + (6.2-4/52)^2 + (6.02-4/52)^2 + (5.75-3/52)^2 + (4.23 - 3/52)^2))))
vanuatu_disproportionality #19.79356

new_democracy_dimension_data <- read_excel("/Users/gussmith/Desktop/notes/Politics/Political Analysis/Essay/Data/New Democracies Data.xlsx")

new_democracy_dimension_data %>%
  group_by(Country) %>%
  select(exec_dominance) %>%
  summarise(mean = mean(exec_dominance))

#1, 2, 1, 4.71, 2.25, 2.67, 4.06, 2.58, 1.5, 1, 3, 3.47, 3, 3.44, 1, 1.57, 3.71

new_democracy_dimension_data %>%
  group_by(Country) %>%
  filter(Country == "Vanuatu") %>%
  select(government_type) %>%
  nrow()

new_democracy_dimension_data %>%
  group_by(Country) %>%
  filter(Country == "Vanuatu") %>%
  filter(government_type == 4) %>%
  nrow()

  
belize_onepartycabinet = 6
belize_minimalwinningcabinet = 
belize_totalcabinet = 6
((6/6 * 100) + (6/6 * 100)) /2 #100

capeverde_onepartycabinet = 5
capeverde_minimalwinning = 
capeverde_totalcabinet = 5
((5/5 * 100) + (5/5 * 100)) /2 #100

chile_oneparty = 0
chile_minimalwinning = 3
chile_totalcabinet = 7
((0/7 * 100) + (3/7 * 100)) /2 #21

mongolia_oneparty = 4
monoglia_minimalwinning = 1
mongolia_minority = 1
mongolia_totalcabinet = 6
((5/6 * 100) + (5/6 * 100)) /2 #83

namibia_oneparty = 4
namibia_totalcabinet = 4
((4/4 * 100) + (4/4 * 100)) /2 #100

panama_oneparty = 0
panama_minimalwinning = 0
panama_minority = 2
panama_totalcabinet = 5
((2/5 * 100) + (0/5 * 100)) /2 #20

southafrica_oneparty = 4
southafrica_totalcabinet = 4
#100

taiwan_oneparty = 5
taiwan_minimalwinning = 2
taiwan_totalcabinet = 7
((5/7 * 100) + (7/7 * 100)) /2 #86

vanuatu_oneparty = 0
vanuatu_minimalwinning = 4
vanuatu_totalcabinet = 7
((0/7 * 100) + (4/7 * 100)) /2 #29

#
#
#
#
#
#
#
#
#
#
#
#
#
#


#calculating effective number of parties
qog_data %>%
  group_by(cname) %>%
  filter(ccodealp == "BLZ"| ccodealp == "BGR" | ccodealp == "CPV" | ccodealp == "CHL" | ccodealp == "CYP" | ccodealp == "EST" | ccodealp == "LVA" | ccodealp == "LTU" | ccodealp == "MNG" | ccodealp == "NAM" | ccodealp == "PAN" | ccodealp == "POL" | ccodealp == "ROU" | ccodealp == "SVN" | ccodealp == "ZAF" | ccodealp == "TWN" | ccodealp == "VUT") %>%
  summarise(gol_enep = gol_enep[which(!is.na(gol_enep))[1]])

#No epp data for Namibia and South Africa in QoG dataset

#ENP formula is (1/∑p^2). Using ENEP - vote share.
#SA; elections from 1994 onwards. Data from African Elections Database.
south_africa_epp = mean((1/(0.62^2 + 0.2039^2 + 0.104^2 + 0.022^2 + 0.07^2 + 0.015^2)), (1/(0.66^2 + 0.096^2 + 0.086^2 + 0.069^2 + 0.034^2 + 0.14^2)), (1/(0.69^2 + 0.124^2 + 0.0697^2 + 0.023^2 + 0.0173^2 + 0.0165^2 + 0.16^2)), (1/(0.659^2+0.1666^2+0.0742^2+0.0455^2)))
#= 2.260397 = 2.26
#Namibia: elections from 1994 onwards. Data from African Elections Database.
namibia_epp = mean((1/(0.739^2+0.28^2+0.0272^2)), (1/(0.7615^2+0.0994^2+0.0948^2+0.00293^2)), (1/(0.7611^2+0.0729^2+0.0511^2+0.0415^2+0.036^2+0.0196^2)), (1/(0.7527^2+0.1131^2+0.0317^2+0.0305^2+0.0243^2+0.0135^2)))
#=1.599333 = 1.6

mean_eff_parties_new_democracies <- c(2.06, 4.12, 1.81, 7.05, 3.62, 8.72, 6.21, 4.58, 2.71, 1.6, 4.71, 13.9, 2.21, 8.34, 2.26, 3.14, 2.56)


#Data from Gallagher Electoral Disproportionality Data, 121 Countries, 1945-2014, v2, Christopher Gandrud
disproportionality_data %>%
  group_by(country) %>%
  filter(iso2c == "BZ"| iso2c == "BG" | iso2c == "CV" | iso2c == "CL" | iso2c == "CY" | iso2c == "EE" | iso2c == "LV" | iso2c == "LT" | iso2c == "MN" | iso2c == "NA" | iso2c == "PA" | iso2c == "PL" | iso2c == "RO" | iso2c == "SI" | iso2c == "ZA" | iso2c == "TW" | country == "Vanuatu") %>%
  summarise(mean_disproportionality = disproportionality[which(!is.na(disproportionality))[1]])

#No disproportionality data for Vanuatu from this dataset
#Calculating Vanuatu disproportionality from 1991-2012, without data for 2004. Source: Wikipedia
vanuatu_disproportionality = mean(sqrt(0.5 * ((30.6-41)^2 + (22.6-21.7)^2 + (20.4-10/46)^2 + (15.4-4/46)^2 + (4.6-1/46)^2 + (2.9-1/46)^2 + (1.9-1/46)^2)), sqrt(0.5 * ((31.4-20/50)^2 + (27.4-17/50)^2 + (23.4-9/50)^2 + (2.7-1/50)^2 + (2.3-0)^2 + (1.8-1/50)^2 + (1.6-0)^2)), sqrt(0.5 * ((21 - 18/52)^2 + (20.1-12/52)^2 + (15.9-11/52)^2 + (14-6/52)^2 + (7.6-1/52)^2 + (2.2-2/52)^2 + (1.1-0)^2)), sqrt(0.5 * ((17-14/52)^2 + (15.1-15/52)^2 + (13.6-8/52)^2 + (7.1-3/52)^2 + (6.2-3/52)^2 + (5.1-1/52)^2 + (4.7-2/52)^2), sqrt(0.5 * ((24.23-11/52)^2 + (15.66-8/52)^2 + (13.26-7/52)^2 + (11.63 -7/52)^2 + (4.92-4/52)^2 + (3.44 - 2/52)^2 + (3.44 - 1/52)^2)), sqrt(0.5 * ((12.19-5/52)^2 + (11.29-8/52)^2 + (8 - 6/52)^2 + (6.2-4/52)^2 + (6.02-4/52)^2 + (5.75-3/52)^2 + (4.23 - 3/52)^2))))
#19.8

mean_disproportionality_new_democracies <- c(19.7, 5.37, 7, 6.3, 17.1, 7.23, 4.14, 9.61, 27.4, 0.93, 7.87, 3.62, 0.9, 2.94, 0.36, 7.05, 19.8)


#Next variable is minimal winning cabinets
#The code used is long and would make the appendix unhelpfully long, and is available on request. I just use dplyr repeatedly to search QoG dataset.

#No QoG data for Belize, Cape Verde, Chile, Mongolia, Namibia, Panama, South Africa, Taiwan, Vanuatu.
cabinet_new_democracy <- c(NA, 30.36, NA, NA, 100, 44.23, 18, 36.54, NA, NA, NA, 40.74, 39.29, 34, NA, NA, NA)

#Data from African Elections and Carribbean Elections and Wikipedia for rest of minimal winning

cabinet_new_democracy <- c(100, 30.36, 100, 21, 100, 44.23, 18, 36.54, 83, 100, 20, 40.74, 39.29, 34, 100, 86, 29)

#Finally, we find the executive dominance measure. Source: PGDS, African Elections, Carribbean Elections, Wikipedia
new_democracy_dimension_data %>%
  group_by(Country) %>%
  select(exec_dominance) %>%
  summarise(mean = mean(exec_dominance))

dominance_new_democracy <- c(1, 2, 1, 4.71, 2.25, 2.67, 4.06, 2.58, 1.5, 1, 3, 3.47, 3, 3.44, 1, 1.57, 3.71)


#Now standardise the variables:
standardised_newdem_epp <- scale(mean_eff_parties_new_democracies)
standardised_newdem_disproportionality <- scale(-mean_disproportionality_new_democracies)
standardised_newdem_cabinet <- scale(-cabinet_new_democracy)
standardised_newdem_dominance <- scale(dominance_new_democracy)

newdemocracy_ep_dimension <- ((standardised_newdem_epp + standardised_newdem_disproportionality + standardised_newdem_cabinet + standardised_newdem_dominance) / 4)

#Checking the internal validity
newdemocracy_ep_dataframe <- cbind(standardised_newdem_epp, standardised_newdem_disproportionality, standardised_newdem_cabinet, standardised_newdem_dominance)
psych::alpha(newdemocracy_ep_dataframe)


cpi_volatility_newdem_1980_2010 <- c(NA, 2.26931, 1.16605, 0.82212, 0.84151, 1.34902, 1.65866, 1.59255, 2.03858, 0.72881, 0.44063, 1.66167, 1.75869, 1.78236, 0.67919, NA, 0.82473)

#World Bank Data
mean_realir_newdem_1980_2010 <- c(12.83, 6.75, 6.57, 6.63, NA, NA, NA, NA, 16.47, 6.72, 7.47, NA, 0.08, NA, 4.35, NA, 8.2)

#World Bank Data
trade_percent_gdp_newdem_1980_2010 <- c(106.58, 86.6, 90, 58.6, 114.79, 135.33, 89.94, 101.97, 103.73, 98.94, 129.29, 64.85, 56.73, 112.96, 47, NA, 98.27)

#World Bank Data
investment_percent_gdp_newdem_1980_2010 <- c(22.6, 25.41, 47.44, 22.51, 25.82, 30.18, 27.49, 22.38, 38.95, 19.28, 29.46, 21.99, 25.06, 25.84, 20.46, NA, 25.39)

#World Bank Data
mean_research_spending_percent_gdp_newdem_1980_2010 <- c(NA, 0.47, NA, 0.34, 0.33, 0.94, 0.46, 0.68, 0.23, 0.14, 0.26, 0.61, 0.36, 1.46, 0.8, NA, NA)


#Recreating the HDI and population controls

population_newdem_data <- read_csv("/Users/gussmith/Desktop/Notes/Politics/Political Analysis/Essay/Data/New Dem Data/Population.csv")

population_newdem_data %>%
  filter(Country == "Belize" | Country == "Bulgaria" | Country == "Cabo Verde" | Country == "Chile" | Country == "Cyprus" | Country == "Estonia" | Country == "Latvia" | Country == "Lithuania" | Country == "Mongolia" | Country == "Namibia" | Country == "Panama" | Country == "Poland" | Country == "Romania" | Country == "Slovenia" | Country == "South Africa" | Country == "Taiwan" | Country == "Vanuatu") %>%
  mutate(pop_in_thousands = Population/1000)

pop_newdem_thousands_2009 <- c(315, 7444, 487, 16886, 1098, 1335, 2142, 3163, 2674, 2081, 3579, 38152, 20367, 2040, 50477, NA, 236)


hdi_newdem_data <- read_excel("/Users/gussmith/Desktop/Notes/Politics/Political Analysis/Essay/Data/New Dem Data/HDI 2010.xlsx")

hdi_newdem_data %>%
  group_by(Country) %>%
  filter(Country == "Belize" | Country == "Bulgaria" | Country == "Cabo Verde" | Country == "Chile" | Country == "Cyprus" | Country == "Estonia" | Country == "Latvia" | Country == "Lithuania" | Country == "Mongolia" | Country == "Namibia" | Country == "Panama" | Country == "Poland" | Country == "Romania" | Country == "Slovenia" | Country == "South Africa" | Country == "Taiwan" | Country == "Vanuatu")

hdi_newdem_2010 <- c(0.693, 0.779, 0.626, 0.8, 0.85, 0.844, 0.817, 0.824, 0.697, 0.588, 0.758, 0.835, 0.797, 0.881, 0.662, NA, 0.585)


#
#
#
#


#Running the regressions

#Investment

#Checking outliers
boxplot(investment_percent_gdp_newdem_1980_2010)
investment_percent_gdp_newdem_1980_2010

investment_percent_gdp_newdem_1980_2010[3] <- NA
investment_percent_gdp_newdem_1980_2010[9] <- NA

multiregression16 <- lm(investment_percent_gdp_newdem_1980_2010 ~ newdemocracy_ep_dimension + hdi_newdem_2010 + log(pop_newdem_thousands_2009))

multiregression17 <- lm(investment_percent_gdp_newdem_1980_2010 ~ newdemocracy_ep_dimension + hdi_newdem_2010 + log(pop_newdem_thousands_2009) + mean_realir_newdem_1980_2010)

#R&D

#Checking for outliers
boxplot(mean_research_spending_percent_gdp_newdem_1980_2010)
mean_research_spending_percent_gdp_newdem_1980_2010

mean_research_spending_percent_gdp_newdem_1980_2010[14] <- NA

multiregression18 <- lm(mean_research_spending_percent_gdp_newdem_1980_2010 ~ newdemocracy_ep_dimension + hdi_newdem_2010 + log(pop_newdem_thousands_2009))

#Inflation Volatility

#Checking for outliers
boxplot(cpi_volatility_newdem_1980_2010)

multiregression19 <- lm(cpi_volatility_newdem_1980_2010 ~ newdemocracy_ep_dimension + hdi_newdem_2010 + log(pop_newdem_thousands_2009))

multiregression20 <- lm(cpi_volatility_newdem_1980_2010 ~ newdemocracy_ep_dimension + hdi_newdem_2010 + log(pop_newdem_thousands_2009) + trade_percent_gdp_newdem_1980_2010)

#Stargazer tables

#Investment
stargazer(multiregression16, multiregression17, title="Table 6: New Democracies' Gross Capital Formation (1980-2010)", covariate.labels = c("New Democracy Executive-Parties Dimension", "HDI 2010", "Population 2009 (logged, 100s)", "Mean Real Interest Rate (1980-2010)"), dep.var.labels = "Investment as % of GDP (1980-2010)", out="regressiontablenew1.html")

#R&D
stargazer(multiregression18, title="Table 7: New Democracies' R&D Spending (1980-2010)", covariate.labels = c("New Democracy Executive-Parties Dimension", "HDI 2010", "Population 2009 (logged, 1000s)"), dep.var.labels = "Research and Development Spending as % of GDP (1980-2010)", out = "regressiontablenew2.html")

#Inflation Volatility

stargazer(multiregression19, multiregression20, title="Table 8: New Democracies' Inflation Volatility", covariate.labels = c("New Democracy Executive-Parties Dimension", "HDI 2010", "Population 2009 (logged, 1000s)", "Economic Openness (1980-2010)"), dep.var.labels = "CPI Inflation Volatility (1980-2010)", out = "regressiontablenew3.html")


#
#
#

newdemcountrylist = c("Belize", "Bulgaria", "Cape Verde", "Chile", "Cyprus", "Estonia", "Latvia", "Lithuania", "Mongolia", "Namibia", "Panama", "Poland", "Romania", "Slovenia", "South Africa", "Taiwan", "Vanuatu")

total_36_data <- data.frame(country = data$country, epdimension=newepdimension, pop_2009=data$pop_in_thousands_2009, hdi_2010=data$hdi_2010, investment_mean=investment_percent_gdp_1980_2010, realir_mean=mean_realir_1980_2010, rand_mean=mean_research_spending_percent_gdp_1980_2010, cpi_mean=cpi_volatility_1980_2010, trade_mean=trade_percent_gdp_1980_2010)

total_new_data <- data.frame(country = newdemcountrylist, epdimension=newdemocracy_ep_dimension, pop_2009=pop_newdem_thousands_2009, hdi_2010=hdi_newdem_2010, investment_mean=investment_percent_gdp_newdem_1980_2010, realir_mean=mean_realir_newdem_1980_2010, rand_mean=mean_research_spending_percent_gdp_newdem_1980_2010, cpi_mean=cpi_volatility_newdem_1980_2010, trade_mean=trade_percent_gdp_newdem_1980_2010)

combined_dataset <- rbind(total_36_data, total_new_data)

#Fix outliers
combined_dataset$investment_mean[5] <- 16.91716
combined_dataset$investment_mean[7] <- 30.49037
combined_dataset$investment_mean[22] <- 34.02656
combined_dataset$investment_mean[35] <- 16.00303

#Running the Regressions

#Investment

#Check for outliers
boxplot(combined_dataset$investment_mean)
quantile(combined_dataset$investment_mean, na.rm = TRUE)

22.10499 - 1.5 * (25.83-22.10499) #16.5

#So, outliers are 22, 35, 39, 45

#Exclude the outliers:

combined_dataset$investment_mean[22] <- NA #KOREA
combined_dataset$investment_mean[35] <- NA #URUGUAY
combined_dataset$investment_mean[39] <- NA #CAPE VERDE
combined_dataset$investment_mean[45] <- NA #MONGOLIA

#Run the regression

multiregression21 <- lm(combined_dataset$investment_mean ~ combined_dataset$epdimension + combined_dataset$hdi_2010 + log(combined_dataset$pop_2009))

multiregression22 <- lm(combined_dataset$investment_mean ~ combined_dataset$epdimension + combined_dataset$hdi_2010 + log(combined_dataset$pop_2009) + combined_dataset$realir_mean)

#R&D

#Check for outliers
boxplot(combined_dataset$rand_mean)

#Run the regression
multiregression23 <- lm(combined_dataset$rand_mean ~ combined_dataset$epdimension + combined_dataset$hdi_2010 + log(combined_dataset$pop_2009))

#Inflation Volatility

#Check for outliers
boxplot(combined_dataset$cpi_mean)
combined_dataset$cpi_mean

#Outliers are 38 Bulgaria, 45 Mongolia

combined_dataset$cpi_mean[38] <- NA
combined_dataset$cpi_mean[45] <- NA

#Run the regression

multiregression24 <- lm(combined_dataset$cpi_mean ~ combined_dataset$epdimension + combined_dataset$hdi_2010 + log(combined_dataset$pop_2009))

multiregression25 <- lm(combined_dataset$cpi_mean ~ combined_dataset$epdimension + combined_dataset$hdi_2010 + log(combined_dataset$pop_2009) + combined_dataset$trade_mean)


#Build stargazer tables

#Investment
stargazer(multiregression21, multiregression22, title="Table 7: Fifty-Three Democracies' Gross Capital Formation", covariate.labels = c("Executive-Parties Dimension", "HDI 2010", "Population 2009 (logged, 100s)", "Mean Real Interest Rate (1980-2010)"), dep.var.labels = "Investment as % of GDP (1980-2010)", out="regressiontablenew1.html")

#R&D
stargazer(multiregression23, title="Table 8: Fifty-Three Democracies' R&D Spending", covariate.labels = c("Executive-Parties Dimension", "HDI 2010", "Population 2009 (logged, 1000s)"), dep.var.labels = "Research and Development Spending as % of GDP (1980-2010)", out = "regressiontablenew2.html")

#Inflation Volatility

stargazer(multiregression24, multiregression25, title="Table 6: Fifty-Three Democracies' Inflation Volatility", covariate.labels = c("Executive-Parties Dimension", "HDI 2010", "Population 2009 (logged, 1000s)", "Economic Openness (1980-2010)"), dep.var.labels = "CPI Inflation Volatility (1980-2010)", out = "regressiontablenew3.html")

1