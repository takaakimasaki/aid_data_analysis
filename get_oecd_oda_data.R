#Created: 2015-6-22 
#Last modified: 2015-6-22
#This R code scrapes ODA data from the OECD website. This is just an extension of what's been written in the following link: http://www.r-bloggers.com/reading-oecd-stat-into-r/.
library(data.table)
library(DataCombine)
library(foreign)
library(XML2R)
setwd("~/Documents/Cornell/Taka_Working_Papers/Cross-National Data/original_dataset/DAC/SCRAPED_DATA")
##Import SDMX DATA URL from stats.oecd.org.
###SDMX URL will look like this if one wants to scrape data on Japanese ODA to Tanzania and Zambia: http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/282+288.701.1.206.A/all?startTime=2004&endTime=2013
####"282+288" refers to the country codes of Tanzania and Zambia
####"701" in ".701.1.206.A" refers to the donor code of Japan
####"1" in ".701.1.206.A" refers to "Part" of the world (1, Developing Countries; 2, Countries in Transition)
####"206" in ".701.1.206.A" refers to aid type (206 is "ODA, Total Net")
####"A" in ".701.1.206.A" refers to the type of price: A=current; D=constant.
####"startTime=2004&endTime=2013" refers to the starting and end years of data (to be scraped) 
##Recipient and donor code are available at http://www.oecd.org/dac/stats/dacandcrscodelists.htm.
##Note that 
###DONOR CODE==20005, ALL DONORS
###DONOR CODE==20001, ALL DAC DONORS
###DONOR CODE==20006, ALL NON-DAC DONORS
###DONOR CODE==20002, ALL MULTILATERALS
###Aid Type==201, Grants, Total
###Aid Type==212, Grants: Debt Forgiveness
###Aid Type==221, Grants: Other Debt Grants
###Aid Type==208, AF/Interest Subsidies
###Aid Type==219, Recoveries
###Aid Type==210, Capital Subscriptions - Deposits
###Aid Type==211, Memo: Capital Subscriptions - Encashments
###Aid Type==204, ODA Gross Loans
###Aid Type==214, Rescheduled Debt
###Aid Type==205, ODA Loan Repayments
###Aid Type==215, Offsetting entries for debt relief
###Aid Type==218, ODA Loans: Total Net
###Aid Type==217, Equity Investment
###Aid Type==206, ODA: Total Net
###Aid Type==207, Technical Cooperation
###Aid Type==213, Development Food Aid
###Aid Type==216, Humanitarian Aid
###Aid Type==209, Interest received
###Aid Type==106, Imputed Multilateral ODA
###Aid Type==240, Memo: ODA Total, Gross disbursements
###Aid Type==250, Memo: ODA Total, excl. Debt
###Aid Type==255, Memo: Net debt relief
###Aid Type==296, ODA per capita ($)
###Aid Type==286, ODA as % GNI (Recipient)

#The R code below scrapes data on Net ODA from all donors across all countries (included OECD website) for the period of 1960-2013. 
#First set the list of recipient countries for which you want data.
#I excluded 807 (UNEP), 903 (IFC), 907 (IMF), 909 (IDB), 915 (AsDB), 990 (EBRD), 992 (UN Agencies), 130 (Algeria), 133 (Libya), 543 (Iraq), and 561 (Qatar).  
list_str=c(20001,20002,20005,20006,1,2,3,4,5,6,7,8,9,10,11,12,18,20,21,22,40,50,61,68,69,76,301,302,701,742,801,820,918,104,811,812,901,905,906,912,913,914,916,921,923,928,944,948,951,953,958,959,960,963,964,966,967,971,974,976,978,988,1311,1312,1313,30,45,55,62,70,72,75,77,82,83,84,87,546,552,566,576,732,764)
for(i in list_str) {
  file<-paste("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2A/10100+10010+71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+225+236+227+287+228+230+229+231+232+233+234+247+235+274+237+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+289+298+10004+10005+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+380+389+10006+425+428+431+434+437+440+443+446+451+454+457+460+463+489+498+10007+10008+725+728+730+732+740+735+738+742+745+748+751+752+753+755+761+764+765+769+789+10009+625+610+611+666+630+612+645+650+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+889+9998.",i,".1.206.A/all?startTime=1960&endTime=2013",sep="")
  obs <- XML2Obs(file)
  tables <- collapse_obs(obs)

# Data we want to scrape are located in the following three nodes:
keys <- tables[["MessageGroup//DataSet//Series//SeriesKey//Value"]]
dates <- tables[["MessageGroup//DataSet//Series//Obs//Time"]]
values <- tables[["MessageGroup//DataSet//Series//Obs//ObsValue"]]

# Extract the donor part of the keys table
donor_list <- keys[keys[,1]== "DONOR"]
# The donor names are stored in the middle third of the above list
donor_list <- as.numeric(donor_list[(length(donor_list)*1/3+1):(length(donor_list)*2/3)])
donor_list

# Extract the recipient part of the keys table
recipient_list <- keys[keys[,1]== "RECIPIENT"]
# The recipient names are stored in the middle third of the above list
recipient_list <- as.numeric(recipient_list[(length(recipient_list)*1/3+1):(length(recipient_list)*2/3)])
recipient_list

# Bind the existing date and value vectors
dat <- cbind.data.frame(as.numeric(dates[,1]),as.numeric(values[,1]))
colnames(dat) <- c('year', 'value')

# Add donor and recipient codes to dat.
dat$donor <- c(donor_list[1], donor_list[cumsum(diff(dat$year) <= 0) + 1])
dat$recipient <- c(recipient_list[1], recipient_list[cumsum(diff(dat$year) <= 0) + 1])

data_title_dta<-paste("data",i,".dta",sep="")
data_title_csv<-paste("data",i,".csv",sep="")
write.dta(dat, file = data_title_dta) #export data in the DTA format
write.csv(dat, file = data_title_csv) #export data in the CSV format
} 
