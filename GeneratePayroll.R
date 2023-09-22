# ---------------------------
#
# Script name: GeneratePayroll.R 
#
# what it does: Generates payroll data for 2023
#
# ---------------------------

library(tidyverse)  # packages that work well with each other and good for data science call tidyverse.packages()
library(lubridate) # dealint with dates
library(logger) # A Lightweight, Modern and Flexible Logging Utility
library(plotly) # library to support plotly plots
library(loggit)
library(readr) # library to support read_csv

# Log Settings....  
log_threshold(WARN) # set this value to show more or less log info
log_layout(layout_glue_colors)

# Factors and levels for ordered factors
month_levels <- c("January","February","March","April","May","June","July","August",
                  "September","October","November","December")
mon_lvls    <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
                 "Sep","Oct","Nov","Dec")

dept_levels <- c("Automation", "DEV", "DevOps","INFO DEV", "QE","UI/UX","Management")

eng_levels <- c("Junior", "Staff", "Senior", "Junior Architect",
                "Project Administrator","Customer Defined",
                "Junior Core Manager","Core Manager","Senior Core Manager","CIO","CTO - VP Engineering")

cb_levels <- c("Junior","JCE","Staff","Senior") # city bonus levels

rev_eng_levels <- c("Senior Core Manager","Core Manager","Junior Core Manager","Project Administrator",
                    "Junior Architect","Senior","Staff","Junior")


# auto_eng_levels <- c("Junior", "Staff", "Senior")
# 
# DEV_eng_levels <- c("Junior", "Staff", "Senior","Junior Architect")
# 
# DevOps_eng_levels <- c("Junior", "Staff")
# 
# INFO_DEV_eng_levels <- c("Junior", "Staff", "Project Administrator")
# 
# QE_eng_levels <- c("Junior", "Staff", "Senior")#, "Project Administrator")
# 
# UI_UX_eng_levels <- c("Junior", "Staff", "Senior")
# 
# Mgt_eng_levels <- c("Junior Core Manager","Core Manager")

unit_levels <- c("Unit 01", "Unit 02", "Unit 03", "Unit 04", "Unit 05",
                 "Unit 06", "Unit 07", "Unit 08", "Unit SES")

# Global file names

PUBLISH <- FALSE # this variable is set to true when generating the data to publish in the shiny server

# BOL data
Bol.EID.File.Name.2022    <- "data/Bol.EID.Engineering.2022.csv"  # Engineer ID table from ERP with export Payrol V2.0
#Bol.Budget.File.Name.2022 <- "data/Bol.Eng.Bgt.Dec2022.csv"       # ERP Budget file name 
Bol.SlrBgt.File.Name.2022 <- "data/Salar.Bol.Eng.Bgt.Dec2022.csv" # Salar Budget file name Bolivia
Bol.SlrBgt.File.Jan2023   <- "data/Salar.Bol.Eng.Bgt.Jan2023.csv" # Salar Budget file name Jan 2023 Bolivia
Bol.SlrBgt.File.Feb2023   <- "data/Salar.Bol.Eng.Bgt.Feb2023.csv" # Salar Budget file name Feb 2023 Bolivia
Bol.SlrBgt.File.Mar2023   <- "data/Salar.Bol.Eng.Bgt.Mar2023.csv" # Salar Budget file name Mar 2023 Bolivia
Bol.SlrBgt.File.Apr2023   <- "data/Salar.Bol.Eng.Bgt.Apr2023.csv" # Salar Budget file name Apr 2023 Bolivia
Bol.SlrBgt.File.May2023   <- "data/Salar.Bol.Eng.Bgt.May2023.csv" # Salar Budget file name May 2023 Bolivia
Bol.SlrBgt.File.Jun2023   <- "data/Salar.Bol.Eng.Bgt.Jun2023.csv" # Salar Budget file name Jun 2023 Bolivia
Bol.SlrBgt.File.Jul2023   <- "data/Salar.Bol.Eng.Bgt.Jul2023.csv" # Salar Budget file name Jul 2023 Bolivia
Bol.SlrBgt.File.Aug2023   <- "data/Salar.Bol.Eng.Bgt.Aug2023.csv" # Salar Budget file name Aug 2023 Bolivia


# LAT data
Lat.SlrBgt.File.Name.2022 <- "data/Salar.Lat.Eng.Bgt.Dec2022.csv" # Salar Budget file name Latam
Lat.SlrBgt.File.Jan2023   <- "data/Salar.Lat.Eng.Bgt.Jan2023.csv" # Salar Budget file name Jan 2023 Latam
Lat.SlrBgt.File.Feb2023   <- "data/Salar.Lat.Eng.Bgt.Feb2023.csv" # Salar Budget file name Feb 2023 Latam
Lat.SlrBgt.File.Mar2023   <- "data/Salar.Lat.Eng.Bgt.Mar2023.csv" # Salar Budget file name Mar 2023 Latam
Lat.SlrBgt.File.Apr2023   <- "data/Salar.Lat.Eng.Bgt.Apr2023.csv" # Salar Budget file name Apr 2023 Latam
Lat.SlrBgt.File.May2023   <- "data/Salar.Lat.Eng.Bgt.May2023.csv" # Salar Budget file name May 2023 Latam
Lat.SlrBgt.File.Jun2023   <- "data/Salar.Lat.Eng.Bgt.Jun2023.csv" # Salar Budget file name Jun 2023 Latam
Lat.SlrBgt.File.Jul2023   <- "data/Salar.Lat.Eng.Bgt.Jul2023.csv" # Salar Budget file name Jul 2023 Latam
Lat.SlrBgt.File.Aug2023   <- "data/Salar.Lat.Eng.Bgt.Aug2023.csv" # Salar Budget file name Aug 2023 Latam

# common data
Bol.Raises.2023           <- "data/Bol.Eng.Raises.2023.csv"       # estimated raises
Bol.ENG.Bonus.2023        <- "data/Bol.Eng.Bonus.2023.csv"        # qntrl eng/lead/city bonus
Bol.BSU.Bonus.2023        <- "data/Bol.BSU.Bonus.2023.csv"        # qntrl bsu bonus
Bol.EID.File.Name.2023    <- "data/Bol.EID.Engineering.2023.current.csv" # current EID file includes latest month
EID.Exceptions.File.Name  <- "data/Bol.EID.Eng.Exceptions.2023.csv" # EID exceptions file which includes engineers who moved out of engineering

# Evaluation qntrl data
Eng.Evals.File.2023       <- "data/Eng.Evals.2023.csv" # File where all Eng Evaluations are stored
Mgt.Evals.File.2023       <- "data/Mgt.Evals.2023.csv" # File where all Mgt Evaluations are stored

# Global Variables   SHOULDNT THIS BE 2023?
Jan.01.this.year.Date <- as.Date("2022-01-01") # first day of the current year...  Used to calculate Month seniority (moved from locally defined function)

# Global Variables Bolivia Specific 2023
Gov.Raise <- 0.03  # ESTIMATED 4.5%.... In april the gov declared
P.Indem	  <- 0.0833 # indeminizacion
P.Aguin1	<- 0.0833 # Aguinaldo1
P.Aguin2	<- 0.0    # Aguinaldo2 estimating that we will have 2nd
Aportes	  <- 0.1671 # aportes; este es el costo adicional a la empresa
#USD	      <- 6.96   # usd exchange rate temporary for quick conversions
BOB2USD   <- 6.96 # BOB TO USD FACTOR
USD2USD   <- 1
BOB2COP   <- 10000 # BOB TO COP FACTOR INCORRECT this should be a lookup table per month
BOB2BOB   <- 1


run <- function(currency = "USD"){
  # this is the main script... when called, it will generate the full 2023 payroll
  
  # set some options to remove warnings from the console and only show logging info
  defaultW <- getOption("warn") 
  options(warn = -1) 
  options(readr.show_col_types = FALSE)
  log_warn('Script version {PR.Version()} starting up... Defined currency is << {currency} >>') # Script starts
  log_warn("Using $2385 BOB for 2023 Bol Min Salary")
  log_error("Currently does NOT support quarterly ENG/BSU bonus")
  
  # Load Common
  Bol.Eng.Data.2022 <- LoadERPData(Bol.EID.File.Name.2022,Include.Inactives=F,Include.USM=F,Include.Exceptions=F)     # data to december 2022
  Month.Seniority<-Calc.Month.Seniority(Bol.Eng.Data.2022,as.Date("2022-12-31"))   # calculate month seniority for dec 2022
  Bol.Eng.SalarPR.Data.2022 <- LoadSalarPayrollData(Bol.SlrBgt.File.Name.2022,"Dec",Month.Seniority,currency,"BOB",F,NULL,NULL)   # load BOL Dec2022 executed salar payroll and convert from BOB to currency
  Lat.Eng.SalarPR.Data.2022 <- LoadSalarPayrollData(Lat.SlrBgt.File.Name.2022,"Dec",Month.Seniority,currency,"USD",F,NULL,NULL)   # load LAT Dec2022 executed salar payroll and convert from BOB to currency
  
  # Generate consolidated BOL+LAT
  Eng.SalarPR.Data.2022 <- Sum.Salar.Payroll.DataFrames(Bol.Eng.SalarPR.Data.2022,Lat.Eng.SalarPR.Data.2022)
  
  #return(left_join(Bol.Eng.Data.2022,Remove.Month.Prefix(Bol.Eng.SalarPR.Data.2022,".Dec"),by="ID"))
  #return(left_join(Bol.Eng.Data.2022,Remove.Month.Prefix(Lat.Eng.SalarPR.Data.2022,".Dec"),by="ID"))
  #return(left_join(Bol.Eng.Data.2022,Remove.Month.Prefix(Eng.SalarPR.Data.2022,".Dec"),by="ID"))
  
  # generate a report for dec 2022 joining ERP and Payroll data
  #Bol.Eng.2022.Base<-GetYearReferenceSalaries(left_join(Bol.Eng.Data.2022,Remove.Month.Prefix(Bol.Eng.SalarPR.Data.2022,".Dec"),by="ID"))
  #Lat.Eng.2022.Base<-GetYearReferenceSalaries(left_join(Bol.Eng.Data.2022,Remove.Month.Prefix(Lat.Eng.SalarPR.Data.2022,".Dec"),by="ID"))
  Eng.2022.Base <-  GetYearReferenceSalaries(left_join(Bol.Eng.Data.2022,Remove.Month.Prefix(Eng.SalarPR.Data.2022,".Dec"),by="ID"))
  
  #Summarize.Month.Payroll(Eng.2022.Base,currency)
  #show_salary_bands(Eng.2022.Base,2022,currency)
  #show_salary_curves(Eng.2022.Base,2022,currency)
  #return(Eng.2022.Base)
  # *** end of 2022 calculations
  
  # *** generate 2023 data
  log_info("Starting to Generate 2023... predetermined currency is {currency}")
  Eng.Data.2023 <- LoadERPData(Bol.EID.File.Name.2023,Include.Inactives=T,Include.USM=F,Include.Exceptions=T)   # exceptions are the ones that moved out of engineering
  
  # Load Estimated and Executed Raises for Bol and Latam
  Raises.2023.Bol<-Load2023Raises(Eng.Data.2023 %>% select(ID, Aniversary,Unit),currency,"Bolivia")
  Raises.2023.Lat<-Load2023Raises(Eng.Data.2023 %>% select(ID, Aniversary,Unit),currency,"Latam")
  Raises.2023.Eng<-Summarize.Estimated.Raises(Raises.2023.Bol %>% select(ID:E.Raise.Prom),Raises.2023.Lat %>% select(ID:E.Raise.Prom),
                                              Eng.Data.2023 %>% select(ID,Hire.End.Date))

  # add estimated Raises / Proms to static data 
  Eng.Data.2023<-left_join(Eng.Data.2023,Raises.2023.Eng %>% select(ID, E.Raise.360,E.Raise.Prom),by="ID") #%>% 
  
  # summarize raises 360/prom per unit 
  Raises.Summary.per.Unit <- Generate.Estimated.Raises.Table(Raises.2023.Eng)
  
  # remove  Raises.2023.Eng since it is no longer needed
  rm(Raises.2023.Eng)
  
  # Load Bonus table for frequencies for Bol and Latam
  # refactoring the "select" to include "payment frequency" 
  Bonus.2023.Bol<-Load2023Bonus(Eng.Data.2023 %>% select(ID, Aniversary),currency,"Bolivia","ENG") %>% select(ID,Name,Start.Date,End.Date,Month,Type,B.Amount,Frequency,Payment)
  Bonus.2023.Lat<-Load2023Bonus(Eng.Data.2023 %>% select(ID, Aniversary),currency,"Latam","ENG") %>% select(ID,Name,Start.Date,End.Date,Month,Type,B.Amount,Frequency,Payment)
  
  BSU.Bonus.2023.Bol<-Load2023Bonus(Eng.Data.2023 %>% select(ID, Aniversary),currency,"Bolivia","BSU") %>% select(ID,Name,Start.Date,End.Date,Month,Type,B.Amount,Frequency,Payment)
  BSU.Bonus.2023.Lat<-Load2023Bonus(Eng.Data.2023 %>% select(ID, Aniversary),currency,"Latam","BSU")  %>% select(ID,Name,Start.Date,End.Date,Month,Type,B.Amount,Frequency,Payment)
  
  Bonus.2023.Bol<-rbind(Bonus.2023.Bol,BSU.Bonus.2023.Bol) # merge ENG and BSU bonus into a single dataframe
  Bonus.2023.Lat<-rbind(Bonus.2023.Lat,BSU.Bonus.2023.Lat) # merge ENG and BSU bonus into a single dataframe
  
  # calculate January 2023 based on 2022 data
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-01-31")) 
  
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.Jan2023,"Jan",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.Jan2023,"Jan",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimateJanuaryPayrollData2023(Remove.Month.Prefix(Bol.Eng.SalarPR.Data.2022,".Dec"),"Jan",Month.Seniority,Raises.2023.Bol,ENG.Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimateJanuaryPayrollData2023(Remove.Month.Prefix(Lat.Eng.SalarPR.Data.2022,".Dec"),"Jan",Month.Seniority,Raises.2023.Lat,ENG.Bonus.2023.Bol,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Eng.Data.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Eng.Data.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Data.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Feb 2023; load if available or estimate from prev month
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-2-28")) 
  
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.Feb2023,"Feb",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.Feb2023,"Feb",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Jan"),"Feb",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Jan"),"Feb",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Mar 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-3-31")) 
 
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.Mar2023,"Mar",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.Mar2023,"Mar",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Feb"),"Mar",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Feb"),"Mar",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)

  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Apr 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-4-30")) 
  
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.Apr2023,"Apr",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.Apr2023,"Apr",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Mar"),"Apr",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Mar"),"Apr",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate May 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-5-31")) 
  
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.May2023,"May",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.May2023,"May",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Apr"),"May",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Apr"),"May",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Jun 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-6-30")) 
  
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.Jun2023,"Jun",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.Jun2023,"Jun",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".May"),"Jun",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".May"),"Jun",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Jul 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-7-31")) 
  
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.Jul2023,"Jul",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.Jul2023,"Jul",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Jun"),"Jul",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Jun"),"Jul",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Aug 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-8-31")) 
  
  Bol.Payroll.Data.2023 <- LoadSalarPayrollData(Bol.SlrBgt.File.Jul2023,"Aug",Month.Seniority,currency,"BOB",T,Raises.2023.Bol,Bonus.2023.Bol %>% subset(Payment=="Bank transfer"))
  Lat.Payroll.Data.2023 <- LoadSalarPayrollData(Lat.SlrBgt.File.Jul2023,"Aug",Month.Seniority,currency,"USD",T,Raises.2023.Lat,Bonus.2023.Lat %>% subset(Payment=="Bank transfer"))
  
  #Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Jul"),"Aug",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  #Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Jul"),"Aug",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Sep 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-9-30")) 
  
  Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Aug"),"Sep",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Aug"),"Sep",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Oct 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-10-31")) 
  
  Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Sep"),"Oct",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Sep"),"Oct",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Nov 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-11-30")) 
  
  Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Oct"),"Nov",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Oct"),"Nov",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # calculate Dec 2023
  Month.Seniority<-Calc.Month.Seniority(Eng.Data.2023,as.Date("2023-12-31")) 
  
  Bol.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Bol.Payroll.Data.2023,".Nov"),"Dec",Month.Seniority,Raises.2023.Bol,Bonus.2023.Bol,"Bolivia")
  Lat.Payroll.Data.2023 <- EstimatePayrollData2023(Remove.Month.Prefix(Lat.Payroll.Data.2023,".Nov"),"Dec",Month.Seniority,Raises.2023.Lat,Bonus.2023.Lat,"Latam")
  Eng.Payroll.Data.2023 <- Sum.Salar.Payroll.DataFrames(Bol.Payroll.Data.2023,Lat.Payroll.Data.2023)
  
  Bol.Payroll.2023<-left_join(Bol.Payroll.2023,Bol.Payroll.Data.2023,by="ID")
  Lat.Payroll.2023<-left_join(Lat.Payroll.2023,Lat.Payroll.Data.2023,by="ID")
  Eng.Payroll.2023<-left_join(Eng.Payroll.2023,Eng.Payroll.Data.2023,by="ID")
  
  # **** this is temp shit
  #Summarize.Month.Payroll(Eng.2022.Base,currency)
  #show_salary_bands(Eng.2022.Base,2022,currency)
  #show_salary_curves(Eng.2022.Base,2022,currency)
  # eng <- get_360.Prom.Budget(Bol.Payroll.2023,Lat.Payroll.2023)
  # 
  # bol<- data.frame(Month=mon_lvls,Inc.360=t(summarise(eng,across(Jan.360:Dec.360,sum))),
  #                  Inc.Prom=t(summarise(eng,across(Jan.Prom:Dec.Prom,sum))))
  # 
  # bol$Month <- ordered(bol$Month, levels = mon_lvls)
  # 
  # eng.summary <- eng %>%
  #   mutate(Tot.360 = rowSums(across(c(Jan.360, Dec.360)))) %>%
  #   mutate(Tot.Prom = rowSums(across(c(Jan.Prom, Dec.Prom)))) %>%
  #   select(Unit,Sum.360,Sum.Prom) %>% mutate(Tot = rowSums(across(c(Tot.360, Tot.Prom))))
  #   
  # return(eng.summary)
  # ***
  
  #return(Get_360.Prom.Budget(Bol.Payroll.2023,Lat.Payroll.2023))
  
  # Move the X_ID Column to the last position in the dataframe
  Bol.Payroll.2023 <- Bol.Payroll.2023 %>% relocate(X_ID, .after = last_col())
  Lat.Payroll.2023 <- Lat.Payroll.2023 %>% relocate(X_ID, .after = last_col())
  Eng.Payroll.2023 <- Eng.Payroll.2023 %>% relocate(X_ID, .after = last_col())
  
  #Temporary fix for the Risk column names, we agreed to keep the format of
  # Risk.month
  columnIntervalforMonths <- 30
  riskIndex <- 49
  startAt <- riskIndex - columnIntervalforMonths
  for (month in 1:12) {
    index <- startAt + (month * columnIntervalforMonths)
    colnames(Bol.Payroll.2023)[index] <- paste0("Risk.", mon_lvls[month])
    colnames(Lat.Payroll.2023)[index] <- paste0("Risk.", mon_lvls[month])
    colnames(Eng.Payroll.2023)[index] <- paste0("Risk.", mon_lvls[month])
  }
  
  
  #write to files  
  if(PUBLISH == TRUE) {
    write_csv(Bol.Payroll.2023,"src/Bol.Payroll.2023.csv")
    write_csv(Lat.Payroll.2023,"src/Lat.Payroll.2023.csv")
    write_csv(Eng.Payroll.2023,"src/Eng.Payroll.2023.csv")
    write_csv(Eng.2022.Base,"src/Eng.2022.Base.csv") # this info also needed by shiny dashboard
    write_csv(Raises.Summary.per.Unit,"src/Estim.Raises.Summary.2023.csv")
  } else {
    write_csv(Bol.Payroll.2023,"out/Bol.Payroll.2023.csv")
    write_csv(Lat.Payroll.2023,"out/Lat.Payroll.2023.csv")
    write_csv(Eng.Payroll.2023,"out/Eng.Payroll.2023.csv")
    write_csv(Eng.2022.Base,"out/Eng.2022.Base.csv") # this info also needed by shiny dashboard
    write_csv(Raises.Summary.per.Unit,"out/Estim.Raises.Summary.2023.csv") # later remove this line
  }
  
  log_info('checksum data: {show_check_sum(Eng.Payroll.2023)}')
  log_info('Script ending normally... current version {PR.Version()}')
  options(warn = defaultW)
  
  print(show_check_sum(Eng.Payroll.2023)) # show a checksum for the dataframe for a quick comparison with other dataframes
  
  return(PR.Version())
}

# *** START 2023 Functions

show_check_sum <- function(Payroll){
  # get some quick stats to compare with other dataframes
  chksum <- summary(Payroll$TotalComp.Jan) %>% rbind(summary(Payroll$TotalComp.Jun)) %>%
    rbind(summary(Payroll$TotalComp.Dec))
  return(chksum)
}

JS_ParseDate <- function(column,File_Type="Error") {
  # receive a dataframe column and parse as date depending on various known formats
  # use lubridate to parse the date to parse date with various formats
  # and return as.Date
  
  if(File_Type=="Eval"){
    JS.Date.Formats <- c(
      "%m-%d-%y %H:%M") # special date and time in qntrl tickets mac)  
  } else if(File_Type=="ERP") {
    JS.Date.Formats <- c(
      "%m/%d/%y") # regular erp date mac
  } else if (File_Type=="Bonus") {
    JS.Date.Formats <- c(
      "%m/%d/%y") # regular bonus type mac 12/1/21	4/30/23
  } else {
    log_fatal("no file type defined")
    return(NULL)
    JS.Date.Formats <- c("%m-%d-%Y", # regular date in qntrl tickets
                         "%m/%d/%Y") # specific for Win
  }
  return(as.Date(parse_date_time(column, orders = JS.Date.Formats)))
}

LoadMgtEvaluations <- function(Div="Engineering B",PR="Latam",Start.Date,Eval.Type="No Type"){
  # data filters are: Division, Execution Month, Payroll AND EVAL.TYPE 
  # Load Engineer Evaluations from QNTRL and produce a short summary FOR EACH 360 AND PROMS SEPARATELY
  # So that they can be treated simultaneously for each engineer... i.e. an engineer can have a
  # 360 eval and raise in one month and a Prom and raise in another month
  # Noticed that there seems to be a problem in the date formats as exported from QNTRL... 
  # *** read_csv in 2.0.0 does not support files with carriage return \r as the line separator
  # work around is to open in excel and write
  
  log_info('Entered LoadMgtEvaluations, Start Date: {Start.Date}') # Entering function info
  
  if (!file.exists(Mgt.Evals.File.2023)){ # check if file exists before reading
    log_fatal('File {Mgt.Evals.File.2023} does NOT exist, aborting in LoadMgtEvaluations...')
    return(NULL)
  }
  
  evals.df <- read_csv(Mgt.Evals.File.2023)
  evals.df <- evals.df[, -c(5,6)]    # drop Assignee  and Requestor 
  
  # rename columns  here we need to rename eventhough we remove them later because we evaluate certain cols
  names(evals.df)[names(evals.df) == "ERP Internal ID"] <- "ID"  
  names(evals.df)[names(evals.df) == "Manager Name"] <- "Name"  
  # Stage    no change
  names(evals.df)[names(evals.df) == "Finance execution date"] <- "Date"
  names(evals.df)[names(evals.df) == "Raise Amount"] <- "Raise"
  # Department	no change
  # Level		    no change
  names(evals.df)[names(evals.df) == "Anniversary"] <- "X.Aniversary"
  # Division	  no change
  # names(evals.df)[names(evals.df) == "Payment Method"] <- "Pay.Method"
  # Payroll	    no change
  # Currency    no change
  
  names(evals.df)[names(evals.df) == "Mgt Raise Type"] <- "Type"
  names(evals.df)[names(evals.df) == "Evaluation Recommendation"] <- "Recommendation"
  names(evals.df)[names(evals.df) == "Retroactive Month"] <- "Ret.Month"
  names(evals.df)[names(evals.df) == "Retroactive"] <- "Retroactive"
  
  evals.df <- relocate(evals.df,ID, .before=Name)   # relocate after ID in first col
  evals.df$Stage <- as.factor(evals.df$Stage)       # convert to factor
  evals.df$Date <- JS_ParseDate(evals.df$Date,"Eval")      # convert to date REFACTORED
  evals.df$Raise[is.na(evals.df$Raise)] <- 0        # set NA values to 0
  
  evals.df[,6:13] <- lapply(evals.df[,6:13], factor) # convert remaining cols to factors
  evals.df$X.Aniversary <- ordered(evals.df$X.Aniversary, levels = month_levels) # convert to ordered factor
  evals.df$Ret.Month <- ordered(evals.df$Ret.Month, levels = month_levels)        # convert to ordered factor
  
  # filter as specified in arguments Month NO LONGER needed...   now dividing by 360 or prom
  if(Eval.Type=="360") {         # subset only 360 cases
    evals.df <- subset(evals.df,Division==Div & Payroll==PR & Date >= Start.Date & (evals.df$Type=="360" | evals.df$Type=="JCE")) 
  } else if (Eval.Type=="Prom"){ # subset only Prom cases
    evals.df <- subset(evals.df,Division==Div & Payroll==PR & Date >= Start.Date & (evals.df$Type=="Promotion" | evals.df$Type=="JCE, Promotion")) 
  } else { # Error
    log_fatal('LoadMgtEvaluations: Invalid Evaluation Type: {Eval.Type}')
    return(NULL)
  }
  
  # now calculate the execution month; this should be the month of the "Date" if the date is late 
  # in the month, however if the date is early in the month, most likely the exec month was the  
  # month before.   lets take 10 days as a safe margin... so if the date is the first 10 days of 
  # the month, then take the former month.
  
  # get the 360 execution month or NA if no 360...  still have the <10 == prev month logic
  # evals.df$X.Mon <- as.integer(format(as.Date(evals.df$Date, format="Y%-%m-%d"),"%m")) # REFACTORED
  evals.df$X.Mon <- ifelse(as.integer(format(as.Date(evals.df$Date, format="Y%-%m-%d"),"%d"))<10, # if day of the month < 10
                           as.integer(format(as.Date(evals.df$Date-20, format="Y%-%m-%d"),"%m")), # then substract 20 days
                           as.integer(format(as.Date(evals.df$Date, format="Y%-%m-%d"),"%m")))    # otherwise do nothing
  
  # now take care of retroactives  
  #evals.df$nx.mon <- ifelse(evals.df$Retroactive == T,as.numeric(evals.df$Ret.Month),NA) # get the month to which is retroactive
  evals.df$XR.fac <- ifelse(evals.df$Retroactive == T, evals.df$X.Mon-as.numeric(evals.df$Ret.Month),NA) # calculate the factor which salary has to be divided by when recursive
  # what happens with the former month?
  
  # always convert to USD
  evals.df$Raise <- ifelse(evals.df$Payroll == "Bolivia" & evals.df$Currency == "BOB",
                           evals.df$Raise/BOB2USD, # CONVERT TO USD
                           evals.df$Raise)         # DO NOTHING
  # here maybe need to convert from cop to usd
  evals.df$Currency <- "USD"
  
  evals.df$X.Raise  <- evals.df$Raise
  
  evals.df <- evals.df[, -c(2:6,8:13)]  
  
  lookup.months<- data.frame(month_levels, mon_lvls)
  evals.df$X.Aniversary<-lookup.months$mon_lvls[match(evals.df$X.Aniversary,lookup.months$month_levels)]
  evals.df$X.Aniversary <-factor(evals.df$X.Aniversary,ordered = T,levels = mon_lvls) # ordered factors
  evals.df$X.Mon <- factor(lookup.months[evals.df$X.Mon,]$mon_lvls,ordered = T, levels = mon_lvls)
  
  # check aduplicates...  
  duplicated.rows <- evals.df[duplicated(evals.df$ID),]
  tot_duplicates<-nrow(duplicated.rows)
  if(tot_duplicates>0) {
    log_error("LoadMgtEvaluations: {tot_duplicates} Duplicates found in Management Evaluations")
    log_error("LoadMgtEvaluations: duplicated: ID: {duplicated.rows$ID}") # No name in this dataframe
    #eid <- eid[!duplicated(eid$ID), ]
  }
  
  # append name extension for correct eval at the end
  names(evals.df) <- paste(names(evals.df),Eval.Type,sep=".") # append month sufix to all cols
  names(evals.df)[1] <- 'ID'               # rename first col
  
  # # sort by ID 
  evals.df <- evals.df[order(evals.df$ID),]
  
  rows<-nrow(evals.df)
  if(rows<=0){# warn when returning 0
    log_info("LoadMgtEvaluations: Returning Dataframe with {rows} rows...  please review your source csv file")
  } else {
    log_debug('LoadMgtEvaluations: Returning Dataframe with {nrow(evals.df)} rows') 
  }
  return(evals.df)
}

LoadEngEvaluations <- function(Div="Engineering B",PR="Latam",Start.Date,Eval.Type="No Type"){
  # data filters are: Division, Execution Month, Payroll AND EVAL.TYPE 
  # Load Engineer Evaluations from QNTRL and produce a short summary FOR EACH 360 AND PROMS SEPARATELY
  # So that they can be treated simultaneously for each engineer... i.e. an engineer can have a
  # 360 eval and raise in one month and a Prom and raise in another month
  # Noticed that there seems to be a problem in the date formats as exported from QNTRL... 
  # *** read_csv in 2.0.0 does not support files with carriage return \r as the line separator
  # work around is to open in excel and write
  
  log_info('Entered LoadEngEvaluations, Start Date: {Start.Date}, Type: {Eval.Type}') # Entering function info
  
  if (!file.exists(Eng.Evals.File.2023)){ # check if file exists before reading
    log_fatal('File {Eng.Evals.File.2023} does NOT exist, aborting in LoadEngEvaluations')
    return(NULL)
  }
  
  evals.df <- read_csv(Eng.Evals.File.2023)
  evals.df <- evals.df[, -c(5,6)]    # drop Assignee  and Requestor 
  
  # rename columns  here we need to rename eventhough we remove them later because we evaluate certain cols
  names(evals.df)[names(evals.df) == "ERP Internal ID"] <- "ID"  
  names(evals.df)[names(evals.df) == "Engineer Name"] <- "Name"  
  # Stage    no change
  names(evals.df)[names(evals.df) == "Finance execution date"] <- "Date"
  names(evals.df)[names(evals.df) == "Raise Amount"] <- "Raise"
  # Department	no change
  # Level		    no change
  names(evals.df)[names(evals.df) == "Anniversary"] <- "X.Aniversary"
  # Division	  no change
  # names(evals.df)[names(evals.df) == "Payment Method"] <- "Pay.Method"
  # Payroll	    no change
  # Currency    no change
  names(evals.df)[names(evals.df) == "Raise Type"] <- "Type"
  names(evals.df)[names(evals.df) == "Evaluation Recommendation"] <- "Recommendation"
  names(evals.df)[names(evals.df) == "Retroactive Month"] <- "Ret.Month"
  names(evals.df)[names(evals.df) == "Retroactive"] <- "Retroactive"
  
  evals.df <- relocate(evals.df,ID, .before=Name)                    # relocate after ID in first col
  evals.df$Stage <- as.factor(evals.df$Stage)                                      # convert to factor
  
  evals.df$Date <- JS_ParseDate(evals.df$Date,"Eval")      # convert to date REFACTORED
  evals.df$Raise[is.na(evals.df$Raise)] <- 0               # set NA values to 0
  
  evals.df[,6:15] <- lapply(evals.df[,6:15], factor) # convert remaining cols to factors
  evals.df$X.Aniversary <- ordered(evals.df$X.Aniversary, levels = month_levels) # convert to ordered factor
  evals.df$Ret.Month <- ordered(evals.df$Ret.Month, levels = month_levels)        # convert to ordered factor
  
  # filter as specified in arguments Month NO LONGER needed...   now dividing by 360 or prom
  if(Eval.Type=="360") {         # subset only 360 cases
    evals.df <- subset(evals.df,Division==Div & Payroll==PR & Date >= Start.Date & (evals.df$Type=="360" | evals.df$Type=="JCE")) 
  } else if (Eval.Type=="Prom"){ # subset only Prom cases
    evals.df <- subset(evals.df,Division==Div & Payroll==PR & Date >= Start.Date & (evals.df$Type=="Promotion" | evals.df$Type=="JCE, Promotion")) 
  } else { # Error
    log_fatal('LoadEngEvaluations: Invalid Evaluation Type: {Eval.Type}')
    return(NULL)
  }
  
  # now calculate the execution month; this should be the month of the "Date" if the date is late 
  # in the month, however if the date is early in the month, most likely the exec month was the  
  # month before.   lets take 10 days as a safe margin... so if the date is the first 10 days of 
  # the month, then take the former month.
  
  # get the 360 execution month or NA if no 360...  still have the <10 == prev month logic
  # evals.df$X.Mon <- as.integer(format(as.Date(evals.df$Date, format="Y%-%m-%d"),"%m")) #  RE REFACTORED
  evals.df$X.Mon <- ifelse(as.integer(format(as.Date(evals.df$Date, format="Y%-%m-%d"),"%d"))<20, # if day of the month < 10
                           as.integer(format(as.Date(evals.df$Date, format="Y%-%m-%d"),"%m"))-1, # then substract 1 month
                           as.integer(format(as.Date(evals.df$Date, format="Y%-%m-%d"),"%m")))    # otherwise do nothing
  
  # now take care of retroactives  
  #evals.df$nx.mon <- ifelse(evals.df$Retroactive == T,as.numeric(evals.df$Ret.Month),NA) # get the month to which is retroactive
  evals.df$XR.fac <-  ifelse(evals.df$Retroactive == T, evals.df$X.Mon-as.numeric(evals.df$Ret.Month),NA) # calculate the factor which salary has to be divided by when recursive
  # what happens with the former month?
  
  # always convert to USD
  evals.df$Raise <- ifelse(evals.df$Payroll == "Bolivia" & evals.df$Currency == "BOB",
                           evals.df$Raise/BOB2USD, # CONVERT TO USD
                           evals.df$Raise)         # DO NOTHING
  # here maybe need to convert from cop to usd
  evals.df$Currency <- "USD"
  
  evals.df$X.Raise  <- evals.df$Raise
  
  #remove unnecesary cols
  evals.df <- evals.df[, -c(2:7,9:14)]  
  
  lookup.months<- data.frame(month_levels, mon_lvls)
  evals.df$X.Aniversary<- factor(lookup.months$mon_lvls[match(evals.df$X.Aniversary,lookup.months$month_levels)],ordered = T,levels = mon_lvls) # aniversary convert to short format
  evals.df$X.Mon       <- factor(lookup.months[evals.df$X.Mon,]$mon_lvls,ordered = T, levels = mon_lvls) # execution month short format
  evals.df$Ret.Month   <- factor(lookup.months$mon_lvls[match(evals.df$Ret.Month,lookup.months$month_levels)],ordered = T,levels = mon_lvls) # retroactive month short format
  
  # check aduplicates...  
  duplicated.rows <- evals.df[duplicated(evals.df$ID),]
  tot_duplicates<-nrow(duplicated.rows)
  if(tot_duplicates>0) {
    log_fatal("LoadEngEvaluations: {tot_duplicates} Duplicates found in Evaluations")
    log_fatal("LoadEngEvaluations: duplicated: ID: {duplicated.rows$ID}") # No name in this dataframe
    log_fatal('Aborting')
    return(NULL)
    #eid <- eid[!duplicated(eid$ID), ]
  }
  
  # append name extension for correct eval at the end
  names(evals.df) <- paste(names(evals.df),Eval.Type,sep=".") # append month sufix to all cols
  names(evals.df)[1] <- 'ID'               # rename first col
  
  # # sort by ID 
  evals.df <- evals.df[order(evals.df$ID),]
  
  rows<-nrow(evals.df)
  if(rows<=0){ # warn when returning 0
    log_error("LoadEngEvaluations: Returning Dataframe with {rows} rows...  please review your source csv file")
  } else {
    log_debug('LoadEngEvaluations: Returning Dataframe with {nrow(evals.df)} rows') 
  }
  return(evals.df)
}

Load2023Raises <-function(Eng,Currency,Payroll){
  # Load Estimated and Executed Raises into a Eng table with ID and Aniversary and return dataframe with 
  # raises in selected currency
  
  log_info("Entered Load2023Raises...")
  
  lookup.months<- data.frame(month_levels, mon_lvls)
  Eng$Aniversary <- factor(lookup.months$mon_lvls[match( Eng$Aniversary,lookup.months$month_levels)],ordered = T,levels = mon_lvls) # ordered factors
  
  # first load Estimated Raises ER.DF
  # returns a table with proposed raises (360 and Prom) for both BOL and LAT payrolls
  # the raises are returned in the specified currency
  
  if (!file.exists(Bol.Raises.2023)){ # check if file exists before reading
    log_fatal('File {Bol.Raises.2023} does NOT exist, aborting in Load2023Raises')
    return(NULL)
  }
  
  # read the estiamated raises file and drop the name since does not seem necesary... will use Unit instead
  R.DF <- read_csv(Bol.Raises.2023) %>% select(-name)
  R.DF[2:5] <- lapply(R.DF[2:5], function(y) as.numeric(gsub("[$,]", "", y))) # get rid of csv format 
  
  # this row does not seem to be necessary but will leave just in case in the future we might get NAs?
  R.DF[is.na(R.DF)] <- 0   # set NA values to 0
  
  # Converting to appropiate currency
  if(Currency=="USD"){
    log_info("Load2023Raises: currency is USD, NO Conversion")
  } else if(Currency=="BOB"){
    R.DF[3:6] <- lapply(R.DF[3:6], function(y)(y*BOB2USD) ) # MULTIPLY BY CONVERSION FACTOR
    #R.DF$I.360 <- R.DF$I.360*(BOB2USD)    # CORRECT
    #R.DF$I.Prom <- R.DF$I.Prom*(BOB2USD)  # CORRECT
  } else if (Currency != "USD") {
    log_error('Load2023Raises: Invalid currency: {currency}') 
  }
  
  R.Rows <- nrow(R.DF)
  R.DF<- left_join(Eng, R.DF, by = "ID") # join aniversaries with raises
  
  N.Rows <- nrow(R.DF)
  if(R.Rows>N.Rows) {
    log_info("Load2023Raises: {R.Rows-N.Rows} Rows removed by left merge!")
  }
  
  # Normalize returned format to match executed raises
  names(R.DF)[names(R.DF) == "Aniversary"] <- "E.Aniversary"
  if(Payroll=="Bolivia"){
    log_info("Load2023Raises: Selecting Bolivia Payroll Raises")
    R.DF <- select(R.DF, -c(Inc.360.Lat,Inc.Prom.Lat))
    names(R.DF)[names(R.DF) == "Inc.360.Bol"] <- "E.Raise.360"
    names(R.DF)[names(R.DF) == "Inc.Prom.Bol"] <- "E.Raise.Prom"
  } else if (Payroll=="Latam"){
    log_info("Load2023Raises: Selecting Latam Payroll Raises")
    R.DF <- select(R.DF, -c(Inc.360.Bol,Inc.Prom.Bol))
    names(R.DF)[names(R.DF) == "Inc.360.Lat"] <- "E.Raise.360"
    names(R.DF)[names(R.DF) == "Inc.Prom.Lat"] <- "E.Raise.Prom"
  } else {
    log_error("Load2023Raises: Invalid Payroll")
    return(NULL)
  }
  # Now load executed Raises
  log_info("Load2023Raises: Loading Executed Raises...")
  
  Eng.X.Raises <- full_join(LoadEngEvaluations("Engineering B",Payroll,as.Date("2023/1/1"),"360"),
                            LoadEngEvaluations("Engineering B",Payroll,as.Date("2023/1/1"),"Prom"),by = "ID")
  Mgt.X.Raises <- full_join(LoadMgtEvaluations("Engineering B",Payroll,as.Date("2023/1/1"),"360"),
                            LoadMgtEvaluations("Engineering B",Payroll,as.Date("2023/1/1"),"Prom"),by = "ID")
  
  X.Raises <- rbind(Eng.X.Raises,Mgt.X.Raises)
  
  # Now Merge executed Raises with 
  R.Rows <- nrow(R.DF)
  R.DF<- left_join(R.DF, X.Raises,by = "ID") # join aniversaries with raises
  
  # after the left join it is possible to have several NA values... set those that we need to be <> NA
  # here we set raises / inc to 0 for new hires 2023.   The Name part will be NA still
  
  #	X.Aniversary.360 Ret.Month.360	Retroactive.360		XR.fac.360			Ret.Month.Prom	Retroactive.Prom	X.Aniversary.Prom	XR.fac.Prom	X.Raise.Prom
  
  R.DF$X.Raise.360[is.na(R.DF$X.Raise.360)]   <- 0 # set 0 for all NA values in Inc 360 Bol
  R.DF$X.Raise.Prom[is.na(R.DF$X.Raise.Prom)]   <- 0 # set 0 for all NA values in Inc 360 Lat
  
  R.DF$X.Mon.360[is.na(R.DF$X.Mon.360)] <- "Dec"   # set to Dec for all NA even if we reach Dec the raise should be 0 from above
  R.DF$X.Mon.Prom[is.na(R.DF$X.Mon.Prom)] <- "Dec" # set to Dec for all NA even if we reach Dec the raise should be 0 from above
  
  N.Rows <- nrow(R.DF)
  if(R.Rows>N.Rows) {
    log_info("Load2023Raises: {R.Rows-N.Rows} Rows removed by left merge!")
  }
  return(R.DF)
}

Summarize.Estimated.Raises <-function(R.Bol,R.Lat,End.Dates){
  log_info("Entered Summarize.Estimated.Raises...")
  BOL.COST <- 1 + P.Indem + P.Aguin1 + P.Aguin2 + Aportes # Labor law cost of bolivian contract
  year <- ordered(mon_lvls, levels = mon_lvls) 
  
  # create a data.frame of the sum of raises bol * 1.33 + raises lat and remove rows where E.Raise.360 = NA
  R.Eng <- cbind(select(R.Bol,ID:Unit),select(R.Bol,E.Raise.360,E.Raise.Prom)*BOL.COST+select(R.Lat,E.Raise.360,E.Raise.Prom)) 
  R.Eng <- left_join(R.Eng,End.Dates,by = "ID") %>% drop_na(E.Raise.360)
  
  # get the effective Hire.End.Date month making sure that it is 2023... if NOT, instead of NA put 13 as in Hire.End.Date = NA
  R.Eng$E.Month <- ifelse(format(as.Date(R.Eng$Hire.End.Date, format="%Y/%m/%d"),"%Y")==2023,format(as.Date(R.Eng$Hire.End.Date, format="%Y/%m/%d"),"%m"),13)
  R.Eng$E.Month <- as.integer(ifelse(is.na(R.Eng$Hire.End.Date),13,R.Eng$E.Month))
  
  # this could definitely be implemented in a couple of lines in an apply statement... maybe next time when refactoring
  # here we consider the end date also so if the engineer left before the end of the year, the estimated is modified according 
  # the date of the engineer leaving...    if the eng left in march the march month is considred but abril and afterwards NOT
  R.Eng$E.360.Jan <- ifelse(R.Eng$E.Aniversary<=year[1] & R.Eng$E.Month>=1,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Jan <- ifelse(R.Eng$E.Aniversary<=year[1]& R.Eng$E.Month>=1,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Feb <- ifelse(R.Eng$E.Aniversary<=year[2] & R.Eng$E.Month>=2,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Feb <- ifelse(R.Eng$E.Aniversary<=year[2]& R.Eng$E.Month>=2,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Mar <- ifelse(R.Eng$E.Aniversary<=year[3] & R.Eng$E.Month>=3,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Mar <- ifelse(R.Eng$E.Aniversary<=year[3]& R.Eng$E.Month>=3,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Apr <- ifelse(R.Eng$E.Aniversary<=year[4] & R.Eng$E.Month>=4,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Apr <- ifelse(R.Eng$E.Aniversary<=year[4]& R.Eng$E.Month>=4,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.May <- ifelse(R.Eng$E.Aniversary<=year[5] & R.Eng$E.Month>=5,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.May <- ifelse(R.Eng$E.Aniversary<=year[5]& R.Eng$E.Month>=5,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Jun <- ifelse(R.Eng$E.Aniversary<=year[6] & R.Eng$E.Month>=6,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Jun <- ifelse(R.Eng$E.Aniversary<=year[6]& R.Eng$E.Month>=6,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Jul <- ifelse(R.Eng$E.Aniversary<=year[7] & R.Eng$E.Month>=7,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Jul <- ifelse(R.Eng$E.Aniversary<=year[7]& R.Eng$E.Month>=7,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Aug <- ifelse(R.Eng$E.Aniversary<=year[8] & R.Eng$E.Month>=8,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Aug <- ifelse(R.Eng$E.Aniversary<=year[8]& R.Eng$E.Month>=8,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Sep <- ifelse(R.Eng$E.Aniversary<=year[9] & R.Eng$E.Month>=9,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Sep <- ifelse(R.Eng$E.Aniversary<=year[9]& R.Eng$E.Month>=9,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Oct <- ifelse(R.Eng$E.Aniversary<=year[10] & R.Eng$E.Month>=10,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Oct <- ifelse(R.Eng$E.Aniversary<=year[10]& R.Eng$E.Month>=10,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Nov <- ifelse(R.Eng$E.Aniversary<=year[11] & R.Eng$E.Month>=11,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Nov <- ifelse(R.Eng$E.Aniversary<=year[11]& R.Eng$E.Month>=11,R.Eng$E.Raise.Prom,0)
  R.Eng$E.360.Dec <- ifelse(R.Eng$E.Aniversary<=year[12] & R.Eng$E.Month>=12,R.Eng$E.Raise.360,0)
  R.Eng$E.Prom.Dec <- ifelse(R.Eng$E.Aniversary<=year[12]& R.Eng$E.Month>=12,R.Eng$E.Raise.Prom,0)
  
  log_debug('Summarize.Estimated.Raises...: Returning Dataframe with {nrow(R.Eng)} rows') 
  return(R.Eng)
}

Generate.Estimated.Raises.Table <- function(R.2023.Eng){
  # generate estimated raises summary table
  return(R.2023.Eng %>%   group_by(Unit) %>%
           summarise(Jan.360 = sum(E.360.Jan),Feb.360 = sum(E.360.Feb),Mar.360 = sum(E.360.Mar),
                     Apr.360 = sum(E.360.Apr),May.360 = sum(E.360.May),Jun.360 = sum(E.360.Jun),
                     Jul.360 = sum(E.360.Jul),Aug.360 = sum(E.360.Aug),Sep.360 = sum(E.360.Sep),
                     Oct.360 = sum(E.360.Oct),Nov.360 = sum(E.360.Nov),Dec.360 = sum(E.360.Dec),
                     Jan.Prom = sum(E.Prom.Jan),Feb.Prom = sum(E.Prom.Feb),Mar.Prom = sum(E.Prom.Mar),
                     Apr.Prom = sum(E.Prom.Apr),May.Prom = sum(E.Prom.May),Jun.Prom = sum(E.Prom.Jun),
                     Jul.Prom = sum(E.Prom.Jul),Aug.Prom = sum(E.Prom.Aug),Sep.Prom = sum(E.Prom.Sep),
                     Oct.Prom = sum(E.Prom.Oct),Nov.Prom = sum(E.Prom.Nov),Dec.Prom = sum(E.Prom.Dec)))
}

split.multiple.bonus <- function(bonus,times.per.year){
  #get rows and based on the "times.per.year" return the needed number of duplicates
  if(times.per.year==2) { # frequency = every 6 monhths then add one row with + 6 months
    month.number <- ifelse(as.integer(bonus$Month)+6>12,as.integer(bonus$Month)+6-12,as.integer(bonus$Month)+6)
    bonus$Month<-factor(mon_lvls[month.number], ordered = T,levels = mon_lvls) # convert to correct factor
  } else if (times.per.year == 4){ # frequency = every 3 monhths then add 3 rows +3 +3 +3
    # this part could be refactored
    month.number <- ifelse(as.integer(bonus$Month)+3>12,as.integer(bonus$Month)+3-12,as.integer(bonus$Month)+3)
    bonus$Month<-factor(mon_lvls[month.number], ordered = T,levels = mon_lvls) # convert to correct factor
    bonus_3 <- bonus
    month.number <- ifelse(as.integer(bonus_3$Month)+3>12,as.integer(bonus_3$Month)+3-12,as.integer(bonus_3$Month)+3)
    bonus_3$Month<-factor(mon_lvls[month.number], ordered = T,levels = mon_lvls) # convert to correct factor
    bonus_6 <- bonus_3
    month.number <- ifelse(as.integer(bonus_6$Month)+3>12,as.integer(bonus_6$Month)+3-12,as.integer(bonus_6$Month)+3)
    bonus_6$Month<-factor(mon_lvls[month.number], ordered = T,levels = mon_lvls) # convert to correct factor
    bonus<-rbind(bonus,rbind(bonus_3,bonus_6)) # bind all
  } else {
    log_error("split.multiple.bonus: Invalid bonus frequency!")
    return(NULL)
  }
  return(bonus)
}

Load2023Bonus <- function(Eng,Currency,PR,Bonus.Type){
  # Load Eng Bonus (Type=ENG) into an Eng table with ID and Aniversary and return dataframe with 
  # the bonuses in the selected currency
  
  log_info("Entered Load2023Bonus Loading:{Bonus.Type} Dataframe...")
  Eng$Aniversary <-recode_factor(Eng$Aniversary, # recode aniversary to short month
                                 "January" = "Jan",
                                 "February" = "Feb",
                                 "March" = "Mar",
                                 "April" = "Apr",
                                 "May" = "May",
                                 "June" = "Jun",
                                 "July" = "Jul",
                                 "August" = "Aug",
                                 "September" = "Sep",
                                 "October" = "Oct",
                                 "November" = "Nov",
                                 "December" = "Dec",
                                 .ordered = T,
                                 .default = levels(mon_lvls))
  
  # now depending on the bonus type load either ENG or BSU
  if(Bonus.Type=="ENG"){
    # Load all ENG bonus
    # returns a table with selected ENG bonus with frequency <> monthly
    # the bonus are returned in the specified currency
    
    if (!file.exists(Bol.ENG.Bonus.2023)){ # check if file exists before reading
      log_fatal('File {Bol.ENG.Bonus.2023} does NOT exist, aborting in Load2023Bonus')
      return(NULL)
    }
    
    B.DF <- read_csv(Bol.ENG.Bonus.2023)
    
    # rename columns  
    names(B.DF)[names(B.DF) == "ERP Internal ID"] <- "ID"  
    names(B.DF)[names(B.DF) == "Bonus Amount"] <- "B.Amount"  
    names(B.DF)[names(B.DF) == "Start Date"] <- "Start.Date"  
    names(B.DF)[names(B.DF) == "End Date"] <- "End.Date"  
    names(B.DF)[names(B.DF) == "Bonus Type"] <- "Type"  
    names(B.DF)[names(B.DF) == "Payment Method"] <- "Payment"  
    names(B.DF)[names(B.DF) == "Payment Frequency"] <- "Frequency"  
    names(B.DF)[names(B.DF) == "Department"] <- "Dept"
    names(B.DF)[names(B.DF) == "Division"] <- "Div"  
    
    names(B.DF)[names(B.DF) == "Currency"] <- "currency"  
    
    B.DF <- B.DF[, -c(1,11,12,13,14,17,18,19)]    # drop unnecesary columns
    
  } else if(Bonus.Type=="BSU"){
    # Load all ENG bonus
    # returns a table with selected ENG bonus with frequency <> monthly
    # the bonus are returned in the specified currency
    
    if (!file.exists(Bol.BSU.Bonus.2023)){ # check if file exists before reading
      log_fatal('File {Bol.BSU.Bonus.2023} does NOT exist, aborting in Load2023Bonus')
      return(NULL)
    }
    
    B.DF <- read_csv(Bol.BSU.Bonus.2023)
    
    # rename columns  
    names(B.DF)[names(B.DF) == "ERP Internal ID"] <- "ID"  
    names(B.DF)[names(B.DF) == "Start Date"] <- "Start.Date"  
    names(B.DF)[names(B.DF) == "End Date"] <- "End.Date"  
    names(B.DF)[names(B.DF) == "BSU Bonus Payroll"] <- "Payroll"  
    names(B.DF)[names(B.DF) == "Bonus Amount"] <- "B.Amount"  
    names(B.DF)[names(B.DF) == "Bonus Type"] <- "Type"  
    names(B.DF)[names(B.DF) == "BSU Bonus Payment Method"] <- "Payment"  
    names(B.DF)[names(B.DF) == "BSU Bonus Payment Frequency"] <- "Frequency"  
    names(B.DF)[names(B.DF) == "Department"] <- "Dept"
    names(B.DF)[names(B.DF) == "Division"] <- "Div"  
    names(B.DF)[names(B.DF) == "Currency"] <- "currency"  
    
    B.DF <- B.DF[, -c(1,11,12,15,16,17)]    # drop unnecesary columns
  } else {
    log_error("Load2023Bonus: Incorrect bonus type...  aborting")
    return(NULL)
  }
  
  B.DF <- relocate(B.DF,ID, .before=Name)       # relocate before name
  
  # "Here depending on the date format this might not work" THIS USED TO BE A WARNING
  B.DF$Start.Date <- JS_ParseDate(B.DF$Start.Date,"Bonus")   # convert to date REFACTORED
  B.DF$End.Date <- JS_ParseDate(B.DF$End.Date,"Bonus")       # convert to date REFACTORED
  
  # convert to factors
  B.DF$Payroll <- as.factor(B.DF$Payroll)
  B.DF$currency <- as.factor(B.DF$currency)
  B.DF$Stage <- as.factor(B.DF$Stage)
  B.DF$Type <- as.factor(B.DF$Type)
  B.DF$Payment <- as.factor(B.DF$Payment)
  B.DF$Frequency <- as.factor(B.DF$Frequency)
  B.DF$Div <- as.factor(B.DF$Div)
  B.DF$Dept <- as.factor(B.DF$Dept)
  
  # Now Filter what is needed before doing any conversions
  #B.DF <- filter(B.DF,(Payroll == PR) & (!Frequency %in% "Monthly" & !Frequency %in% NA) & (Stage %in% "Bonus Executed" | Stage %in% "Waiting for Finance Action"))
  # the line above is the former command to not include "MONTHLY"
  
  # use group by with tmp<-lat%>%group_by(ID,Month,Name) %>% summarise(Sum=sum(B.Amount),.groups='drop')
  
  # here we now know that there are no bonus with empty frequency but lets keep the !Frequency %in% NA for the future
  # B.DF <- filter(B.DF,(Payroll == PR) & (Type == "Eng") &(!Frequency %in% NA) & (Stage %in% "Bonus Executed" | Stage %in% "Waiting for Finance Action"))
  if(Bonus.Type=="ENG"){ # filter only ENG bonus 
    B.DF <- filter(B.DF,(Payroll == PR) & (Type == "Eng") & (Stage %in% "Bonus Executed" | Stage %in% "Waiting for Finance Action"))
  } else if (Bonus.Type=="BSU") { # filter only BSU; 
    B.DF <- filter(B.DF,(Payroll == PR) & (Type == "BSU") & (Stage %in% "Bonus Executed" | Stage %in% "Waiting for Finance Action"))
  } # else? we know that there are no other bonus types
  
  # now this is a fix for those bonus that dont have any frequency
  tot.na<-sum(is.na(B.DF$Frequency)) # count how many Freq=NA bonus we have
  if(tot.na > 0){ # if 1 or more then convert to Monthly
    log_info("Setting {tot.na} Eng Bonus with Frequency=NA to Frequency=Monthly")
    B.DF$Frequency[is.na(B.DF$Frequency)] <- "Monthly"  
  }
  
  # calculate the aniversary month when the first bonus of the year will be paid 
  B.DF$Month<-factor(mon_lvls[as.integer(strftime( B.DF$Start.Date, "%m"))], ordered = T,levels = mon_lvls) #
  # relocate after PA1
  B.DF <- relocate(B.DF,Month, .after=Start.Date)
  
  added_6mon_rows<-added_3mon_rows<-0
  # here we would need to split any bonuses that are <> yearly into components
  more_rows<-split.multiple.bonus(filter(B.DF,(Frequency %in% "Every 6 months")),2)
  added_6mon_rows<-nrow(more_rows)
  B.DF<-rbind(B.DF,more_rows)
  more_rows<-split.multiple.bonus(filter(B.DF,(Frequency %in% "Every 3 months")),4)
  added_3mon_rows<-nrow(more_rows)
  B.DF<-rbind(B.DF,more_rows)
  
  if(added_6mon_rows>0 | added_3mon_rows>0){
    log_info("Added {added_6mon_rows} every 6 month rows and added {added_3mon_rows} every 3 month rows")
  }
  
  # Converting to appropiate currency
  if(Currency=="USD"){
    log_info("Converting to USD")
    B.DF <- mutate(B.DF,B.Amount=case_when(currency == "BOB" ~ B.Amount/(BOB2USD), # convert BOB to USD
                                           currency == "USD" ~ B.Amount,           # no conversion
                                           currency == "COP" ~ NA_real_))          # put NA in case of COP for now
    B.DF$currency <- factor("USD") # we now know that all is USD and for COP the value will be NA
    
  } else if(Currency=="BOB"){
    log_info("Converting to BOB")
    B.DF <- mutate(B.DF,B.Amount=case_when(Currency == "BOB" ~ B.Amount,            # no conversion
                                           Currency == "USD" ~ B.Amount*(BOB2USD),  # convert USD to BOB
                                           Currency == "COP" ~ NA_real_))           #  put NA in case of COP for now
    B.DF$Currency <- "BOB" # we now know that all is BOB and for COP the value will be NA
  } else if (Currency != "USD") {
    log_error('Invalid currency: {currency}') 
  }
  
  B.DF <- B.DF[order(B.DF$ID),]   # # sort by ID
  return(B.DF)
}

EstimatePayrollData2023 <- function(Month.2023,Mon,MS,Raises,E.Bonus,Payroll){
  # create a dataframe with current month payroll data based on a given payroll month and payroll data
  # added condition if Tot.Seniority is 0 then data is invalid and set all values to 0
  log_info('Entering EstimatePayrollData2023 for {Mon}, {Payroll}...') 
  if( Payroll!="Bolivia" & Payroll!="Latam") {
    log_error("EstimatePayrollData2023: No such Payroll, returning NULL")
    return(NULL)
  }
  if(Mon == "Jan"){ # return error if Month == Jan
    log_error("EstimatePayrollData2023: Incorrect {Mon} == Jan ")
    return(NULL)
  }
  
  lookup.months<- data.frame(month_levels, mon_lvls) # load month lookup table
  BA <- Load.Seniority.Bonus.Table(Payroll,"USD") # Load Seniority Bonus Table based on Payroll
  Mon<-factor(Mon,ordered = T,levels = mon_lvls) # convert to factor
  MS$Aniversary<-lookup.months$mon_lvls[match(MS$Aniversary,lookup.months$month_levels)] # change 2 short name
  MS$Aniversary <-factor(MS$Aniversary,ordered = T,levels = mon_lvls) # ordered factors
  
  PR <- left_join(MS, Create.PR.DF(), by = "ID") # create empty dataframe with seniority
  PR <- left_join(PR,Raises, by = "ID") # left join with raises
  
  # now in each asignement check if Tot.Seniority is > 0 for it to be valid
  
  PR$WDays <- ifelse(PR$Tot.Seniority<=0,0,30)
  PR$HB  <- ifelse(PR$Tot.Seniority<=0,0,Month.2023$HB)      # HB is always HB to the end of the year
  PR$Inc.HB <- ifelse(PR$Tot.Seniority<=0,0,PR$HB*Gov.Raise) # here we need to define this according to Gov Raise
  
  # set 360/prom values
  PR$Inc.360 <- ifelse(PR$Tot.Seniority<=0,0,ifelse(PR$Aniversary < Mon, Month.2023$Inc.360, # if aniversary < January then prev month
                                                    ifelse(PR$Aniversary == Mon, PR$E.Raise.360,0))) # if aniversary == current month, else 0
  PR$Inc.Prom <- ifelse(PR$Tot.Seniority<=0,0,ifelse(PR$Aniversary < Mon, Month.2023$Inc.Prom, # if aniversary < January then prev month
                                                     ifelse(PR$Aniversary == Mon, PR$E.Raise.Prom,0))) # if aniversary == current month, else 0
  PR$Inc.360[is.na(PR$Inc.360)] <- 0    # Set NA values to 0... this can happen with new hires who are Not in the Raises DF  
  PR$Inc.Prom[is.na(PR$Inc.Prom)] <- 0  # Set NA values to 0... this can happen with new hires who are Not in the Raises DF 
  
  PR$Retroactive  <- 0   # no retroactive when estimating
  PR$BJP <- ifelse(PR$Tot.Seniority<=0,0,Month.2023$BJP) # BJP remains the same through the year
  
  # *** here we need to define which column to use
  if(Mon == "Feb" | Mon == "Mar" | Mon == "Apr") {
    log_info('EstimatePayrollData2023: Using BA with ***2022*** Min Salary')
    PR$BA <- ifelse(PR$Tot.Seniority<=0,0,BA[as.integer(floor(PR$LL.Seniority)+1),2])    # Before May
  } else {
    log_info('EstimatePayrollData2023: Using BA with ***2023*** Min Salary')
    PR$BA <- ifelse(PR$Tot.Seniority<=0,0,BA[as.integer(floor(PR$LL.Seniority)+1),3])    # May and After
  }
  
  PR$B.Aliment    <- 0   # no Aliment for engineering
  PR$B.Lead <- ifelse(PR$Tot.Seniority<=0,0,Month.2023$B.Lead) 
  
  # calculate the ENG bonus
  # first include the monthly bonus
  PR <- left_join(PR,E.Bonus %>% filter(Type=="Eng" & Frequency == "Monthly"), by = "ID") # ok to do this because now we know that bonus only contains current month bonus with no duplicates
  PR$B.Amount[is.na(PR$B.Amount)] <- 0 # set those NA to 0... this has to be here because of the left join introduces NAs
  PR$B.ENG <- ifelse(PR$Tot.Seniority<=0,0,PR$B.Amount) # SET the monthly bonus to all as a base
  PR <- PR %>% select(-c(Name,Month,Start.Date,End.Date,B.Amount,Type,Frequency,Payment))
  
  # now include the anual bonus
  PR <- left_join(PR,E.Bonus %>% filter(Type=="Eng" & Frequency == "Yearly" & Month == Mon), by = "ID") # ok to do this because now we know that bonus only contains current month bonus with no duplicates
  PR$B.Amount[is.na(PR$B.Amount)] <- 0 # set those NA to 0... this has to be here because of the left join introduces NAs
  PR$B.ENG <- ifelse(PR$Tot.Seniority<=0,0,PR$B.ENG + PR$B.Amount) # ADD the yearly bonus to all 
  PR <- PR %>% select(-c(Name,Month,Start.Date,End.Date,B.Amount,Type,Frequency,Payment))
  
  # now include the every 6 months bonus
  PR <- left_join(PR,E.Bonus %>% filter(Type=="Eng" & Frequency == "Every 6 months" & Month == Mon), by = "ID") # ok to do this because now we know that bonus only contains current month bonus with no duplicates
  PR$B.Amount[is.na(PR$B.Amount)] <- 0 # set those NA to 0... this has to be here because of the left join introduces NAs
  PR$B.ENG <- ifelse(PR$Tot.Seniority<=0,0,PR$B.ENG + PR$B.Amount) # ADD the yearly bonus to all 
  PR <- PR %>% select(-c(Name,Month,Start.Date,End.Date,B.Amount,Type,Frequency,Payment))
  
  # now add the quarterly. this has to be separated from anual because there could be a coincidence of the same month
  
  # calculate the BSU bonus
  # first include the monthly bonus
  PR <- left_join(PR,E.Bonus %>% filter(Type=="BSU" & Frequency == "Monthly"), by = "ID") # ok to do this because now we know that bonus only contains current month bonus with no duplicates
  PR$B.Amount[is.na(PR$B.Amount)] <- 0 # set those NA to 0... this has to be here because of the left join introduces NAs
  PR$B.BSU <- ifelse(PR$Tot.Seniority<=0,0,PR$B.Amount) # SET the monthly bonus to all as a base
  PR <- PR %>% select(-c(Name,Month,Start.Date,End.Date,B.Amount,Type,Frequency,Payment))
  
  # now include the anual bonus
  PR <- left_join(PR,E.Bonus %>% filter(Type=="BSU" & Frequency == "Yearly" & Month == Mon), by = "ID") # ok to do this because now we know that bonus only contains current month bonus with no duplicates
  PR$B.Amount[is.na(PR$B.Amount)] <- 0 # set those NA to 0... this has to be here because of the left join introduces NAs
  PR$B.BSU <- ifelse(PR$Tot.Seniority<=0,0,PR$B.BSU + PR$B.Amount) # ADD the yearly bonus to all
  PR <- PR %>% select(-c(Name,Month,Start.Date,End.Date,B.Amount,Type,Frequency,Payment))
  
  # now include the every 6 months bonus
  PR <- left_join(PR,E.Bonus %>% filter(Type=="BSU" & Frequency == "Every 6 months" & Month == Mon), by = "ID") # ok to do this because now we know that bonus only contains current month bonus with no duplicates
  PR$B.Amount[is.na(PR$B.Amount)] <- 0 # set those NA to 0... this has to be here because of the left join introduces NAs
  PR$B.BSU <- ifelse(PR$Tot.Seniority<=0,0,PR$B.BSU + PR$B.Amount) # ADD the yearly bonus to all
  PR <- PR %>% select(-c(Name,Month,Start.Date,End.Date,B.Amount,Type,Frequency,Payment))
  
  # now add the quarterly. this has to be separated from anual because there could be a coincidence of the same month
  
  # maybe we should do the same for Client bonus?
  PR$B.Client <- ifelse(PR$Tot.Seniority<=0,0,Month.2023$B.Client)
  
  CB <- Load.City.Bonus.Table(Payroll,"USD")
  
  # here is the city bonus estimation when an engineer is promted and from another city...   
  # the logic is simple... only match the current city bonus and go to the next level if promoted
  # here we arready have a bug when an engineer is promoted and is NOT jce.. his next city bonus will go to JCE instead of staff
  # maybe remove the JCE bonus and just use jr -> staff
  PR$B.City <- ifelse(PR$Tot.Seniority<=0,0,ifelse(PR$Aniversary == Mon & PR$Inc.Prom > 0 & Month.2023$B.City > 0, # If aniversary == month means at aniversary 
                                                   CB[match(Month.2023$B.City,CB$bonus,0)+1,]$bonus,               # AND if promotion AND if had city bonus before then raise city bonus
                                                   Month.2023$B.City)) # otherwise keep the former bonus
  
  PR$EST.WW       <- 0   # no EST.WW when estimating
  
  PR$TG <- PR$HB + PR$BJP + PR$Inc.360 + PR$Inc.Prom + PR$BA + PR$B.Lead + PR$B.BSU + PR$B.ENG + PR$B.Client + PR$B.City + PR$Inc.HB #fixing bug 69
  
  # estimate Other Bonus (which is the payoneer transfer fee for Latam based on TG)
  
  if(Payroll=="Latam"){
    # this has been approved by Rupay
    PR$B.Other[PR$TG >= 4000 & PR$Tot.Seniority>0] <- 80                
    PR$B.Other[PR$TG < 4000 & PR$TG >= 3000 & PR$Tot.Seniority>0] <- 60
    PR$B.Other[PR$TG < 3000 & PR$TG >= 2000 & PR$Tot.Seniority>0] <- 40
    PR$B.Other[PR$TG < 2000 & PR$TG >= 1500 & PR$Tot.Seniority>0] <- 30
    PR$B.Other[PR$TG < 1500 & PR$TG >= 1000 & PR$Tot.Seniority>0] <- 20
    PR$B.Other[PR$TG < 1000 & PR$TG >  0 & PR$Tot.Seniority>0] <- 15   # just to make sure not cents creep in
    PR$B.Other[PR$TG <= 0]     <- 0 # consider NA in TG
    temp <- PR %>% filter(B.Other==0)
    PR$TG <- PR$TG + PR$B.Other # recalcualte Total Ganado including Payoneer Fee
  }
  else{
    PR$B.Other     <- 0 
  }
  
  if(Payroll=="Bolivia"){ # beneficios only in Labor Law contracts
    PR$PI  <- ifelse(PR$Tot.Seniority<=0,0,PR$TG * P.Indem)  
    PR$PA1 <- ifelse(PR$Tot.Seniority<=0,0,PR$TG * P.Aguin1)  
    PR$PA2 <- ifelse(PR$Tot.Seniority<=0,0,PR$TG * P.Aguin2)  
    PR$AP  <- ifelse(PR$Tot.Seniority<=0,0,PR$TG * Aportes)  
  } else if (Payroll=="Latam"){ # for latam beneficios is 0
    PR$PI  <- 0 
    PR$PA1 <- 0 
    PR$PA2 <- 0
    PR$AP  <- 0
  } else {
    log_error('EstimatePayrollData2023: Invalid Payroll: {Payroll}... aborting') # there should NOT be an else here
    return(NULL)
  }
  
  # Calculate indicators
  PR$Basic <- PR$HB + PR$BJP + PR$Inc.360 + PR$Inc.Prom + PR$BA + PR$Inc.HB + PR$B.Other  # here Inc.HB was NOT included and bug 69 discovered on ver 3.2.5... ADDING ALSO B.Other
  PR$Risk <- PR$B.BSU + PR$B.Client + PR$B.ENG 
  PR$TotalComp <- PR$Basic + PR$Risk +PR$PI + PR$PA1 
  PR$NetPay <- PR$TotalComp + PR$B.Lead + PR$B.City 
  PR$TotalCost <- PR$NetPay + PR$AP       
  PR <- select(PR, -c(Aniversary,E.Aniversary:X.Raise.Prom)) # remove added "raises" columns
  
  # append name extension for correct month at the end
  names(PR) <- paste(names(PR),Mon,sep=".") # append month sufix to all cols
  names(PR)[1] <- 'ID'               # rename first col
  
  # Sanity check: see if there are any duplicate IDs in the dataframe
  duplicates<-subset(PR,duplicated(ID))
  if(nrow(duplicates>0)){
    log_fatal("Duplicate rows found in dataframe:")
    log_fatal("Ofending IDs: {duplicates$ID}")
    return(NULL)
  }
  
  log_debug('EstimatePayrollData2023: Returning Dataframe with {nrow(PR)} rows') 
  return(PR)
}

EstimateJanuaryPayrollData2023 <- function(Payroll.2022,Month,MS,Raises,Payroll){
  # create a dataframe with January payroll data based on a given payroll month and payroll data
  # deprecated function since january execution has not been updated
  log_fatal('This function outdated and needs to be updated as with estimatePayrollData')
  return(NULL)
  log_info('Entering EstimateJanuaryPayrollData2023 for {Payroll}') 
  if( Payroll!="Bolivia" & Payroll!="Latam") {
    log_error("EstimateJanuaryPayrollData2023: No such Payroll, returning NULL")
    return(NULL)
  }
  if(Month != "Jan"){ # return error if Month not Jan
    log_error("EstimateJanuaryPayrollData2023: Incorrect {Month} != Jan ")
    return(NULL)
  }
  
  lookup.months<- data.frame(month_levels, mon_lvls) # load month lookup table
  BA <- Load.Seniority.Bonus.Table(Payroll,"USD") # Load Seniority Bonus Table based on Payroll
  Month<-factor(Month,ordered = T,levels = mon_lvls) # convert to factor
  MS$Aniversary<-lookup.months$mon_lvls[match(MS$Aniversary,lookup.months$month_levels)] # change 2 short name
  MS$Aniversary <-factor(MS$Aniversary,ordered = T,levels = mon_lvls) # ordered factors
  
  # add rows from MS that are not in Payroll.2022
  RowsToAdd <- setdiff(MS$ID,Payroll.2022$ID)
  
  TotRowsToAdd <- length(RowsToAdd)
  if(TotRowsToAdd>0){
    log_warn("EstimateJanuaryPayrollData2023: Adding {TotRowsToAdd} from MS NOT in Payroll2022")
    log_warn("EstimateJanuaryPayrollData2023: here we should refactor so that all aniversaries handle the short month version")
    Payroll.2022 <- bind_rows(Payroll.2022,MS[MS$ID %in% RowsToAdd,]) # Add missing rows to dataframe
    
    Payroll.2022[is.na(Payroll.2022)] <- 0                       # set 0 to all NA values. REFACTORING NEEDED
    # only the added rows should be zeroed
    Payroll.2022 <- Payroll.2022[order(Payroll.2022$ID),]        # sort by ID 
    log_warn("EstimateJanuaryPayrollData2023: ID: {RowsToAdd} added!")
  }
  
  PR <- left_join(MS, Create.PR.DF(), by = "ID") # create empty dataframe with seniority
  PR <- left_join(PR,Raises, by = "ID") 
  
  Payroll.2022 <- Payroll.2022[Payroll.2022$ID %in% MS$ID,] # remove rows from Payroll2022 that are not in MS
  
  PR$WDays <- 30
  PR$HB  <- Payroll.2022$HB # seems like Inc.HB is no longer available here+ Payroll.2022$Inc.HB # calculate New Starting HB
  PR$Inc.HB <- PR$HB*Gov.Raise    # here we need to define this according to Gov Raise
  PR$Retroactive  <- 0   # no retroactive when estimating
  PR$B.Aliment    <- 0   # no Aliment for engineering
  PR$EST.WW       <- 0   # no EST.WW when estimating
  PR$BJP <- Payroll.2022$BJP # here we used to include 360 and prom + Payroll.2022$Inc.360 + Payroll.2022$Inc.Prom # calculate New Starting BJP
  
  # set 360/prom values
  PR$Inc.360 <- ifelse(PR$Aniversary < Month, 0, # if aniversary < January then 0, 
                       ifelse(PR$Aniversary == Month, PR$E.Raise.360,0)) # if aniversary == current month, else 0
  PR$Inc.Prom <- ifelse(PR$Aniversary < Month, 0, # if aniversary < January then 0, 
                        ifelse(PR$Aniversary == Month, PR$E.Raise.Prom,0)) # if aniversary == current month, else 0
  
  # PR$Inc.360[is.na(PR$Inc.360)] <- 0    # Set NA values to 0... this can happen with new hires who are Not in the Raises DF  
  # PR$Inc.Prom[is.na(PR$Inc.Prom)] <- 0  # Set NA values to 0... this can happen with new hires who are Not in the Raises DF 
  
  # here because we know it is January we always use the former year Seniority Bonus
  PR$BA <- BA[as.integer(floor(PR$LL.Seniority)+1),2]    # NEED TO VERIFY Bono de antiguedad calculado + 1 porque el vector indice del vector empieza en 1
  
  PR$B.Lead <- Payroll.2022$B.Lead 
  PR$B.BSU  <- Payroll.2022$B.BSU
  PR$B.ENG  <- Payroll.2022$B.ENG
  PR$B.Client <- Payroll.2022$B.Client
  PR$B.City <- Payroll.2022$B.City # here we should revise if there is a promotion
  
  PR$TG <- PR$HB + PR$BJP + PR$Inc.360 + PR$Inc.Prom + PR$BA + PR$B.Lead + PR$B.BSU + PR$B.ENG + PR$B.Client + PR$B.City + PR$Inc.HB # fixing bug 69
  
  # estimate Other Bonus (which is the payoneer transfer fee for Latam based on TG)
  currency<-"USD" # small fix so that currency consideration code below can remain
  if(Payroll=="Bolivia"){
    # No Payoneer Fee if Bolivia Payroll
    conversion.factor<-0
  } else if(Payroll=="Latam"){
    # only if Latam payroll there is a payoneer fee
    if(currency == "USD"){
      # if usd currency then no conversion
      conversion.factor<-1
    } else if(currency == "BOB"){
      # if bob then convert to bob
      conversion.factor<-BOB2USD
    } else {
      log_error("EstimateJanuaryPayrollData2023: Unknow currency: {currency}")
      return(NULL)
    }
  } else {
    log_error("EstimateJanuaryPayrollData2023: Unknown Payroll Selection: {Payroll}")
    return(NULL)
  }
  
  PR$B.Other[PR$TG >= 4000]                <-80 * conversion.factor
  PR$B.Other[PR$TG < 4000 & PR$TG >= 3000] <-60 * conversion.factor
  PR$B.Other[PR$TG < 3000 & PR$TG >= 2000] <-40 * conversion.factor
  PR$B.Other[PR$TG < 2000 & PR$TG >= 1500] <-30 * conversion.factor
  PR$B.Other[PR$TG < 1500 & PR$TG >= 1000] <-20 * conversion.factor
  PR$B.Other[PR$TG < 1000 & PR$TG >  1]    <-15 * conversion.factor # just to make sure not cents creep in
  PR$B.Other[PR$TG < 1]                    <- 0 * conversion.factor
  
  PR$TG <- PR$TG + PR$B.Other # recalcualte Total Ganado including Payoneer Fee
  
  if(Payroll=="Bolivia"){ # beneficios only in Labor Law contracts
    PR$PI  <- PR$TG * P.Indem  
    PR$PA1 <- PR$TG * P.Aguin1  
    PR$PA2 <- PR$TG * P.Aguin2  
    PR$AP  <- PR$TG * Aportes  
  } else if (Payroll=="Latam"){ # for latam beneficios is 0
    PR$PI  <- 0 
    PR$PA1 <- 0 
    PR$PA2 <- 0
    PR$AP  <- 0
  } else {
    log_error("EstimateJanuaryPayrollData2023: Invalid Payroll: {Payroll}... aborting") # there should NOT be an else here
    return(NULL)
  }
  
  # Calculate indicators
  PR$Basic <- PR$HB + PR$BJP + PR$Inc.360 + PR$Inc.Prom + PR$BA + PR$Inc.HB + PR$B.Other  # here Inc.HB was NOT included and bug 69 discovered on ver 3.2.5 INCLUDING B.Other
  PR$Risk <- PR$B.BSU + PR$B.Client + PR$B.ENG 
  PR$TotalComp <- PR$Basic + PR$Risk +PR$PI + PR$PA1 # Calculate Total Comp should we include City?
  PR$NetPay <- PR$TotalComp + PR$B.Lead + PR$B.City 
  PR$TotalCost <- PR$NetPay + PR$AP       # include aportes USED TO BE TOTALCOSTCALC
  
  PR <- select(PR, -c(Aniversary,E.Aniversary,name,E.Raise.360,E.Raise.Prom,X.Aniversary,X.Raise.360,X.Raise.Prom, )) # remove added "raises" columns
  
  # append name extension for correct month at the end
  names(PR) <- paste(names(PR),Month,sep=".") # append month sufix to all cols
  names(PR)[1] <- 'ID'               # rename first col
  
  log_debug('EstimateJanuaryPayrollData2023: Returning Dataframe with {nrow(PR)} rows') 
  return(PR)
}

Create.PR.DF <- function(){
  log_info('Creating empty payroll dataframe...') # Script starts
  y <- data.frame(ID=integer(),          # internal id
                  WDays=integer(),       # Worked Days (not needed but necesary for consistency)
                  HB=numeric(),          # Haber Basico
                  Inc.HB=numeric(),      # Incremento al Haber Basico (gobierno)
                  Inc.360=numeric(),     # Incremento por 360
                  Inc.Prom=numeric(),    # Incremento por Promocion
                  Retroactive=numeric(), # Retroactivo 
                  BJP=numeric(),         # Bono Jerarquico Profesional
                  BA=numeric(),          # Bono Antiguedad
                  B.Aliment=numeric(),   # Bono Alimentacion (unused in engineering)
                  B.Lead=numeric(),   # Bono Lead
                  B.BSU=numeric(),    # Bono BSU 
                  B.ENG=numeric(),    # Bono Eng 
                  B.Client=numeric(), # Bono Cliente 
                  B.City=numeric(),   # Bono Ciudad 
                  EST.WW=numeric(),   # EST / Weekend Work
                  B.Other=numeric(),  # Otros Bonos
                  TG=numeric(),       # Total Ganado 
                  PI=numeric(),       # Prevision Indeminizacion
                  PA1=numeric(),      # Prevision Aguinaldo 1 
                  PA2=numeric(),      # Prevision Aguinaldo 2
                  AP=numeric())       # Aportes Patronales
  return(y)
}

Load.Seniority.Bonus.Table <- function(Payroll,currency){
  # Return a table to calculate "bono de antiguedad" for 2020 and for 2022 & 2023
  # if payroll="Bolivia" returned data may be in BOB or USD depending on specified currency
  log_info('Loading Seniority.Bonus.Table for {Payroll} and currency {currency}...') # Entering function info
  
  # FL min salary data
  # smn2020 <- 2122 # salario min 2020   NOT NEEDED
  # smn2021 <- 2164 # salario min 2021   NOT NEEDED
  if(currency=="BOB") {
    smn2022 <- 2250   # salario min 2022
    smn2023 <- 2362.5 # salario min 2023 ESTIMATED 2385, Now 2362.5
  } else if (currency=="USD"){
    smn2022 <- 2250/BOB2USD   # salario min 2022 in usd
    smn2023 <- 2362.5/BOB2USD # salario min 2023 in usd
  } else
  {
    log_error("Load.Seniority.Bonus.Table: Unknown currency for Seniority Bonus {currency}")
    return(NULL)
  }
  
  year    <- c(0:30)
  if(Payroll=="Bolivia") {
    percent <- c(0,0,0.05,0.05,0.05,0.11,0.11,0.11,0.18,0.18,0.18,0.26,0.26,0.26,0.26,0.34,0.34,0.34,0.34,0.34,0.42,0.42,0.42,0.42,0.42,0.5,0.5,0.5,0.5,0.5,0.5)
  } else { # this is most likely Latam
    percent <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)   
  }
  # y.2021  <- 3*percent*smn2021
  y.2022  <- 3*percent*smn2022  # Seniority Bonus for 2022
  y.2023  <- 3*percent*smn2023  # Seniority Bonus for 2023
  y.2023d <- y.2023-y.2022      # difference from 2023 to 2022 (not sure we need this)
  Srt.Bonus.Table <- data.frame(year,y.2022,y.2023,y.2023d)
  
  log_debug('Load.Seniority.Bonus.Table: Returning Dataframe with {nrow(Srt.Bonus.Table)} rows') 
  return(Srt.Bonus.Table)
}

Load.City.Bonus.Table <- function(Payroll,currency){
  # Return a table with city bonus per level 2023
  # if payroll="Bolivia" returned data may be in BOB or USD deppending on specified currency
  # this will not work in Latam because we need to know which city the eng is... this coudl be estimated from current bonus but not necesarily
  log_info('Loading City.Bonus.Table for {Payroll} and currency {currency}...') # Entering function info
  
  level <- factor(cb_levels,ordered = T,levels = cb_levels) 
  if(Payroll== "Bolivia") {  
    bonus   <- c(522,870, 1392, 2088) # this is in BOB regardless of the city
  } else if (Payroll == "Latam") { 
    bonus   <- c(522,870, 1392, 2088) # this is in BOB regardless of the city
  } else {
    log_fatal('Invalid payroll type: {Payrooll}')
    return(NULL)
  }
  
  if(currency=="BOB") {
    # do nothing
  } else if (currency=="USD"){
    bonus <- bonus/BOB2USD # convert to usd 
  } else {
    log_error("Load.Seniority.Bonus.Table: Unknown currency for Seniority Bonus {currency}")
    return(NULL)
  }
  City.Bonus.Table <- data.frame(level,bonus)
  log_debug('City.Bonus.Table: Returning Dataframe with {nrow(City.Bonus.Table)} rows') 
  return(City.Bonus.Table)
}

# *** END 2023 Functions

elapsed_months <- function(end_date, start_date) {
  # calculate the number of elapsed months between two dates
  log_info('Entered elapsed_months...') # Entering function info
  
  return(12 * (as.integer(format(end_date,"%y")) - as.integer(format(start_date,"%y"))) + 
           (as.integer(format(end_date,"%m")) - as.integer(format(start_date,"%m"))))
}

calculate_worked_months <- function(year,ED){
  # calculate the number of months worked in the year specified by year
  log_info('Entered calculate_worked_months ...') # Entering function info
  
  # now fill hire end date NAs with start of next year
  ED$Hire.End.Date <- replace_na(ED$Hire.End.Date,as.Date(paste(year+1,"-01-01",sep="")))
  
  # now calculate number of worked months 
  ED$wm<-elapsed_months(ED$Hire.End.Date,ED$Hire.Date)
  
  ED <- ED[, -c(2,3)] # remove Hire and Hire.End date columns
  
  # do not allow worked months to go over 12 in a year
  ED$wm[ED$wm > 12] <- 12 
  
  return(ED)
}

Sum.Salar.Payroll.DataFrames <- function(BolDF,LatDF){
  # mergesum Bolivia and Latam Dataframes 
  log_info('Entered Sum.Salar.Payroll.DataFrames ...') # Entering function info
  
  # verify that salar Bol and salar Latam Payroll matrices have the same ID rows
  if(!all(LatDF[1:5]==BolDF[1:5])){
    #if here then there are some columns 1:5 that differ in some rows of Lat and Bol
    log_error("Sum.Salar.Payroll.DataFrames: Check on Lat and Bol DataFrames with identical ID rows FAILED")
    return(NULL)
  } else {
    log_info("Sum.Salar.Payroll.DataFrames: Check on Lat and Bol DataFrames with identical ID rows PASSED")
  }
  
  sal.cols <- ncol(BolDF)
  Base <- BolDF[1:6] # get the base from Bolivia Payroll (ID + seniority info + worked days) 
  Salary <- BolDF[7:sal.cols] + LatDF[7:sal.cols] # add salary columns 
  Base.Salary<-cbind(Base,Salary)
  return(Base.Salary)
}

Remove.Month.Prefix <- function(Month.Payroll.Data,Prefix){
  # remove the month prefix of the names for a specific month
  names(Month.Payroll.Data) <- sapply(strsplit(names(Month.Payroll.Data), Prefix), `[[`, 1)
  return(Month.Payroll.Data)
}

ConvertToCurrency <- function(RawD,target.currency=NA,source.currency=NA,start.col=NA,end.col=NA){
  if(is.na(start.col) | is.na(end.col)){
    log_error("ConvertToCurrency: Could Not Convert currency due to start.col = {start.col} or end.col = {end.col}")
    return(NULL)
  }
  if(source.currency==target.currency) {
    log_info("ConvertToCurrency: source currency == target currency, NO CONVERSION DONE")
  }
  else if(source.currency=="BOB" & target.currency=="USD"){ # convert from BOB2USD
    log_info("ConvertToCurrency: Converting from BOB to USD using {BOB2USD} convertion factor")
    RawD[start.col:end.col] <- lapply(RawD[start.col:end.col], function(x) x/BOB2USD)
  }
  else if(currency == "COP") {
    log_error("ConvertToCurrency: using INCORRECT convertion factor")
    RawD[start.col:end.col] <- lapply(RawD[start.col:end.col], function(x) x/BOB2COP)
  } else {
    log_error("ConvertToCurrency: Incorrect CONVERSION; No value returned")
    return(NULL)
  }
  return(RawD)
}

LoadPayrollData2021 <- function(FileName,Include.Inactives,ID.2022){
  # Load 2021 payrolldata and return december only
  log_info('Entered LoadPayrollData2021...') # Entering function info
  eid <- read.csv(FileName,check.names = F,header=T,sep=",",dec=".",fill=T)
  log_debug('LoadPayrollData2021: Read {FileName} with {nrow(eid)} rows')
  remaining.rows <- nrow(eid)
  
  # Remove Rows with errors...
  if (Include.Inactives == F) {         # if Include.Inactives, do not remove inactives 
    eid <- eid[eid$Active==T,]          # Remove rows with Active = False
    removed.rows <- remaining.rows - nrow(eid) # count how many rows removed
    if(removed.rows > 0) { log_info('LoadPayrollData2021: Removed {remaining.rows-nrow(eid)} rows with Active = F')}
    remaining.rows <- remaining.rows-removed.rows
  }
  
  # CLEANUP RECENTLY LOADED FILE
  eid <- eid[ ,-c(6,7,16,17,23,25,27:29,34,36:41)]             # Drop unnecessary columns 
  
  names(eid)[01] <- "ID"           # Row 01
  names(eid)[02] <- "Name"         # Row 02
  names(eid)[03] <- "Active"       # Row 03
  names(eid)[04] <- "Hire.Date"    # Row 04
  names(eid)[05] <- "Hire.End.Date"# Row 05
  names(eid)[06] <- "Aniversary"   # Row 06
  names(eid)[07] <- "Unit"         # Row 07
  names(eid)[08] <- "Dept"         # Row 08
  names(eid)[09] <- "Level"        # Row 09
  names(eid)[10] <- "Seniority"    # Row 10
  names(eid)[11] <- "LL.Seniority" # Row 10
  names(eid)[12] <- "HB"           # Row 12
  names(eid)[13] <- "Inc.HB"       # Row 13
  names(eid)[14] <- "BJP"          # Row 14
  names(eid)[15] <- "BA"           # Row 15
  names(eid)[16] <- "Inc.BA.SMN"   # Row 16
  names(eid)[17] <- "Inc.BA.Ant"  # Row 17
  names(eid)[18] <- "Inc.360"      # Row 18
  names(eid)[19] <- "Inc.Prom"     # Row 19
  names(eid)[20] <- "B.Lead"       # Row 20
  names(eid)[21] <- "B.BSU"        # Row 21
  names(eid)[22] <- "B.Client"     # Row 22
  names(eid)[23] <- "B.ENG"        # Row 23
  
  names(eid)[24] <- "B.City"       # Row 24
  names(eid)[25] <- "TG"           # Row 25
  names(eid)[26] <- "PI"    # Row 26
  names(eid)[27] <- "PA"       # Row 27
  names(eid)[28] <- "AP"     # Row 28
  names(eid)[29] <- "TotalCost"    # Row 33   Total Costo
  names(eid)[30] <- "B.Jala"    # Row 33   Total Costo
  
  eid$ID <- as.integer(eid$ID)   # convert ID to integer
  
  eid <- eid[is.na(eid$Hire.End.Date),]          # Remove rows with Hire.End.Date != NA
  removed.rows <- remaining.rows - nrow(eid) # count how many rows removed
  if(removed.rows > 0) { log_info('LoadPayrollData2021: Removed {remaining.rows-nrow(eid)} rows with Hire.End.Date != NA')}
  remaining.rows <- remaining.rows-removed.rows
  #eid[is.na(eid)] <- 0            # replace ramaining NA with 0
  
  eid[12:30] <- lapply(eid[12:30], function(y) as.numeric(gsub("[$,]", "", y))) # get rid of csv format 
  eid <- eid %>%  mutate_at(c(12:30), ~replace_na(.,0)) # replace cols 12:30 with NA with 0
  
  # Join MD data frame with payroll data...    here it is possible to have "empty rows"
  eid <- left_join(ID.2022, eid, by = "ID") # left join retains the MS data while removing rows from RawD that do not match
  removed.rows <- remaining.rows - nrow(eid) # count how many rows removed
  if(removed.rows > 0) { log_info('LoadPayrollData2021: Removed {removed.rows} rows from eng 2021 raw data by left merge')}
  remaining.rows <- remaining.rows-removed.rows
  eid <- select(eid, -c(Name.y)) # remove added "Name" column
  names(eid)[names(eid) == "Name.x"] <- "Name" # rename name col
  
  # CALCUALTE PARTIALS WHICH ARE USED TO VISUALIZE SALARIES 
  # NEEDs REVIEW
  
  eid$Avg.JB <- eid$B.Jala/12 #calculate average jala bonus                                 	             	
  eid$Basic <- eid$HB + eid$Inc.HB + eid$BJP + eid$BA + eid$Inc.BA.SMN +  eid$Inc.BA.Ant + eid$Inc.360 + eid$Inc.Prom   # Calculate Basic Salary
  eid$Risk <- eid$B.BSU + eid$B.Client + eid$B.ENG + eid$Avg.JB        # Calculate Risk Bonus
  eid$TotalComp <- eid$Basic + eid$Risk +eid$PI + eid$PA       # Calculate Total Comp should we include City?
  eid$NetPay <- eid$TotalComp + eid$B.Lead + eid$B.City        # include all other bonuses
  eid$TotalCost <- eid$TotalCost+eid$Avg.JB
  
  # # sort by ID 
  eid <- eid[order(eid$ID),]
  
  log_debug('LoadPayrollData2021: Returning Dataframe with {nrow(eid)} rows') 
  return(eid)
}

LoadSalarPayrollData <- function(FileName,Mon,MS,target.currency="NA",source.currency="NA",ConsiderRaises=F,Raises=NULL,BT=NULL){
  # here we load the corresponding month payroll data from Salar
  # and return in the specified currency
  # BT is a data frame with data of bonuses via bank transfer
  
  # we know that Month Seniority has the list of valid IDs from the employee file, including New Hires and Inactives
  
  log_info('Entered LoadSalarPayrollData... target.currency={target.currency} source.currency={source.currency}')  # Entering function info
  if(target.currency!="USD" & target.currency!="BOB" |
     source.currency!="USD" & source.currency!="BOB") {     # verify valid currencies
    log_error('LoadSalarPayrollData: Source Currency {source.currency} or Target Currency {target.currency} NOT defined...   exiting and returning NULL')
    return(NULL)
  }
  
  if (!file.exists(FileName)){ # check if file exists before reading
    log_fatal('File {FileName} does NOT exist, aborting in LoadSalarPayrollData')
    return(NULL)
  }
  
  RawD <- read.csv(FileName,check.names = F,header=T,sep=",",dec=".",fill=T)
  log_debug('LoadSalarPayrollData: Read {FileName} with {nrow(RawD)} rows')
  
  # CLEANUP RECENTLY LOADED FILE
  RawD <- RawD[ ,-c(2:4)]             # Drop unnecesary columns 
  
  #  1. ID                keep	
  #  2. Nombre empleado	  REMOVED
  #  3. Division	        REMOVED
  #  4. Departamento	    REMOVED
  
  names(RawD)[names(RawD) == "Dias trabajados"] <- "WDays"   # rename col needed to calculate monthly cost.
  #  6. HB	              keep
  names(RawD)[names(RawD) == "Incremento HB"] <- "Inc.HB"     # rename col
  names(RawD)[names(RawD) == "Retroactivo"] <- "Retroactive"  # rename col
  #  9. BJP	              keep
  # 10. BA	              keep
  names(RawD)[names(RawD) == "Bono Alimentacion"] <- "B.Aliment"  # rename col
  names(RawD)[names(RawD) == "EST WW"] <- "EST.WW"  # rename col
  names(RawD)[names(RawD) == "Bono Lead"] <- "B.Lead"  # rename col
  names(RawD)[names(RawD) == "Bono BSU"] <- "B.BSU"   # rename col
  names(RawD)[names(RawD) == "Bono ENG"] <- "B.ENG"   # rename col
  names(RawD)[names(RawD) == "Bono Cliente"] <- "B.Client"   # rename col
  names(RawD)[names(RawD) == "Bono City"] <- "B.City"   # rename col
  names(RawD)[names(RawD) == "Otros bonos"] <- "B.Other"   # rename col  
  names(RawD)[names(RawD) == "Total ganado"] <- "TG"   # rename col  
  names(RawD)[names(RawD) == "Prevision Indemnizacion"] <- "PI"   # rename col  
  names(RawD)[names(RawD) == "Provision Aguinaldo"] <- "PA1"   # rename col  
  #names(RawD)[names(RawD) == "Provision Aguinaldo"] <- "PA2"   # rename col  
  names(RawD)[names(RawD) == "Aportes patronales"] <- "AP"   # rename col  
  
  RawD$PA2  <- 0.0                         # segundo aguinaldo col
  RawD <- relocate(RawD,PA2, .after=PA1)   # relocate after PA1
  
  # DO NOT REMOVE ANY ROWS WITH LESS THAN 30 *** fixing issue related to bug 167
  # lessthan30 <- RawD[RawD$WDays<30,]  
  # RawD <- RawD[RawD$WDays>=30,]             # Keep rows with WDays >= 30 days 
  # removed.rows <- nrow(lessthan30) # count how many rows removed
  # if(removed.rows > 0 ){ 
  #   log_info('LoadSalarPayrollData {FileName}: Removed {removed.rows} rows with less than 30 days')
  #   log_success('LoadSalarPayrollData {FileName}: Less than 30 days: ID: {lessthan30$ID} WDays: {lessthan30$WDays} removed!')
  # } 

  # add the following columns
  # these 2 Columns SHOULD NOT affect the TG...   
  RawD$Inc.360  <- 0.0   # 360 raise col
  RawD$Inc.Prom <- 0.0  # Promotion raise col
  
  # relocate after Inc.HB
  RawD <- relocate(RawD,Inc.Prom, .after=Inc.HB)
  RawD <- relocate(RawD,Inc.360, .after=Inc.HB)
  
  # R.360.Col <- grep("Inc.360.", colnames(RawD)) # get the 360 and prom col numbers from dataframe
  # R.Prom.Col<- grep("Inc.Prom.", colnames(RawD))
  
  # Join MD data frame with payroll data...    here it is possible to have "empty rows"
  removed.rows<- anti_join(RawD,MS,by = "ID")
  RawD <- left_join(MS, RawD, by = "ID") # left join retains the MS data while removing rows from RawD that do not match

  # change to short month name
  lookup.months<- data.frame(month_levels, mon_lvls) # change to short month name
  RawD$Aniversary<-factor(lookup.months$mon_lvls[match(RawD$Aniversary,lookup.months$month_levels)],ordered = T,levels = mon_lvls) # ordered factors
  Mon <- factor(Mon,ordered = T,levels = mon_lvls) # convert Month to ordered factor

  rem.rows <- nrow(removed.rows) # count how many rows removed
  if(rem.rows > 0) { 
    log_info('LoadSalarPayrollData {FileName}: Removed {rem.rows} rows from Raw Salary data most likely because NOT present in EID list')
    log_success('LoadSalarPayrollData {FileName}: ID: {removed.rows$ID}, removed!')
  }
  
  # Convert to numeric starting at ID col
  RawD[7:ncol(RawD)] <- lapply(RawD[7:ncol(RawD)], function(y) as.numeric(gsub("[$,]", "", y))) # get rid of csv format and convert to numeric
  RawD[is.na(RawD)] <- 0                          # set those remaining NA to 0; 
  
  #verify calculation
  #RawD$AP.V <- RawD$TG * Aportes
  #log_error("There is an error of 0.01 in several cases for the Aportes Calculation")
  
  # convert to target currency... if we convert before calculating we are still ok
  RawD <- ConvertToCurrency(RawD,target.currency,source.currency,start.col = 8,end.col = ncol(RawD)) # convert from source.currency to currency

  # *** *** *** *** ***
  # ONLY ONCE WE HAVE CONVERTED CURRENCY here is the part where we load 360 info
  if(ConsiderRaises==TRUE){
    log_error('LoadSalarPayrollData {FileName}: need to load former aniversaries as well when NOT in Jan')
    
    # add executed and estimated raises with months to the dataframe
    RawD<-left_join(RawD,select(Raises,ID,X.Aniversary.360:X.Raise.Prom), by = "ID")
    
    # set all NAs to 0 or false so that they can be tested in conditions below
    RawD$X.Raise.360[is.na(RawD$X.Raise.360)]  <- 0 
    RawD$X.Raise.Prom[is.na(RawD$X.Raise.Prom)]<- 0 
    
    RawD$Retroactive.360[is.na(RawD$Retroactive.360)]  <- FALSE 
    RawD$Retroactive.Prom[is.na(RawD$Retroactive.Prom)]<- FALSE
    
    # set 360/prom values 
    # here we dont really care when the aniversary of the engineer is...   we will set the inc.360 or prom 
    # based on the execution month of either 360 or prom X.Aniversary.360 & X.Aniversary.Prom
    # Fixing bug 167... here we also need to consider if the employee is beyong his last day and in that case = 0
    # additionally if the current month is the last month then inc.360 x worked days / 30
    RawD$Inc.360 <-  ifelse(RawD$X.Mon.360 <= Mon, RawD$X.Raise.360,0) # if aniversary < current month executed raise
    RawD$Inc.Prom <- ifelse(RawD$X.Mon.Prom <= Mon, RawD$X.Raise.Prom,0) # if aniversary < current month then prev month
    
    # now consider those raises when the engineer worked less than a full month
    RawD$Inc.360 <-  ifelse(RawD$WDays < 30, RawD$Inc.360*RawD$WDays/30,RawD$Inc.360) # if worked less than 30 days raise*WDays/30
    RawD$Inc.Prom <- ifelse(RawD$WDays < 30, RawD$Inc.Prom*RawD$WDays/30,RawD$Inc.Prom)    # raise otherwise
    
    # actually, because the retroactives are being paid in another column, instead of substract in on month and add to another, 
    # lets just leave it in the retroactive column and forget the former column...  it just logic too complex
    
    # now consider the retroactive cases...   if retroactive then add to retroactive column 
    # actually decided NOT to consider retroactive cases because they are being considered in the "B.Other" Column
    #RawD$Retroactive <- RawD$Retroactive + ifelse(RawD$Retroactive.360 == TRUE,RawD$Inc.360*RawD$XR.fac.360,0) +
    #                                       ifelse(RawD$Retroactive.Prom == TRUE,RawD$Inc.Prom*RawD$XR.fac.Prom,0)
    
    #RawD <- RawD[ ,-c(2, 27:33)] # remove added rows...
    RawD <- select(RawD, -c(Aniversary,X.Aniversary.360:X.Raise.Prom)) # remove added "raises" columns
  } else {
    log_info('LoadSalarPayrollData {FileName}: NOT Loading Evaluations as per function flag ConsiderRaises=F')
  }
  # *** *** *** *** ***
  
  # here we will find those bonus paid via "bank transfer" and Latam and include them in the payroll
  # since Salar has no knowledge of such bonus
  # we use a separate section so that it will be easy to make changes / remove in the future
  
  # first include the monthly bonus
  if(!is.null(BT)){ # check if NOT null although these should be included in Consider.Raises==FALSE also
    # add Bank transfer bonus because they are not included with the Salar data
    # here we need to add the logic for monthly, etc...
    # also this part should be outside this considerraises==true
    log_warn('LoadSalarPayrollData: calculating monthly and anual bonus paid via "bank transfer"')
    
    # initialize
    jan.01.23 <- as.Date("2023-01-01")
    B.ENG <- 0
    B.BSU <- 0
    
    # join with Monthly bonus first
    RawD<-left_join(RawD,BT %>% filter(Frequency=="Monthly"), by = "ID")
    RawD$B.Amount[is.na(RawD$B.Amount)]<- 0 
    
    B.ENG <- B.ENG + ifelse((!is.na(RawD$Type) & !is.na(RawD$Month) & !is.na(RawD$Start.Date) & # make sure there are no NAs
                               (RawD$Start.Date < jan.01.23 | RawD$Month <= Mon) & RawD$Type=="Eng"),RawD$B.Amount,0.0) # SET the monthly bonus to all as a base
    B.BSU <- B.BSU + ifelse((!is.na(RawD$Type) & !is.na(RawD$Month) & !is.na(RawD$Start.Date) & # make sure there are no NAs
                               (RawD$Start.Date < jan.01.23 | RawD$Month <= Mon) & RawD$Type=="BSU"),RawD$B.Amount,0.0) # SET the monthly bonus to all as a base
    
    RawD <- select(RawD, -c(Name:Payment)) # remove added "bonus" columns
    
    # join with yearly bonus next 
    RawD<-left_join(RawD,BT %>% filter(Frequency=="Yearly"), by = "ID")
    RawD$B.Amount[is.na(RawD$B.Amount)]<- 0 
    
    B.ENG <- B.ENG + ifelse((!is.na(RawD$Type) & !is.na(RawD$Month) & # make sure there are no NAs
                               (RawD$Month == Mon) & RawD$Type=="Eng"),RawD$B.Amount,0.0) # now add the yearly bonus if the month matches
    B.BSU <- B.BSU + ifelse((!is.na(RawD$Type) & !is.na(RawD$Month) & # make sure there are no NAs
                               (RawD$Month == Mon) & RawD$Type=="BSU"),RawD$B.Amount,0.0) # now add the yearly bonus if the month matches
    
    RawD <- select(RawD, -c(Name:Payment)) # remove added "bonus" columns
    
    # now add the calculated "bank transfer bonus to the appropiate column
    RawD$B.ENG <- RawD$B.ENG + B.ENG # SET the monthly bonus to all as a base
    RawD$B.BSU <- RawD$B.BSU + B.BSU # SET the monthly bonus to all as a base
    
    # and correct the TG 
    RawD$TG <- RawD$TG + B.ENG + B.BSU
  }
  
  # Fix BJP before calculating anything
  RawD$BJP <- RawD$BJP-RawD$Inc.360-RawD$Inc.Prom # this because these raises come attached to the BJP from salar
  
  # everything else should remain the same: TG, AP, Idem, Aguin, etc
  
  # fixing bug 167 (Eng moved to latam should no longer have salaries after the labor law contract end date)
  # the fix should be: verifty if the hire end date is < than current month then set all to 0
  # fix the values before recalculating PARTIALS
  # if the hire end date of the engineer is passed... then the calculated seniority (Calc.Month.Seniority) returns 0
  # therefore Set all values to 0 if the Tot.Seniority is less than or equal to 0
  
  # WDays
  RawD$HB[RawD$Tot.Seniority <= 0]          = 0.0
  RawD$Inc.HB[RawD$Tot.Seniority <= 0]      = 0.0
  RawD$Inc.360[RawD$Tot.Seniority <= 0]     = 0.0
  RawD$Inc.Prom[RawD$Tot.Seniority <= 0]    = 0.0
  RawD$Retroactive[RawD$Tot.Seniority <= 0] = 0.0
  RawD$BJP[RawD$Tot.Seniority <= 0]         = 0.0
  RawD$BA[RawD$Tot.Seniority <= 0]          = 0.0
  RawD$B.Aliment[RawD$Tot.Seniority <= 0]   = 0.0
  RawD$B.Lead[RawD$Tot.Seniority <= 0]      = 0.0
  RawD$B.BSU[RawD$Tot.Seniority <= 0]       = 0.0
  RawD$B.ENG[RawD$Tot.Seniority <= 0]       = 0.0
  RawD$B.Client[RawD$Tot.Seniority <= 0]    = 0.0
  RawD$B.City[RawD$Tot.Seniority <= 0]      = 0.0
  RawD$EST.WW[RawD$Tot.Seniority <= 0]      = 0.0
  RawD$B.Other[RawD$Tot.Seniority <= 0]     = 0.0
  RawD$TG[RawD$Tot.Seniority <= 0]          = 0.0
  RawD$PI[RawD$Tot.Seniority <= 0]          = 0.0
  RawD$PA1[RawD$Tot.Seniority <= 0]         = 0.0
  RawD$PA2[RawD$Tot.Seniority <= 0]         = 0.0
  RawD$AP[RawD$Tot.Seniority <= 0]          = 0.0
  #Basic
  #Risk
  #TotalComp
  #NetPay
  #TotalCost 
  
  
  
  
  # CALCUALTE PARTIALS WHICH ARE USED TO VISUALIZE SALARIES 
  # HERE WE SHOULD PROBABLY RECALCULATE BJP SO THAT: BJP = BJP - Inc.360 - Inc.Prom
  #log_error("Note, we have included Inc.360 & Inc.Prom as part of the salar DataFrame")
  RawD$Basic <- RawD$HB + RawD$Inc.HB + RawD$BJP + RawD$Inc.360 + RawD$Inc.Prom + RawD$BA + RawD$B.Other    # Calculate Basic Salary including B.Other since now we use it
  RawD$RiskBonus <- RawD$B.BSU + RawD$B.Client + RawD$B.ENG   # Calculate Risk Bonus
  RawD$TotalComp <- RawD$Basic + RawD$RiskBonus + RawD$PI + RawD$PA1 + RawD$PA2#    # Calculate Total Comp 
  RawD$NetPay <- RawD$TG + RawD$PI + RawD$PA1 + RawD$PA2      # include all other bonuses
  RawD$TotalCost <- RawD$NetPay + RawD$AP       # include aportes USED TO BE TOTALCOSTCALC
  
   # append name extension for correct month at the end
  names(RawD) <- paste(names(RawD),Mon,sep=".") # append month sufix to all cols
  names(RawD)[1] <- 'ID'               # rename first col
  
  log_debug('LoadSalarPayrollData {FileName}: Returning Dataframe with {nrow(RawD)} rows') 
  return(RawD)
}

GetYearReferenceSalaries <- function(eng){
  log_info('Entered GetYearReferenceSalaries...') # Entering function info
  log_warn('GetYearReferenceSalaries: Not considered DevOps nor InfoDev categories')
  
  rows <- nrow(eng)
  # make sure only actives
  eng <- eng[eng$Active==T,]          # Remove rows with Active = False
  
  # select the columns that we want to keep
  eng<-subset(eng, select=c(ID, Name,	Aniversary.x,	Unit,	Dept,	Level, JCE,	Tot.Seniority,	TG,	Basic,		TotalComp,	NetPay,	TotalCost))
  
  # rename columns  
  names(eng)[names(eng) == "Tot.Seniority"] <- "Seniority"
  names(eng)[names(eng) == "Aniversary.x"] <- "Aniversary"
  
  # remove outliers... 
  outlier.list<-c(
    # JR automation
    3197, 2650, 3410, 3244, 3055, 3277, 2463, 1670,        # jr auto confirmed
    3111,                                                  # removed in round 2 04.23
    
    # Jr DEV
    2605, 2627, 3186, 3049, 1615, 2979, 3393, 3235, 3199, 3128, 3542, 3543, # Jr dev + confirmed
    3350, 3161, 3051, 3349, 3242, 3348, 3353, 3390,                         # jr dev - confirmed
    2315, 3367, 2461,                                                       # removed in round 2 04.23
    
    # JR QE
    2794, 3403, 2802, 3002,                         # jr qe approved
    
    # JR DevOps    
    3196,    # outlier and no longer in Jalasoft
    
    # JR INFODEV    
    #3068, # jr infodev
    
    # staff DEV
    2829, 2573, 2355, 2070, 2035, 2148, 2759, 2012, 2991, 1924, 1434, # staff dev - approved
    2177, 2153, 1910, 1526, 1844, 1336,                               # removed in round 2 04.23
    
    # STAFF AUTOMATION
    1335, 2129, 2308, 1824, 3003, 1475, 1911,           # staff auto approved
    
    # STAFF QE
    3071, 2531, 1824, 2387, 1836, 1543, 1310, 2197, 1691, 1879, 3041, 2341, # STAFF QE - Approved
    
    # STAFF DEVOPS
    #3323, 
    
    # Sr AUTO
    1646,      # Sr Auto - approved
    
    # Sr QE
    1730, 1758, 1502, 1362, 1542, 1492, 1724, 1382, # SR QE - approved
    
    # SENIOR DEV
    1606, 1351        # Sr Dev - approved
  )
  
  log_info('GetYearReferenceSalaries: Removing outliers: {outlier.list}')
  eng <- eng[!eng$ID %in% c(outlier.list),]
  
  new_rows <- nrow(eng)
  log_debug('GetYearReferenceSalaries: Deleted {rows-new_rows} rows')
  return(eng)
}

Calc.Month.Seniority <- function(E.ID,M.Dat){
  # calculate month seniority columns for given month and return a data.frame.
  # M.Dat is the date of the month with respect to which seniority data will be calculated
  # need to validate these calculations
  # REFACTORED TO short month name
  
  # calculate the first day of the month
  Month.Start <- as.Date(ISOdate(year  = year(M.Dat),
                                 month = month(M.Dat),
                                 day=1))
  
  log_info('Entered Calc.Month.Seniority ...') # Entering function info
  
  #calculate delta days
  years.to.m <- as.numeric(M.Dat - Jan.01.this.year.Date)/365       # seniority from M.Dat to first day of the year
  
  # create a seniority dataframe from ERP for a given month... Includes 2023 New Hires
  # if engineer hire end date is NOT NA and < that current date then set all seniority to 0
  # here we are refactoring so that we calculate Hire.End.Date>=Month.Start instead of >= M.Dat to allow partial work in a momth
  m.srt <- data.frame(ID = E.ID$ID, 
                      Aniversary = E.ID$Aniversary,                                                              # LL needed for bono de antiuedad
                      LL.Seniority = ifelse(is.na(E.ID$Hire.End.Date)|E.ID$Hire.End.Date>=Month.Start,round(as.numeric(Jan.01.this.year.Date-E.ID$Hire.Date)/365 + years.to.m,2),0), # calculate labor law seniority to desired month
                      JS.Pre.Seniority = ifelse(is.na(E.ID$Hire.End.Date)|E.ID$Hire.End.Date>=Month.Start,round(E.ID$J.Pre.Seniority,2),0),       # this doesnt change
                      JS.Seniority = ifelse(is.na(E.ID$Hire.End.Date)|E.ID$Hire.End.Date>=Month.Start,round(as.numeric(Jan.01.this.year.Date-E.ID$Hire.Date)/365 + years.to.m+E.ID$J.Pre.Seniority,2),0),  # calculate total jala seniority
                      Tot.Seniority = ifelse(is.na(E.ID$Hire.End.Date)|E.ID$Hire.End.Date>=Month.Start,round(E.ID$Ex.Seniority+as.numeric(Jan.01.this.year.Date-E.ID$Hire.Date)/365 + years.to.m+E.ID$J.Pre.Seniority,2),0)) # calculate tot seniority
  
  #m.srt$JS.Pre.Seniority[m.srt$JS.Pre.Seniority<0]<-0.0 # set any # calcualted PreSeniority values < 0 to 0
  
  log_debug('Calc.Month.Seniority: Returning Dataframe with {nrow(m.srt)} rows') 
  return(m.srt)
}

LoadERPData <- function(EID.File.Name,Include.Inactives=F,Include.USM=F,Include.Exceptions=F){
  # Load Employee information from ERP
  log_info('LoadERPData: Loading {EID.File.Name} & Exceptions {EID.Exceptions.File.Name}...')
  
  if (!file.exists(EID.File.Name)){ # check if file exists before reading
    log_fatal('File {EID.File.Name} does NOT exist, aborting in LoadERPData')
    return(NULL)
  }
  
  eid<-read_csv(EID.File.Name) 
  
  if(Include.Exceptions==TRUE){ # only load exceptions if Include.Exceptions == TRUE
    if (!file.exists(EID.Exceptions.File.Name)){ # check if file exists before reading
      log_fatal('File {EID.Exceptions.File.Name} does NOT exist, aborting in LoadERPData')
      return(NULL)
    }
    exceptions <- read_csv(EID.Exceptions.File.Name)
    eid<-rbind(eid,exceptions) # binding exceptions to eid list
  }
  
  # clean up ERP data
  usm.list <- c(1304,1311,1328,1329,1342,1345,1404,1484,1486,1497,1540,1649,1656,1561)
  log_info('LoadERPData: Loaded {nrow(eid)} eid rows... Now cleaning data frame...')
  
  # refactored column suppresion by name
  # keep Xternal ID number
  eid <- within(eid, rm("Division/Display Name", "Is Manager","Shadow Category")) #  Manager, Shadow cat, 
  
  # rename columns  
  
  names(eid)[names(eid) == "External ID"] <- "X_ID"  
  names(eid)[names(eid) == "Internal ID"] <- "ID"  
  names(eid)[names(eid) == "Resource Name"] <- "Name"
  names(eid)[names(eid) == "Active"] <- "Active"
  names(eid)[names(eid) == "Hire Date"] <- "Hire.Date"
  names(eid)[names(eid) == "Hire End Date"] <- "Hire.End.Date"
  names(eid)[names(eid) == "Jala Seniority"] <- "J.Seniority"       # changing names to avoid conflict with payroll
  names(eid)[names(eid) == "External Seniority"] <- "Ex.Seniority"  # changing names to avoid conflict with payroll
  names(eid)[names(eid) == "Salary Review Month"] <- "Aniversary"   # short name
  names(eid)[names(eid) == "Engineering Unit/Unit Name"] <- "Unit"
  names(eid)[names(eid) == "Department/Department Name"] <- "Dept"
  names(eid)[names(eid) == "Job Title/Job Name"] <- "Level"
  names(eid)[names(eid) == "City/Name"] <- "Work.City"
  names(eid)[names(eid) == "Country/Display Name"] <- "Work.Country"
  names(eid)[names(eid) == "Is Lead"] <- "Is.Lead"
  names(eid)[names(eid) == "Is Manager"] <- "Is.Manager"
  names(eid)[names(eid) == "JCE"] <- "JCE"
  names(eid)[names(eid) == "Manager/Display Name"] <- "Manager"  
  names(eid)[names(eid) == "Project Code"] <- "Project"
  names(eid)[names(eid) == "Project Code"] <- "Project"
  names(eid)[names(eid) == "Billing Status"] <- "Billing.Status"
  
  # check and remove duplicates...  
  duplicated.rows <- eid[duplicated(eid$ID),]
  tot_duplicates<-nrow(duplicated.rows)
  if(tot_duplicates>0) {
    log_error('{LoadERPData: tot_duplicates} Duplicates found in LoadERPData')
    log_error('LoadERPData: duplicated: ID: {duplicated.rows$ID}, NAME: {duplicated.rows$Name}, removed!')
    eid <- eid[!duplicated(eid$ID), ]
  }
  
  # parse external ID to remove "__export__.hr_employee_" 
  eid$X_ID <- gsub("[^0-9]", "", eid$X_ID)
  
  # convert to dates 
  eid$Hire.End.Date <- JS_ParseDate(eid$Hire.End.Date,"ERP") # REFACTORING
  eid$Hire.Date <- JS_ParseDate(eid$Hire.Date,"ERP")         # REFACTORING
  
  eid$Ex.Seniority <- round(eid$Ex.Seniority,2) # round to 2 decimal places for clarity
  eid$JCE <- if_else(eid$JCE==TRUE, "JCE", "NOT JCE") # set JCE field to text values
  
  # Remove Rows with inactives when Include.Inactives == False
  if (Include.Inactives == F) {         # if Include.Inactives, do not remove inactives 
    inactives <- eid[eid$Active==F,]    # get inactives list
    eid <- eid[eid$Active==T,]          # Remove rows with Active = False
    removed.rows <- nrow(inactives) # count how many rows removed
    if(removed.rows > 0) { 
      log_info('LoadERPData: Removed {removed.rows} rows with Active = F')
      log_success('LoadERPData: inactive: ID: {inactives$ID}, NAME: {inactives$Name}, removed!')
    }
  }
  
  # Remove Rows of UnitSMs and Staffing when Include.USM == False
  if (Include.USM == F) {         # if Include.USM, do not remove USM
    usm <- eid[eid$ID %in% usm.list,]     # get usm list
    eid <- eid[!eid$ID %in% usm.list,]    # remove usm list from eid
    removed.rows <- nrow(usm) # count how many rows removed
    if(removed.rows > 0) { 
      log_info('LoadERPData: Removed {removed.rows} usm rows')
      log_success('LoadERPData: unit SM: ID: {usm$ID}, NAME: {usm$Name}, removed!')
    }
  }
  
  # Remove Interns
  interns <- eid[grep("Intern", eid$Level),]
  eid <- eid[ grep("Intern", eid$Level, invert = TRUE) , ] # Remove rows with Level <> Intern
  removed.rows <- nrow(interns) # count how many rows removed
  if(removed.rows > 0 ){ 
    log_info('LoadERPData: Removed {removed.rows} rows with Level == Intern')
    log_info('LoadERPData: intern: ID: {interns$ID}, NAME: {interns$Name}, removed!')
  }
  
  # Remove rows with Hire Date == NA
  NoHireDate <- eid[is.na(eid$Hire.Date),]        # rows with Hire.Date = NA
  eid <- eid[!is.na(eid$Hire.Date),]        # Remove rows with Hire.Date = NA
  removed.rows <- nrow(NoHireDate) # count how many rows removed
  if(removed.rows > 0 ){ 
    log_info('LoadERPData: Removed {removed.rows} rows with Hire.Date = NA')
    log_info('LoadERPData: No Hire Date: ID: {NoHireDate$ID}, NAME: {NoHireDate$Name}, removed!')
  }
  
  # temp comment out 30 day removal to test less than 30 day bug
  #   # Remove Rows with less than 30 worked days
  #   lastday <- as.Date("2022-12-31")
  # #  eid$S.Days <- as.integer(difftime(eid$Hire.Date,lastday,units="days")) # add a temp column
  #   eid$S.Days <- as.integer(difftime(lastday,eid$Hire.Date,units="days")) # add a temp column
  #   S.Days.Col.Num <- which( colnames(eid)=="S.Days" )
  #   lessthan30 <- eid[eid$S.Days<30,]  
  #   eid <- eid[eid$S.Days>=30,]             # Remove rows with Seniority < 30 days -> 30/365 
  #   removed.rows <- nrow(lessthan30) # count how many rows removed
  #   if(removed.rows > 0 ){ 
  #     log_warn('LoadERPData: Removed {removed.rows} rows with less than 30 days')
  #     log_success('LoadERPData: Less than 30 days: ID: {lessthan30$ID}, NAME: {lessthan30$Name}, removed!')
  #   }
  #   eid <- eid[, -S.Days.Col.Num] # drop the latest added column
  
  # Convert to factors, this needs to be done here so that there are no foreign factors like "intern"
  # but also there are any factors NOT defined will now show up as NA
  eid$Dept <- ordered(eid$Dept, levels = dept_levels)
  eid$Level <- ordered(eid$Level, levels = eng_levels)
  eid$Aniversary <- ordered(eid$Aniversary, levels = month_levels)
  eid$Unit <- ordered(eid$Unit, levels = unit_levels)
  
  NoLevel <- eid[is.na(eid$Level),]
  eid <- eid[!is.na(eid$Level),]        # Remove rows with Level = NA (for those NOT defined in eng_levels)
  removed.rows <- nrow(NoLevel) # count how many rows removed
  if(removed.rows > 0 ){ 
    log_info('LoadERPData: Removed {removed.rows} rows with Level = NA')
    log_success('LoadERPData: No Level: ID: {NoLevel$ID}, NAME: {NoLevel$Name}, removed!')
  }
  
  # add calculated Jalasoft seniority former to current contract
  # this is calculated as JS.Seniority - Labor law seniority with respect to the date of file.creation.date
  # there is a bit of an error here: when calculating JS.Pre.Seniority some times it comes out as -0.01
  # this error is about 3-4 days...  seems like it is a rounding error only
  
  log_debug('LoadERPData: Calculating JS.Pre.Seniority...')
  eid$J.Pre.Seniority <- eid$J.Seniority - round(as.numeric(as.Date(file.info(EID.File.Name)$ctime)-eid$Hire.Date)/365,2)
  eid$J.Pre.Seniority[eid$J.Pre.Seniority<0]<-0.0 # set any # calcualted PreSeniority values < 0 to 0
  eid <- relocate(eid,J.Pre.Seniority, .before = Ex.Seniority)      # relocate LL.Seniority column
  
  # # sort by ID 
  eid <- eid[order(eid$ID),]
  
  log_debug('LoadERPData: Returning eid with {nrow(eid)} rows')
  return(eid)
}

PR.Version <- function(){
  # Updated version number so that it matches Dashboard version
  return("Version 3.12.1 - Aug.2023") # Return Script version 
}

# shiny test functions
get_sel_month_df <- function(Sel_Month,Payroll){
  
  # here the easy way is to partially match column names with the selected month but I assume that doing this makes the
  # code run much slower.... so therefore, we will use the same approach as the former version: by col number
  # here we could do some error checking but it is not worth it because the functionw will NOT be called with incorrect 
  # data  
  
  return(switch(Sel_Month,   # select the columns for the "current" month (side bar)
                "January"  = select(Payroll,1:5,9:14,16:19,  23: 49),
                "February" = select(Payroll,1:5,9:14,16:19,  53: 79),
                "March"    = select(Payroll,1:5,9:14,16:19,  83:109),
                "April"    = select(Payroll,1:5,9:14,16:19, 113:139),
                "May"      = select(Payroll,1:5,9:14,16:19, 143:169),
                "June"     = select(Payroll,1:5,9:14,16:19, 173:199),
                "July"     = select(Payroll,1:5,9:14,16:19, 203:229),
                "August"   = select(Payroll,1:5,9:14,16:19, 233:259),
                "September"= select(Payroll,1:5,9:14,16:19, 263:289),
                "October"  = select(Payroll,1:5,9:14,16:19, 293:319),
                "November" = select(Payroll,1:5,9:14,16:19, 323:349),
                "December" = select(Payroll,1:5,9:14,16:19, 353:379)) %>% 
           
           # rename columns of the selected month 
           setNames(., c("ID",	"Name",	"Active",	"Hire.Date", "Hire.End.Date",	"Aniversary",	"Unit",	"Dept",	"Level",	"Work.City",	
                         "Is.Lead",	"JCE", "Project",	"Billing.Status",	"Client", # until here the first part of the dataframe         
                         "Tot.Seniority",	"WDays",	"HB",	"Inc.HB",	"Inc.360",	"Inc.Prom",	"Retroactive",	"BJP",	"BA",	
                         "B.Aliment",	"B.Lead",	"B.BSU",	"B.ENG",	"B.Client",	"B.City",	"EST.WW",	"B.Other",	"TG",	"PI",	"PA1",	
                         "PA2",	"AP",	"Basic",	"RiskBonus",	"TotalComp",	"NetPay",	"TotalCost")) %>%
           
           # relocate some columns for visibility
           
           #relocate(Client, .after = Full) %>%
           relocate(Billing.Status, .after = JCE))
}      

##### graphic & reporting functions

run_comp <- function(){
  # this function will compare 2021 2022 and 2023 hipotetical
  eng.2023 <- read.csv("analysis/Eng.2023.Dec.csv",check.names = F,header=T,sep=",",dec=".",fill=T)
  eng.2022 <- read.csv("analysis/Eng.2022.Oct.csv",check.names = F,header=T,sep=",",dec=".",fill=T)
  eng.2021 <- LoadPayrollData2021("analysis/BolEng.Bgt.2021.csv",Include.Inactives=F,eng.2022[,1:2])
  
  #Summarize.Month.Payroll(eng.2022)
  #show_salary_bands2023(eng.2021,2021)
  #show_salary_bands(eng.2022,2022)
  #show_salary_bands2023(eng.2023,2023)
  
  x<-compare_salary_bands(eng.2021,eng.2022,eng.2023,"Automation")
  return(x)
  compare_salary_bands(eng.2021,eng.2022,eng.2023,"DEV")
  compare_salary_bands(eng.2021,eng.2022,eng.2023,"DevOps")
  compare_salary_bands(eng.2021,eng.2022,eng.2023,"INFO DEV")
  compare_salary_bands(eng.2021,eng.2022,eng.2023,"QE")
  # compare_salary_bands(eng.2021,eng.2022,eng.2023,"Management")
  
}

compare_salary_bands <- function(Eng.2021,Eng.2022,Eng.2023,Sel.Dept){
  DataTitle<-paste("Comparison 2022-2023 for ",Sel.Dept)
  
  #subset 2021 by department and arrange Level Factors accordingly
  sel.21 <- subset(Eng.2021,Dept==Sel.Dept)
  sel.21$Level <- ordered(sel.21$Level, rev_eng_levels)  
  #convert to usd
  sel.21$Basic <- sel.21$Basic / USD
  sel.21$TotalComp <- sel.21$TotalComp / USD
  sel.21$NetPay <- sel.21$NetPay / USD
  sel.21$Percent <- NA#rnorm(nrow(sel.21), mean=2, sd=1)
  
  #subset 2022 by department and arrange Level Factors accordingly
  sel.22 <- subset(Eng.2022,Dept==Sel.Dept)
  sel.22$Level <- ordered(sel.22$Level, rev_eng_levels)  
  #convert to usd
  sel.22$Basic <- sel.22$Basic / USD
  sel.22$TotalComp <- sel.22$TotalComp / USD
  sel.22$NetPay <- sel.22$NetPay / USD
  
  sel.22<-left_join(sel.22,select(sel.21,ID,TotalComp),by= "ID")
  names(sel.22)[names(sel.22) == "TotalComp.x"] <- "TotalComp" # rename name col
  names(sel.22)[names(sel.22) == "TotalComp.y"] <- "TC.2021" # rename name col
  sel.22$Percent <- (sel.22$TotalComp/sel.22$TC.2021 - 1)*100
  return(sel.22)
  
  #subset 2023 by department and arrange Level Factors accordingly
  sel.23 <- subset(Eng.2023,Dept==Sel.Dept)
  sel.23$Level <- ordered(sel.23$Level, rev_eng_levels)  
  #convert to usd
  sel.23$Basic <- sel.23$Basic / USD
  sel.23$TotalComp <- sel.23$TotalComp / USD
  sel.23$NetPay <- sel.23$NetPay / USD
  
  sel.23<-left_join(sel.23,select(sel.22,ID,TotalComp),by= "ID")
  names(sel.23)[names(sel.23) == "TotalComp.x"] <- "TotalComp" # rename name col
  names(sel.23)[names(sel.23) == "TotalComp.y"] <- "TC.2022" # rename name col
  sel.23$Percent <- (sel.23$TotalComp/sel.23$TC.2022 - 1)*100
  
  sel.22.Plt <- BoxPlotColumn(sel.22,"2022 Plot")
  sel.23.Plt <- BoxPlotColumn(sel.23,"2023 Plot")
  sel.22.Per.Plt <- BoxPlotColumnPercent(sel.22,"2022 Plot")
  sel.23.Per.Plt <- BoxPlotColumnPercent(sel.23,"2023 Plot")
  
  fullfig <- subplot(sel.22.Plt,sel.23.Plt,sel.22.Per.Plt,sel.23.Per.Plt,nrows=2) %>% layout(title = DataTitle,showlegend = F)
  print(fullfig)
}

Summarize.Month.Payroll <- function(Eng.Payroll,currency){
  DataTitle<-"All Engineering"
  
  print(paste("March 2023"))
  print(paste("Reporting for: ",DataTitle))
  print(paste("Total Engineers: ",count(Eng.Payroll)))
  
  #subset by department and arrange Level Factors accordingly
  Auto <- subset(Eng.Payroll,Dept=="Automation")
  Auto$Level <- ordered(Auto$Level, levels = c("Junior","Staff","Senior"))  
  Dev <- subset(Eng.Payroll,Dept=="DEV")  
  Dev$Level <- ordered(Dev$Level, levels = c("Junior","Staff","Senior","Junior Architect"))  
  DevOps <- subset(Eng.Payroll,Dept=="DevOps")
  DevOps$Level <- ordered(DevOps$Level, levels = c("Junior","Staff","Senior"))  
  InfoDev <- subset(Eng.Payroll,Dept=="INFO DEV")
  InfoDev$Level <- ordered(InfoDev$Level, levels = c("Junior","Staff","Project Administrator"))  
  QE <- subset(Eng.Payroll,Dept=="QE")
  QE$Level <- ordered(QE$Level, levels = c("Junior","Staff","Senior"))  
  Mgt <- subset(Eng.Payroll,Dept=="Management")
  Mgt$Level <- ordered(Mgt$Level, levels = c("Junior Core Manager","Core Manager"))  
  
  print(paste("Reporting for: Automation: ",count(Auto)))
  print(paste("Reporting for: Dev: ",count(Dev)))
  print(paste("Reporting for: InfoDev: ",count(InfoDev)))
  print(paste("Reporting for: Management: ",count(Mgt)))
  print(paste("Reporting for: QE: ",count(QE)))
  print(paste("Reporting for: DevOps: ",count(DevOps)))
  
  par(mfrow=c(4,6))
  # auto.stats<-boxplot(Basic~Level, data=Auto,horizontal = F, ylim = c(500, 4000),main="Automation Basic")
  # dev.stats<-boxplot(Basic~Level, data=Dev,horizontal = F, ylim = c(500, 4000),main="Development Basic")
  # devops.stats<-boxplot(Basic~Level, data=DevOps,horizontal = F, ylim = c(500, 4000),main="DevOps Basic")
  # infodev.stats<-boxplot(Basic~Level, data=InfoDev,horizontal = F, ylim = c(500, 4000),main="InfoDev Basic")
  # mgt.stats<-boxplot(Basic~Level, data=Mgt,horizontal = F, ylim = c(500, 4000),main="Management Basic")
  # qe.stats<-boxplot(Basic~Level, data=QE,horizontal = F, ylim = c(500, 4000),main="QE Basic")
  # 
  # auto.stats.TG <- boxplot(TotalComp~Level, data=Auto,horizontal = F, ylim = c(500, 6500), main="Automation Total")
  # dev.stats.TG <- boxplot(TotalComp~Level, data=Dev,horizontal = F,ylim = c(500, 6500), main="Development Total")
  # devops.stats.TG <- boxplot(TotalComp~Level, data=DevOps,horizontal = F,ylim = c(500, 6500), main="DevOps Total")
  # infodev.stats.TG <- boxplot(TotalComp~Level, data=InfoDev,horizontal = F,ylim = c(500, 6500), main="InfoDev Total")
  # mgt.stats.TG <- boxplot(TotalComp~Level, data=Mgt,horizontal = F,ylim = c(500, 6500), main="Management Total")
  # qe.stats.TG <- boxplot(TotalComp~Level, data=QE,horizontal = F,ylim = c(500, 6500), main="QE Total")
  
  auto.stats.TG <- boxplot(TotalComp~Level, data=Auto,horizontal = F, main="Automation Total",ylab=currency)
  dev.stats.TG <- boxplot(TotalComp~Level, data=Dev,horizontal = F, main="Development Total",ylab=currency)
  devops.stats.TG <- boxplot(TotalComp~Level, data=DevOps,horizontal = F, main="DevOps Total",ylab=currency)
  infodev.stats.TG <- boxplot(TotalComp~Level, data=InfoDev,horizontal = F, main="InfoDev Total",ylab=currency)
  mgt.stats.TG <- boxplot(TotalComp~Level, data=Mgt,horizontal = F, main="Management Total",ylab=currency)
  qe.stats.TG <- boxplot(TotalComp~Level, data=QE,horizontal = F, main="QE Total",ylab=currency)
  
  auto.stats.TG <- boxplot(TotalComp~Level, data=Auto,horizontal = F, main="Automation Total",ylab=currency)
  dev.stats.TG <- boxplot(TotalComp~Level, data=Dev,horizontal = F, main="Development Total",ylab=currency)
  devops.stats.TG <- boxplot(TotalComp~Level, data=DevOps,horizontal = F, main="DevOps Total",ylab=currency)
  infodev.stats.TG <- boxplot(TotalComp~Level, data=InfoDev,horizontal = F, main="InfoDev Total",ylab=currency)
  mgt.stats.TG <- boxplot(TotalComp~Level, data=Mgt,horizontal = F, main="Management Total",ylab=currency)
  qe.stats.TG <- boxplot(TotalComp~Level, data=QE,horizontal = F, main="QE Total",ylab=currency)
  
  hist(Auto$Seniority,ylim = c(0, 40),xlim=c(0,30),breaks=20,main="Automation Seniority Hist")
  hist(Dev$Seniority,ylim = c(0, 80),xlim=c(0,30),breaks=20,main="Dev Seniority Hist")
  hist(DevOps$Seniority,ylim = c(0, 80),xlim=c(0,30),breaks=20,main="DevOps Seniority Hist")
  hist(InfoDev$Seniority,ylim = c(0, 6),xlim=c(0,8),breaks=20,main="InfoDev Seniority Hist")
  hist(Mgt$Seniority,ylim = c(0, 20),xlim=c(0,30),breaks=20,main="Mgt Seniority Hist")
  hist(QE$Seniority,ylim = c(0, 90),xlim=c(0,30),breaks=20,main="QE Seniority Hist")
  
  plot(density(Auto$Seniority,na.rm = T),main="Automation Density")
  plot(density(Dev$Seniority,na.rm = T),main="Dev Density")
  plot(density(DevOps$Seniority,na.rm = T),main="DevOps Density")
  plot(density(InfoDev$Seniority,na.rm = T),main="InfoDev Density")
  plot(density(Mgt$Seniority,na.rm = T),main="Mgt Density")
  plot(density(QE$Seniority,na.rm = T),main="QE Density")
}

show_salary_bands2023 <- function(EData,Current.Year){
  
  DataTitle<-"All Engineering"
  
  print(paste("December ",Current.Year))
  print(paste("Reporting for: ",DataTitle))
  print(paste("Total Engineers: ",count(EData)))
  
  #convert to usd
  EData$Basic <- EData$Basic / USD
  EData$Risk <- EData$Risk / USD
  EData$TotalComp <- EData$TotalComp / USD
  EData$NetPay <- EData$NetPay / USD
  
  #subset by department and arrange Level Factors accordingly
  Auto <- subset(EData,Dept=="Automation")
  Auto$Level <- ordered(Auto$Level, levels = c("Senior","Staff","Junior"))  
  Dev <- subset(EData,Dept=="DEV")  
  Dev$Level <- ordered(Dev$Level, levels = c("Junior Architect","Senior","Staff","Junior"))  
  DevOps <- subset(EData,Dept=="DevOps")
  DevOps$Level <- ordered(DevOps$Level, levels = c("Senior","Staff","Junior"))  
  InfoDev <- subset(EData,Dept=="INFO DEV")
  InfoDev$Level <- ordered(InfoDev$Level, levels = c("Project Administrator","Staff","Junior"))  
  QE <- subset(EData,Dept=="QE")
  QE$Level <- ordered(QE$Level, levels = c("Senior","Staff","Junior"))  
  Mgt <- subset(EData,Dept=="Management")
  Mgt$Level <- ordered(Mgt$Level, levels = c("Core Manager","Junior Core Manager"))  
  
  print(paste("Reporting for: Automation"))
  print(paste("Total Engineers: ",count(Auto)))
  print(paste("Reporting for: Dev"))
  print(paste("Total Engineers: ",count(Dev)))
  print(paste("Reporting for: InfoDev"))
  print(paste("Total Engineers: ",count(InfoDev)))
  print(paste("Reporting for: Management"))
  print(paste("Total Engineers: ",count(Mgt)))
  print(paste("Reporting for: QE"))
  print(paste("Total Engineers: ",count(QE)))
  print(paste("Reporting for: DevOps"))
  print(paste("Total Engineers: ",count(DevOps)))
  
  # Generate all BoxPlots
  BoxPlotFigs<-GenerateAllBoxPlots(Auto,Dev,DevOps,InfoDev,QE,Mgt)
  print(BoxPlotFigs)
  
  #HistPlotFigs<-GenerateAllHistPlots(Auto,Dev,DevOps,InfoDev,QE,Mgt)
  #print(HistPlotFigs)
  
  return()
  
  hist(Auto$Tot.Seniority.Dec,ylim = c(0, 40),xlim=c(0,30),breaks=20)
  hist(Dev$Tot.Seniority.Dec,ylim = c(0, 80),xlim=c(0,30),breaks=20)
  hist(DevOps$Tot.Seniority.Dec,ylim = c(0, 80),xlim=c(0,30),breaks=20)
  hist(InfoDev$Tot.Seniority.Dec,ylim = c(0, 6),xlim=c(0,8),breaks=20)
  hist(Mgt$Tot.Seniority.Dec,ylim = c(0, 20),xlim=c(0,30),breaks=20)
  hist(QE$Tot.Seniority.Dec,ylim = c(0, 90),xlim=c(0,30),breaks=20)
  
  
  plot(density(Auto$Tot.Seniority.Dec,na.rm = T))
  plot(density(Dev$Tot.Seniority.Dec,na.rm = T))
  plot(density(DevOps$Tot.Seniority.Dec,na.rm = T))
  plot(density(InfoDev$Tot.Seniority.Dec,na.rm = T))
  plot(density(Mgt$Tot.Seniority.Dec,na.rm = T))
  plot(density(QE$Tot.Seniority.Dec,na.rm = T))
  
}

show_salary_bands <- function(EData,Current.Year,currency){
  
  DataTitle<-"All Engineering"
  
  print(paste("December ",Current.Year))
  print(paste("Reporting for: ",DataTitle))
  print(paste("Total Engineers: ",count(EData)))
  
  #subset by department and arrange Level Factors accordingly
  Auto <- subset(EData,Dept=="Automation")
  Auto$Level <- ordered(Auto$Level, levels = c("Senior","Staff","Junior"))  
  Dev <- subset(EData,Dept=="DEV")  
  Dev$Level <- ordered(Dev$Level, levels = c("Junior Architect","Senior","Staff","Junior"))  
  DevOps <- subset(EData,Dept=="DevOps")
  DevOps$Level <- ordered(DevOps$Level, levels = c("Senior","Staff","Junior"))  
  InfoDev <- subset(EData,Dept=="INFO DEV")
  InfoDev$Level <- ordered(InfoDev$Level, levels = c("Project Administrator","Staff","Junior"))  
  QE <- subset(EData,Dept=="QE")
  QE$Level <- ordered(QE$Level, levels = c("Senior","Staff","Junior"))  
  Mgt <- subset(EData,Dept=="Management")
  Mgt$Level <- ordered(Mgt$Level, levels = c("Core Manager","Junior Core Manager"))  
  
  print(paste("Reporting for: Automation"))
  print(paste("Total Engineers: ",count(Auto)))
  print(paste("Reporting for: Dev"))
  print(paste("Total Engineers: ",count(Dev)))
  print(paste("Reporting for: InfoDev"))
  print(paste("Total Engineers: ",count(InfoDev)))
  print(paste("Reporting for: Management"))
  print(paste("Total Engineers: ",count(Mgt)))
  print(paste("Reporting for: QE"))
  print(paste("Total Engineers: ",count(QE)))
  print(paste("Reporting for: DevOps"))
  print(paste("Total Engineers: ",count(DevOps)))
  
  # Generate all BoxPlots
  BoxPlotFigs<-GenerateAllBoxPlots(Auto,Dev,DevOps,InfoDev,QE,Mgt,currency)
  print(BoxPlotFigs)
  
  #HistPlotFigs<-GenerateAllHistPlots(Auto,Dev,DevOps,InfoDev,QE,Mgt)
  #print(HistPlotFigs)
  
  return()
  
  hist(Auto$Tot.Seniority.Dec,ylim = c(0, 40),xlim=c(0,30),breaks=20)
  hist(Dev$Tot.Seniority.Dec,ylim = c(0, 80),xlim=c(0,30),breaks=20)
  hist(DevOps$Tot.Seniority.Dec,ylim = c(0, 80),xlim=c(0,30),breaks=20)
  hist(InfoDev$Tot.Seniority.Dec,ylim = c(0, 6),xlim=c(0,8),breaks=20)
  hist(Mgt$Tot.Seniority.Dec,ylim = c(0, 20),xlim=c(0,30),breaks=20)
  hist(QE$Tot.Seniority.Dec,ylim = c(0, 90),xlim=c(0,30),breaks=20)
  
  
  plot(density(Auto$Tot.Seniority.Dec,na.rm = T))
  plot(density(Dev$Tot.Seniority.Dec,na.rm = T))
  plot(density(DevOps$Tot.Seniority.Dec,na.rm = T))
  plot(density(InfoDev$Tot.Seniority.Dec,na.rm = T))
  plot(density(Mgt$Tot.Seniority.Dec,na.rm = T))
  plot(density(QE$Tot.Seniority.Dec,na.rm = T))
  
}

show_salary_curves <- function(EData,Current.Year,currency){
  
  DataTitle<-"All Engineering"
  
  print(paste("December ",Current.Year))
  print(paste("Reporting for: ",DataTitle))
  print(paste("Total Engineers: ",count(EData)))
  
  #subset by department and arrange Level Factors accordingly
  Auto <- subset(EData,Dept=="Automation")
  Auto$Level <- ordered(Auto$Level, levels = c("Senior","Staff","Junior"))  
  Dev <- subset(EData,Dept=="DEV")  
  Dev$Level <- ordered(Dev$Level, levels = c("Junior Architect","Senior","Staff","Junior"))  
  DevOps <- subset(EData,Dept=="DevOps")
  DevOps$Level <- ordered(DevOps$Level, levels = c("Senior","Staff","Junior"))  
  InfoDev <- subset(EData,Dept=="INFO DEV")
  InfoDev$Level <- ordered(InfoDev$Level, levels = c("Project Administrator","Staff","Junior"))  
  QE <- subset(EData,Dept=="QE")
  QE$Level <- ordered(QE$Level, levels = c("Senior","Staff","Junior"))  
  Mgt <- subset(EData,Dept=="Management")
  Mgt$Level <- ordered(Mgt$Level, levels = c("Core Manager","Junior Core Manager"))  
  
  print(paste("Reporting for: Automation"))
  print(paste("Total Engineers: ",count(Auto)))
  print(paste("Reporting for: Dev"))
  print(paste("Total Engineers: ",count(Dev)))
  print(paste("Reporting for: InfoDev"))
  print(paste("Total Engineers: ",count(InfoDev)))
  print(paste("Reporting for: Management"))
  print(paste("Total Engineers: ",count(Mgt)))
  print(paste("Reporting for: QE"))
  print(paste("Total Engineers: ",count(QE)))
  print(paste("Reporting for: DevOps"))
  print(paste("Total Engineers: ",count(DevOps)))
  
  # Generate all Curves
  CurveFigs<-GenerateAllCurves(Auto,Dev,DevOps,InfoDev,QE,Mgt,currency)
  #print(CurveFigs)
}

GenerateAllHistPlots <- function(Auto,Dev,DevOps,InfoDev,QE,Mgt){  
  # generate histograms for all levels
  
  Auto.Plt <- HistPlot(Auto,auto_eng_levels,"Automation Plot")
  Dev.Plt <- HistPlot(Dev,DEV_eng_levels,"Development Plot")
  DevOps.Plt <- HistPlot(DevOps,DevOps_eng_levels,"DevOps Plot")
  QE.Plt <- HistPlot(QE,QE_eng_levels,"QE Plot")
  InfoDev.Plt <- HistPlot(InfoDev,INFO_DEV_eng_levels,"InfoDev Plot")
  Mgt.Plt <- HistPlot(Mgt,Mgt_eng_levels,"Management Plot")
  
  fullfig <- subplot(Auto.Plt, Dev.Plt,DevOps.Plt,QE.Plt,InfoDev.Plt,Mgt.Plt,
                     nrows=2) %>% layout(title = '',showlegend = F)
  annotations =
    list(list(x = 0.15, y = 1.0, text = "Automation",
              xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.5, y = 1,text = "Development",
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.8, y = 1, text = "DevOps",
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.15,y = .45,text = "QE",
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.5, y = .45, text = "InfoDev",
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.8,y = .45,text = "Management",
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE))
  
  fullfig <- fullfig %>%layout(annotations = annotations)
  return(fullfig)
}

GenerateAllBoxPlots <- function(Auto,Dev,DevOps,InfoDev,QE,Mgt,currency){  
  # generate all BoxPlots for all levels
  Auto.Plt <- BoxPlotColumn(Auto, "Automation Plot")
  Dev.Plt <- BoxPlotColumn(Dev, "Development Plot")
  DevOps.Plt <- BoxPlotColumn(DevOps, "DevOps Plot")
  QE.Plt <- BoxPlotColumn(QE, "QE Plot")
  InfoDev.Plt <- BoxPlotColumn(InfoDev, "InfoDev Plot")
  Mgt.Plt <- BoxPlotColumn(Mgt, "Management Plot")
  
  fullfig <- subplot(Auto.Plt, Dev.Plt,DevOps.Plt,QE.Plt,InfoDev.Plt,Mgt.Plt,
                     nrows=2) %>% layout(title = '',showlegend = F)
  annotations =
    list(list(x = 0.15, y = 1.0, text = paste("Automation ",currency),
              xref = "paper", yref = "paper", xanchor = "center", yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.5, y = 1,text = paste("Development ",currency),
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.8, y = 1, text = paste("DevOps ",currency),
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.15,y = .45,text = paste("QE ",currency),
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.5, y = .45, text = paste("InfoDev ",currency),
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE),
         list(x = 0.8,y = .45,text = paste("Management",currency),
              xref = "paper",yref = "paper",xanchor = "center",yanchor = "bottom",
              showarrow = FALSE))
  
  fullfig <- fullfig %>%layout(annotations = annotations)
  return(fullfig)
}

GenerateAllCurves <- function(Auto,Dev,DevOps,InfoDev,QE,Mgt,currency){  
  # generate all curves for all levels
  
  # automation curves
  auto_curves <-  Create_New_Salary_Bands("Automation")
  auto.jr <- CurvePlotColumn(Auto %>% filter(Level %in% "Junior"),
                             auto_curves %>% filter(Level %in% "Junior"),
                             "TotalComp",500,1500,currency,"Automation Junior Plot")
  auto.staff <- CurvePlotColumn(Auto %>% filter(Level %in% "Staff"),
                                auto_curves %>% filter(Level %in% "Staff"),
                                "TotalComp",1000,3000,currency,"Automation Staff Plot")
  auto.sr <- CurvePlotColumn(Auto %>% filter(Level %in% "Senior"),
                             auto_curves %>% filter(Level %in% "Senior"),
                             "TotalComp",2000,5000,currency,"Automation Senior Plot")
  
  # development curves
  dev_curves <-  Create_New_Salary_Bands("DEV")
  dev.jr <- CurvePlotColumn(Dev %>% filter(Level %in% "Junior"),
                            dev_curves %>% filter(Level %in% "Junior"),
                            "TotalComp",500,1500,currency,"Dev Junior Plot")
  dev.staff <- CurvePlotColumn(Dev %>% filter(Level %in% "Staff"),
                               dev_curves %>% filter(Level %in% "Staff"),
                               "TotalComp",1000,3000,currency,"Dev Staff Plot")
  dev.sr <- CurvePlotColumn(Dev %>% filter(Level %in% "Senior"),
                            dev_curves %>% filter(Level %in% "Senior"),
                            "TotalComp",2000,5000,currency,"Dev Senior Plot")
  
  # QE curves
  qe_curves <-  Create_New_Salary_Bands("QE")
  qe.jr <- CurvePlotColumn(QE %>% filter(Level %in% "Junior"),
                           qe_curves %>% filter(Level %in% "Junior"),
                           "TotalComp",500,1500,currency,"QE Junior Plot")
  qe.staff <- CurvePlotColumn(QE %>% filter(Level %in% "Staff"),
                              qe_curves %>% filter(Level %in% "Staff"),
                              "TotalComp",1000,3000,currency,"QE Staff Plot")
  qe.sr <- CurvePlotColumn(QE %>% filter(Level %in% "Senior"),
                           qe_curves %>% filter(Level %in% "Senior"),
                           "TotalComp",2000,5000,currency,"QE Senior Plot")
  
  # DevOps.Plt <- CurvePlotColumn(DevOps,"DevOps Plot")
  # InfoDev.Plt <- CurvePlotColumn(InfoDev,"InfoDev Plot")
  # Mgt.Plt <- CurvePlotColumn(Mgt,"Management Plot")
  
  #print(auto.jr)
  #print(dev.jr)
  #print(qe.jr)
  #print(dev.staff)
  #print(qe.staff)
  #print(auto.staff)
  print(dev.sr)
  print(qe.sr)
  print(auto.sr)
  
  return(NULL)
  # fullfig <- subplot(dev.jr,qe.jr,auto.jr,
  #                    dev.staff,qe.staff,auto.staff,
  #                    dev.sr,qe.sr,auto.sr,
  #                    nrows=3) %>% layout(title = '',showlegend = F)
  # 
  # return(fullfig)
}

BoxPlotColumnPercent <- function(Dept,PlotTitle){
  # box and whiskers plot for Dept
  # here we should have a case statement to select which column to plot
  fig <- plot_ly(y=Dept$Percent,color=Dept$Level,type = "box")
  fig <- fig %>% layout(title = PlotTitle,  yaxis = list(range=list(0,50)))
  
  return(fig)
}

BoxPlotColumn <- function(Dept,PlotTitle){
  # box and whiskers plot for Dept
  # here we should have a case statement to select which column to plot
  fig <- plot_ly(y=Dept$TotalComp,color=Dept$Level,type = "box")
  fig <- fig %>% layout(title = PlotTitle,  yaxis = list(range=list(0,7500)))
  
  return(fig)
}

Create_New_Salary_Bands <- function(Dept,currency){
  # these are the proposed salary bands 2ND CUTOFF
  # currently being calculated in usd... currency should be considered afterwards
  if(Dept=="Automation"){
    junior <- data.frame(Dept="Automation",Level="Junior",Seniority=0,Full=0,Ref=0) # No data
    Seniority<-seq(3,19,0.1)
    staff  <- data.frame(Dept="Automation",Level="Staff",Seniority,
                         Full= (487.36*log(Seniority)+1023.5),
                         Ref = (487.36*log(Seniority)+1023.5) - (405.40*log(Seniority)+847.33))
    Seniority<-seq(6,15,0.1)
    senior <- data.frame(Dept="Automation",Level="Senior",Seniority,
                         Full= (695.13*log(Seniority)+1030.5),
                         Ref = (695.13*log(Seniority)+1030.5) - (348.02*log(Seniority)+1632.0))
  } else if (Dept=="DEV") {
    junior <- data.frame(Dept="DEV",Level="Junior",Seniority=0,Full=0,Ref=0) # No data
    Seniority<-seq(2,21,0.1)
    staff  <- data.frame(Dept="DEV",Level="Staff",Seniority,
                         Full= (568.19*log(Seniority)+1144.2),
                         Ref = (568.19*log(Seniority)+1144.2) - (495.45*log(Seniority)+802.25))
    Seniority<-seq(6,26,0.1)
    senior <- data.frame(Dept="DEV",Level="Senior",Seniority,
                         Full= (1703.6*log(Seniority)-165.48),
                         Ref = (1703.6*log(Seniority)-165.48) - (1100.3*log(Seniority)+497.40))
  } else if (Dept=="QE") {
    junior <- data.frame(Dept="QE",Level="Junior",Seniority=0,Full=0,Ref=0) # No data
    Seniority<-seq(2,19,0.1)
    staff  <- data.frame(Dept="QE",Level="Staff",Seniority,
                         Full= (304.42*log(Seniority)+1209.6),
                         Ref = (304.42*log(Seniority)+1209.6) - (403.15*log(Seniority)+709.74))
    Seniority<-seq(10,18,0.1)
    senior <- data.frame(Dept="QE",Level="Senior",Seniority,
                         Full= (1965.6*log(Seniority)-2837.8),
                         Ref = (1965.6*log(Seniority)-2837.8) - (1744.6*log(Seniority)-2371.3))
  } else if(Dept=="Management") {
    # these 0 value curves where missing and causing a bug with Mgt
    # all dept and levels must have entries here even if the entries are 0
    junior <- data.frame(Dept="Management",Level="Junior Core Manager",Seniority=0,Full=0,Ref=0) # No data
    staff <- data.frame(Dept="Management",Level="Core Manager",Seniority=0,Full=0,Ref=0) # No data
    senior <- data.frame(Dept="Management",Level="Senior Core Manager",Seniority=0,Full=0,Ref=0) # No data
  } else if(Dept=="DevOps"){    
    # same as above
    junior <- data.frame(Dept="DevOps",Level="Junior",Seniority=0,Full=0,Ref=0) # No data
    staff <- data.frame(Dept="DevOps",Level="Staff",Seniority=0,Full=0,Ref=0) # No data
    senior <- data.frame(Dept="DevOps",Level="Senior",Seniority=0,Full=0,Ref=0) # No data
  }
  
  df<-rbind(junior,staff,senior) # merge all together
  return(df)
}

CurvePlotColumn <- function(eng_dpt_level,trgt_curves,input_curve="TotalComp",ymin,ymax,input_currency,title="Staff Dev"){
  
  plt.data <- eng_dpt_level %>% mutate(PlotCol = if(input_curve == "Basic")
  {Basic} else 
  {TotalComp})
  
  fig<-ggplot(plt.data) +
    
    # this is the New 2ND CUTOFF Curve
    #geom_line(data=trgt_curves,aes(x=Seniority,y=Full),color="deeppink3",size=1,se=F,alpha = 0.5) +
    
    ggtitle(title)  + # Title according to selection
    xlab("") + ylab(input_currency) + # set correct currency
    scale_x_continuous(breaks=seq(1,30,1)) +
    #scale_y_continuous(limits=c(ymin,ymax)) +
    geom_point(aes(x=Seniority,y=PlotCol,label=ID),alpha = 0.5) +
    geom_smooth(aes(x=Seniority,y=PlotCol),method="lm",formula=y~log(x)) #+
  
  return(plotly::ggplotly(fig))  
}