#Rscript for bookdown
#packages
packages<-c("here","tidyverse","ggthemr","readxl","tseries","modelsummary","rtf","glue",
            "janitor","broom","devtools","dint","lubridate","ggthemes","patchwork","ggtext",
            "xts","tidyquant","timetk","lmtest","car","performance","forecast","nlme",
            "grid","gridExtra","cowplot","kableExtra","flextable")
#sapply(packages,install.packages,character.only=T)
sapply(packages,library,character.only=T)


#Load custom functions
source("functions_itsa.R")
source_url("https://raw.githubusercontent.com/nithinmkp/R-Talk/main/unitroots.R")


#read data
df<-read_excel("data.xlsx")

#some data cleaning to make the data in standard format
varnames<-c(df[4,1],df[2,-1])
df<-df[-c(1:4),]
colnames(df)<-varnames
df$Year<-excel_numeric_to_date(as.numeric(as.character(df$Year)), 
                               date_system = "modern")  

#Make all columns except the date column as numeric
df[,-1]<-map_if(df[,-1],is.character,as.numeric)

nvar<-df %>% dplyr::select(-Year) %>% names() %>% length()

#filter if necessary
df<-df %>% filter(Year>=as.Date("2018-01-01"))

#create a time index
df$time<-seq_along(df$Year) 

#input intervention date
break_date<-as.Date("2020-04-01")
break_index<-df$time[which(df$Year==break_date)]
period_end_index<-nrow(df)

#create level and trend variable
df<-df %>% mutate(level=if_else(Year>=break_date,1,0),
                  trend=if_else(Year<break_date,0,(row_number(Year)-break_index)+1))



#pivot data for basic plots
df_long<- df %>% dplyr::select(-c(time,level,trend)) %>% 
        pivot_longer(-Year,names_to = "Series",values_to = "Values")

#Getting variables for testing for unitroot
varlist<-df %>% dplyr::select(-c(Year,time,level,trend)) #Variables data
vars<-names(varlist) #Variable names
ncols=length(vars) #number of variables to test for stationarity

#Model-OLS
mod_ols<-varlist %>% map(ols_fn,df) 

#Add predicted values
df_final_list<-add_pred_fn(df,mod_ols,vars)

#creating y axis limits as a lists
lim_vec<-list(c(10000,35000),
              c(15000,50000),
              c(-20000,1000))

#Tables-Publication
gof_map2 <- tribble(
        ~raw,      ~clean,          ~fmt,  ~omit,
        "nobs",      "Observations",     0,  FALSE,
        "r.squared", "$R^2$",               3,  FALSE,
        "adj.r.squared","Adj.$R^2$",3,FALSE,
        "AIC","AIC",3,F
)
gof_map2_html <- tribble(
        ~raw,      ~clean,          ~fmt,  ~omit,
        "nobs",      "Observations",     0,  FALSE,
        "r.squared", "R<sup>2</sup>",               3,  FALSE,
        "adj.r.squared","Adj.R<sup>2</sup>",3,FALSE,
        "AIC","AIC",3,F
)

reg_table<-map_df(mod_ols,tidy,.id = "Variable") 


calc_table<-pmap(list(df_final_list,vars,mod_ols),calc_val_fn,break_index,
                 period_end_index) %>% 
        reduce(bind_rows)  
