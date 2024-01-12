library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(corrplot)
library(tidyverse)
library(car)
library(lmtest)
library(tseries)
  options(dplyr.width = Inf)
 options(scipen = 100)
df_2019 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Dropout Rate by Gender, Level of School Education and Social Category_State Name Karnataka District - All District_19.xlsx",range = "R5C1:R175C8")
df_2020 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Dropout Rate by Gender, Level of School Education and Social Category_State Name Karnataka District - All District_20.xlsx",range = "R5C1:R175C8")
df_2021 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Dropout Rate by Gender, Level of School Education and Social Category_State Name Karnataka District - All District_21.xlsx",range = "R5C1:R175C8")
df_2022 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Dropout Rate by Gender, Level of School Education and Social Category_State Name Karnataka District - All District_22.xlsx",range = "R5C1:R175C8")

df_2019$year <- 2019
df_2020$year <- 2020
df_2021$year <- 2021
df_2022$year <- 2022

df <- rbind(df_2019, df_2020, df_2021, df_2022)
df <- df[!(df$`Social Category` %in% c("General", "SC", "ST", "OBC")), ]
df <- df[ -c(3,4,5,6,7) ]
df = rename(df,Overall_Secondary=Overall...8)
df[df==0] <- NA
df <- na.omit(df)
 

school_2019 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Number of Schools by Availability of Infrastructure and Facilities, School Management and School Category_State Name Karnataka District - All District_19.xlsx",range = "R4C1:R200C41")
school_2020 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Number of Schools by Availability of Infrastructure and Facilities, School Management and School Category_State Name Karnataka District - All District_20.xlsx",range = "R4C1:R193C41")
school_2021 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Number of Schools by Availability of Infrastructure and Facilities, School Management and School Category_State Name Karnataka District - All District_21.xlsx",range = "R4C1:R159C41")
school_2022 <- read_excel("C:/Users/Admin/Desktop/QDAPP CP/Number of Schools by Availability of Infrastructure and Facilities, School Management and School Category_State Name Karnataka District - All District_22.xlsx",range = "R4C1:R158C41")


school_2019$year <- 2019
school_2020$year <- 2020
school_2021$year <- 2021
school_2022$year <- 2022
school_df<-rbind(school_2019,school_2020,school_2021,school_2022)

school_df <- school_df[ -c(2,4) ]




summary(df)
df %>% 
  group_by(year) %>%
  summarise(across(-c(1,2), list(mean = mean, median = median, max = max, min = min)))

final_df<-arrange(df, Location,year)

summary(school_df)
school_df %>% group_by(year) %>%
  summarise(across(-c(1,2,3), list(mean = mean, median = median, max = max, min = min)))
 
# Create the plot
df$year <- factor(df$year)
school_df$year <- factor(school_df$year)

ggplot(df, aes(x=year, y=Overall_Secondary)) + geom_boxplot() + labs(title="Box and whisker plot",x="Year",y="Dropout Rate")


# Print the dataframe
school_combined <- school_df %>%
  group_by(Location,year) %>%
  summarise(across(3:38, ~sum(.x, na.rm = TRUE), .names = "Total_{col}"))

 school_complete <- school_combined[-c(129,137),]

school_complete = rename(school_complete,Total_Schools=`Total_Total No. of Schools`, Total_FuncElectr=`Total_Functional Electricity`,
                         Total_FuncBoysToilet=`Total_Functional Boy's Toilet`,Total_FuncGirlsToilet=`Total_Functional Girl's Toilet`,
                         Total_FuncToilet=`Total_Functional Toilet Facility`,Total_FuncBoysUrinal=`Total_Functional Urinal Boy's`,
                         Total_FuncGirlsUrinal=`Total_Functional Urinal Girl's`,Total_FuncTU=`Total_Functional Toilet and Urinal`,
                         Total_FuncDW=`Total_Functional Drinking Water`,Total_CompAvai=`Total_Computer Available`)

secondary_schooldf <- merge(df, school_complete, by = c("Location", "year"))

 
gap3 <- ggplot(secondary_schooldf, aes(x =Total_Internet, y =Overall_Secondary , color = Location,shape=Location)) + geom_point(aes(frame =year))
ggplotly(gap3)

gap4 <- ggplot(secondary_schooldf, aes(x =Total_FuncTU, y =Overall_Secondary , color = Location,shape=Location)) + geom_point(aes(frame =year))
ggplotly(gap4)

gap5 <- ggplot(secondary_schooldf, aes(x =Total_CompAvai, y =Overall_Secondary , color = Location)) + geom_point(aes(frame =year))
ggplotly(gap5)

 library(plm)

 panel_data <- pdata.frame(secondary_schooldf, index = c("Location","year"))
  
  panel_model <- plm(Overall_Secondary ~  + Total_Playground + Total_FuncElectr +Total_Ramps
                    + Total_FuncBoysToilet + Total_FuncGirlsToilet  + 
                      Total_FuncBoysUrinal + Total_FuncGirlsUrinal + Total_FuncTU + Total_FuncDW
                    + Total_Internet + Total_CompAvai, data = secondary_schooldf, model = "random")
 
 summary(panel_model)
 vif(panel_model)
 
 
 
 m3.ols <- lm(Overall_Secondary ~  + Total_Playground + Total_FuncElectr 
              + 
                Total_FuncTU    + Total_FuncDW
              + Total_Internet + Total_CompAvai, data = design.matrix)
 
 # Calculate VIF scores
 car::vif(m3.ols)
 
 
 
panel_model_rd <- plm(Overall_Secondary ~  + Total_Playground + Total_FuncElectr 
                      + 
                     Total_FuncTU    + Total_FuncDW
                    + Total_Internet + Total_CompAvai, data = panel_data, model = "random")
 
 summary(panel_model_rd)
 vif(panel_model_rd)
 panel_model_fd <- plm(Overall_Secondary ~  + Total_Playground + Total_FuncElectr 
                      + 
                        Total_FuncTU    + Total_FuncDW
                      + Total_Internet + Total_CompAvai, data = secondary_schooldf,index="Location", model = "within")
 
 summary(panel_model_fd)

 pcdtest(panel_model_fd, test = c("lm"))
 
 pcdtest(panel_model_fd, test = c("cd"))
 
 
 summary(fixef(panel_model_fd))
 phtest(panel_model_fd,panel_model_rd)
 
 
 design.matrix <- as.data.frame(model.matrix(panel_model_fd))
 
 # Get the time-demeaned response variable, lifeExp
 design.matrix$Overall_Secondary <- plm::Within(
   plm::pdata.frame(secondary_schooldf, index = 'Location')$Overall_Secondary)
 
 
 summary(secondary_schooldf)
 
 # Fit the OLS model on the demeaned dataset
 m3.ols <- lm(Overall_Secondary ~  + Total_Playground + Total_FuncElectr 
              + 
                Total_FuncTU    + Total_FuncDW
              + Total_Internet + Total_CompAvai, data = design.matrix)
 
 # Calculate VIF scores
 car::vif(m3.ols)
 
 
 #second
 # panel_model_new_test <- plm(Overall_Secondary ~  +Total_Playground+ Total_Ramps  
 #                             + 
 #                               Total_FuncTU 
 #                             + Total_Internet + Total_CompAvai, data = secondary_schooldf,index="Location", model = "within")
 # 
 # # 
 # summary(panel_model_new_test)
 # 
 # r.squared(panel_model_new_test)
 # 
 # 
 # panel_model_new_random <- plm(Overall_Secondary ~  +Total_Playground+ Total_Ramps  
 #                             + 
 #                               Total_FuncTU 
 #                             + Total_Internet + Total_CompAvai, data = secondary_schooldf,index="Location", model = "random")
 # 
 # summary(panel_model_new_random)
 
 predict(panel_model_fd)
 
 secondary_schooldf$p <- predict(panel_model_fd)
 plotting<-ggplot(secondary_schooldf, aes(x=year, y=Overall_Secondary,color=Location)) 
   geom_point()
 
 ggplotly(plotting)
 
 
phtest(panel_model_fd,panel_model_rd)
 
 
 # 
 # design.matrixnew <- as.data.frame(model.matrix(panel_model_new_test))
 # 
 # design.matrixnew$Overall_Secondary <- plm::Within(
 #   plm::pdata.frame(secondary_schooldf, index = 'Location')$Overall_Secondary)
 # 
 # # Fit the OLS model on the demeaned dataset
 # m4.ols <- lm(Overall_Secondary ~  +Total_Playground+ Total_Ramps  
 #              + 
 #                Total_FuncTU 
 #              + Total_Internet + Total_CompAvai, data = design.matrixnew)
 # 
 # # Calculate VIF scores
 # car::vif(m4.ols)
 # 

 
 
 
 adf.test(secondary_schooldf$Overall_Secondary)


 bptest(Overall_Secondary ~  + Total_Playground + Total_FuncElectr 
        + 
          Total_FuncTU    + Total_FuncDW
        + Total_Internet + Total_CompAvai + factor(Location), data = secondary_schooldf)
 
 
 
 
 
 
 subsetting<-secondary_schooldf |> select(Overall_Secondary,Total_Playground ,Total_FuncElectr , Total_FuncBoysToilet , Total_FuncGirlsToilet  ,Total_FuncBoysUrinal ,Total_FuncGirlsUrinal ,Total_FuncTU , Total_FuncDW  ,Total_Internet ,Total_CompAvai)
 
 summary(subsetting)
 
 
 
 coeftest(panel_model_fd, vcov=vcovBK(panel_model_fd, cluster = "time"))
 