#Preliminaries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, rddensity, rdd, MatchIt)

#Load Data

final.data<- read_rds("data/output/final_ma_data.rds")

#Group Data

sum.data <- final.data %>%
  group_by(county, year) %>%
  summarize(planid = n())

#Create Plot

boxwhisker.plot<- ggplot(sum.data, aes(x = factor(year), y = planid)) +
  geom_boxplot() +
  labs(x = "Year", y = "Plan Counts (#)", title = "Distribution of Plan Counts by County Over Time") +
  theme_classic()

boxwhisker.plot

#Filter by 2010, 2012 and 2015

filtered.data <- final.data %>%
  filter(year %in% c(2010, 2012, 2015))

#Count Star Ratings

rating.counts <- filtered.data %>%
  group_by(year, Star_Rating) %>%
  summarise(count = n())

#Create Graph

bar.graph<- ggplot(rating.counts, aes(x = factor(Star_Rating), y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, scales = "free") +
  labs(x = "Star Rating", y = "Count (#)", title = "Distribution of Star Ratings Over Time") +
  theme_classic()

bar.graph

#Filter by years 2010 to 2015

filtered.data <- final.data %>%
  filter(year >= 2010 & year <= 2015)

#Group Data

avg.payments <- filtered.data %>%
  group_by(year) %>%
  summarize(avg.payments = mean(ma_rate, na.rm = TRUE))

#Create Graph

benchmark.graph <- ggplot(avg.payments, aes(x = year, y = avg.payments)) +
  geom_line() +
  labs(title = "Average Benchmark Payment from 2010 to 2015",
       x = "Year",
       y = "Average Benchmark Payment") +
  theme_classic()

benchmark.graph

#Enrollment Shares


enrollment.summary <- final.data %>%
  group_by(year) %>%
  summarize(med.proportion = sum(avg_enrollment, na.rm = TRUE) / sum(avg_eligibles, na.rm = TRUE))

#Create Graph

shares.graph <- ggplot(enrollment.summary, aes(x = year, y = med.proportion)) +
  geom_line() +
  labs(x = "Year", 
       y = "Avg. Share of MA", 
       title = "Avg. Share of MA from 2010 to 2015") +
  theme_classic()

shares.graph

##Estimate ATEs

#Install 'rdrobust' Package
if (!require("rdrobust")) install.packages("rdrobust")
library(rdrobust)

#Read and Clean Data

ma.data <- read_rds("data/output/final_ma_data.rds")

ma.data.clean <- ma.data %>%
  filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) 

ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess
          ,nodelays,carequickly,
          overallrating_care,overallrating_plan,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,bloodpressure,ra_manage,
          copd_test,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type, partd)


ma.rounded <- ma.data.clean %>%
  select(raw_rating) %>%
  mutate(three = sum(ifelse(raw_rating >= 2.75 & raw_rating < 3.25, 1, 0), na.rm = T), 
  three_five = sum(ifelse(raw_rating >= 3.25 & raw_rating < 3.75, 1, 0), na.rm = T), 
  four = sum(ifelse(raw_rating >= 3.75 & raw_rating < 4.25, 1, 0), na.rm = T), 
  four_five = sum(ifelse(raw_rating >= 4.25 & raw_rating < 4.75, 1, 0), na.rm = T), 
  five = sum(ifelse(raw_rating <= 3.75, 1, 0), na.rm = T))%>%
  head(n=1)


#Create Estimate (3 Stars)

ma.threestar.rd <- ma.data.clean %>%
  filter(Star_Rating==2.5 | Star_Rating==3) %>%
  mutate(score = raw_rating - 3,
         treat = (score>=0),
         first = (score>=-.125 & score<=.125),
         second = (score>=-.125 & score<=.125),
         mkt.share = avg_enrollment/avg_eligibles,
         ln.share = log(mkt.share),
         score.treat=score*treat)

threestar.estimate <- rdrobust(y=ma.threestar.rd$mkt.share, x=ma.threestar.rd$score, c=0,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")

summary(threestar.estimate)

#Extract Specific Values for Table

three.table <- rbind(threestar.estimate$coef,threestar.estimate$p, threestar.estimate$z, threestar.estimate$se)

three.table


#Create New Estimate (3.5 Stars)

ma.threefive.rd <- ma.data.clean %>%
  filter(Star_Rating==3 | Star_Rating==3.5) %>%
  mutate(score = raw_rating - 3.5,
         treat = (score>=0),
         first = (score>=-.125 & score<=.125),
         second = (score>=-.125 & score<=.125),
         mkt.share = avg_enrollment/avg_eligibles,
         ln.share = log(mkt.share),
         score.treat=score*treat)

threefive.estimate <- rdrobust(y=ma.threefive.rd$mkt.share, x=ma.threefive.rd$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

summary(threefive.estimate)

threefive.table <- rbind(threefive.estimate$coef,threefive.estimate$p, threefive.estimate$z, threefive.estimate$se)

threefive.table

#Define New Bandwidths

bandwidths <- c(0.1, 0.12, 0.13, 0.14, 0.15)

#Initialize DF

results <- data.frame()

#Loop BWs

for (h in bandwidths) {
  threestar.estimate <- rdrobust(y=ma.threestar.rd$mkt.share, x=ma.threestar.rd$score, c=0, h=h, p=1, kernel="uniform", vce="hc0", masspoints="off")
  results <- rbind(results, data.frame(Bandwidth=h, Star_Rating=3, Estimate=threestar.estimate$coef[1]))
  threefive.estimate<- rdrobust(y=ma.threefive.rd$mkt.share, x=ma.threefive.rd$score, c=0, h=h, p=1, kernel="uniform", vce="hc0", masspoints="off")
  results <- rbind(results, data.frame(Bandwidth=h, Star_Rating=3.5, Estimate=threefive.estimate$coef[1]))
}

#Create Graph

bandwidth.graph <- ggplot(results, aes(x=Bandwidth, y=Estimate)) +
  geom_line() +
  labs(x="Bandwidth", 
       y="Estimate", 
       title="RD Estimates at New Bandwidths",
       color="Star Rating") +
  theme_classic()

bandwidth.graph

#Load Packages

if (!require("rddensity")) install.packages("rddensity")
library(rddensity)

if (!require("rdd")) install.packages("rdd")
library(rdd)

#Create Plots

density.three <- rddensity(ma.threestar.rd$score, c=0)
rdplotdensity(density.three, ma.threestar.rd$score)

density.threefive <- rddensity(ma.threefive.rd$score, c=0)
rdplotdensity(density.threefive, ma.threefive.rd$score)

#Load

library(dplyr)
if (!require("cobalt")) install.packages("cobalt")
library(cobalt)

#Create Variables

lp.vars <- ma.data.clean %>% 
  ungroup() %>%
  filter((raw_rating >= 2.75 - .125 & Star_Rating == 2.5) | 
           (raw_rating <= 2.75 + .125 & Star_Rating == 3) & 
           (plan_type == "HMO/HMOPOS")) %>%
  mutate(rounded = (Star_Rating == 3)) %>%
  select(plan_type, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% 
select(plan_type, partd)

first.plot <- love.plot(bal.tab(lp.covs, treat = lp.vars$rounded), 
                     colors = "#12e2d4", 
                     shapes = "square") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")+
                     labs(y = "Plan Types",
                     title = "Plans Above Threshold Value") +
  theme_classic()

first.plot


save.image("submission2/hwk4_workspace.Rdata")
