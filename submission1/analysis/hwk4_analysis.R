#Preliminaries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, rddensity, rdd, MatchIt)

#Install 'rdrobust' Package
if (!require("rdrobust")) install.packages("rdrobust")
library(rdrobust)

if (!require("rddensity")) install.packages("rddensity")
library(rddensity)

if (!require("rdd")) install.packages("rdd")
library(rdd)

if (!require("MatchIt")) install.packages("MatchIt")
library(MatchIt)

#Load Data

final.data<- read_rds("data/output/final_ma_data.rds")

#Group Data

sum.data <- final.data %>%
  group_by(county, year) %>%
  summarize(planid = n())

#Create Plot

boxwhisker.plot<- ggplot(sum.data, aes(x = factor(year), y = planid)) +
  geom_boxplot() +
  labs(x = "Year", 
  y = "Plan Counts (#)", 
  title = "Distribution of Plan Counts by County Over Time") +
  theme_classic()

boxwhisker.plot

#Filter by 2010, 2012 and 2015

filtered.data <- final.data %>%
  filter(year %in% c(2010, 2012, 2015))

#Count Star Ratings

rating.counts <- filtered.data %>%
  group_by(Star_Rating, year) %>%
  summarise(count = n())

#Create Graph

bar.graph<- ggplot(rating.counts, aes(x = factor(Star_Rating), y = count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year, scales = "free") +
  labs(x = "Star Rating", 
  y = "Count (#)", 
  title = "Distribution of Star Ratings Over Time") +
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


enrollment.summary <- ffs.costs %>%
  group_by(year) %>%
  summarize(total_parta_enroll = sum(parta_enroll, na.rm = TRUE))

print(enrollment.summary)

ma.eligibles <- ma.penetration %>%
  group_by(year) %>%
  summarize(ma.eligibles = sum(avg_eligibles, na.rm = TRUE))

print(ma.eligibles)

#Join Data

total.eligibles <- enrollment.summary %>%
  left_join(ma.eligibles, by = "year") %>%
    mutate(total_medicare_eligibles = ma.eligibles + total_parta_enroll)

print(total.eligibles)

#Filter by Year

filtered.data <- total.eligibles %>%
  filter(year >= 2010 & year <= 2015)

#Calculate Shares

filtered.data <- filtered.data %>%
  mutate(average_ma_share = ma.eligibles / total_medicare_eligibles)

#Create Graph

shares.graph <- ggplot(filtered.data, aes(x = year, y = average_ma_share)) +
  geom_line() +
  labs(x = "Year", 
  y = "Avg. Share of MA", 
  title = "Avg. Share of MA from 2010 to 2015") +
  theme_classic()

shares.graph

##Estimate ATEs


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
         bid, avg_ffscost, ma_rate)

#Calculate Star Rating

ma.data.clean <- ma.data.clean %>%
  mutate(star_rating = round(raw_rating * 2) / 2)

#Count Plans

star.rating.counts <- ma.data.clean %>%
  group_by(star_rating) %>%
  summarize(number_of_plans = n())

print(star.rating.counts)

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

bandwidth.graph <- ggplot(results, aes(x=Bandwidth, y=Estimate, color=factor(Star_Rating))) +
  geom_line() +
  labs(x="Bandwidth", y="Estimate", color="Star Rating") +
  theme_classic()

bandwidth.graph

#Create Plots

density.three <- rddensity(ma.threestar.rd$score, c=0)
rdplotdensity(density.three, ma.threestar.rd$score)

density.threefive <- rddensity(ma.threefive.rd$score, c=0)
rdplotdensity(density.threefive, ma.threefive.rd$score)

#Matching Propensity Scores

match.belowthree <- matchit(treat ~ plan_type + partd, 
                         data = ma.threestar.rd %>% 
                               filter(score =< 3, 
                                      !is.na(treat), 
                                      plan_type == "HMO", 
                                      !is.na(partd)),
                             method = NULL, distance = "mahalanobis")

 match.abovethree <- matchit(treat ~ plan_type + partd, 
                             data = ma.threestar.rd %>% 
                               filter(score >= 3, 
                                      !is.na(treat), 
                                      plan_type == "HMO", 
                                      !is.na(partd)),
                             method = NULL, distance = "mahalanobis")


#Estimate Effect
 estimate.three.hmo <- rdrobust(y=ma.threestar.rd$HMO, x=ma.threestar.rd$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

 summary(estimate.three.hmo)

 estimate.three.partd <- rdrobust(y=ma.threestar.rd$PartD, x=ma.threestar.rd$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

 summary(estimate.three.partd)

 #Repeat for 3.5 Star
 estimtate.threefive.hmo <- rdrobust(y=ma.threefive.rd$HMO, x=ma.threefive.rd$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

 summary(estimate.threefive.hmo)

estimate.threefive.partd <- rdrobust(y=ma.threefive.rd$PartD, x=ma.threefive.rd$score, c=0,
                  h=0.125, p=1, kernel="uniform", vce="hc0",
                  masspoints="off")

 summary(estimate.threefive.partd)

save.image("submission1/hwk4_workspace.Rdata")
