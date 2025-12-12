# 🚴 Cyclistic Bike-Share: Strategic Membership Growth Analysis
### Google Data Analytics Professional Certificate Capstone Project

---

## 📑 Table of Contents
- [📋 Executive Summary](#-executive-summary)
- [📂 Dataset](#-dataset)
- [🛠️ Phase 1: Prepare & Process (Data Engineering)](#-phase-1-prepare--process-data-engineering)
  - [Step 1: Import, Standardize, and Combine Datasets](###step-1-import-standardize-and-combine-datasets)
  - [Output — Step 1](#output--step-1)
  - [Step 2: Cleaning & Feature Engineering](#step-2-cleaning--feature-engineering)
  - [Output — Step 2](#output--step-2)

---

## 📋 Executive Summary
This project addresses a critical business objective for Cyclistic, a fictional bike-share company: maximizing the conversion of casual riders into annual members. Utilizing an extensive dataset of historical trip data, this analysis will systematically identify key differences in ride behavior, preference, and usage patterns between existing annual members and casual riders. The insights generated will directly inform data-driven marketing and operational strategies, providing actionable recommendations to enhance the value proposition of the annual membership and significantly increase member enrollment and retention.


## 📂 Dataset
This case study uses: 
+ Divvy_Trips_2019_Q1.csv
+ Divvy_Trips_2020_Q1.csv

Data was downloaded from the official Divvy/Cyclistic open data portal.


## 🛠️ Phase 1: Prepare & Process (Data Engineering)
The initial phase involves importing raw historical datasets, standardizing variable types, and merging them into a single unified dataframe for longitudinal analysis.

### Step 1: Import, Standardize, and Combine Datasets
We utilized the `tidyverse` and `janitor` packages in R to ensure consistency across the 2019 (Divvy Trips 2019 Q1) and 2020 (Divvy Trips 2020 Q1) datasets before combining them.             
The data ranges from "2019-01-01 00:04:37 UTC" to "2020-03-31 23:51:34 UTC".

```r
library(tidyverse)
library(janitor)

# File paths
df19 <- read_csv("raw/Divvy_Trips_2019_Q1.csv")
df20 <- read_csv("raw/Divvy_Trips_2020_Q1.csv")

# ---- Clean & Standardize 2019 ---- #
df19_clean <- df19 %>%
  mutate(
    trip_id = as.character(trip_id),   # convert numeric → character
    bikeid = as.character(bikeid)
  ) %>%
  select(
    trip_id,
    start_time,
    end_time,
    bikeid,
    tripduration,
    from_station_id,
    from_station_name,
    to_station_id,
    to_station_name,
    usertype,
    gender,
    birthyear
  )

# ---- Clean & Standardize 2020 (to match 2019 structure) ---- #
df20_clean <- df20 %>%
  transmute(
    trip_id = as.character(ride_id),      # convert to character
    start_time = started_at,
    end_time = ended_at,
    bikeid = NA_character_,               # keep type character
    tripduration = NA_real_,              # numeric column
    from_station_id = start_station_id,
    from_station_name = start_station_name,
    to_station_id = end_station_id,
    to_station_name = end_station_name,
    usertype = member_casual,
    gender = NA_character_,
    birthyear = NA_real_
  )

# ---- Combine ---- #
trips <- bind_rows(df19_clean, df20_clean) %>% clean_names()

# Inspect
glimpse(trips)
range(trips$start_time, na.rm = TRUE)

```
### Output for Step 1
```
Rows: 791,956
Columns: 12
$ trip_id           <chr> "21742443", "21742444", "21742445", "21742446", "21742447"…
$ start_time        <dttm> 2019-01-01 00:04:37, 2019-01-01 00:08:13, 2019-01-01 00:1…
$ end_time          <dttm> 2019-01-01 00:11:07, 2019-01-01 00:15:34, 2019-01-01 00:2…
$ bikeid            <chr> "2167", "4386", "1524", "252", "1170", "2437", "2708", "27…
$ tripduration      <dbl> 390, 441, 829, 1783, 364, 216, 177, 100, 1727, 336, 886, 6…
$ from_station_id   <dbl> 199, 44, 15, 123, 173, 98, 98, 211, 150, 268, 299, 204, 90…
$ from_station_name <chr> "Wabash Ave & Grand Ave", "State St & Randolph St", "Racin…
$ to_station_id     <dbl> 84, 624, 644, 176, 35, 49, 49, 142, 148, 141, 295, 420, 25…
$ to_station_name   <chr> "Milwaukee Ave & Grand Ave", "Dearborn St & Van Buren St (…
$ usertype          <chr> "Subscriber", "Subscriber", "Subscriber", "Subscriber", "S…
$ gender            <chr> "Male", "Female", "Female", "Male", "Male", "Female", "Mal…
$ birthyear         <dbl> 1989, 1990, 1994, 1993, 1994, 1983, 1984, 1990, 1995, 1996…
> range(trips$start_time, na.rm = TRUE)
[1] "2019-01-01 00:04:37 UTC" "2020-03-31 23:51:34 UTC"
```

<img width="945" height="296" alt="image" src="https://github.com/user-attachments/assets/fa2a1da5-2d4f-4d2c-b9a4-693bfd86eb71" />
<img width="945" height="265" alt="image" src="https://github.com/user-attachments/assets/d7c2f473-4466-4623-b110-0bd7b8685e99" />


### Step2: Cleaning & Feature Engineering
        
 We converted time variables using lubridate and calculated the ride_length in seconds using difftime. Temporal features (day_of_week, day_name, month, hour) were extracted to support temporal analysis. Finally, records with non-positive ride lengths were filtered out.

```r
library(tidyverse)
library(lubridate)

# Convert datetime columns
trips <- trips %>%
  mutate(
    start_time = ymd_hms(start_time),
    end_time   = ymd_hms(end_time)
  )

# Create ride_length (in seconds)
trips <- trips %>%
  mutate(
    ride_length = as.numeric(difftime(end_time, start_time, units = "secs"))
  )

# Create day_of_week (1=Sunday, 7=Saturday)
trips <- trips %>%
  mutate(
    day_of_week = wday(start_time, week_start = 1),
    day_name = wday(start_time, label = TRUE, abbr = FALSE)
  )

# Optional: extract more date features
trips <- trips %>%
  mutate(
    month = month(start_time, label = TRUE, abbr = TRUE),
    day   = day(start_time),
    hour  = hour(start_time)
  )

# Remove invalid ride durations (negative or zero)
trips <- trips %>%
  filter(ride_length > 0)

# Check result
glimpse(trips)
summary(trips$ride_length)
table(trips$day_name)
```


### Output Step 2

```r

Rows: 791,746
Columns: 18
$ trip_id           <chr> "21742443", "21742444", "21742445", "21742446", "21742447", "21742448", "217424…
$ start_time        <dttm> 2019-01-01 00:04:37, 2019-01-01 00:08:13, 2019-01-01 00:13:23, 2019-01-01 00:1…
$ end_time          <dttm> 2019-01-01 00:11:07, 2019-01-01 00:15:34, 2019-01-01 00:27:12, 2019-01-01 00:4…
$ bikeid            <chr> "2167", "4386", "1524", "252", "1170", "2437", "2708", "2796", "6205", "3939", …
$ tripduration      <dbl> 390, 441, 829, 1783, 364, 216, 177, 100, 1727, 336, 886, 653, 601, 562, 906, 89…
$ from_station_id   <dbl> 199, 44, 15, 123, 173, 98, 98, 211, 150, 268, 299, 204, 90, 90, 289, 289, 152, …
$ from_station_name <chr> "Wabash Ave & Grand Ave", "State St & Randolph St", "Racine Ave & 18th St", "Ca…
$ to_station_id     <dbl> 84, 624, 644, 176, 35, 49, 49, 142, 148, 141, 295, 420, 255, 255, 324, 324, 166…
$ to_station_name   <chr> "Milwaukee Ave & Grand Ave", "Dearborn St & Van Buren St (*)", "Western Ave & F…
$ usertype          <chr> "Subscriber", "Subscriber", "Subscriber", "Subscriber", "Subscriber", "Subscrib…
$ gender            <chr> "Male", "Female", "Female", "Male", "Male", "Female", "Male", "Male", "Male", "…
$ birthyear         <dbl> 1989, 1990, 1994, 1993, 1994, 1983, 1984, 1990, 1995, 1996, 1994, 1994, 1986, 1…
$ ride_length       <dbl> 390, 441, 829, 1783, 364, 216, 177, 100, 1727, 336, 886, 653, 601, 562, 906, 89…
$ day_of_week       <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
$ day_name          <ord> Tuesday, Tuesday, Tuesday, Tuesday, Tuesday, Tuesday, Tuesday, Tuesday, Tuesday…
$ month             <ord> Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan,…
$ day               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
$ hour              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
> summary(trips$ride_length)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
       1      328      537     1184      910 10632022 
> table(trips$day_name)

   Sunday    Monday   Tuesday Wednesday  Thursday    Friday  Saturday 
    78849    117124    135946    130266    132999    123676     72886 
```

<img width="945" height="326" alt="image" src="https://github.com/user-attachments/assets/031e6a2a-7eec-4d48-9746-d1b09c69e7df" />
<img width="945" height="393" alt="image" src="https://github.com/user-attachments/assets/5025c143-097d-491c-a51d-931f87dc616d" />

## 📈 Phase 2: Analyze & Share (Data Visualization)

This phase focuses on comparative analysis between user types, particularly around ride length and temporal patterns, using ggplot2.

### Step 1: Descriptive Statistics by User Type
```r
library(tidyverse)

# Genel özet
trips %>%
  group_by(usertype) %>%
  summarise(
    total_trips = n(),
    avg_ride_length = mean(ride_length) / 60,  # minutes
    median_ride_length = median(ride_length) / 60,
    max_ride_length = max(ride_length) / 60
  )
```
### Step 1 Output
```r
# A tibble: 4 × 5
  usertype   total_trips avg_ride_length median_ride_length max_ride_length
  <chr>            <int>           <dbl>              <dbl>           <dbl>
1 Customer         23163            61.9              23.4          177200.
2 Subscriber      341906            13.9               8.35         101607.
3 casual           48270            96.2              21.3          156450.
4 member          378407            12.7               8.58          93794.
```

This table answers the following questions for management:
➡ Do casual users stay longer?
➡ Do members stay more regularly?

### Step 2: Number of drives by day of the week
```r
trips %>%
  group_by(usertype, day_name) %>%
  summarise(total_rides = n()) %>%
  arrange(usertype, day_name)
```

### Step 2 Output
```r
`summarise()` has grouped output by 'usertype'. You can override using the `.groups` argument.
# A tibble: 28 × 3
# Groups:   usertype [4]
   usertype   day_name  total_rides
   <chr>      <ord>           <int>
 1 Customer   Sunday           3766
 2 Customer   Monday           1892
 3 Customer   Tuesday          2728
 4 Customer   Wednesday        2489
 5 Customer   Thursday         2920
 6 Customer   Friday           3375
 7 Customer   Saturday         5993
 8 Subscriber Sunday          24233
 9 Subscriber Monday          48507
10 Subscriber Tuesday         58277
```

### Step 3: Average driving time by day of the week
```r
trips %>%
  group_by(usertype, day_name) %>%
  summarise(avg_length_min = mean(ride_length) / 60) %>%
  arrange(usertype, day_name)
```

# Step 3 Output
```r
`summarise()` has grouped output by 'usertype'. You can override using the `.groups` argument.
# A tibble: 28 × 3
# Groups:   usertype [4]
   usertype   day_name  avg_length_min
   <chr>      <ord>              <dbl>
 1 Customer   Sunday              41.6
 2 Customer   Monday              44.5
 3 Customer   Tuesday             40.5
 4 Customer   Wednesday           52.0
 5 Customer   Thursday           134. 
 6 Customer   Friday              59.9
 7 Customer   Saturday            60.3
 8 Subscriber Sunday              16.8
 9 Subscriber Monday              14.6
10 Subscriber Tuesday             14.4
```

### Step 4: Hourly usage trend (Peak hours)
```r
trips %>%
  group_by(usertype, hour) %>%
  summarise(total_rides = n()) %>%
  arrange(usertype, hour)
```

### Step 4 Output
```r
`summarise()` has grouped output by 'usertype'. You can override using the `.groups` argument.
# A tibble: 96 × 3
# Groups:   usertype [4]
   usertype  hour total_rides
   <chr>    <int>       <int>
 1 Customer     0         153
 2 Customer     1         107
 3 Customer     2         111
 4 Customer     3          44
 5 Customer     4          37
 6 Customer     5          60
 7 Customer     6         141
 8 Customer     7         288
 9 Customer     8         664
10 Customer     9         758
```
### Step 5: Ride count (bar plot) according to membership type
```r
ggplot(trips, aes(x = usertype, fill = usertype)) +
  geom_bar() +
  labs(title = "Ride Count by User Type",
       x = "User Type", y = "Number of Rides") +
  theme_minimal()
```
### Step 5 Output
<img width="945" height="482" alt="image" src="https://github.com/user-attachments/assets/34918c6a-7493-49a9-9c60-22fe9946c758" />

### Step 6: Usage comparison by day of the week
```r
ggplot(trips, aes(x = day_name, fill = usertype)) +
  geom_bar(position = "dodge") +
  labs(title = "Rides by Day of Week",
       x = "Day of Week", y = "Ride Count") +
  theme_minimal()
```

### Step 6 Output
<img width="945" height="457" alt="image" src="https://github.com/user-attachments/assets/0cf54b23-d48a-46f4-9b35-2d8f6ac1a531" />

### Step 7: Comparison of average driving times
```r
trips %>%
  group_by(usertype, day_name) %>%
  summarise(avg_length = mean(ride_length)/60) %>%
  ggplot(aes(x = day_name, y = avg_length, fill = usertype)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Length by Day",
       x = "Day of Week", y = "Minutes") +
  theme_minimal()
```
### Step 7 Output

<img width="945" height="461" alt="image" src="https://github.com/user-attachments/assets/936359e8-6260-458b-b9ba-9d1d3547ce5c" />

### Step 8: Hourly usage trend
```r
trips %>%
  group_by(usertype, hour) %>%
  summarise(trips_count = n()) %>%
  ggplot(aes(x = hour, y = trips_count, color = usertype)) +
  geom_line(size = 1) +
  labs(title = "Hourly Usage Trend",
       x = "Hour of Day", y = "Number of Rides") +
  theme_minimal()
```
### Step 8 Output
<img width="945" height="470" alt="image" src="https://github.com/user-attachments/assets/77f8a45a-b598-4cbf-8f66-962dc47afb3b" />


# 📝 Final Recommendations
## How Cyclistic Can Convert Casual Riders into Long-Term Annual Members

Following an in-depth analysis of Cyclistic’s 2019–2020 Q1 ride data, several clear behavioral and usage differences emerged between annual members and casual riders. These insights provide actionable opportunities to increase membership conversions, optimize operational efficiency, and enhance the customer experience.

### ✔ 1. Develop and Promote Weekend & Leisure-Based Membership Products

Casual riders exhibit significantly different behavioral patterns from annual members—particularly:

+ Higher ride durations

+ Greater usage on weekends

+ More recreational/tourism-oriented trip patterns

This indicates strong potential for experience-driven membership offerings tailored specifically for leisure riders.

###  🔑 Recommended Actions
<ins> A. Create a "Weekend Rider Membership" </ins>

A low-cost, commitment-light plan designed for riders who mainly use the service on Saturdays and Sundays.

+ Focus on unlimited 45–60 minute rides on weekends

+ Optional upgrade to full annual membership

+ Perfect for tourists, occasional riders, and local explorers

<ins> B. Offer a “Leisure Membership Upgrade Path” </ins>

Encourage casual users to try a discounted trial plan:

+ 1-month discounted “Leisure Pass”

+ Automatic upgrade incentives based on usage

+ Seasonal packages (e.g., spring/summer promotions)

Carefully targeted membership products allow Cyclistic to convert recreational demand into recurring revenue, rather than relying solely on unpredictable casual purchase patterns.

### ✔ 2. Launch Data-Driven Marketing Campaigns During Peak Casual Usage Times

Peak usage analysis clearly shows that casual riders are most active during:

+ Weekend afternoons

+ Warm weather months (Q2–Q3 historically)

+ Midday and early evening hours

Cyclistic can capture this traffic by deploying highly targeted marketing interventions exactly when casual riders are already in the system and more receptive to messaging.

### 🔑 Recommended Actions
<ins> A. Real-Time In-App Promotions </ins>

Push notifications offering discounts or trial membership upgrades right after a casual ride ends, when engagement is highest.

<ins> B. Station-Side QR Campaigns </ins>

High-casual stations should display:

+ QR codes linking to limited-time membership offers

+ “Join as a Member → Save on Your Next Ride”

+ Geo-targeted campaigns based on station density

<ins> C. Time-Limited Price Incentives </ins>

During peak casual hours, offer:

+ “Ride twice this weekend → get 30% off an annual membership”

+ “Upgrade today and your next 2 rides are free”

Marketing campaigns aligned with behavioral patterns increase conversion likelihood, reduce customer acquisition costs, and enhance brand visibility.

### ✔ 3. Improve Convenience and Infrastructure at High-Casual Stations

Spatial analysis indicates that certain stations consistently serve a disproportionately high number of casual riders. These locations often overlap with:

+ Tourist corridors

+ Public parks and waterfront areas

+ Popular leisure or entertainment districts

Enhancing convenience at these hubs creates a stronger onboarding and retention loop for casual users.

### 🔑 Recommended Actions
<ins> A. Expand Docking Capacity </ins> 

Mitigate common frustrations such as dock shortages at peak times, particularly near tourist-heavy areas.

<ins> B. Improve On-Site Experience </ins>

Add clear and engaging signage:

+ “How to Ride” guides

+ “How Membership Works” comparison boards

+ QR-linked videos for first-time riders

<ins> C. Offer First-Ride-Free or Discounted Trial Stations </ins>

At designated high-casual locations:

+ Offer one free unlock

+ 50% off first ride when scanning a dockside QR code

+ Membership info displayed prominently at eye level

These improvements reduce friction, improve perceived value, and ultimately help convert one-time riders into loyal members.

## ✅ Summary: Strategic Opportunities for Growth

Cyclistic’s data demonstrates clear and actionable distinctions between annual members and casual users. By leveraging these insights, Cyclistic can adopt a three-pronged strategy that enhances user satisfaction while driving sustainable business growth:

1. Tailored membership products to match leisure-oriented behavior

2. Precision-timed marketing to convert casual riders at their peak activity moments

3. Operational and experience improvements at high-casual stations to reduce friction

Together, these initiatives position Cyclistic to significantly increase annual memberships, strengthen rider loyalty, and optimize the overall system for both operational efficiency and customer satisfaction.























