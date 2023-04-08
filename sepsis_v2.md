Sepsis Assignment
================

## Task 2

- Fread is faster than read_delim and it takes longer to read in data
  the more observations that you are reading in

## Task 4

#### The date and time of the report

    Hello! The last time the report was run: 2023-04-08 08:16:20

#### Recent heart rate, temperature, and respiratory rate for patients who currently have sepsis

``` r
if(nrow(septic_data) >=1){
table1 <- most_recent_data %>%
  filter(SepsisLabel == 1) %>%
  select(c(PatientID, HR, Temp, Resp, obsTime)) %>%
  ungroup() %>%
  gt() %>%
  cols_label(Temp = "Temperature",
             HR = "Heart Rate",
             Resp = "Respiration",
             obsTime = "Last Update") %>%
  tab_header(title = "Most recent vital signs for Septic Patients")

table1
} else{
  print("Hooray, no septic patients to report")
}
```

    [1] "Hooray, no septic patients to report"

#### Plots for all patients who have sepsis that show the complete history of their heart rate, temperature, and respiratory rate during their time in the ICU.

``` r
filtered_sepsis <- new_data %>%
group_by(PatientID) %>%
  mutate(any_sepsis = ifelse(any(SepsisLabel == 1), 1, 0)) %>%
  filter(any_sepsis == 1) 


patient_ids <- unique(filtered_sepsis$PatientID)
if(nrow(filtered_sepsis)>=1) {
  
for (i in patient_ids) {
  patient_data <- filtered_sepsis %>% filter(PatientID == i)
  
  if (nrow(patient_data) > 0) {
    p <-ggplot(patient_data, aes(x = obsTime, y = HR)) +
      geom_point(color = "red") +
      geom_line(color= "red") + 
      ggtitle(paste("HR History for Patient ID:", i)) +
      theme_minimal()
    
    print(p)
  }
} 

for (i in patient_ids) {
  patient_data <- filtered_sepsis %>% filter(PatientID == i)
  
  if (nrow(patient_data) > 0) {
    t <-ggplot(patient_data, aes(x = obsTime, y = Temp)) +
      geom_point(color = "darkgreen") +
      geom_line(color= "darkgreen") + 
      ggtitle(paste("Temp History for Patient ID:", i)) +
      theme_minimal()
    
    print(t)
  }
}

for (i in patient_ids) {
  patient_data <- filtered_sepsis %>% filter(PatientID == i)
if(nrow(patient_data) >0){
    r <-ggplot(patient_data, aes(x=obsTime, y= Resp)) +
      geom_point(color= "lightblue") +
      geom_line(color = "lightblue") +
      ggtitle(paste("Resp History for Patient ID:", i)) +
      theme_minimal()
    print(r)
  }
}
} else{
  print("Hooray, no septic patients to graph")
}
```

    [1] "Hooray, no septic patients to graph"

#### A table showing the change in heart rate, temperature, and respiratory rate between the last two measurements for all patients

``` r
new_data %>%
  group_by(PatientID) %>%
  arrange(obsTime, .by_group = TRUE, desc=TRUE) %>%
  slice_max(n=2, order_by=obsTime) %>%
mutate(prev_HR = lag(HR, order_by = obsTime),
         HR_change = (HR- prev_HR),
         prev_resp = lag(Resp, order_by = obsTime),
         Resp_change = (Resp - prev_resp),
         prev_temp = lag(Temp, order_by = obsTime),
         temp_change = (Temp - prev_temp)) %>%
  select(PatientID, HR_change, temp_change, Resp_change) %>%
  ungroup() %>%
  knitr::kable()
```

| PatientID | HR_change | temp_change | Resp_change |
|:----------|----------:|------------:|------------:|
| 000003    |        -1 |          NA |           6 |
| 000003    |        NA |          NA |          NA |
| 000052    |         0 |        0.15 |          -2 |
| 000052    |        NA |          NA |          NA |
| 000054    |        -6 |          NA |          -4 |
| 000054    |        NA |          NA |          NA |
| 000057    |         3 |          NA |          -3 |
| 000057    |        NA |          NA |          NA |
| 000122    |        -8 |          NA |          11 |
| 000122    |        NA |          NA |          NA |
| 000180    |        -8 |          NA |         -11 |
| 000180    |        NA |          NA |          NA |
| 000315    |        -7 |          NA |           4 |
| 000315    |        NA |          NA |          NA |
| 000388    |        -6 |          NA |          -5 |
| 000388    |        NA |          NA |          NA |
| 000714    |        -4 |          NA |           1 |
| 000714    |        NA |          NA |          NA |
| 001000    |         8 |        0.27 |           4 |
| 001000    |        NA |          NA |          NA |
| 001258    |         2 |          NA |           0 |
| 001258    |        NA |          NA |          NA |
| 001262    |        -1 |          NA |          -2 |
| 001262    |        NA |          NA |          NA |
| 001286    |         2 |          NA |           7 |
| 001286    |        NA |          NA |          NA |
| 001759    |        -1 |       -0.05 |           3 |
| 001759    |        NA |          NA |          NA |
| 001948    |       -14 |          NA |          -2 |
| 001948    |        NA |          NA |          NA |
| 002017    |         3 |          NA |           7 |
| 002017    |        NA |          NA |          NA |
| 002077    |         3 |          NA |           6 |
| 002077    |        NA |          NA |          NA |
| 002467    |        -6 |          NA |           4 |
| 002467    |        NA |          NA |          NA |
| 002516    |         4 |          NA |          -9 |
| 002516    |        NA |          NA |          NA |
| 002699    |        -3 |          NA |          -3 |
| 002699    |        NA |          NA |          NA |
| 002719    |         0 |        0.10 |           0 |
| 002719    |        NA |          NA |          NA |
| 003342    |         0 |       -0.30 |           0 |
| 003342    |        NA |          NA |          NA |
| 003364    |         1 |          NA |          -1 |
| 003364    |        NA |          NA |          NA |
| 003404    |         6 |          NA |           0 |
| 003404    |        NA |          NA |          NA |
| 003751    |        -6 |        0.50 |           3 |
| 003751    |        NA |          NA |          NA |
| 003797    |         9 |        0.20 |           3 |
| 003797    |        NA |          NA |          NA |
| 004370    |         8 |          NA |           4 |
| 004370    |        NA |          NA |          NA |
| 004842    |        NA |          NA |          NA |
| 004842    |        NA |          NA |          NA |
| 005042    |        -1 |          NA |          -1 |
| 005042    |        NA |          NA |          NA |
| 005496    |        -8 |          NA |          -2 |
| 005496    |        NA |          NA |          NA |
| 006095    |        NA |          NA |          NA |
| 006095    |        NA |          NA |          NA |
| 006455    |        NA |          NA |          NA |
| 006455    |        NA |          NA |          NA |
| 006767    |         3 |          NA |          -1 |
| 006767    |        NA |          NA |          NA |
| 006936    |        -8 |          NA |           0 |
| 006936    |        NA |          NA |          NA |
| 008226    |         0 |       -0.10 |          -2 |
| 008226    |        NA |          NA |          NA |
| 008268    |        -7 |       -0.10 |          -5 |
| 008268    |        NA |          NA |          NA |
| 008524    |        -1 |          NA |           6 |
| 008524    |        NA |          NA |          NA |
| 008536    |        -2 |          NA |           1 |
| 008536    |        NA |          NA |          NA |
| 008574    |       -10 |          NA |          -4 |
| 008574    |        NA |          NA |          NA |
| 008614    |         1 |          NA |          -5 |
| 008614    |        NA |          NA |          NA |
| 008803    |         4 |          NA |           0 |
| 008803    |        NA |          NA |          NA |
| 008882    |        -2 |          NA |           8 |
| 008882    |        NA |          NA |          NA |
| 009312    |       -14 |          NA |          -3 |
| 009312    |        NA |          NA |          NA |
| 009429    |        13 |        0.40 |          11 |
| 009429    |        NA |          NA |          NA |
| 009433    |        -3 |          NA |           0 |
| 009433    |        NA |          NA |          NA |
| 009454    |        10 |          NA |           2 |
| 009454    |        NA |          NA |          NA |
| 009511    |        -3 |          NA |          -3 |
| 009511    |        NA |          NA |          NA |
| 009657    |        -1 |          NA |           3 |
| 009657    |        NA |          NA |          NA |
| 009814    |         0 |          NA |          -2 |
| 009814    |        NA |          NA |          NA |
| 009982    |        11 |          NA |           0 |
| 009982    |        NA |          NA |          NA |
