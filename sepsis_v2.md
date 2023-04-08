Sepsis Assignment
================

## Task 2

- Fread is faster than read_delim and it takes longer to read in data
  the more observations that you are reading in

## Task 4

#### The date and time of the report

    Hello! The last time the report was run: 2023-04-08 02:29:41

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
  gt()
```

<div id="uturczlwjx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uturczlwjx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#uturczlwjx thead, #uturczlwjx tbody, #uturczlwjx tfoot, #uturczlwjx tr, #uturczlwjx td, #uturczlwjx th {
  border-style: none;
}

#uturczlwjx p {
  margin: 0;
  padding: 0;
}

#uturczlwjx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uturczlwjx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#uturczlwjx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uturczlwjx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uturczlwjx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uturczlwjx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uturczlwjx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uturczlwjx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uturczlwjx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uturczlwjx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uturczlwjx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uturczlwjx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uturczlwjx .gt_spanner_row {
  border-bottom-style: hidden;
}

#uturczlwjx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#uturczlwjx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uturczlwjx .gt_from_md > :first-child {
  margin-top: 0;
}

#uturczlwjx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uturczlwjx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uturczlwjx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#uturczlwjx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#uturczlwjx .gt_row_group_first td {
  border-top-width: 2px;
}

#uturczlwjx .gt_row_group_first th {
  border-top-width: 2px;
}

#uturczlwjx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uturczlwjx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#uturczlwjx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#uturczlwjx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uturczlwjx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uturczlwjx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uturczlwjx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#uturczlwjx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uturczlwjx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uturczlwjx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uturczlwjx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uturczlwjx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uturczlwjx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#uturczlwjx .gt_left {
  text-align: left;
}

#uturczlwjx .gt_center {
  text-align: center;
}

#uturczlwjx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uturczlwjx .gt_font_normal {
  font-weight: normal;
}

#uturczlwjx .gt_font_bold {
  font-weight: bold;
}

#uturczlwjx .gt_font_italic {
  font-style: italic;
}

#uturczlwjx .gt_super {
  font-size: 65%;
}

#uturczlwjx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#uturczlwjx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#uturczlwjx .gt_indent_1 {
  text-indent: 5px;
}

#uturczlwjx .gt_indent_2 {
  text-indent: 10px;
}

#uturczlwjx .gt_indent_3 {
  text-indent: 15px;
}

#uturczlwjx .gt_indent_4 {
  text-indent: 20px;
}

#uturczlwjx .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="PatientID">PatientID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="HR_change">HR_change</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="temp_change">temp_change</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Resp_change">Resp_change</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="PatientID" class="gt_row gt_right">000003</td>
<td headers="HR_change" class="gt_row gt_right">-3</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-17</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000003</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000052</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">0.60</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000052</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000054</td>
<td headers="HR_change" class="gt_row gt_right">4</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">2</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000054</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000057</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000057</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000122</td>
<td headers="HR_change" class="gt_row gt_right">-4</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-2</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000122</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000180</td>
<td headers="HR_change" class="gt_row gt_right">14</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000180</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000315</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000315</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000388</td>
<td headers="HR_change" class="gt_row gt_right">3</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000388</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000714</td>
<td headers="HR_change" class="gt_row gt_right">-6</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">000714</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001000</td>
<td headers="HR_change" class="gt_row gt_right">11</td>
<td headers="temp_change" class="gt_row gt_right">-0.50</td>
<td headers="Resp_change" class="gt_row gt_right">-2</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001000</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001258</td>
<td headers="HR_change" class="gt_row gt_right">-3</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001258</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001262</td>
<td headers="HR_change" class="gt_row gt_right">1</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001262</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001286</td>
<td headers="HR_change" class="gt_row gt_right">4</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001286</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001759</td>
<td headers="HR_change" class="gt_row gt_right">-9</td>
<td headers="temp_change" class="gt_row gt_right">0.00</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001759</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001948</td>
<td headers="HR_change" class="gt_row gt_right">-7</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-4</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">001948</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002017</td>
<td headers="HR_change" class="gt_row gt_right">4</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-9</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002017</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002077</td>
<td headers="HR_change" class="gt_row gt_right">4</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002077</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002467</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-2</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002467</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002516</td>
<td headers="HR_change" class="gt_row gt_right">5</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-4</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002516</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002699</td>
<td headers="HR_change" class="gt_row gt_right">1</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">6</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002699</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002719</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">-0.30</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">002719</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003342</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">-0.15</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003342</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003364</td>
<td headers="HR_change" class="gt_row gt_right">-14</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-5</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003364</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003404</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003751</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">0.10</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003751</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003797</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">0.10</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">003797</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">004370</td>
<td headers="HR_change" class="gt_row gt_right">-10</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">004370</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">004842</td>
<td headers="HR_change" class="gt_row gt_right">-3</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-2</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">004842</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">005042</td>
<td headers="HR_change" class="gt_row gt_right">-12</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-10</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">005042</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">005496</td>
<td headers="HR_change" class="gt_row gt_right">-10</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">005496</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006095</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006095</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006455</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006455</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006767</td>
<td headers="HR_change" class="gt_row gt_right">-5</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006767</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006936</td>
<td headers="HR_change" class="gt_row gt_right">6</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">006936</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008226</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">0.05</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008226</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008268</td>
<td headers="HR_change" class="gt_row gt_right">3</td>
<td headers="temp_change" class="gt_row gt_right">0.00</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008268</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008524</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008524</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008536</td>
<td headers="HR_change" class="gt_row gt_right">0</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">17</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008536</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008574</td>
<td headers="HR_change" class="gt_row gt_right">2</td>
<td headers="temp_change" class="gt_row gt_right">0.00</td>
<td headers="Resp_change" class="gt_row gt_right">3</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008574</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008614</td>
<td headers="HR_change" class="gt_row gt_right">-4</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008614</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008803</td>
<td headers="HR_change" class="gt_row gt_right">-1</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">4</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008803</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008882</td>
<td headers="HR_change" class="gt_row gt_right">4</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-4</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">008882</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009312</td>
<td headers="HR_change" class="gt_row gt_right">5</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">2</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009312</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009429</td>
<td headers="HR_change" class="gt_row gt_right">-7</td>
<td headers="temp_change" class="gt_row gt_right">-0.15</td>
<td headers="Resp_change" class="gt_row gt_right">1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009429</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009433</td>
<td headers="HR_change" class="gt_row gt_right">9</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">2</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009433</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009454</td>
<td headers="HR_change" class="gt_row gt_right">1</td>
<td headers="temp_change" class="gt_row gt_right">0.72</td>
<td headers="Resp_change" class="gt_row gt_right">-1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009454</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009511</td>
<td headers="HR_change" class="gt_row gt_right">-2</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">0</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009511</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009657</td>
<td headers="HR_change" class="gt_row gt_right">-2</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">-1</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009657</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009814</td>
<td headers="HR_change" class="gt_row gt_right">2</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">4</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009814</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
    <tr><td headers="PatientID" class="gt_row gt_right">009982</td>
<td headers="HR_change" class="gt_row gt_right">NA</td>
<td headers="temp_change" class="gt_row gt_right">NA</td>
<td headers="Resp_change" class="gt_row gt_right">NA</td></tr>
  </tbody>
  
  
</table>
</div>
