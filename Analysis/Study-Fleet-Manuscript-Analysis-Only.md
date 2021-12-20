Study Fleet Manuscript Analysis
================
Andy Jones
Date & Time - 2021 December 20

Getting into the analysis with some summaries

# Table 1

Number of vessels, trips, and efforts by year and reporting type
(contracted study Fleet ‘STFLT’ or other voluntary haul-level reporting
Northeast Cooperative Research Program ‘NCRP’). The general trend in
each category is of rapid increase, however the voluntary records do not
begin until 2011. Vessels reporting at the haul-level that are not part
of the contracted Study Fleet but were participating in another NEFSC
program that mandated haul-level reporting (such as pilot electronic
monitoring programs) are grouped under the NCRP category.

``` r
#Summarizing the numbers for the study fleet and other haul-level reporting
trip_counts <- crpp_trip %>% mutate(YEAR=year(SAIL_DATE_GMT)) %>% dplyr::select(YEAR,SOURCE,TRIP_ID) %>% distinct() %>% group_by(YEAR,SOURCE) %>% tally() %>% pivot_wider(names_from = YEAR,values_from = n)

ves_counts <- crpp_trip %>% mutate(YEAR=year(SAIL_DATE_GMT)) %>% dplyr::select(YEAR,SOURCE,VESSEL_PERMIT_NUM) %>% distinct() %>% group_by(YEAR,SOURCE) %>% tally() %>% pivot_wider(names_from = YEAR,values_from = n)

effort_counts <- crpp_trip %>% mutate(YEAR=year(SAIL_DATE_GMT)) %>% dplyr::select(TRIP_ID,YEAR) %>% left_join(., crpp_effort %>% mutate(effort_id=paste(TRIP_ID,EFFORT_NUM)),by='TRIP_ID') %>% dplyr::select(YEAR,SOURCE,effort_id) %>% distinct() %>% group_by(YEAR,SOURCE) %>% tally() %>% pivot_wider(names_from = YEAR,values_from = n) %>% drop_na(SOURCE)

gear_counts <- crpp_trip %>% mutate(YEAR=year(SAIL_DATE_GMT)) %>% dplyr::select(TRIP_ID,YEAR) %>% left_join(., crpp_effort %>% mutate(effort_id=paste(TRIP_ID,EFFORT_NUM)),by='TRIP_ID') %>% dplyr::select(YEAR,GC_GEAR_CODE,TRIP_ID) %>% distinct() %>% 
  mutate(GC_GEAR_CODE=fct_lump_n(GC_GEAR_CODE,n=1)) %>%
  group_by(YEAR,GC_GEAR_CODE) %>% tally() %>% pivot_wider(names_from = YEAR,values_from = n) %>% drop_na(GC_GEAR_CODE)
```

``` r
#Making two tables from these summaries
bind_rows(tibble(Metric='Number of Vessels',ves_counts),
          tibble(Metric='Number of Trips',trip_counts),
          tibble(Metric='Number of Efforts',effort_counts)) %>% 
  dplyr::select(-`2006`,-`2021`) %>% replace(is.na(.), 0) %>% group_by(Metric) %>% gt() %>% tab_header(title = "Metrics of Study Fleet Participation", subtitle ="From 2007 - 2020")
```

<div id="cakhquzlaa" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="15" class="gt_heading gt_title gt_font_normal" style>Metrics of Study Fleet Participation</th>
    </tr>
    <tr>
      <th colspan="15" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>From 2007 - 2020</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">SOURCE</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2007</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2008</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2009</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2010</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2011</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2012</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2013</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2014</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2015</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2016</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2017</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2018</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2019</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2020</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="15" class="gt_group_heading">Number of Vessels</td>
    </tr>
    <tr><td class="gt_row gt_left">STFLT</td>
<td class="gt_row gt_right">12</td>
<td class="gt_row gt_right">23</td>
<td class="gt_row gt_right">23</td>
<td class="gt_row gt_right">28</td>
<td class="gt_row gt_right">27</td>
<td class="gt_row gt_right">33</td>
<td class="gt_row gt_right">29</td>
<td class="gt_row gt_right">38</td>
<td class="gt_row gt_right">37</td>
<td class="gt_row gt_right">42</td>
<td class="gt_row gt_right">43</td>
<td class="gt_row gt_right">42</td>
<td class="gt_row gt_right">38</td>
<td class="gt_row gt_right">38</td></tr>
    <tr><td class="gt_row gt_left">NCRP</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">1</td>
<td class="gt_row gt_right">1</td>
<td class="gt_row gt_right">16</td>
<td class="gt_row gt_right">49</td>
<td class="gt_row gt_right">30</td>
<td class="gt_row gt_right">33</td>
<td class="gt_row gt_right">28</td>
<td class="gt_row gt_right">35</td>
<td class="gt_row gt_right">40</td>
<td class="gt_row gt_right">36</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="15" class="gt_group_heading">Number of Trips</td>
    </tr>
    <tr><td class="gt_row gt_left">STFLT</td>
<td class="gt_row gt_right">343</td>
<td class="gt_row gt_right">613</td>
<td class="gt_row gt_right">1261</td>
<td class="gt_row gt_right">1413</td>
<td class="gt_row gt_right">1650</td>
<td class="gt_row gt_right">1638</td>
<td class="gt_row gt_right">2041</td>
<td class="gt_row gt_right">1903</td>
<td class="gt_row gt_right">1887</td>
<td class="gt_row gt_right">1988</td>
<td class="gt_row gt_right">2481</td>
<td class="gt_row gt_right">2238</td>
<td class="gt_row gt_right">2169</td>
<td class="gt_row gt_right">2139</td></tr>
    <tr><td class="gt_row gt_left">NCRP</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">3</td>
<td class="gt_row gt_right">1</td>
<td class="gt_row gt_right">99</td>
<td class="gt_row gt_right">981</td>
<td class="gt_row gt_right">1072</td>
<td class="gt_row gt_right">1299</td>
<td class="gt_row gt_right">990</td>
<td class="gt_row gt_right">1467</td>
<td class="gt_row gt_right">1480</td>
<td class="gt_row gt_right">1480</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="15" class="gt_group_heading">Number of Efforts</td>
    </tr>
    <tr><td class="gt_row gt_left">STFLT</td>
<td class="gt_row gt_right">1955</td>
<td class="gt_row gt_right">5429</td>
<td class="gt_row gt_right">8989</td>
<td class="gt_row gt_right">9511</td>
<td class="gt_row gt_right">10048</td>
<td class="gt_row gt_right">9432</td>
<td class="gt_row gt_right">12355</td>
<td class="gt_row gt_right">11492</td>
<td class="gt_row gt_right">13300</td>
<td class="gt_row gt_right">16692</td>
<td class="gt_row gt_right">18231</td>
<td class="gt_row gt_right">15415</td>
<td class="gt_row gt_right">14831</td>
<td class="gt_row gt_right">13083</td></tr>
    <tr><td class="gt_row gt_left">NCRP</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">0</td>
<td class="gt_row gt_right">52</td>
<td class="gt_row gt_right">33</td>
<td class="gt_row gt_right">1024</td>
<td class="gt_row gt_right">10242</td>
<td class="gt_row gt_right">8663</td>
<td class="gt_row gt_right">7001</td>
<td class="gt_row gt_right">4125</td>
<td class="gt_row gt_right">5683</td>
<td class="gt_row gt_right">6348</td>
<td class="gt_row gt_right">6062</td></tr>
  </tbody>
  
  
</table>
</div>

# Table 2

The number of trips by gear type and year. Generally, otter trawl gear
has been the dominant category of gear used by vessels in the program.
Other gears have become more common in recent years suggesting that the
ELB is capable of supporting these fleets.

``` r
#Gear type summaries
gear_counts %>% dplyr::select(-`2006`,-`2021`) %>% replace(is.na(.), 0) %>%
  mutate(GC_GEAR_CODE = as.character(GC_GEAR_CODE)) %>%
  mutate(GC_GEAR_CODE=case_when(GC_GEAR_CODE=="092OTF" ~ 'Otter trawl fish',TRUE ~ GC_GEAR_CODE)) %>%
  rename('Gear type'=GC_GEAR_CODE) %>%
  gt() %>% 
  tab_header(title = "Study Fleet Participation by Gear Type",
             subtitle ="From 2007 - 2020") %>% as_raw_html(., inline_css=FALSE)
```

<div id="fyjuzhklop" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  <style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

</style>
  <table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="15" class="gt_heading gt_title gt_font_normal" style>Study Fleet Participation by Gear Type</th>
    </tr>
    <tr>
      <th colspan="15" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>From 2007 - 2020</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">Gear type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2007</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2008</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2009</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2010</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2011</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2012</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2013</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2014</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2015</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2016</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2017</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2018</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2019</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">2020</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Otter trawl fish</td>
<td class="gt_row gt_right">311</td>
<td class="gt_row gt_right">460</td>
<td class="gt_row gt_right">1056</td>
<td class="gt_row gt_right">1184</td>
<td class="gt_row gt_right">1439</td>
<td class="gt_row gt_right">1371</td>
<td class="gt_row gt_right">1649</td>
<td class="gt_row gt_right">2055</td>
<td class="gt_row gt_right">1930</td>
<td class="gt_row gt_right">2096</td>
<td class="gt_row gt_right">2275</td>
<td class="gt_row gt_right">2348</td>
<td class="gt_row gt_right">2492</td>
<td class="gt_row gt_right">2619</td></tr>
    <tr><td class="gt_row gt_left">Other</td>
<td class="gt_row gt_right">31</td>
<td class="gt_row gt_right">154</td>
<td class="gt_row gt_right">214</td>
<td class="gt_row gt_right">264</td>
<td class="gt_row gt_right">207</td>
<td class="gt_row gt_right">272</td>
<td class="gt_row gt_right">469</td>
<td class="gt_row gt_right">792</td>
<td class="gt_row gt_right">995</td>
<td class="gt_row gt_right">1165</td>
<td class="gt_row gt_right">1226</td>
<td class="gt_row gt_right">1361</td>
<td class="gt_row gt_right">1177</td>
<td class="gt_row gt_right">1047</td></tr>
  </tbody>
  
  
</table>
</div>

# Figure 1

A conceptual diagram of the data flow and purpose of the Study Fleet
program. Starting with the reporting vessels and fishermen (yellow)
information flows to the Science Center (purple and blue) where it is
curated and developed into analytical products for science and
management. These products are presented as part of the assessment
process (darker green) and then communicated back to participating
fishermen (light green). This cycle is simplified in many ways as
iterative loops are likely to improve the products and assessments.

``` r
im <- load.image('C:/Users/andrew.jones/Desktop/FMRD_CRB_SF_Overview_MS/Analysis/images/Study_Fleet_MS_Figure_1.JPG')
plot(im, axes=FALSE)
```

![](Figs/Loading%20figure%20image-1.png)<!-- -->

# Figure 2

The number of trips that were collected by the ELB system. Haul-by-haul
reporting (high-resolution) trips are shown in purple, and trip
reporting (lower resolution) in yellow. This shows the expansion of the
ELB for regulatory and scientific purposes.

``` r
#Plotting out trip count figure
#For Study Fleet data
trip_list %>%  mutate(YEAR=year(SAIL_DATE_LCL)) %>% 
  group_by(YEAR,REPORT_SOURCE) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  mutate(REPORT_SOURCE2=case_when(REPORT_SOURCE=='HBH'~'Haul-level',TRUE ~ 'Trip-level')) %>%
  #pivot_wider(names_from = YEAR,values_from = n)
  ggplot(aes(x=YEAR,y=n,color=REPORT_SOURCE2)) + geom_point(size=4,shape=1) + geom_path(size=2) +
  labs(x='Year',y='Number of trips',color='Report type') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
  scale_color_manual(values=c('#352A87','#FBCD2D')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](Figs/A%20figure%20of%20trip%20counts-1.png)<!-- -->

#Figure 3 The breakdown of Study Fleet number of vessels, trips, and
hauls by target species through time. The trends through time for each
level of the data are shown in panels A, B, and C. The annual
proportions of vessels, trips, and hauls are shown in panels D, E, and
F.

``` r
#Breaking out records by target (trips/)
target_haul_trend <- plot_data_comb_ves %>%
  mutate(target=as.character(target)) %>%
  mutate(target=case_when(target=='LOLIGO SQUID' ~ 'LONGFIN SQUID',
                          target=='ILLEX SQUID'~'SHORTFIN SQUID',TRUE ~ target)) %>%
  filter(source=='crb') %>%
  drop_na(target) %>%
  mutate(target=fct_lump_n(target,n=10)) %>%
  dplyr::select(haul_id,YEAR,target) %>%
  distinct() %>%
  group_by(YEAR,target) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  ggplot(aes(x=YEAR,y=n,colour=target)) + geom_point(size=2,shape=1) + geom_path(size=1) + 
  labs(x='Year',y='Number of hauls',color='Target species') +
  scale_x_continuous(breaks=c(2007:2020)) + 
  theme_bw() +
  theme(legend.position = 'None') +
  scale_colour_manual(values=as.vector(parula(11))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

target_haul_prop <- plot_data_comb_ves %>%
  mutate(target=as.character(target)) %>%
  mutate(target=case_when(target=='LOLIGO SQUID' ~ 'LONGFIN SQUID',
                          target=='ILLEX SQUID'~'SHORTFIN SQUID',TRUE ~ target)) %>%
  filter(source=='crb') %>%
  drop_na(target) %>%
  mutate(target=fct_lump_n(target,n=10)) %>%
  dplyr::select(haul_id,YEAR,target) %>%
  distinct() %>%
  group_by(YEAR,target) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  ggplot(aes(x=YEAR,y=n,fill=target)) + geom_col(position='fill') +
  labs(x='Year',y='Percentage of hauls',fill='Target species') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values=as.vector(parula(11))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

target_trip_trend <- plot_data_comb_ves %>%
  mutate(target=as.character(target)) %>%
  mutate(target=case_when(target=='LOLIGO SQUID' ~ 'LONGFIN SQUID',
                          target=='ILLEX SQUID'~'SHORTFIN SQUID',TRUE ~ target)) %>%
  filter(source=='crb') %>%
  drop_na(target) %>%
  mutate(target=fct_lump_n(target,n=10)) %>%
  dplyr::select(trip_id,YEAR,target) %>%
  distinct() %>%
  group_by(YEAR,target) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  ggplot(aes(x=YEAR,y=n,colour=target)) + geom_point(size=2,shape=1) + geom_path(size=1) + 
  labs(x='Year',y='Number of trips',color='Target species') +
  scale_x_continuous(breaks=c(2007:2020)) +
    theme_bw() +
  theme(legend.position = 'None') +
  scale_colour_manual(values=as.vector(parula(11))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

target_trip_prop <- plot_data_comb_ves %>%
  mutate(target=as.character(target)) %>%
  mutate(target=case_when(target=='LOLIGO SQUID' ~ 'LONGFIN SQUID',
                          target=='ILLEX SQUID'~'SHORTFIN SQUID',TRUE ~ target)) %>%
  filter(source=='crb') %>%
  drop_na(target) %>%
  mutate(target=fct_lump_n(target,n=10)) %>%
  dplyr::select(trip_id,YEAR,target) %>%
  distinct() %>%
  group_by(YEAR,target) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  ggplot(aes(x=YEAR,y=n,fill=target)) + geom_col(position='fill') +
  labs(x='Year',y='Percentage of trips',fill='Target species') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values=as.vector(parula(11))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

target_vessel_trend <- plot_data_comb_ves %>%
  mutate(target=as.character(target)) %>%
  mutate(target=case_when(target=='LOLIGO SQUID' ~ 'LONGFIN SQUID',target=='ILLEX SQUID'~'SHORTFIN SQUID',TRUE ~ target)) %>%
  filter(source=='crb') %>%
  drop_na(target) %>%
  mutate(target=fct_lump_n(target,n=10)) %>%
  dplyr::select(permit,YEAR,target) %>%
  distinct() %>%
  group_by(YEAR,target) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  ggplot(aes(x=YEAR,y=n,colour=target)) + geom_point(size=2,shape=1) + geom_path(size=1) + 
  labs(x='Year',y='Percentage of vessels',color='Target species') +
  scale_x_continuous(breaks=c(2007:2020)) +
  theme_bw() +
  theme(legend.position = 'None') +
  scale_colour_manual(values=as.vector(parula(11))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

target_vessel_prop <- plot_data_comb_ves %>%
  mutate(target=as.character(target)) %>%
  mutate(target=case_when(target=='LOLIGO SQUID' ~ 'LONGFIN SQUID',
                          target=='ILLEX SQUID'~'SHORTFIN SQUID',TRUE ~ target)) %>%
  filter(source=='crb') %>%
  drop_na(target) %>%
  mutate(target=fct_lump_n(target,n=10)) %>%
  dplyr::select(permit,YEAR,target) %>%
  distinct() %>%
  group_by(YEAR,target) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  ggplot(aes(x=YEAR,y=n,fill=target)) + geom_col(position='fill') +
  labs(x='Year',y='Proiportion of vessels',fill='Target species') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
  scale_fill_manual(values=as.vector(parula(11))) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


((target_vessel_trend + target_trip_trend + target_haul_trend) / (target_vessel_prop + target_trip_prop + target_haul_prop)) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') + theme(legend.position = 'bottom')
```

![](Figs/Looking%20at%20the%20study%20fleet%20summaries%20by%20target%20species-1.png)<!-- -->

# Figure 4

Trends in annual trips (A) and hauls (B) for the Study Fleet program are
shown in blue. For comparison the annual number of trips and hauls from
trawl vessels is shown from another large monitoring program in the
region, the fishery observer programs. The goals and sampling schemes of
these programs are quite different, but together they represent a large
amount of fishery dependent data from the region. Of note is the dip in
total trips in the observer program in 2020, a result of a pause in
operations for the COVID-19 pandemic.

``` r
#Plotting out a comparison to the observer programs (trips and hauls)
a <- plot_data_comb_ves %>%
  dplyr::select(trip_id,YEAR,source) %>%
  distinct() %>%
   group_by(YEAR,source) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  mutate(source2=case_when(source=='obs'~'Observer',TRUE ~ 'Study Fleet')) %>%
  #pivot_wider(names_from = YEAR,values_from = n)
  ggplot(aes(x=YEAR,y=n,color=source2)) + geom_point(size=4,shape=1) + geom_path(size=2) +
  labs(x='Year',y='Number of trips',color='Data source') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
   scale_color_manual(values=c('#352A87','#FBCD2D')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

b <- plot_data_comb_ves %>%
  dplyr::select(haul_id,YEAR,source) %>%
  distinct() %>%
   group_by(YEAR,source) %>% tally() %>%
  filter(YEAR > 2006, YEAR < 2021) %>%
  mutate(source2=case_when(source=='obs'~'Observer',TRUE ~ 'Study Fleet')) %>%
  #pivot_wider(names_from = YEAR,values_from = n)
  ggplot(aes(x=YEAR,y=n,color=source2)) + geom_point(size=4,shape=1) + geom_path(size=2) +
  labs(x='Year',y='Number of hauls',color='Data source') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
  scale_color_manual(values=c('#352A87','#FBCD2D')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

library(patchwork)
a/b + plot_annotation(tag_levels = 'A')
```

![](Figs/Plots%20comparing%20the%20number%20of%20records%20in%20the%20study%20fleet%20and%20observer%20programs-1.png)<!-- -->

# Figure 5

Maps showing the spatial distribution of Study Fleet trawl records for
2010 - 2020 gridded into 5-minute squares. In panel A, the total number
of hauls are shown. For comparison the distribution of observer records
for trawl gear are shown in panel B. The data collected by the programs
spans similar geographic ranges, however the Study Fleet data set
includes more records from southern New England and fewer records from
the Gulf of Maine. Panel C shows the total number of trips by Study
Fleet vessels, indicating a high degree of overlap between trip and haul
level record totals. Panel D shows the total catch by Study Fleet
vessels (lbs), again showing a higher degree of similarity to the
distributions of trips and hauls. In all panels cells with fewer than
three unique permits (records from fewer than three vessels) have been
omitted.

``` r
#Making more maps that remove the cells with fewer than three permits
#Subsetting data down to just a single gear type (otter trawl)
plot_data_comb_map <- plot_data_comb_ves %>% 
  filter(str_sub(gear_code_vtr,1,2)=='OT') %>% drop_na('start_lon','start_lat')

#Pulling in bathy info and setting map boundaries
#Depths are in meters
atl <- marmap::getNOAA.bathy(-76.5,-65, 34.5, 45, res = 1, keep=TRUE)
```

    ## Registered S3 methods overwritten by 'adehabitatMA':
    ##   method                       from
    ##   print.SpatialPixelsDataFrame sp  
    ##   print.SpatialPixels          sp

    ## File already exists ; loading 'marmap_coord_-76.5;34.5;-65;45_res_1.csv'

``` r
lons = c(-75.5, -66)
lats = c(35.5, 44)

#Making an object to show the coastlines
reg = map_data("world2Hires")
reg = subset(reg, region %in% c('Canada', 'USA'))
reg$long = (360 - reg$long)*-1

#Making a raster grid
boxsize <- 10
xmn <- -77
xmx <- -57
ymn <- 30
ymx <- 50
bmin <- 3
nrow <- (ymx-ymn)*60/boxsize
ncol <- (xmx-xmn)*60/boxsize
crs <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

#Setting the raser box size
r = raster(nrow=(ymx-ymn)*60/boxsize
               , ncol= (xmx-xmn)*60/boxsize
               , xmn = xmn
               , xmx = xmx
               , ymn = ymn
               , ymx = ymx
               , crs = crs) 

#Setting up the coordinates and CRS in the data
coordinates(plot_data_comb_map) <- c('start_lon','start_lat')
plot_data_comb_map@proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

####
#Making rasters of the study fleet data
crb_plot_data_comb_map_raster_weight <- rasterize(plot_data_comb_map %>% st_as_sf() %>% filter(source=='crb'), r, 'TOT_CATCH',fun=sum)
crb_plot_data_comb_map_raster_permits <- rasterize(plot_data_comb_map %>% st_as_sf() %>% filter(source=='crb'), r, 'permit', fun=function(x, ...) length(unique(x)))
crb_plot_data_comb_map_raster_efforts <- rasterize(plot_data_comb_map %>% st_as_sf() %>% filter(source=='crb'), r, 'haul_id', fun=function(x, ...) length(unique(x)))
crb_plot_data_comb_map_raster_trips <- rasterize(plot_data_comb_map %>% st_as_sf() %>% filter(source=='crb'), r, 'trip_id', fun=function(x, ...) length(unique(x)))

#Stacking the study fleet rasters
crb_stack <- stack(crb_plot_data_comb_map_raster_efforts,crb_plot_data_comb_map_raster_permits,crb_plot_data_comb_map_raster_weight,crb_plot_data_comb_map_raster_trips) %>% st_as_stars(values=layer) %>% st_as_sf() %>% filter(layer.2>3) %>% drop_na(layer.1)

#####
#Making rasters for the observer data 
obs_plot_data_comb_map_raster_weight <- rasterize(plot_data_comb_map %>% st_as_sf() %>% filter(source=='obs'), r, 'TOT_CATCH',fun=sum)
obs_plot_data_comb_map_raster_permits <- rasterize(plot_data_comb_map %>% st_as_sf() %>% filter(source=='obs'), r, 'permit', fun=function(x, ...) length(unique(x)))
obs_plot_data_comb_map_raster_efforts <- rasterize(plot_data_comb_map %>% st_as_sf() %>% filter(source=='obs'), r, 'haul_id', fun=function(x, ...) length(unique(x)))

#Stacking the observer rasters
obs_stack <- stack(obs_plot_data_comb_map_raster_efforts,obs_plot_data_comb_map_raster_permits) %>% st_as_stars(values=layer) %>% st_as_sf() %>% filter(layer.2>3) %>% drop_na(layer.1)

#Making an observer plot
observer <- ggplot() +
  scale_fill_gradientn(colours = brewer.purples(10),guide='colourbar',trans='log10') +
  geom_sf(data=obs_stack,aes(fill=layer.1)) +
  geom_polygon(data = reg, aes(x=long, y = lat, group = group),fill='grey60',colour='dark gray') +
  geom_contour(data = atl,
                aes(x=x, y=y, z=z),
                breaks=c(-50,-100,-1000),
                size=c(0.5),
                colour="black",alpha=0.5) +
  coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=NAD83") +
    labs(x='Longitude',y='Latitude',fill='Total\nhauls') + 
  theme_bw() +
  theme(legend.position = c(0.85, 0.25))

#Making a study fleet plots
study_fleet <- ggplot() +
  scale_fill_gradientn(colours = brewer.blues(10),guide='colourbar',trans='log10') +
  geom_sf(data=crb_stack,aes(fill=layer.1)) +
  geom_polygon(data = reg, aes(x=long, y = lat, group = group),fill='grey60',colour='dark gray') +
  geom_contour(data = atl,
                aes(x=x, y=y, z=z),
                breaks=c(-50,-100,-1000),
                size=c(0.5),
                colour="black",alpha=0.5) +
  coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
    labs(x='Longitude',y='Latitude',fill='Total\nhauls') + 
  theme_bw() +
  theme(legend.position = c(0.85, 0.25))

study_fleet_w <- ggplot() +
  scale_fill_gradientn(colours = brewer.blues(10),guide='colourbar',trans='log10') +
  geom_sf(data=crb_stack,aes(fill=layer.3)) +
  geom_polygon(data = reg, aes(x=long, y = lat, group = group),fill='grey60',colour='dark gray') +
  geom_contour(data = atl,
                aes(x=x, y=y, z=z),
                breaks=c(-50,-100,-1000),
                size=c(0.5),
                colour="black",alpha=0.5) +
  coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
    labs(x='Longitude',y='Latitude',fill='Total\nweight (lbs)') + 
  theme_bw() +
  theme(legend.position = c(0.85, 0.25))

study_fleet_t <- ggplot() +
  scale_fill_gradientn(colours = brewer.blues(10),guide='colourbar',trans='log10') +
  geom_sf(data=crb_stack,aes(fill=layer.4)) +
  geom_polygon(data = reg, aes(x=long, y = lat, group = group),fill='grey60',colour='dark gray') +
  geom_contour(data = atl,
                aes(x=x, y=y, z=z),
                breaks=c(-50,-100,-1000),
                size=c(0.5),
                colour="black",alpha=0.5) +
  coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
    labs(x='Longitude',y='Latitude',fill='Total\ntrips') + 
  theme_bw() +
  theme(legend.position = c(0.85, 0.25))


#Putting the plots together
((study_fleet + observer) + plot_annotation(tag_levels = 'A')) / (study_fleet_t + study_fleet_w) + plot_annotation(tag_levels = 'A')
```

![](Figs/Making%20rasters%20of%20data%20and%20mapping%20out%20records-1.png)<!-- -->

# Figure 6

The proportion of each species’ regional landings reported through Study
Fleet (purple), relative to the trend in total commercial landings of
the species (yellow). Here six species of high commercial interest are
shown. The trend for most species is of an increasing proportion in the
Study Fleet data set relative to the whole fishery. However, as in the
case of Atlantic cod this is partially driven by a decreasing trend in
total catch.

``` r
#Extracting the VTR data

#This runs into memory problems
#Making a sum of landings for the region for specific species
# dealer_sum <- do.call(rbind.data.frame,cfders_spp) %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
#   filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
#   mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
#   dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE))


dealer_sum_2007 <- cfders_spp[[1]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE))
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2007 <- cfders_spp[[2]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2008 <- cfders_spp[[3]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2009 <- cfders_spp[[4]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2010 <- cfders_spp[[1]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2011 <- cfders_spp[[5]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2012 <- cfders_spp[[6]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2013 <- cfders_spp[[7]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2014 <- cfders_spp[[8]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2015 <- cfders_spp[[9]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2016 <- cfders_spp[[10]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2017 <- cfders_spp[[11]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2018 <- cfders_spp[[12]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2019 <- cfders_spp[[13]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum_2020 <- cfders_spp[[14]] %>% dplyr::select(SPECIES_ITIS,YEAR,SPPLNDLB) %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  mutate(YEAR=as.numeric(YEAR)) %>% group_by(YEAR,SPECIES_ITIS) %>% 
  dplyr::summarise(SPPLNDLB=sum(SPPLNDLB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
dealer_sum <- rbind(dealer_sum_2020,dealer_sum_2019,dealer_sum_2018,dealer_sum_2017,
                    dealer_sum_2016,dealer_sum_2015,dealer_sum_2014,dealer_sum_2013,
                    dealer_sum_2012,dealer_sum_2011,dealer_sum_2010,dealer_sum_2009,
                    dealer_sum_2008,dealer_sum_2007)

#Making a sum for the study fleet
study_fleet_sum <- pulled_data_edit_cf %>%
  filter(SPECIES_ITIS %in% c('172735','082372','082521','169182','164744','172908','172877','164712')) %>%
  group_by(YEAR,SPECIES_ITIS) %>% sample_frac(0.5) %>%
  dplyr::summarise(SUM_HAIL_AMOUNT_LB=sum(HAIL_AMOUNT_LB,na.rm=TRUE)) 
```

    ## `summarise()` has grouped output by 'YEAR'. You can override using the `.groups` argument.

``` r
#Putting these time
dealer_sum %>% inner_join(.,study_fleet_sum) %>% mutate(prop=SUM_HAIL_AMOUNT_LB/SPPLNDLB) %>%
  mutate(SPECIES_ITIS=case_when(SPECIES_ITIS=='082521'~'82521',SPECIES_ITIS=='082372'~'82372',TRUE~SPECIES_ITIS)) %>%
  left_join(.,OBSPEC %>% 
              filter(SPECIES_ITIS %in% c('172735','82372','82521',
                                         '169182','164744','172908','172877','164712')) %>%
            dplyr::select(SPECIES_ITIS,COMNAME) %>% distinct()
            ) %>%
  filter(COMNAME!='SQUID EGGS, ATL LONG-FIN',COMNAME!='FLOUNDER, AMERICAN PLAICE') %>%
  group_by(COMNAME) %>%
  mutate(SPPLNDLB=rescale(SPPLNDLB)) %>%
  ggplot() +
  geom_point(aes(x=YEAR,y=prop,color='Prop. in SF'),colour='#352A87', size=3,shape=1) +
  geom_line(aes(x=YEAR,y=prop,color='Prop. in SF'),colour='#352A87', size=1.2) +
  geom_point(aes(x=YEAR,y=SPPLNDLB/5,color='Total trend'),colour='#FBCD2D', size=3) +
  geom_line(aes(x=YEAR,y=SPPLNDLB/5,color='Total trend'),colour='#FBCD2D', size=1.2) +
  labs(x='Year',y='Proportion of total species landings (Open)') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~COMNAME) +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Total landings trend (Closed)")) +
  theme(axis.line.y.right = element_line(color = "#FBCD2D"),
       axis.ticks.y.right = element_line(color = "#FBCD2D"),
       #axis.text.y.right = element_text(color = "#FBCD2D"),
       #axis.title.y.right = element_text(color="#FBCD2D"),
       axis.line.y.left = element_line(color = "#352A87"),
       axis.ticks.y.left = element_line(color = "#352A87"),
       axis.text.y.left = element_text(color = "#352A87")) 
```

    ## Joining, by = c("YEAR", "SPECIES_ITIS")

    ## Joining, by = "SPECIES_ITIS"

![](Figs/Comparisons%20of%20study%20fleet%20landings%20to%20total%20landings-1.png)<!-- -->

#Figure 7 The trend in oceanographic records through time and across
space. Panel A shows the density of sampling across space for all years
sampled (2010-2019). Cells with fewer than three unique permits (records
from fewer than three vessels) have been omitted. Panel B shows the
number of fishing efforts with co-registered oceanographic data from
2010 to 2020. Data from 2019 and 2020 (shaded gray) are still being
processed and sums represent incomplete totals.

``` r
#Making a similar plot for GTE data
plot_data_comb_ves_gte_filter <- plot_data_comb_ves_gte %>% filter(GTE_DATA=='YES') %>% 
  filter(YEAR>2009,YEAR<2021) %>% drop_na('start_lon','start_lat')

coordinates(plot_data_comb_ves_gte_filter) <- c('start_lon','start_lat')
plot_data_comb_ves_gte_filter @proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

crb_plot_data_comb_raster_efforts_gte <- rasterize(plot_data_comb_ves_gte_filter, r, 'haul_id', fun=function(x, ...) length(unique(x)))
crb_plot_data_comb_raster_permits_gte <- rasterize(plot_data_comb_ves_gte_filter, r, 'permit', fun=function(x, ...) length(unique(x)))

crb_stack_gte  <- stack(crb_plot_data_comb_raster_efforts_gte,crb_plot_data_comb_raster_permits_gte) %>% st_as_stars(values=layer) %>% st_as_sf() %>% filter(layer.2>3) %>% drop_na(layer.1)

#Map of GTE data
map_gte_2 <- ggplot() +
  scale_fill_gradientn(colours = brewer.blues(10),guide='colourbar',trans='log10') +
  geom_sf(data=crb_stack_gte,aes(fill=layer.1)) +
  geom_polygon(data = reg, aes(x=long, y = lat, group = group),
               fill='grey60',colour='white') +
  geom_contour(data = atl,
                aes(x=x, y=y, z=z),
                breaks=c(-50,-100,-1000),
                size=c(0.5),
                colour="black",alpha=0.5) +
  coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
    labs(x='Longitude',y='Latitude',fill='Total\nhauls') + 
  theme_bw() +
  theme(legend.position = c(0.85, 0.25))

#Trends in GTE records by target species
gte_trends <- plot_data_comb_ves_gte %>% filter(GTE_DATA=='YES',YEAR<2020) %>%
  mutate(target=as.character(target)) %>%
  mutate(target=case_when(target=='LOLIGO SQUID' ~ 'LONGFIN SQUID',
                          target=='ILLEX'~'SHORTFIN SQUID',TRUE ~ target)) %>%
  mutate(target=fct_lump_n(target,n=10)) %>%
  drop_na(target) %>%
  dplyr::select(haul_id,YEAR,target) %>% distinct() %>% 
  group_by(YEAR,target) %>% tally() %>%
  filter(YEAR>2009) %>% ggplot(aes(x=YEAR,y=n,colour=target)) + 
  geom_vline(xintercept = 2019.5,size=20,alpha=0.5) +
  geom_point(size=2) + geom_line(size=1.2) +
    labs(x='Year',y='Number of oceanographic records',colour='Target species') +
  scale_x_continuous(breaks=c(2010:2020),limits=c(2010,2020)) + theme_bw() +
  scale_colour_manual(values=as.vector(parula(11))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  
map_gte_2 + gte_trends + plot_annotation(tag_levels = 'A')
```

![](Figs/Makimh%20GTE%20plots-1.png)<!-- -->

# Appendix C

Participation in the haul-by-haul program between 2007 and 2020. The
number of trips in each year for each vessel is shown with shading
(darker colors indicate more trips in a given year). Vessels are broken
out by the state which the vessel is associated with. Other includes
vessels from NY, NH, and ME.

``` r
#Bringing together participant data
vessels_of_interest <- pulled_data_edit_cf$VESSEL_PERMIT_NUM %>% unique()

#Making a count of trips per year per vessel
participation_data <- pulled_data_edit_cf %>% 
  dplyr::select(YEAR,VESSEL_NAME,VESSEL_PERMIT_NUM,TRIP_ID) %>% 
  mutate(VESSEL_NAME = toupper(VESSEL_NAME)) %>%
  distinct() %>% group_by(YEAR,VESSEL_NAME,VESSEL_PERMIT_NUM) %>% tally() %>%
  filter(YEAR  > 2006) %>% 
  mutate(year_sum=sum(n)) %>%
  ungroup() %>%
  group_by(VESSEL_NAME,VESSEL_PERMIT_NUM) %>% 
  mutate(sum=sum(n)) %>%
  mutate(min_year=min(YEAR)) %>%
  #filter(sum > 500) %>%
  arrange(-sum)

VESSEL_NUMBER <- participation_data %>% ungroup() %>% 
  arrange(year_sum) %>% dplyr::select(VESSEL_NAME,VESSEL_PERMIT_NUM) %>%
  distinct() %>% mutate(VESSEL_NUM=row_number())

VESSEL_PORT <- VESSEL_NUMBER %>%
  left_join(.,vessel_info %>% mutate(VP_NUM=as.character(VP_NUM)) %>%
              filter(VP_NUM %in% vessels_of_interest,AP_YEAR>2006) %>%
              ungroup() %>%
              dplyr::select(VES_NAME,VP_NUM,PPST) %>% distinct() %>% 
              group_by(VP_NUM,VES_NAME) %>% slice(1), 
            by=c("VESSEL_PERMIT_NUM"="VP_NUM","VESSEL_NAME"="VES_NAME"))


#This is the final plot of participation by state
participation_data %>% left_join(VESSEL_PORT) %>% filter(YEAR < 2021) %>%
  ungroup() %>%
  mutate(Port=fct_lump(PPST,3)) %>%
  drop_na() %>%
  ggplot(aes(x=YEAR,y=reorder(VESSEL_NUM,-min_year))) + 
  geom_tile(aes(fill=n),colour='Gray') +
  labs(x='Year',y='Vessels',fill='Number of trips') +
  scale_x_continuous(breaks=c(2007:2020)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.75, hjust=1),
        axis.text.y = element_text(size=10),
        legend.position = 'bottom') +
  facet_wrap(~Port,scales='free_y') +
  scale_fill_gradient(low=as.vector(brewer.blues(3)[1]),
                      high=as.vector(brewer.blues(3)[3]))
```

    ## Joining, by = c("VESSEL_NAME", "VESSEL_PERMIT_NUM")

![](Figs/Making%20plots%20of%20ind%20vessel%20participation-1.png)<!-- -->
