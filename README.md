# Kickstarter data
> This repo is under construction.

## Kickstarter campaign data visualization and prediction

This repo is a showcase of data visualisation and prediction for the data on Kickstarter campaigns, focusing on the factors contributing to a successful launch of a Kickstarter campaign.

There are two separate parts under this repo:

1. Data visualisation with R shinyApp dashboard in R
2. Data analysis and prediction with machine learning in Python



## Data

The dataset employed was obtained from <url>webrobots.io</url>, a data collection website hosting scrapped data.

The data collected consists of CSVs in monthly intervals, from April 2014 to November 2018. The files undergone data cleaning process where multiple CSVs are appended into one file, containing data of all historically launched Kickstarter projects, with a total of 209506 records.

The attributes in the dataset are listed as follows:

| Column                 | Attribute                                                    |
| ---------------------- | ------------------------------------------------------------ |
| backers_count          | number of backers who supported the project                  |
| country                | country of origin of the project                             |
| launched_at            | date of launch                                               |
| state                  | result of the project (successful/failed/others including live, suspended, cancelled) |
| usd_pledged            | amount pledged by backers in usd                             |
| usd_goal               | target amount to be pledged                                  |
| days                   | live period of days where the project is accepting pledge    |
| launch_at_year         | year of launch of the project                                |
| main_category          | the nature of the project                                    |
| usd_pledged_per_backer | average amount for each backer pledged in the project        |



## Data Visualization

The data visualization is made in R with R shinyApp, a web application dashboard. The dashboard with analytics result can be found [here](https://tysonwu.shinyapps.io/Kickstarter/).