Problem Statement
Big data analytics allows you to analyse data at scale. It has applications in almost every industry in the world. Let’s consider an unconventional application that you wouldn’t ordinarily encounter.

New York City is a thriving metropolis. Just like most other metros that size, one of the biggest problems its citizens face, is parking. The classic combination of a huge number of cars, and a cramped geography is the exact recipe that leads to a huge number of parking tickets.

In an attempt to scientifically analyse this phenomenon, the NYC Police Department has collected data for parking tickets. Out of these, the data files from 2014 to 2017 are publicly available on Kaggle. We will try and perform some exploratory analysis on this data. Spark will allow us to analyse the full files at high speeds, as opposed to taking a series of random samples that will approximate the population.

For the scope of this analysis, we wish to compare phenomenon related to parking tickets over three different years - 2015, 2016, 2017. All the analysis steps mentioned below should be done for 3 different years. Each metric you derive should be compared across the 3 years.

Data dictionary: https://www.kaggle.com/new-york-city/nyc-parking-tickets/data

Data: https://www.kaggle.com/new-york-city/nyc-parking-tickets/data
For this analysis only files corresponding to 2015, 2016, 2017 were used.

Steps:
A> Uploading data into AWS S3 
It is standard practice to use data from AWS S3 while performing analysis in Spark. So data was uploaded into S3.

B> Analysis
Analysis was performed on RStudio mounted on the AWS cluster, using the SparkR library. The analysis was done for all the 3 years, and metrics and insights across the years were compared.
