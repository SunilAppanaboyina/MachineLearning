Problem Statement

Country Beeristan, a high potential market, accounts for nearly 10% of Stallion & Co.’s global beer sales. 
Stallion & Co. has a large portfolio of products distributed to retailers through wholesalers (agencies). 
There are thousands of unique wholesaler-SKU/products combinations. In order to plan its production and distribution 
as well as help wholesalers with their planning, it is important for Stallion & Co. to have an accurate estimate of 
demand at SKU level for each wholesaler.
Currently demand is estimated by sales executives, who generally have a “feel” for the market and predict the net effect of forces of supply, demand and other external factors based on past experience. The more experienced a sales exec is in a particular market, the better a job he does at estimating. Joshua, the new Head of S&OP for Stallion & Co. just took an analytics course and realized he can do the forecasts in a much more effective way. He approaches you, the best data scientist at Stallion, to transform the exercise of demand forecasting.

Datasets

You are provided with the following data:

price_sales_promotion.csv: ($/hectoliter) Holds the price, sales & promotion in dollar value per hectoliter at Agency-SKU-month level

historical_volume.csv: (hectoliters) Holds sales data at Agency-SKU-month level from Jan 2013 to Dec 2017

weather.csv: (Degree Celsius) Holds average maximum temperature at Agency-month level

industry_soda_sales.csv: (hectoliters) Holds industry level soda sales

event_calendar.csv: Holds event details (sports, carnivals, etc.)

industry_volume.csv: (hectoliters) Holds industry actual beer volume

demographics.csv: Holds demographic details (Yearly income in $)


Submission Formats

Volume_forecast.csv: You need to first forecast the demand volume for Jan’18 of all agency-SKU combination.

sku_recommendation.csv: Secondly, you need to suggest 2 SKUs which can be sold by Agency06 & Agency14. These two agencies are new and company wants to find out which two products would be the best products for these two agencies.
Summarize the analysis carried out in a one pager mentioning the techniques used for forecasting and the approach to arrive at the suggested SKUs for Agency06 & Agency14.


