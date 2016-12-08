# Final Project

#### Contributors: Wynston Hsu, Soichi Tanabe, Yubo Wang

The project is a web application built using Shiny, and is deployed to the Shiny servers and can be accessed by clicking the "Final Project" below:  

[**Final Project**](https://yubo-w.shinyapps.io/final-project-i-mwithher/)  
[**Final Project** (Backup link, in case first link has connection issues)](https://wynhsu.shinyapps.io/final-project-I-mWithHer/)  
Source of data: https://www.kaggle.com/benhamner/2016-us-election
<br>

### If there are connection issues even after trying the second link, refresh the page, or try the first link again.

___

### Documentation:

This web application is based off of data that is published [**Ben Hamner**](https://www.kaggle.com/benhamner/2016-us-election) on Kaggle. These CSV files are made by Ben Hamner. This data comes from very reputable and trustworthy sources, such as CNN and US Census.
<br>

Our web application uses this data and makes visual displays of it. We allow the user to adjust the following inputs:

- Ethnicity: African American, Caucasian, Asian, and Hispanic.
- Education: High School graduate, College graduate.
- Income: household Income.

The data is displayed using three different visualizations:

(Everything is done with Plotly library) 
 
- Bar Graph (To show the number of counties won by each candidate and the overall popular vote.)
- Pie Chart (To show the percentage of counties won by each candidate and the percentage of the overall popular vote each candidate received.)
- Map (Available for the Democratic party. Republican party does not have a map because a dispersion chart is much more effective since there are many more candidates compared to the democratic party.)

All Republican Candidates are accounted for in our visuals. However, in the Democratic party, **we did not include Martin O'Malley** because there were only 752 votes in in total for Martin O'Malley, so it is not a significant enough of votes to be included in the visual data. Therefore, we chose to not include Martin O'Malley.

The web app also has tabs, which allows someone to switch between the Democratic and Republican party. So data is provided for the two largest parties in United States politics.

This web application shows the 2016 United States presidential primary for 46 states. **The 4 states that are missing** are listed below and reasoning is provided:

- Alaska: Alaska does not have presidential primary elections [(Source)](https://www.elections.alaska.gov/vi_hv_vote_pres.php).
- Kansas: Votes by congressional districts, so it cannot be mapped on a county map.
- Minnesota: Data is missing from primary election results file.
- North Dakota: Votes by districts, so cannot be mapped on a county map.