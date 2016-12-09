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

___

### Our Process:
1. We searched online for data and found 2 CSV files from Ben Hammer. One CSV file displayed data about how many votes went into each candidate from every county, and the other displayed county information such as the ethnical dispersion, education level percentages, and average income.
2. We combined the 2 CSV files using left_join and joining them by the primay and foreign keys.
3. The keys for 2 states, Louisiana and New Hamshire, didn't match for whatever reason so we had to manually add these states.
4. We filtered out and kepy only the category columns we were interested in: data on ethnical percentages, education level percentages, and average income.
5. Once we had our data ready to use, we built our ui.r and server.R to build our Shiny web application.
6. We built the basic layout in ui.r and created a nav bar with multiple tabs.
7. We built a side bar with sliders (for ethnicity, education, and income) in ui.r to take in user input: users can adjust it to manipulate the data based on ethnicity, education, and income.
8. We made a seprate functions.R file under our scripts file to store all our functions.
9. We made a function to take in all the user inputs and filter our data to only keep the range they are looking for. 
10. We made a ByCounty function that groups our data for each column. This was necessary because the CSV file we were working with had multiple rows for different candidates for each county. We combined these rows into 1 and made new columns that would display each of their votes and the winner.
11. We calculated the winner for each county and also the overall popular vote and made 2 bar charts for both of those for the Democratic party.
12. We used those same numbers to make 2 pie charts which displayed percentages. Pie charts made it easier to see the differences between the candidates.
13. We then created a ByState function that grouped the data even further to get the vote dispersions as well as the overall winner for each state.
14. We used this new data to make a stacked bar chart and a choropleth USA map. These 2 charts worked together to effectively visualize the data. A user can now see how close the other candidate was winning for each state with the stacked bar chart, and can also easily see an overview of which candidate won in different states with the map.
15. A quick summary table was added in the middle of these 2 state charts so users can get a quick count of how many states each candidate one.
16. We then re-did all this work for the Republican candidates. The difficulty was that the Republican party had more than just 2 candidates.
17. Donald Trump had won every state so we decided to replace the states map with a horizontal stacked chart instead to display percentages for each Republican candidate.
18. We finally finished!


___

### Challenges:
1. CSV file data had holes in it. A lot of counties didn't have data regarding ethnicity, education, and income. This means a lot of entries were displayed as NA.
   * **Solution:** Before we could do anything else with our data, we filtered out all the NAs with `na.omit()`.
2. Louisiana and New Hamshire's keys didn't match for some reason.
   * **Solution:** We had to manually match the data.
3. Choropleth map for Plotly was meant for showing a range between values: continuum vs dichotomy. For our Democratic State map, we only wanted to display 3 possible values: blue for Hillary, orange for Bernie, and white if the data for that state was not available. 
   * **Solution:** Creating a z-index column which either was a 0 or 1. This way, we were able to do a choropleth map with just 2 values (and white for null), and we set `showscale=FALSE` so you can't tell the map was meant to show a range.
4. Republican party had multiple candidates, not just 2.
   * **Solution:** We had to make additional comparing statements to find the max number of votes to see which candidate was the winner per county and state.
5. Merge conflicts on Github. 
   * **Solution:** All group members needed to understand how to resolve merge conflicts. We also made a github cheat sheet so we can just copy and paste the commands on the paper such as `git pull --rebase origin master `.





