# Final project proposal

Evaluate the Main driver of Amazon Forest Degradation

Group member: Jiawen Tang, Xiangrong (Mark) Sun

The Amazon rainforest is the single largest carbon storage ecosystem on land and provides extremely valuable ecological services. But the Amazon rainforest is also the fastest shrinking forest area, and there is strong evidence that human activity and climate change are playing a major role. When protecting forests, we need to understand what is worth protecting and how to do it effectively. We will explore the causes and future of Amazon forest degradation from the perspective of climate change.

According to The article "The drivers and impacts of Amazon forest degradation" (DOI: 10.1126/science.abp8622), drought and fire are two of the four main causes of Amazon forest degradation. Data source (https://doi.org/10.25824/redu/EGJAYI) to store the 2001-2018 forest degradation due to the reasons of the observed data. Projections for 2019-2050 are plotted for different pathways. The format of the data is divided into a minimum of 900*900 meters to 50*50km depending on the resolution. The data is stored in Tag Image File Format.

The three questions we will answer and explain in our project are:
1. What is the climate drivers of the Amazon Forest Degradation based on the observation data?
2. What is different in the drivers of the Degradation under climate and deforestation governance (GOV) and business-as-usual (BAU) scenarios?
3. What are the estimating carbon emissions in the 2019–2050 period resulting from deforestation (DFT) and degradation (DGR)?

We will display in the form of maps and charts, organize and visualize relevant data (including Cluster data and spatial data), and restore graphs as much as possible.






## Project Guidelines

### Project questions must illustrate all of the following tasks:

- Some form of data access / reading into R
- Data tidying preparation
- Initial data visualization
- Use of GitHub
- Reproducible execution with use of Travis
- RMarkdown writeup, with final submission as a nicely formatted PDF document that includes code and results.
- Overall clean and clear presentation of repository, code, and explanations.

### and at least three of the following skills (this list may be modified/extended):

- Use of at least 5 `dplyr` verbs / functions
- Writing / working with custom R functions
- Creating an R package for functions used in the analysis
- Interaction with an API
- Use of regular expressions
- Use of an external relational database
- Preparing processed data for archiving / publication
- Parsing extensible data formats (JSON, XML)
- Use of spatial vector data (`sf` package) and visualization of spatial data
- Creation of an R package
- Expansion of ggplot functions (to include more than default characteristics)
- Making layout and presentation into secondary output (e.g. .pdf, website) - should enhance presentaiton
- use lintr to clean code (checks adherence to a given style, syntax errors and possible semantic issues)

# Final Rubric 30 pts total

 - 5pts Proposal, turned in on time and with team member names, background, dataset, and 3 potential questions.

 - 10pts Polished github repository, including:
	 -  3pt updated readme with functional travis badge 
	 -  2pt passing travis build 
	 -  2pt clean and well formatted output document (html, pdf, or md with associated files). 
	 -  3pt enough supporting text that we can easily understand the project undertaken.
	 
 - 15 pts Project Substance: Objectives, Code, Visualization. Do you meet all of the required project objectives and at least 3 of the supplementary objectives.
	 - 15pts: exceptional
	 - 13pts: adequate and complete
	 - 11pts: adequate 2 questions, meeting 3 supplementary objectives
	 - 9pts: adequate 2 q, meeting 1-2 supplementary objectives
	 - 7pts: adequate 1 q, meeting 3 supplementary objectives
	 - 5pts: adequate 1q, meeting 1-2 supplementary objectives
