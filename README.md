# Teaching Lab
Official Github for all Teaching Lab software development, dashboards, data, images, and more.

<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://www.teachinglab.org/">
    <img src="Images/Logos/teachinglab_logo.png" alt="Logo" width="120" height="80">
  </a>

  <h3 align="center">Table of Contents Below</h3>
  </p>
</p>

## Installation

``` r
options(timeout = 9999999)
devtools::install_github("https://github.com/duncangates/TeachingLab", auth_token = "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")
```

## File organization will be as such

- Analysis: All files for analysis of Teaching Lab work
	- 2021-2022
	- 2020-2021
	- DashboardData
	- District 11
	- Footer/Styles
	- Miscellaneous
	- New Mexico
	- Templates
	- 2019-2020
- Dashboards
	- [ParticipantFeedback](https://teachinglabhq.shinyapps.io/ParticipantFeedback/): Course Feedback Visualization Dashboard from pre 2021-2022
	- DiagnosticComplete: An app for tracking diagnostic dashboard completeness
	- ExternalShare: An incomplete shiny app at the moment
	- PartnerShare: Intended to be a method for sharing reports with partners
	- Staffing: Staffing system for Teaching Lab facilitators based on PM inputs
	- Survey: Tracking system for TL facilitator requests
	-YearlyDashboard: Course Survey Tracking Dashboard from SY21-22
	-YearlyDashboard2: Session Survey Tracking Dashboard from SY21-22
- Data-Clean: All cleaned data that needs to be stored
- Data: All read-in data that should be stored
- HTML: All html files
- Images: All images created or stored for project usage
	- D11
	- 2019-2020
	- 2020-2021
	- 2021-2022
	- Backgrounds
	- CRT Review
	- Legacy Graphs
	- Legacy Tables
	- Logos
	- Miscellaneous
	- Robinhood
	- Styles Guide
	- SY19-20 Tables
	- SY20-21 Tables
- inst: Rmd template building files
- R: Internal package development .R files for Teaching Lab
- Rmd: Storage for Rmd reports that can be broadly used, such as generated 2021 reports, as well as a template for Teaching Lab reports
