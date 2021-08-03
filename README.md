# OfficialTeachingLab
Official github for all Teaching Lab software development

## Installation

``` r
options(timeout = 9999999)
devtools::install_github("https://github.com/duncangates/TeachingLab", auth_token = "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")
```

## File organization will be as such

- Analysis: All files for analysis of Teaching Lab work
- Data-Clean: All cleaned data that needs to be stored
- Data: All read-in data that should be stored
- ExternalShare: An incomplete shiny app at the moment
- HTML: All html files
- Images: All images created or stored for project usage
- inst: Rmd template building files
- PDF: All PDF created or stored for project usage
- [ParticipantFeedback](https://teachinglabhq.shinyapps.io/ParticipantFeedback/): Course Feedback Visualization Dashboard
- PartnerShare - Intended to be a method for sharing reports with partners
- R: Internal package development .R files for Teaching Lab
- Rmd: Storage for Rmd reports that can be broadly used, such as generated 2021 reports
- Staffing: Staffing system for Teaching Lab facilitators based on PM inputs
- Survey: Tracking system for TL facilitator requests
- Tokens: Extra dependency based on
- test: miscellaneous testing folder
