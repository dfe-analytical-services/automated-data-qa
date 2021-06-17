# Automated data QA

## Introduction

These template snippets cover the <b>minimum</b> QA checks that teams should be carrying out as part of producing files for their statistical releases, in order to meet [good practice](https://rsconnect/rsc/stats-production-guidance/rap.html#basic-automated-qa) for DfE's RAP standards. These snippets will be built in to our [DfE data QA app](https://rsconnect/rsc/dfe-published-data-qa/) in the near future

## Requirements

### i. Software requirements 

- Installation of R 3.6.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for adapting code)

- R at a beginner level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/) and [DfE analytics academy](https://trello.com/b/QdDx3VmA/analytics-academy-an-r-training-course)

### iii. Access

- This code is hosted publicly on github, and can be shared outside of DfE.

---

## How to use

The templates are written as functions, which you can apply to your data. 

Detailed information on how each script works can be found in [the wiki](https://github.com/dfe-analytical-services/automated-data-qa/wiki)

You can take the template code and adapt it for your own QA reports, a step-by-step example of how you could start adapting it can be found below:

1. Open the autaomated-data-qa R project
2. Open the <b>0_get_file_info.R </b> script
3. Change the filenames for the data and metadata to point at wherever your files are saved
4. Save the <b>0_get_file_info.R </b> script - this defines the list of filters and indicators the code will check against
5. Run the code snippets as required

---
## Contact
statistics.DEVELOPMENT@education.gov.uk


