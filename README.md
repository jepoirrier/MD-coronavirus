# MD-coronavirus

Data and trend figures about Coronavirus in Maryland, US (2020), from https://coronavirus.maryland.gov/

## R scripts to generate trends

Explanations on this blog page: https://jepoirrier.org/mdcovid19/

Please note that the data is provided as is, as reported. Please consult the official MDH website for official data.

I rewrote the code (in src/) to remove manual transcription and rely on the MDH API (to automatically download the data in data/). This work should be over.

## How to run this?

1) Go to the src/ directory, usually with: `setwd("PATH/TO/src")`
2) Execute (from R), the run_all.R script: `source("run_all.R")`

## License

Feel free to use data, scripts and figures following the GNU GPL license (see LICENSE file)
