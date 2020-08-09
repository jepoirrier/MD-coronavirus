# MD-coronavirus

Data and trend figures about Coronavirus in Maryland, US (2020), from https://coronavirus.maryland.gov/

## R scripts to generate trends

Explanations on this blog page: https://jepoirrier.org/mdcovid19/

Please note that the data is provided as is, as reported. There is a lot to transcribe and it's still a manual operation. So they may be typo. Please consult the official MDH website for official data.

I'm in the process of rewriting this code (in src/) to remove manual transcription and rely on the MDH API (to automatically download the data in data/). This is work in progress.

## How to run this?

1) Go to the src/ directory, usually with: `setwd("PATH/TO/src")`
2) Execute (from R), the run_all.R script: `source("run_all.R")`

## License

Feel free to use data, scripts and figures following the GNU GPL license (see LICENSE file)
