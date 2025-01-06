#!/bin/bash
# Download data

set -euo pipefail
IFS=$'\n\t'

curl -o uk-daily-births.csv https://www.ons.gov.uk/visualisations/nesscontent/dvc307/line_chart/data.csv
