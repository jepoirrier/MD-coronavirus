#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
For files where I pasted the data from the MDH dashboard

TODO: define the date variable!

Created on Tue Apr 14 21:36:28 2020

@author: jepoirrier
"""

# TODO CHANGE -- 1 --- DATE

import os

currentDate = '200524' # I don't want to fetch today's date in case I miss a day

os.chdir('/Users/jepoirrier/Documents/software/MD-coronavirus') # Adapt to your needs

inputFileName = 'ZIP-daily-copies/MD-corona-ZIPpasted' + currentDate + '.txt'
outputFileName = 'MD-corona-ZIP.txt'

inputFile = open(inputFileName, mode = "r")
outputFile = open(outputFileName, mode = "a")

for line in inputFile:
    elems = line.split()
    if len(elems) == 4:
        outputFile.write(currentDate + " " + elems[0] + " " + elems[2] + "\n")

outputFile.close()
inputFile.close()
