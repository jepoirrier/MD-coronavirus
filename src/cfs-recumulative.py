#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Recreating a true cumulative cases/deaths in nursing homes for Maryland
Currently (7/15) cases/deaths are dropped if facility doesn't report after 14 days
Cases URL: https://data.imap.maryland.gov/datasets/mdcovid19-numberofcasesbyaffected
Cases file to download: https://opendata.arcgis.com/datasets/c9baf0608722435184323a1fc57d9c4b_0.csv
Deaths URL: https: https://data.imap.maryland.gov/datasets/mdcovid19-numberofdeathsbyaffected
Deaths file to download: https://opendata.arcgis.com/datasets/58f80f7ebd9043e29b0eb84dd2ef891e_0.csv
"""

import csv      # to read/write CSV file
import os       # to exit/debug - can be removed later
import requests # to download files from URLs; sudo pip3.8 install requests

# Create a DataPoint class:
class DataPoint:
    """DataPoint object for each day / county+facility and numbers"""

    def __init__(self, date, county, facility, staffPrivate, residentPrivate, staffPublic, patientsPublic, inmatePublic, youthPublic):
        """Constructor"""
        self.date = date.split(' ')[0]
        self.county = county
        self.facility = facility
        self.staffPrivate = staffPrivate
        self.residentPrivate = residentPrivate
        self.staffPublic = staffPublic
        self.patientsPublic = patientsPublic
        self.inmatePublic = inmatePublic
        self.youthPublic = youthPublic
        #if len(staffPrivate) > 0:
        #    self.staffPrivate = int(staffPrivate)
        #else:
        #    self.staffPrivate = 0
        #if len(residentPrivate) > 0:
        #    self.residentPrivate = int(residentPrivate)
        #else:
        #    self.residentPrivate = 0
        #if len(staffPublic) > 0:
        #    self.staffPublic = int(staffPublic)
        #else:
        #    self.staffPublic = 0
        #if len(patientsPublic) > 0:
        #    self.patientsPublic = int(patientsPublic)
        #else:
        #    self.patientsPublic = 0
        #if len(inmatePublic) > 0:
        #    self.inmatePublic = int(inmatePublic)
        #else:
        #    self.inmatePublic = 0
        #if len(youthPublic) > 0:
        #    self.youthPublic = int(youthPublic)
        #else:
        #    self.youthPublic = 0
        #self.sum = staffPrivate + residentPrivate + staffPublic + patientsPublic + inmatePublic + youthPublic
        #print(self.date + self.facility)

    def getDate(self):
        """Returns just the date"""
        return self.date

    def getCounty(self):
        """Returns the county name"""
        return self.county

    def getFacility(self):
        """Returns the facility name"""
        return self.facility

    def getSP(self):
        return self.staffPrivate
    def getRP(self):
        return self.residentPrivate
    def getSPu(self):
        return self.staffPublic
    def getPP(self):
        return self.patientsPublic
    def getIP(self):
        return self.inmatePublic
    def getYP(self):
        return self.youthPublic

    def getSum(self):
        """"Returns the sum for this facility"""
        return self.sum

    def getAll(self):
        """Returns a string with everything"""
        return self.date + "/" + self.county + "/" + self.facility + "/" + str(self.staffPrivate) + "/" + str(self.residentPrivate) + "/" + str(self.staffPublic) + "/" + str(self.patientsPublic) + "/" + str(self.inmatePublic) + "/" + str(self.youthPublic)

    def printAll(self):
        """Returns a string with everything separated by comma"""
        return self.date + ";" + self.county + ";" + self.facility + ";" + str(self.staffPrivate) + ";" + str(self.residentPrivate) + ";" + str(self.staffPublic) + ";" + str(self.patientsPublic) + ";" + str(self.inmatePublic) + ";" + str(self.youthPublic + "\n")

if __name__ == "__main__":
    # 0. Some variables
    CFCURL = "https://opendata.arcgis.com/datasets/c9baf0608722435184323a1fc57d9c4b_0.csv"
    CFCFile = "../data/cfs-cases-o.csv" # this is the file from MDH
    CFCFileOut = "../data/cfs-cases.csv"# this is the file for R
    dataPoints = []      # this stores all input
    outPoints = []       # this stores all for output
    headerPassed = False # will allow to bypass header
    firstDate = True     # will differentiate first date from second/third/fourth date on step 3 below

    ### CASES

    # 1. Download cases
    print("Download cases")
    #print("DOWNLOAD SKIPPED - to get back in PROD")
    r = requests.get(CFCURL, allow_redirects=True)
    open(CFCFile, 'wb').write(r.content)

    # Read the CSV file for cases and store them in dataPoints
    print("Read CSV file for cases and store them in dataPoints")
    with open(CFCFile) as csvDataFile:
        csvReader = csv.reader(csvDataFile)
        for row in csvReader:
            if not(headerPassed):
                headerPassed = True
            else:
                # Correcting 2 things: a date zith "0200" and "\n" messing with data
                datPoint = DataPoint(row[1].replace("0200", "2020"), row[2], row[3].replace("\n", " "),  row[4], row[5], row[6], row[7], row[8], row[9])
                dataPoints.append(datPoint)

    print("     Import of objects done")

    # 2. Get a unique list of dates and facilities
    print("Get a unique list of dates and facilities")
    allDates = []
    allFacilities = []
    for dp in dataPoints:
        allDates.append(dp.getDate())
        allFacilities.append(dp.getFacility())
    # dates first
    allDatesSet = set(allDates)
    allDates = list(allDatesSet)
    allDates.sort()
    print("     Found " + str(len(allDates)) + " unique dates")
    # facilities then
    allFacilitiesSet = set(allFacilities)
    allFacilities = list(allFacilitiesSet)
    allFacilities.sort()
    print("     Found " + str(len(allFacilities)) + " unique facilities in total")

    # Dictionary between facilities and counties **********
    print("Building the facility : county dictionary")
    FCdict = {}
    for dp in dataPoints:
        FCdict[dp.getFacility()] = dp.getCounty()
    #print(FCdict)
    print(FCdict['Abbey Manor Assisted Living'])
    print(FCdict['Adelphi Nursing & Rehabilitation'])
    #os._exit(1)

    # Inject a DataPoint at 0 for '2020/04/28' (i.e. before everything) so we can refer to that in the future without interferring with counts
    print("Injecting point zero on 2020/04/28")
    allDates.append('2020/04/28')
    allDates.sort()
    for f in allFacilities:
        #fac = f.getFacility()
        datPoint = DataPoint('2020/04/28', FCdict[f], f, '0', '0', '0', '0', '0', '0')
        dataPoints.append(datPoint)

    # DEBUG point
    #str2print = "All dates:"
    #for d in allDates:
    #    str2print = str2print + " " + d
    #print(str2print)
    #str2print = "All facilities"
    #for f in allFacilities:
    #    str2print = str2print + " : " + f
    #print(str2print)
    #os._exit(1)

    # For each facility, get all date and add all points - can probably be more efficient
    print("Browsing each facility, for each date")
    prevPoint = DataPoint('2020/04/28', 'X', 'Y', '0', '0', '0', '0', '0', '0')
    fi = 0 # index on facilities
    for f in allFacilities:
        print("   Facility: " + f)
        # TODO: Remove duplicates and approximately same locations? **********
        prevPoint = DataPoint('2020/04/28', FCdict[f], f, '0', '0', '0', '0', '0', '0')
        di = 0 # index on dates
        for d in allDates:
            # # TODO: change date 0200 to 2020
            #print("      Date: " + d)
            pointFound = False
            for point in dataPoints:
                #print(point.getAll())
                if point.getDate() == d and point.getFacility() == f:
                    pointFound = True
                    prevPoint = DataPoint(d, FCdict[f], f, point.getSP(), point.getRP(), point.getSPu(), point.getPP(), point.getIP(), point.getYP())
                    outPoints.append(prevPoint)
                    #print("         point added (new): " + prevPoint.getAll())
                    break
            if pointFound == False:
                prevPoint = DataPoint(d, FCdict[f], f, prevPoint.getSP(), prevPoint.getRP(), prevPoint.getSPu(), prevPoint.getPP(), prevPoint.getIP(), prevPoint.getYP())
                outPoints.append(prevPoint)
                #print("         point added (old): " + prevPoint.getAll())
            di = di + 1
        #print("   Finished " + str(di) + "th date: " + d)
        fi = fi + 1
        #if fi == 2:
        #    os._exit(1)

    print("Writing all this to file")
    outFile = open(CFCFileOut, "w")
    for point in outPoints:
        outFile.write(point.printAll())
    outFile.close()

    print("Done")
