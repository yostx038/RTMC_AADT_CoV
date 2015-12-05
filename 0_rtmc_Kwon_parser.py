# Aug 2015: TAY comments
# This python script parses a Kwon-style detector equiv. file
# and turns it into a CSV that can be easily read/manipulated in R.
# I tried to rewrite this functionality in R, but.. it would have taken too long,
# and this already-written python code does the trick. 
# 
# This script should only be run/called by the R code; there's no need for
# end-users to run this.
#
# IMPORTANT: hard-coded input file (Kwon-style) and output file (CSV for R functions)
# need to be done correctly. Enter them here:

infile = "N:/Traffic Monitoring/Continuous/Data_Collection/RTMC/Mainline/RCoV/DetSetsMarch2015.txt"
outfilename = "N:/Traffic Monitoring/Continuous/Data_Collection/RTMC/Mainline/RCoV/DetSetsClean.csv"

import string
import csv
import sys

# okay. any line starting with a semicolon should be ignored (comment)
# most lines seem to start with a space; the semicolon can be the 2nd char in string

# Format is like so:
# 10779, 7, T, P, 597, 598, 707, 708, End
# or
#  10310,1,T, P, 1901,1902,1903, S,1362,1363,1383,1364,End
#  ;10345,5,T, P, 2904,2905,2906,5681, S,2888,2889,289, T,2891,2892,2893,2894,End
#  10338,5,T, P, 1959,1960,1961, S,1962,-1963,1786,1964,1965,End



csvfile = open(infile, 'rb')
csvrows = csvfile.readlines()
numrows = len(csvrows)

csvout = open(outfilename, 'wb')
csvout.write("seqnum, dir, order, detector")

highdir = {}
lowdir  = {}


i = 0

while i < numrows:
    r = csvrows[i]
    if ";" not in r: # remember, semicolon means it's a comment
        rvec = r.split(",")  # first split by commas
        seqnum = rvec[0]  # pull out seqnum
        dirnum = int(rvec[1])  # pull out direction code number
        dircode = "99999"
        if dirnum in [1,2,3,4]: dircode = "0"
        if dirnum in [5,6,7,8]: dircode = "1"
        #
        order = "0" # 0 for P, 1 for S, 2 for T
        detectorset = seqnum + "," + dircode + "," + order
        # output format:
        # seqnum, dircode, ordercode,
        #print rvec
        for element in range(4, len(rvec)):
            elm = rvec[element].strip()
            elmsmaller = elm[1:]
            if elm == "End\r\n":
                # nothing to do here
                pass
            if elm == "S":
                # switch order to 1
                order = "1"
                detectorset = seqnum + "," + dircode + "," + order
                #print detectorset
            if elm == "T":
                #  switch order to 2
                order = "2"
                detectorset = seqnum + "," + dircode + "," + order
            if elmsmaller.isdigit():
                detstr = detectorset + "," + rvec[element] + "\r\n"
                csvout.write(detstr)
                #print detstr
    # increment our happy little counter
    i = i + 1
    #

# all done
csvout.close()
csvfile.close()

















