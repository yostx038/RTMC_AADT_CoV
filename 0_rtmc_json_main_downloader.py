###___###___###___###___###___###___###
# TAY Aug 2015 Notes:
# This python script was written to get data from the RTMC in 30-second bins, and turn that into a single
# CSV file for each detector that contains the summed daily volumes for that detector over 1 year.
#
# IMPORTANT NOTES: this program has several things kinda-hardcoded in, so if you don't change those values,
# things will break. See comments below for what to fiddle with.
#
# Looks like the 'requests' library is required here, if memory serves it's to do with handling the JSON formatting.
#
###___###___###___###___###___###___###
#
# RTMC json leech script
# TAY Apr 13 2015


import requests
import string
import csv

########### HARDCODED STUFF TO VERIFY.
# Year we are getting data for, formatted as a string.
curr_year = "2015"

# First detector to get data for
first_detector = 1

# Last detector number to obtain data for
last_detector = 7500

# Output path/filename
# May need to be changed to "C:/Users/Flin1Mar/Desktop/RTMCAADT/rtmc/rpy" or somesuch, make sure it
# keeps the 'rpy' filename prefix
output_path = "C:/Users/Yost1Tyl/Desktop/Rcode/rtmc/rpy"



# first we get a list of valid days from RTMC. this'll probably just be all days in the year.
ds = requests.get(("http://data.dot.state.mn.us:8080/trafdat/metro/" + curr_year))
valid_dates = string.split(ds.text, "\n")[:-1]


def rtmc_year(one_detector, valid_dates):
    # This function returns the daily volumes for a single detector in a given year.
    # It takes as input: detector number, list of valid dates to get data for (typically 1 years' worth)
    # It outputs a set of (typically 1 years' worth) daily total volumes for each valid day provided as input.
    #
    daily_volumes = list()  # initialize a list that'll hold the end product per-day volumes
    for day in valid_dates:  # for each day in valid_dates list we got from RTMC
        # first retrieve one days' worth of 30second traffic volume bins
        # here's a sample if you want to see what this looks like real quick:
        # http://data.dot.state.mn.us:8080/trafdat/metro/20140214/1234.v30.json
        urlstring = "http://data.dot.state.mn.us:8080/trafdat/metro/" + str(day) + "/" + str(one_detector) + ".v30.json"
        #
        # the try/except are used here to keep the program going if data can't be obtained for any one detector/day
        # during development, this happened frequently.
        try:
            day_request = requests.get(urlstring)  # get the day's 30s volume bins
            day_vol = day_request.text  # format it as text.. ??
            if day_request.status_code == 404:  # if the http request returned a 404
                day_vol = "[0,0,0,0,0]"  # assume daily volume was 0. note that this doesn't have to be the same
                # length as a 'full' day with all 30s bins - if it 404s, we assume zero volume for the day.
        except:
            day_vol = "[0,0,0,0,0]"  # if something we tried above bombed out, just assume daily volume is 0
            #
        #
        day_vol = day_vol[1:-1]  # strip off brackets on either side
        # sometimes 'null' appears in the 30s volume bins.
        # all 'null' values are stripped out and replaced with zeroes
        day_vol = string.split(day_vol, ",")
        day_vol = [i.replace('null', '0') for i in day_vol]
        # then convert to integers
        day_vol = map(int, day_vol)
        day_vol = sum(day_vol)
        # Append this daily volume to a vector that collects all daily volumes.
        daily_volumes.append(day_vol)
    #
    # Return the daily volumes for a single detector in a given year.
    return daily_volumes


# now we have a rtmc_year function
# let's loop it through all detectors...


for detector in range(first_detector, last_detector):  # first and last detectors; as detectors are added,
    # last_detector needs to be set higher and higher.
    csv_filename = output_path + str(detector) + "-" + curr_year + ".csv"  # define output filename
    csvfile = open(csv_filename, 'wb')
    w = csv.writer(csvfile, delimiter=",")
    volumes = rtmc_year(detector, valid_dates)  # run the rtmc_year function that appears above this code, returning
    # a vector of (usually) 365 daily volumes.
    w.writerow(volumes)  # write volumes to file as CSV
    print "finished detector " + repr(detector)  # provide prined output for human watching this all go.
    csvfile.close()

#


















