# Aug 2015: TAY Notes:
# This file contains the R functions that form the core of the project. 
# Some things are hardcoded; many half-written functions were removed from this
# last 'good' version to simplify things for future readers.
# A subfolder should be present with previous versions of all of these functions,
# very little in the way of comments, and... well, a mess.
#
# the dplyr package is required for this to work. jsonlite and lubridate were listed
# as required before, but I think those aren't used anywhere in the current version.
# the require(package) function should make sure they have been installed and load 
# them; if not, they'll be downloaded and installed (this will take a minute).
#
# First we make sure dplyr is present and loaded (thank you Hadley!)
require(dplyr)



############ GLOBALS that may need to be changed
curr_year     = "2014"
aadt_path     = "N:/Traffic Monitoring/Continuous/Data_Collection/RTMC/Mainline/RCoV/"
CSV_filename  = paste0(aadt_path, "DetSetsClean.csv")
batch_filename= "0_batchfile_update.bat"


#################### Bargraph colors
# WARNING/NOTE: for simplicity I used 28-day months.. so if you count the 
# gray/black patches, there are 13 of them... they're present only to
# approximate months. Something for fixing when someone has time...
# Change where exactly the 'red' appears to make Sundays stand out in the plots.
# This starts filling in colors from Jan 1 of the year in question onward, so this will
# change every year.
#
colA_2014=c("black", "black", "black", "black", "red", "black", "black")
colB_2014=c("gray30", "gray30", "gray30", "gray30", "red", "gray30", "gray30")
bargraph_colors = c(colA_2014, colA_2014,colA_2014,colA_2014,colB_2014,colB_2014,colB_2014,colB_2014)

# This sets up the graphic display to show 4 detectors at a time, unless otherwise instructed.
par(mfrow=c(4,1))

# this adds the aadt_path to the system path (internal to R only, not for other Windows apps)
# this might not be necessary, but... likely does no harm. keep the switch set to 'more 
# magic' and all, right?
Sys.setenv(
  PATH = paste(
    Sys.getenv("PATH"), 
    aadt_path, 
    sep = ";"
  )
)


####################################################################
####################################################################
####################################################################
# This first section contains all of the functions one might use
# when doing analysis on volume data.
# There's a second set of functions that are internal (not needed by user)
# farther down in this file. 
####################################################################
####################################################################
####################################################################

########################################################
# quikchek: NOTE THE SPELLING! Not quikcheck or quickcheck, etc...
# This plots all kinds of stuff to allow a quick visual
# check using the CoV method. See Word document accompanying all of this
# for a more thorough description. The only input is a sequence number.
#
quikchek = function(seq){
  # mandatory refresh of CSV from Kwon file first
  rtmc.dets.refresh(aadt_path = aadt_path, batch_filename = batch_filename)
  plot.new()
  title(sub=seq)
  #  first 0 dir pairwise, then 1 dir pairwise, then 2way primary pairwise
  par(mfrow=c(4,1))
  par(mar=c(1,5,3,1))
  pairplot.1way(seq, 0, header=paste0(seq, " increasing side (primary minus secondary)"))
  pairplot.1way(seq, 1, header = "decreasing side (primary minus secondary)")
  print("primaries up/down")
  pairplot.2way(seq, 0, header="(increasing minus decreasing) primary detectors")
  print("secondaries up/down")
  pairplot.2way(seq, 1, header="(increasing minus decreasing) secondary detectors")
}


########################################################
# goodmonth
# This function computes mean daily volumes for multiple detector locations
# and then sums them. If there are gaps in the first 4 months of the year, this
# function can ignore the first N days (or last N, etc) of the year and produce 
# an average from specific parts of the calendar.
# See the Word documentation for this one; the syntax is sorta unusual/long/particular.
# Brief example: goodmonth( c(1163, 1164, 1165), c(1, 180) )
#
goodmonth = function(dets, days){ 
  aadt = 0
  for(det in dets){ # for each detector in the list
    yrcount = rtmc.yearfile(det) # get the yearly volume file
    aadt = aadt + mean(yrcount[days[1]:days[2]]) # and clip off only the part we want, then add
  }
  return(aadt)
}


########################################################
# dets.2way
# This function takes as input a single sequence (not detector) number,
# and produces as output plots of daily volumes from PRIMARY detectors
# only in BOTH directions.
#
dets.2way = function(seq){
  rtmc.dets.refresh(aadt_path = aadt_path, batch_filename = batch_filename)
  # see each detector's daily plot to find bad ones
  dsets = read.csv(file=CSV_filename)
  one_loc = filter(dsets, seqnum == seq, order == 0)
  dets = one_loc$detector
  howmany = length(dets)
  par(mfrow=c(howmany, 1))
  par(mar=c(2,3,2,0))
  sum = 0
  for(det in dets){
    year_data = rtmc.yearfile(det)
    sum = sum + mean(year_data, na.rm = TRUE)
    maintxt = paste0("seq ", seq, "         rtmc detector ", det, "          adt = ", round(mean(year_data, na.rm=TRUE)))
    barplot((year_data/1000), xaxt="n", main=maintxt, width=1, space=0, border=NA,  col=bargraph_colors)
  }
  print(paste0("sum of all AADTs: ", round(sum,0)))
}


########################################################
# dets.1way 
# This function takes as input a sequence number and a (binary) direction, where
# 0 represents (N, NE, E, SE), and 1 represents (S, SW, W, NW)
# It then plots all detectors (primary and secondary) associated with that seqnum
# in the Kwon file.
#
dets.1way = function(seq, direction){
  rtmc.dets.refresh(aadt_path = aadt_path, batch_filename = batch_filename)
  # see each detector's daily plot to find bad ones
  dsets = read.csv(file=CSV_filename)
  one_loc = filter(dsets, seqnum == seq, dir == direction)
  dets = one_loc$detector
  print(paste0( one_loc$order, " - ", one_loc$detector))
  howmany = length(dets)
  par(mfrow=c(howmany, 1))
  par(mar=c(1,3,1,0))
  for(det in dets){
    year_data = rtmc.yearfile(det)
    maintxt = paste0("seq ", seq, "         rtmc detector ", det, "          adt = ", round(mean(year_data, na.rm=TRUE)))
    barplot((year_data/1000), xaxt="n", main=maintxt, width=1, space=0, border=NA, col=bargraph_colors)
  }
}



########################################################
# dets.one
# This is a very simple function that plots daily volumes for the
# year as a bargraph. 
#
dets.one = function(detr){
  # see one detector's daily volumes for the year
  # par(mar..) is to set border widths for the graphic display
  par(mar=c(2,3,2,0))
  yeardata = rtmc.yearfile(detr)
  maintxt = paste0("detector ", detr, "          adt = ", round(mean(yeardata, na.rm=TRUE)))
  barplot((yeardata/1000), xaxt="n", main=maintxt, width=1, space=0, border=NA,  col=bargraph_colors)
}



####################################################################
####################################################################
####################################################################
# Functions below this point are internal and not called directly by 
# the user/analyst. 
####################################################################
####################################################################
####################################################################



########################################################
# pairplot.2way
# This is an internal function called by quikchek(), and is used as a wrapper 
# around rtmc.pairplot(). Same basic idea as pairplot.1way, except compares
# detector sets for opposite travel directions. See pairplot.1way for more details.
#
pairplot.2way = function(seq, ord, header){
  dsets = read.csv(file=CSV_filename)
  one_loc = filter(dsets, seqnum == seq, order == ord)
  posdet = filter(one_loc, dir == 0)$detector
  negdet = filter(one_loc, dir == 1)$detector
  print(paste0("UP: ", posdet))
  print(paste0("DOWN: ", negdet))
  aadtyear = goodmonth(posdet, c(1,360)) + goodmonth(negdet, c(1,360))
  print(paste0("aadt for year = ", round(aadtyear)))
  pdir = c()
  ndir = c()
  for(d in posdet){ # ugly but this will work
    pdir = rbind(pdir, rtmc.yearfile(d))
  }
  for(d in negdet){ 
    ndir = rbind(ndir, rtmc.yearfile(d))
  }
  rtmc.pairplot(seq, pdir, ndir, header)
}


########################################################
# pairplot.1way
# This is an internal function called by quikchek().
# It is a wrapper around rtmc.pairplot().
# pairplot.1way and .2way take in a sequence number, (direction or P/S order), 
# and a header that will be plotted on the resulting graph/figure.
# This code then reads the CSV equivalent of the Kwon file, thus figuring out which
# detectors are associated with a given seqnum, etc., and plots the results.
#
pairplot.1way = function(seq, direction, header){
  dsets = read.csv(file=CSV_filename)
  one_loc = filter(dsets, seqnum == seq, dir == direction)
  posdet = filter(one_loc, order == 0)$detector
  negdet = filter(one_loc, order == 1)$detector
  pdir = c()
  ndir = c()
  for(d in posdet){ # ugly but this will work
    pdir = rbind(pdir, rtmc.yearfile(d))
  }
  for(d in negdet){ 
    ndir = rbind(ndir, rtmc.yearfile(d))
  }
  rtmc.pairplot(seq, pdir, ndir, header)
}



########################################################
# rtmc.yearfile
# This function returns a vector with daily volumes for
# a given detector. 
#
rtmc.yearfile = function(detector, year=curr_year){
  # this only works for 2014 data that we've downloaded
  coeff = 1
  if(detector < 0){
    # if detector is something like -234, set a coefficient to be negative
    # such that those volumes will be subtracted from a collection of 
    # detector daily volumes. This will also cause them to be plotted upside-down.
    detector = 0 - detector
    coeff = -1
  }
  filename = paste0(aadt_path, "rpy", detector, "-", year, ".csv")
  yr = read.csv(file=filename, header=FALSE)
  yr = as.integer(yr) * coeff
  return(yr)
}



########################################################
# rtmc.pairplot
# This function plots contrasts between pairs of detector groups, and is called from
# the quikchek function (any others?). 
# It takes in txt_tag (which looks like it isn't used, great coding Tyler)...
# det_a and det_b are daily volume data for both sets of detectors to be compared
# and, header is a string to be placed above the plot.
#
rtmc.pairplot = function(txt_tag, det_a, det_b, header){
  #
  group_a = apply(det_a, 2, sum)
  group_b = apply(det_b, 2, sum)
  #
  months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "")
  #
  group_a_adt = round(mean(group_a, na.rm=TRUE))
  group_b_adt = round(mean(group_b, na.rm=TRUE))
  mean_adt = mean(c(group_a_adt, group_b_adt))
  maintxt = header
  #
  diffs = (group_a - group_b)/ 1000
  y_limits = c((0-mean_adt)*.2, mean_adt*.2) / 1000
  plot(diffs, ylab = "", ylim=y_limits, pch=16, main = maintxt, col=bargraph_colors)
  abline(a = 0, b = 0, lwd=3, lty=2)
}



########################################################
# rtmc.plotheight
# This function is just a wrapper for par(mfrow=c(x,1))
#
rtmc.plotheight = function(x){
  par(mfrow=c(x,1))
}


########################################################
# rtmc.dets.refresh
# This function runs an old-school DOS batch file, which in
# turn runs the python script that updates the CSV detector list
# from the Kwon file.
#
# This function should be called at the start of every user-level
# analysis function, to ensure that any changes to the Kwon file
# are reflected in the resulting graphics/output.
#
# NOTE that the paths/filenames in both the batch file and the python
# code are hardcoded; if this suddenly breaks, that'd be the first place
# to check...
# 
rtmc.dets.refresh = function(aadt_path, batch_filename){
  # set the correct working directory to aadt_path
  setwd(aadt_path)
  # then run the batch file that'll run the python script
  shell(batch_filename)
  return()
}



########################################################
########################################################
########################## THE END #####################
########################################################
########################################################














