#!/usr/bin/env python

import urllib2, datetime

todays_actual_url = 'http://www.nws.noaa.gov/climate/getclimate.php?date=&wfo=lot&sid=ORD&pil=CLI&recent=yes'
todays_text = urllib2.urlopen(todays_actual_url).read()
a = str(datetime.datetime.now())
filename = a[:a.find(' ')]
f = open('/home/cory/Weather/Observed/data/' + filename, 'w')
f.write(todays_text)
f.close()
