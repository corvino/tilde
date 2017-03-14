#! /usr/bin/env python

import datetime
import json
import os
import sys

def date_pretty(timestamp):
    try:
        dt = datetime.datetime.utcfromtimestamp(timestamp/1000.0)
        return dt.isoformat()
    except:
        return "N/A"

def find_minmax(filename):
    min_t = sys.maxint
    max_t = 0

    file = open(filename)
    data = json.load(file)
    file.close()

    for item in data:
        if "time" in item:
            time = item["time"]
            min_t = min(min_t, time)
            max_t = max(max_t, time)

    return (min_t,max_t)

min_t = sys.maxint
max_t = 0

if 2 != len(sys.argv):
    print "usage: find-time-range.py [dir-of-json-files]"
    exit(1)

file = sys.argv[1]

if os.path.isdir(file):
    for filename in os.listdir(file):
        if not filename.endswith("json"):
            continue

        (new_min, new_max) = find_minmax(os.path.join(file, filename))
        min_t = min(min_t, new_min)
        max_t = max(max_t, new_max)
elif os.path.isfile(file):
    if file.endswith("json"):
        (min_t, max_t) = find_minmax(file)

print "min: {} - {}".format(min_t, date_pretty(min_t))
print "max: {} - {}".format(max_t, date_pretty(max_t))
