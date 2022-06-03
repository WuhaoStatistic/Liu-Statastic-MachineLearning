from __future__ import division
from math import radians, cos, sin, asin, sqrt, exp
from datetime import datetime
from pyspark import SparkContext

sc = SparkContext(appName="lab_kernel")


def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat / 2) ** 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ** 2
    c = 2 * asin(sqrt(a))
    km = 6367 * c
    return km


def ker_loca(dis, h_dis):
    return exp(-dis ** 2 / h_dis ** 2)


def ker_day(day1, day2, h_day):
    d1 = datetime.strptime(day1, "%Y-%m-%d")
    d2 = datetime.strptime(day2, '%Y-%m-%d')
    day_dis = abs((d1 - d2).days) % 365 + 0.2 * abs((d1 - d2).days) / 365
    return exp(-day_dis ** 2 / h_day ** 2)


def ker_hour(t1, t2, h_hour):
    time1_h = int(t1[0:2])
    time1_m = int(t1[3:5])
    time2_h = int(t2[0:2])
    time2_m = int(t2[3:5])
    time1 = time1_h * 60 + time1_m
    time2 = time2_h * 60 + time2_m
    t_dis = abs(time1 - time2) / 60
    return exp(-t_dis ** 2 / h_hour ** 2)


def predict(time, data, mode='sum'):
    kernel_value = data.map(lambda x: (x[0], x[1], ker_hour(time, x[2], h_time), float(x[3])))

    # sum
    # kernel_value = kernel_value.map(lambda x:((x[0]+x[1]+x[2])*x[3],(x[0]+x[1]+x[2])))

    # multiple
    kernel_value = kernel_value.map(lambda x: ((x[0] * x[1] * x[2]) * x[3], (x[0] * x[1] * x[2])))

    kernel_value = kernel_value.reduce(lambda a, b: (a[0] + b[0], a[1] + b[1]))
    res = kernel_value[0] / kernel_value[1]
    return res


h_distance = 40  # Up to you
h_date = 10  # Up to you
h_time = 2  # Up to you

a = 58.4274  # Up to you lat
b = 14.826  # Up to you long
date = "2013-07-04"  # Up to you

int_date = int(date[0:4] + date[5:7] + date[8:10])

stations = sc.textFile("BDA/input/stations.csv")
lines_stations = stations.map(lambda line: line.split(";"))
stations = lines_stations.map(lambda x: (x[0], (haversine(b, a, float(x[4]), float(x[3])))))

m = sc.parallelize(stations.collect()).collectAsMap()
stations_data = sc.broadcast(m)

# now we have the station number and their distance to target location
#    (u'102170', 6234.382614181623)

temps = sc.textFile('BDA/input/temperature-readings.csv')
lines_temps = temps.map(lambda line: line.split(";"))
# [u'102170', u'2013-11-01', u'06:00:00', u'6.8', u'G']
# check the distance kernel
# temp = lines_temps.map(lambda x:(x[0],stations_data.value[x[0]],ker_loca(stations_data.value[x[0]],h_distance))).distinct().sortBy(ascending = True, keyfunc=lambda k: k[2])

temp = lines_temps.map(lambda x: (x[0], (x[1], x[2], x[3], stations_data.value[x[0]])))

# (u'102170', (u'2013-11-01', u'06:00:00', u'6.8', 234.382614181623))

temp = temp.filter(lambda x: int(x[1][0][0:4] + x[1][0][5:7] + x[1][0][8:10]) < int_date)
# (u'102190', (u'1955-09-08', u'12:00:00', u'17.5', 243.523599180525))

# get the data which are very close to target input to evaluate the result.
# temp = temp.filter(lambda x: int(x[1][0][0:4]+x[1][0][5:7]+x[1][0][8:10])==int_date)
# temp = temp.filter(lambda x: x[1][3]<100).sortBy(ascending = True, keyfunc=lambda k: k[1][3])

data = temp.map(lambda x: (ker_loca(x[1][3], h_distance), ker_day(x[1][0], date, h_date), x[1][1], x[1][2])).cache()

# check day kernel
# data = temp.map(lambda x:(x[1][0],ker_day(x[1][0],date,h_date))).distinct().sortBy(ascending = True, keyfunc=lambda k: k[1])
# (0.013150606009360008, 0.027051846866350416, u'18:00:00', u'14.2')

# check hour kernel
# data = data.map(lambda x:(x[2],ker_hour('12:00:00',x[2],h_time))).distinct().sortBy(ascending = True, keyfunc=lambda k: k[1])

# temp.saveAsTextFile("BDA/output")
# Your code here
time_list = ["24:00:00", "22:00:00", "20:00:00", "18:00:00", "16:00:00",
             "14:00:00", "12:00:00", "10:00:00", "08:00:00", "06:00:00", "04:00:00"]

res = [predict(x, data) for x in time_list]
res = sc.parallelize([res])
res.saveAsTextFile("BDA/output")