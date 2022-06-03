from pyspark import SparkContext
###############################
# Q1
# sc = SparkContext(appName="lab1")
# path = 'BDA/input/temperature-readings.csv'
# data=sc.textFile(path)
# data_line = data.map(lambda line:line.split(";"))

# temp = data_line.map(lambda t:(t[1][0:4],(float(t[3]),float(t[3]))))
# temp = temp.filter(lambda t: int(t[0])>=1950 and int(t[0])<=2014)

# temp_pair = temp.reduceByKey(lambda a,b:(max(a[0],b[0]),min(a[1],b[1])))
# temp_pair = temp_pair.sortBy(ascending = False, keyfunc=lambda k: k[1][0])

# temp_pair.saveAsTextFile("BDA/output")
###############################
# Q2
# sc = SparkContext(appName="lab1")
# path = 'BDA/input/temperature-readings.csv'
# data=sc.textFile(path)
# data_line = data.map(lambda line:line.split(";"))
# temp = data_line.map(lambda t:((t[0],t[1][0:4],t[1][5:7]),float(t[3])))
#
# temp = temp.filter(lambda t: int(t[0][1])>=1950 and int(t[0][1])<=2014)
# temp = temp.filter(lambda t: t[1]>=10)
#
# temp = temp.map(lambda t: ((t[0][1],t[0][2]),(t[0],1))).distinct()
#
# # code below will select distinct pair of (year,month)
# #temp = temp.map(lambda t:(t[0],t[1][1])).reduceByKey(lambda a,b:a+b)
#
# #code below will select distinct month(only 12 results corresponding to 12 month)
# temp = temp.map(lambda t:(t[0][1],t[1][1])).reduceByKey(lambda a,b:a+b)
#
# temp = temp.sortBy(ascending = False, keyfunc=lambda k: k[1])
#
# temp.saveAsTextFile("BDA/output")
###############################
# Q3
# sc = SparkContext(appName="lab1")
# path = 'BDA/input/temperature-readings.csv'
# data=sc.textFile(path)
#
# data_line = data.map(lambda line:line.split(";"))
#
# temp = data_line.map(lambda t:((t[1][0:4],t[1][5:7],t[1][8:10],t[0]),(float(t[3]),float(t[3]))))
# # ((u'2013', u'11', u'01', u'102170'), (6.8, 6.8)) (year month day sta) (temp,temp)
# temp = temp.filter(lambda t: int(t[0][0])>=1960 and int(t[0][0])<=2014)
#
#
# temp = temp.reduceByKey(lambda a,b:(max(a[0],b[0]),min(a[1],b[1])))
# #((u'1981', u'10', u'05', u'128370'), (9.8, 7.7)) (year month day sta) (h_temp,l_temp)
# temp = temp.map(lambda t:((t[0][0],t[0][1],t[0][3]),((t[1][0]+t[1][1]),2)))
#
#
# temp = temp.reduceByKey(lambda a,b:(a[0]+b[0],a[1]+b[1]))
# temp = temp.map(lambda t: (t[0],round(t[1][0]/t[1][1],2)))
#
#
# temp.saveAsTextFile("BDA/output")
###############################
# Q4
# sc = SparkContext(appName = "lab1")
# temp_f = sc.textFile('BDA/input/temperature-readings.csv')
# pre_f = sc.textFile('BDA/input/precipitation-readings.csv')
# temp_line = temp_f.map(lambda line: line.split(";"))
# pre_line = pre_f.map(lambda line: line.split(";"))
# # [u'103100', u'1995-08-01', u'00:00:00', u'0.0', u'Y']
# temp = temp_line.map(lambda t: (t[0],float(t[3])))
# # (sta)(temp)
# pre_day = pre_line.map(lambda p:((p[0],p[1][0:4],p[1][5:7],p[1][8:10]),float(p[3])))
# # (sta year mon day)(pre)
# max_temp = temp.reduceByKey(max)
# pre_day = pre_day.reduceByKey(lambda a,b:a+b)
#
# max_temp = max_temp.filter(lambda t: t[1]>=25 and t[1]<=30)
# max_temp = max_temp.sortBy(ascending = False, keyfunc=lambda k: k[0])
#
# pre_day = pre_day.map(lambda p: (p[0][0],p[1]))
# max_pre_day = pre_day.reduceByKey(max)
# max_pre_day = max_pre_day.filter(lambda p: p[1]>=100 and p[1]<=200)
#
# join = max_temp.join(max_pre_day)
#
# join.saveAsTextFile("BDA/output")
###############################
# Q5
sc = SparkContext(appName="lab1")
sta_f = sc.textFile("BDA/input/stations-Ostergotland.csv")
pre_f = sc.textFile("BDA/input/precipitation-readings.csv")
sta_line = sta_f.map(lambda line: line.split(";"))
pre_line = pre_f.map(lambda line: line.split(";"))
stations = sta_line.map(lambda x: (1, (x[0],)))
# (key, value) = (station_number,year,month,day,precipitation)
stations = stations.reduceByKey(lambda a, b: a + b).map(lambda x: x[1])
# collect and broadcast
stations = stations.collect()
stations = sc.broadcast(stations)
pre_day = pre_line.map(lambda x: ((x[0], x[1][0:4], x[1][5:7]), float(x[3])))
# ((u'103100', u'1995', u'08'), 0.0)
# (sta,year,mon)(pre)
pre_day = pre_day.filter(lambda x: int(x[0][1]) >= 1993 and int(x[0][1]) <= 2016) \
    .filter(lambda x: x[0][0] in stations.value[0])

pre_mon = pre_day.reduceByKey(lambda a, b: a + b)
pre_mon = pre_mon.map(lambda x: ((x[0][1], x[0][2]), (x[1], 1)))
# (year,mon)(pre,count)
pre_mon_all = pre_mon.reduceByKey(lambda a, b: (a[0] + b[0], a[1] + b[1]))
pre_mon_avg = pre_mon_all.map(lambda x: (int(x[0][0]), int(x[0][1]), round(x[1][0] / x[1][1], 2)))

pre_mon_avg.saveAsTextFile("BDA/output")
