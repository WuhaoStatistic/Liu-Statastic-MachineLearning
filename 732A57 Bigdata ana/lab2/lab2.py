from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

#####################################
# Q1
# What are the lowest and highest temperatures measured each year for the period 1950-2014.
# Provide the lists sorted in the descending order with respect to the maximum temperature. In
# this exercise you will use the temperature-readings.csv file.
# The output should at least contain the following information (You can also include a Station
# column so that you may find multiple stations that record the highest (lowest)
# temperature.):
# year, station with the max, maxValue ORDER BY maxValue DESC
# year, station with the min, minValue ORDER BY minValue DESC

sc = SparkContext(appName="lab 2")
sqlContext = SQLContext(sc)
rdd = sc.textFile("BDA/input/temperature-readings.csv")
parts = rdd.map(lambda l: l.split(";"))

tempReadings = parts.map(lambda p: Row(station=p[0], year=p[1].split("-")[0], temp=float(p[3])))
# (year, station, temperature)
schemaTempReadings = sqlContext.createDataFrame(tempReadings)
schemaTempReadings.registerTempTable("tempReadings")

schemaTempReadings = schemaTempReadings.filter((schemaTempReadings.year >= 1950) & (schemaTempReadings.year <= 2014))

max_temp = schemaTempReadings.groupBy('year').agg(F.max('temp').alias('temp'))
min_temp = schemaTempReadings.groupBy('year').agg(F.min('temp').alias('temp'))

max_temp = max_temp.join(schemaTempReadings, ['year', 'temp']).select('year', 'station', 'temp').withColumnRenamed(
    'temp', 'max_temp').orderBy(['temp'], ascending=0).show()

min_temp = min_temp.join(schemaTempReadings, ['year', 'temp']).select('year', 'station', 'temp').withColumnRenamed(
    'temp', 'min_temp').orderBy(['temp'], ascending=0).show()
#####################################
# Q2
# Count the number of readings for each month in the period of 1950-2014 which are higher
# than10degrees.Repeat the exercise,this time taking only distinct readings from each station.
# That is, if a station reported a reading above 10 degrees in some month, then it appears only
# once in the count for that month.
# In this exercise you will use the temperature-readings.csv file.
# The output should contain the following information:
# Year, month, count
# year, month, value ORDER BY value DESC
# year, month, value ORDER BY value DESC

sc = SparkContext(appName = "lab 2")
sqlContext = SQLContext(sc)
rdd = sc.textFile("BDA/input/temperature-readings.csv")
parts = rdd.map(lambda l: l.split(";"))

tempReadings = parts.map(lambda p: Row(year=p[1].split("-")[0], month=p[1].split("-")[1], station=p[0], count=1 if p[3]>10 else 0)).distinct()
# (year, month, station, count)
schemaTempReadings = sqlContext.createDataFrame(tempReadings)
schemaTempReadings.registerTempTable("tempReadings")

schemaTempReadings = schemaTempReadings.filter((schemaTempReadings.year>=1950)&(schemaTempReadings.year<=2014))

schemaTempReadingsSort=schemaTempReadings.groupBy('year', 'month').agg(F.count('count').alias('count')).orderBy(['count'],ascending = 0)

schemaTempReadingsSort.show()
######################################
# Q3
# Find the average monthly temperature for each available station in Sweden. Your result
# should include average temperature for each station for each month in the period of 1960-
# 2014. Bear in mind that not every station has the readings for each month in this timeframe.
# In this exercise you will use the temperature-readings.csv file.
# The output should contain the following information:
# Year, month, station number, average monthly temperature
# year, month, station, avgMonthlyTemperature ORDER BY avgMonthlyTemperature DESC

sc = SparkContext(appName = "lab 2")
sqlContext = SQLContext(sc)
rdd = sc.textFile("BDA/input/temperature-readings.csv")
parts = rdd.map(lambda l: l.split(";"))

tempReadings = parts.map(lambda p: Row(year=p[1].split("-")[0], month=p[1].split("-")[1], station=p[0], temp=p[3],count=1))
schemaTempReadings = sqlContext.createDataFrame(tempReadings)
schemaTempReadings.registerTempTable("tempReadings")

schemaTempReadings = schemaTempReadings.filter((schemaTempReadings.year>=1960)&(schemaTempReadings.year<=2014))

schemaTempReadingsSort=schemaTempReadings.groupBy('year', 'month','station').agg((F.sum('temp')/F.sum('count')).alias('avg_mon_temp')).orderBy(['avg_mon_temp'],ascending = 0)
schemaTempReadingsSort.show()
######################################
# Q4
# Provide a list of stations with their associated maximum measured temperatures and
# maximum measured daily precipitation. Show only those stations where the maximum
# temperature is between 25 and 30 degrees and maximum daily precipitation is between 100
# mm and 200mm.
# In this exercise you will use the temperature-readings.csv and precipitation-readings.csv
# files.
# The output should contain the following information:
# Station number, maximum measured temperature, maximum daily precipitation
# station, maxTemp, maxDailyPrecipitation ORDER BY station DESC

sc = SparkContext(appName = "exercise 2")
sqlContext = SQLContext(sc)
rdd = sc.textFile("BDA/input/temperature-readings.csv")
parts = rdd.map(lambda l: l.split(";"))
tempReadings = parts.map(lambda p: Row(station=p[0],value=float(p[3])))

rdd2 = sc.textFile("BDA/input/precipitation-readings.csv")
parts2 = rdd2.map(lambda l: l.split(";"))

preReadings = parts2.map(lambda p: Row(station=p[0], date=p[1], value=float(p[3])))

schemaTempReadings = sqlContext.createDataFrame(tempReadings)
schemaTempReadings.registerTempTable("tempReadings")

schemaPreReadings = sqlContext.createDataFrame(preReadings)
schemaPreReadings.registerTempTable("preReadings")

schemaTempReadingsMax = schemaTempReadings.groupBy('station').agg(F.max('value').alias('max_temp')).orderBy(['max_temp'],ascending = 0)
schemaTempReadingsMax = schemaTempReadingsMax.filter((schemaTempReadingsMax.max_temp>=25)&(schemaTempReadingsMax.max_temp<=30))


schemaPreReadingsDaily = schemaPreReadings.groupBy('date', 'station').agg(F.sum('value').alias('daily_pre'))
schemaPreReadingsMax = schemaPreReadingsDaily.groupBy('station').agg(F.max('daily_pre').alias('max_daily')).orderBy(['max_daily'],ascending = 0)
schemaPreReadingsMax = schemaPreReadingsMax.filter((schemaPreReadingsMax.max_daily>=100)&(schemaPreReadingsMax.max_daily<=200))

join_res= schemaPreReadingsMax.join(schemaTempReadingsMax,schemaTempReadingsMax['station']==schemaPreReadingsMax['station'], 'inner')
join_res = join_res.filter(join_res.max_temp!='null')
join_res.show()
######################################
# Q5
# Calculate the average monthly precipitation for the Östergotland region (list of stations is
# provided in the separate file) for the period 1993-2016. In order to do this, you will first need to
# calculate the total monthly precipitation for each station before calculating the monthly
# average (by averaging over stations).
# In this exercise you will use the precipitation-readings.csv and stations-Ostergotland.csv
# files. HINT (not for the SparkSQL lab): Avoid using joins here! stations-Ostergotland.csv is
# small and if distributed will cause a number of unnecessary shuffles when joined with
# precipitationRDD. If you distribute precipitation-readings.csv then either repartition your
# stations RDD to 1 partition or make use of the collect function to acquire a python list and
# broadcast function to broadcast the list to all nodes.
# The output should contain the following information:
# Year, month, average monthly precipitation
# year, month, avgMonthlyPrecipitation ORDER BY year DESC, month DESC

sc = SparkContext(appName="lab2")
sqlContext = SQLContext(sc)
rdd = sc.textFile("BDA/input/stations-Ostergotland.csv")
parts = rdd.map(lambda l: l.split(";"))

staReadings = parts.map(lambda p: Row(station=p[0]))

rdd2 = sc.textFile("BDA/input/precipitation-readings.csv")
parts2 = rdd2.map(lambda l: l.split(";"))

preReadings = parts2.map(
    lambda p: Row(station=p[0], year=p[1].split("-")[0], month=p[1].split("-")[1], value=float(p[3])))

schemaStaReadings = sqlContext.createDataFrame(staReadings)
schemaStaReadings.registerTempTable("staReadings")

schemaPreReadings = sqlContext.createDataFrame(preReadings)
schemaPreReadings.registerTempTable("preReadings")

schemaPreReadings = schemaPreReadings.filter((schemaPreReadings.year >= 1993) & (schemaPreReadings.year <= 2016))

schemaPreReadings = schemaPreReadings.groupBy('year', 'month', 'station').agg((F.sum('value')).alias('sum_pre'))

# There is not only the stations in Ostland in precipitation-readings.csv,
# so we need to select the needed stations
join_res = schemaStaReadings.join(schemaPreReadings, schemaStaReadings['station'] == schemaPreReadings['station'],
                                  'inner')

join_res = join_res.groupBy('year', 'month').avg('sum_pre')

join_res = join_res.orderBy(['year', 'month'], ascending=[0, 0])
join_res.show()
