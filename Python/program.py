#This is the program for the entire process
#Seperated jobs include download, unzip
#The local path is C:\myproject\API


from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from datetime import datetime, timedelta
from bs4 import BeautifulSoup
import requests
import time
import zipfile
import glob, os, os.path
import MySQLdb
import csv
import time
import sys


#days
daypoint1  = datetime.now() - timedelta(days=30)  #last week today
day1      = daypoint1.strftime('%m/%d/%Y') #end
daypoint2 = datetime.now() - timedelta(days = 30)  #last week 
day2      = daypoint2.strftime('%m/%d/%Y') #start
name_date = daypoint2.strftime('%m-%d-%Y') #show only date


#browser set up
#phantomjs = 'C:\myproject\API\phantomjs.exe' #run prgram without opening a browser
#browser = webdriver.PhantomJS(phantomjs)
chromeOptions = webdriver.ChromeOptions()
prefs = {'download.default_directory' : 'C:\myproject\API'} #change default location
chromeOptions.add_experimental_option('prefs',prefs)
chromedriver = 'C:\myproject\API\chromedriver.exe'
browser = webdriver.Chrome(chromedriver, chrome_options=chromeOptions)
browser.get('https://app.fitabase.com/DownloadData/CreateBatch/f4def67f-9081-4534-bbf4-3741be7d59df')


#log in
username = browser.find_element_by_id('UserName')
password = browser.find_element_by_id('Password')
username.send_keys('ahgruber')
password.send_keys('iubmlPA2015')
login_attempt = browser.find_element_by_xpath("//*[@type='submit']")
login_attempt.submit()

#fill the form
browser.find_element_by_id('Name').clear()
browser.find_element_by_id('StartDate').clear()
browser.find_element_by_id('EndDate').clear()

exportname = browser.find_element_by_id('Name')
startdate = browser.find_element_by_id('StartDate')
enddate = browser.find_element_by_id('EndDate')
exportname.send_keys('Export-'+ name_date) #job name contains only date
startdate.send_keys(day2)
enddate.send_keys(day1)


browser.find_element_by_id('batchParam_IncludeFitbitStepsMinutesNarrow').click()
browser.find_element_by_id('batchParam_IncludeFitbitIntensityMinutesNarrow').click()
##browser.find_element_by_id('batchParam_IncludeFitbitCaloriesMinutesNarrow').click()
browser.find_element_by_id('batchParam_IncludeFitbitSleepLogs').click()
##browser.find_element_by_id('batchParam_IncludeFitbitWeightLogs').click()
##browser.find_element_by_id('batchParam_IncludeFitbitMETsMinutesNarrow').click()
##browser.find_element_by_id('batchParam_IncludeFitbitHeartRateRawSesconds').click()
##browser.find_element_by_id('batchParam_IncludeFitbitStepsMinutes').click()
##browser.find_element_by_id('batchParam_IncludeFitbitIntensityMinutes').click()
##browser.find_element_by_id('batchParam_IncludeFitbitCaloriesMinutes').click()
##browser.find_element_by_id('batchParam_IncludeFitbitMETsMinutes').click()
##browser.find_element_by_id('batchParam_IncludeFitbitStepsHourly').click()
##browser.find_element_by_id('batchParam_IncludeFitbitIntensityHourly').click()
##browser.find_element_by_id('batchParam_IncludeFitbitCaloriesHourly').click()
##browser.find_element_by_id('batchParam_IncludeFitbitDailyActivity').click()
##browser.find_element_by_id('batchParam_IncludeFitbitDailySteps').click()
##browser.find_element_by_id('batchParam_IncludeFitbitDailyIntensity').click()
##browser.find_element_by_id('batchParam_IncludeFitbitDailyCalories').click()
##browser.find_element_by_id('batchParam_IncludeFitbitDailySleep').click()

browser.find_element_by_xpath("//*[@class='btn']").click()

#submit 
project_create = browser.find_element_by_xpath("//*[@type='submit']")
project_create.submit()
print 'Project created successfully'
print 'Wait for 300 seconds to generate the project'
#browser.save_screenshot('C:\myproject\API\screen.jpg')
#postpone for a minute to get the link
time.sleep(300) #two options take ca.30s

#download
download_page = browser.get('https://app.fitabase.com/DownloadData/Project/f4def67f-9081-4534-bbf4-3741be7d59df')
html          = browser.page_source
soup          = BeautifulSoup(html)
variable      = soup.find_all('a')[5]
link          = 'https://app.fitabase.com' + variable.attrs['href']
browser.get(link)
print 'Download!'

#unzip file and delete zip file
#find zip file
os.chdir('C:\myproject\API')
filename = glob.glob("*.zip")
new = str(filename).strip("'[]'") #xxx.zip
##unzip
path = 'C:\myproject\API\\' + new
zip_ref = zipfile.ZipFile(path, 'r')
zip_ref.extractall('C:\myproject\API')
zip_ref.close()
print new + ' is unzipped'

os.remove(path)#delete file

##upload files to database & delete files
#connector
conn = MySQLdb.connect (user="root",
                        host="localhost",
                        db="fitabase")
cursor = conn.cursor()


#intensity
new2 = day2   
cursor.execute("INSERT INTO history (name) VALUES (%s)", {new2})
cursor.execute("LOAD DATA LOCAL INFILE 'C:\\\myproject\\\API\\\minuteIntensitiesNarrow_merged.csv' INTO TABLE intens FIELDS TERMINATED BY ',' \
                IGNORE 1 LINES (ID, @timevar, intensity) \
                set in_time = STR_TO_DATE(@timevar, '%m/%d/%Y %r'),\
                    record_ID = LAST_INSERT_ID()")
conn.commit()
print "Intensity uploaded!"
os.remove('C:\\\myproject\\\API\\\minuteIntensitiesNarrow_merged.csv')#delete file
print "Intensity deleted!"

#step
cursor.execute("LOAD DATA LOCAL INFILE 'C:\\\myproject\\\API\\\minuteStepsNarrow_merged.csv' INTO TABLE step FIELDS TERMINATED BY ',' \
                IGNORE 1 LINES (ID, @timevar, step)\
                set st_time = STR_TO_DATE(@timevar, '%m/%d/%Y %r')")
conn.commit()
print "Step uploaded!"
os.remove('C:\\\myproject\\\API\\\minuteStepsNarrow_merged.csv')#delete file
print "Step deleted!"

#sleep
cursor.execute("LOAD DATA LOCAL INFILE 'C:\\\myproject\\\API\\\minuteSleep_merged.csv' INTO TABLE sleep FIELDS TERMINATED BY ',' \
                IGNORE 1 LINES (ID, @timevar, sleep)\
                set sl_time = STR_TO_DATE(@timevar, '%m/%d/%Y %r')")
conn.commit()
print "Sleep uploaded!"
os.remove('C:\\\myproject\\\API\\\minuteSleep_merged.csv')#delete file
print "Sleep deleted!"

print "Finished upload the data of " + day2















