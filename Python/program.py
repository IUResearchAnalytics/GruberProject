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
import glob, os


#days
day1      = datetime.now().strftime('%m/%d/%Y')
name_date = datetime.now().strftime('%m-%d-%Y %I_%M%p')
yesterday = datetime.now() - timedelta(days=1)
day2      = yesterday.strftime('%m/%d/%Y')

#browser set up
#phantomjs = 'C:\myproject\API\phantomjs.exe'
#browser = webdriver.PhantomJS(phantomjs)
chromeOptions = webdriver.ChromeOptions()
prefs = {'download.default_directory' : 'C:\myproject\API'} #change default location
chromeOptions.add_experimental_option('prefs',prefs)
chromedriver = 'C:\myproject\API\chromedriver.exe'
browser = webdriver.Chrome(chromedriver, chrome_options=chromeOptions)
browser.get('.....')


#log in
username = browser.find_element_by_id('UserName')
password = browser.find_element_by_id('Password')
username.send_keys('...')
password.send_keys('...')
login_attempt = browser.find_element_by_xpath("//*[@type='submit']")
login_attempt.submit()

#fill the form
browser.find_element_by_id('Name').clear()
browser.find_element_by_id('StartDate').clear()
browser.find_element_by_id('EndDate').clear()

exportname = browser.find_element_by_id('Name')
startdate = browser.find_element_by_id('StartDate')
enddate = browser.find_element_by_id('EndDate')
exportname.send_keys('Export-'+ name_date)
startdate.send_keys(day2)
enddate.send_keys(day1)


browser.find_element_by_id('batchParam_IncludeFitbitStepsMinutesNarrow').click()
browser.find_element_by_id('batchParam_IncludeFitbitIntensityMinutesNarrow').click()
##browser.find_element_by_id('batchParam_IncludeFitbitCaloriesMinutesNarrow').click()
##browser.find_element_by_id('batchParam_IncludeFitbitSleepLogs').click()
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
print 'Wait for 60 seconds to generate the project'
browser.save_screenshot('C:\myproject\API\screen.jpg')
#postpone for a minute to get the link
time.sleep(60) #two options take ca.30s

#download
download_page = browser.get('....')
html          = browser.page_source
soup          = BeautifulSoup(html)
variable      = soup.find_all('a')[5]
link          = 'https://app.fitabase.com' + variable.attrs['href']
browser.get(link)
print 'Download!'

#unzip file and delete zip file
os.chdir('C:\myproject\API')
filename = glob.glob('*.zip')
new = str(filename).strip("'[]'")
path = 'C:\myproject\API\\' + new
print path
zip_ref = zipfile.ZipFile(path, 'r')
zip_ref.extractall('C:\myproject\API')
zip_ref.close()
os.remove(path)

#upload files to database & delete files
#see upzip.py
