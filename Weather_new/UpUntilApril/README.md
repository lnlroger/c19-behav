# Gathering weather data for JHU's COVID19 infection data

<br>
<br>
---<br>
update 3/27/2020: JHU has changed some of its grouping for certain regions and/or countries.<br>
I may start to manually break down some of the more significant regions in the US,<br>
Such as Seattle, NYC, etc, and elsewhere. I need to think about how I want to do this.<br>
---<br>
<br>
<br>
<b>The data files are in the './csv' folder</b><br>
This is all just raw data. I have not done any analysis yet.<br>

Weather data is very generously Powered by Dark Sky: https://darksky.net/poweredby/

JHU's time_series_19-covid-Confirmed.csv was taken, and repurposed it to get weather data for the dates and locations that are listed.

<b>The following csv files are generated in the './csv' folder:<br></b>
  tMax.csv       - pulling 'temperatureHigh' from API call<br>
  tMin.csv       - pulling 'temperatureLow' from API call<br>
  humidity.csv   - pulling 'humidity' from API call, and multiplying by 100<br>
  uv.csv         - pulling 'uvIndex' from API call<br>
  cloud.csv      - pulling 'cloudCover' from API call and mulitplying by 100<br>
  precip.csv     - pulling 'precipProbability' from API call and multiplying by 100<br>
  dew.csv        - pulling 'dewPoint' from API call<br>
  pressure.csv   - pulling 'pressure" from API call<br>
  wind.csv       - pulling 'windSpeed' from API call<br>
  ozone.csv      - pulling 'ozone' from API call<br>
  sunrise.csv    - pulling 'sunriseTime' from API call<br>
  sunset.csv     - pulling 'sunsetTime' from API call<br>
  
<b>Values of -1000 are dummy values for when there was some sort of error in the JSON response.<br>
This will eventually be investigated and corrected.</b><br><br>
Please see https://darksky.net/dev/docs for meaning and units of each value. Where applicable, units are pulled in SI units.


The JSON returned hourly values and daily values. For the purposes of this project, daily values were used. The header for each column was taken and passed as a Unix time value to retrieve data for that date.<br><br><br>

Also, don't bother trying anything with my API key. It is reset with every push. If you want to try it for yourself, Dark Sky very generously offers 1,000 free API calls per day. https://darksky.net/poweredby/



=======
# Below is JHU's original information for the README.md


# 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE


This is the data repository for the 2019 Novel Coronavirus Visual Dashboard operated by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Also, Supported by ESRI Living Atlas Team and the Johns Hopkins University Applied Physics Lab (JHU APL).

<br>

<b>Visual Dashboard (desktop):</b><br>
https://www.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6
<br><br>
<b>Visual Dashboard (mobile):</b><br>
http://www.arcgis.com/apps/opsdashboard/index.html#/85320e2ea5424dfaaa75ae62e5c06e61
<br><br>
<b>Lancet Article:</b><br>
[An interactive web-based dashboard to track COVID-19 in real time](https://doi.org/10.1016/S1473-3099(20)30120-1)
<br><br>
<b>Provided by Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE):</b><br>
https://systems.jhu.edu/
<br><br>
<b>Data Sources:</b><br>
* World Health Organization (WHO): https://www.who.int/ <br>
* DXY.cn. Pneumonia. 2020. http://3g.dxy.cn/newh5/view/pneumonia.  <br>
* BNO News: https://bnonews.com/index.php/2020/02/the-latest-coronavirus-cases/  <br>
* National Health Commission of the People’s Republic of China (NHC): <br>
 http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml <br>
* China CDC (CCDC): http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm <br>
* Hong Kong Department of Health: https://www.chp.gov.hk/en/features/102465.html <br>
* Macau Government: https://www.ssm.gov.mo/portal/ <br>
* Taiwan CDC: https://sites.google.com/cdc.gov.tw/2019ncov/taiwan?authuser=0 <br>
* US CDC: https://www.cdc.gov/coronavirus/2019-ncov/index.html <br>
* Government of Canada: https://www.canada.ca/en/public-health/services/diseases/coronavirus.html <br>
* Australia Government Department of Health: https://www.health.gov.au/news/coronavirus-update-at-a-glance <br>
* European Centre for Disease Prevention and Control (ECDC): https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases 
* Ministry of Health Singapore (MOH): https://www.moh.gov.sg/covid-19
* Italy Ministry of Health: http://www.salute.gov.it/nuovocoronavirus
* 1Point3Arces: https://coronavirus.1point3acres.com/en
* WorldoMeters: https://www.worldometers.info/coronavirus/
* COVID Tracking Project: https://covidtracking.com/data. (US Testing and Hospitalization Data. We use the maximum reported value from "Currently" and "Cumulative" Hospitalized for our hospitalization number report ed for each state.)

<br>
<b>Additional Information about the Visual Dashboard:</b><br>
https://systems.jhu.edu/research/public-health/ncov/
<br><br>

<b>Contact Us: </b><br>
* Email: jhusystems@gmail.com
<br><br>

<b>Terms of Use:</b><br>

This GitHub repo and its contents herein, including all data, mapping, and analysis, copyright 2020 Johns Hopkins University, all rights reserved, is provided to the public strictly for educational and academic research purposes.  The Website relies upon publicly available data from multiple sources, that do not always agree. The Johns Hopkins University hereby disclaims any and all representations and warranties with respect to the Website, including accuracy, fitness for use, and merchantability.  Reliance on the Website for medical guidance or use of the Website in commerce is strictly prohibited.
