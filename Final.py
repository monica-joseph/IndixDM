# setting the working directory
import os as os
os.chdir("D:\Learn R\IndixDM")
#reading train data
import pandas as pd
Indix_Train = pd.read_csv("data.csv",sep=",", encoding="utf-8")
print(Indix_Train.shape[0])
#reading train data
import pandas as pd
Indix_Test = pd.read_csv("blindset_table_out.csv",sep=",", encoding="utf-8")
print(Indix_Test.shape[0])
##############################################################################################################
import pandas as pd
fullDataText=pd.read_csv("fullData.csv",sep=",", encoding="utf-8")
#split into Train and Test
IndixDM_Train_Text=fullDataText[fullDataText['label']!='Test']
IndixDM_Test_Text=fullDataText[fullDataText['label']=='Test']
##############################################################################################################
#testing HTML parser
from HTMLParser import HTMLParser

# create a subclass and override the handler methods
class MyHTMLParser(HTMLParser):
       def handle_data(self, data):
        #print "Encountered some data  :", data
        return data

# instantiate the parser and fed it some HTML
parser = MyHTMLParser()
parser.feed('<html><head><title>Test</title></head>'
            '<body><h1>Parse me!</h1></body></html>')
##############################################################################################################
#final HTML parser
from HTMLParser import HTMLParser

pstring = sample


class myhtmlparser(HTMLParser):
    def __init__(self):
        self.reset()
        self.HTMLDATA = []
    def handle_data(self, data):
        self.HTMLDATA.append(data.strip(' \n\t\r'))

    def clean(self):
        self.HTMLDATA = []

parser = myhtmlparser()
parser.feed(pstring)

# Extract data from parser
data  = parser.HTMLDATA

#remove invalids
data=filter(None,data)
# Clean the parser
parser.clean()

# Print out our data

print data
##############################################################################################################
#Splitting into Tokens
import time
start_time = time.time()
import re
delimiters = "/"
regexPattern = '|'.join(map(re.escape, delimiters))
#Indix_Done=[]
#initializing parser
parser = myhtmlparser()
count_done=0
#existing columns ---Index([u'label', u'table-text', u'url'], dtype='object')
#adding ten columns with tokens obtained
try:
    for r in zip(Indix_Train['table-text'],Indix_Train['label'],Indix_Train['url']):
        if(count_done>45362):
            parser.feed(r[0])
            # Extract data from parser
            data  = parser.HTMLDATA
            #remove invalids
            data=filter(None,data)
            root  = re.split(regexPattern, r[2])

            if (len(data)<10):
                    data[len(data):11]=[u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None']
            #remove invalids
            data=filter(None,data)
            Indix_Done.append({'label':r[1],
                                'table-text':r[0],
                                'url':r[2],
                                'root':root[0],
                                'V1':data[0],
                                'V2':data[1],
                                'V3':data[2],
                                'V4':data[3],
                                'V5':data[4],
                                'V6':data[5],
                                'V7':data[6],
                                'V8':data[7],
                                'V9':data[8],
                                'V10':data[9]
                                })
        count_done=count_done+1
        # Clean the parser
        parser.clean()
except Exception:
    print ("In Excception Block",count_done)
    pass
print("--- %s seconds ---" % (time.time() - start_time))
##############################################################################################################
#get the HTML data for Test
import time
start_time = time.time()
Indix_Done=[]
#initializing parser
parser = myhtmlparser()
count_done=0
#existing columns ---Index([u'label', u'table-text', u'url'], dtype='object')
#adding ten columns with tokens obtained
try:
    for r in zip(Indix_Test['table-text'],Indix_Test['id'],Indix_Test['url'],Indix_Test['site']):
            parser.feed(r[0])
            # Extract data from parser
            data  = parser.HTMLDATA
            #remove invalids
            data=filter(None,data)
            if (len(data)<10):
                    data[len(data):11]=[u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None',u'None']
            #remove invalids
            data=filter(None,data)
            Indix_Done.append({'label':'Test',
                                'table-text':r[0],
                                'id':r[1],
                                'url':r[2],
                                'site':r[3],
                                'V1':data[0],
                                'V2':data[1],
                                'V3':data[2],
                                'V4':data[3],
                                'V5':data[4],
                                'V6':data[5],
                                'V7':data[6],
                                'V8':data[7],
                                'V9':data[8],
                                'V10':data[9]
                                })
            count_done=count_done+1
            # Clean the parser
            parser.clean()
except Exception:
    print ("In Excception Block",count_done)
    pass
print("--- %s seconds ---" % (time.time() - start_time))
##############################################################################################################
#getting data into dataframes
Indix_Tokens=pd.DataFrame.from_records(Indix_Done)
Indix_Tokens_Test=pd.DataFrame.from_records(Indix_Done)
fullData = pd.concat([Indix_Tokens,Indix_Tokens_Test],axis=0) #Combined both Train and Test Data set
##############################################################################################################
#fill missing data
#variables
ID_col = ['id']
target_col = ['label']
cat_cols = ['site','url','table-text','V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','Root']
num_cols= []
other_col=[] #Test and Train Data set identifier
num_cat_cols = num_cols+cat_cols # Combined numerical and Categorical variables
#Impute categorical missing values with -9999
fullDataText[cat_cols] = fullDataText[cat_cols].fillna(value = -9999)
##############################################################################################################
#assign numerical codes to columns
fullDataText.dtypes
for var in cat_cols:
    fullDataText[var]=fullDataText[var].astype('category')
#applying category code to convert into numeric
cat_columns = fullDataText.select_dtypes(include=['category']).columns

fullData[cat_columns] = fullDataText[cat_columns].apply(lambda x: x.cat.codes)
#list of features
NA_Cols = ['V2_NA', 'V3_NA','V4_NA', 'V5_NA','V6_NA', 'V7_NA','V8_NA', 'V9_NA', 'V10_NA']
features=list(set(list(fullData.columns))-set(ID_col)-set(target_col)-set(other_col)-set(NA_Cols))
features
fullData['label']=fullData['label'].astype('object')
fullData['label'][fullData['label']=='yes']=1
fullData['label'][fullData['label']=='no']=0
fullData['label'].value_counts()
#split Train and Test
IndixDM_Train=fullData[fullData['label']!='Test']
IndixDM_Test=fullData[fullData['label']=='Test']
##############################################################################################################
#Run 1 --FIRST HIGH SCORE .81
x_train = IndixDM_Train[list(features)].values #removed Train because auc is not working
y_train = IndixDM_Train["label"].values #
x_test=IndixDM_Test[list(features)].values
features
#without only data tokens
import random as random
from  sklearn.ensemble import RandomForestClassifier
random.seed(100)
rf = RandomForestClassifier(n_estimators=1000)
rf.fit(x_train, y_train)
final_status_class = rf.predict(x_test)
len(final_status_class)
df=pd.DataFrame.from_records((final_status_class.astype('str')))
df[0].value_counts()
df[0]=df[0].apply(lambda x:yesno(x))
df[0].value_counts()
df.to_csv('RF_submit1.csv',columns=[0],index=False)
#run 1
#0    10902
#1      940
##############################################################################################################
#Run 2 --WITH Root or site variable
#get the root variable
import time
start_time = time.time()
import re
delimiters = "/"
regexPattern = '|'.join(map(re.escape, delimiters))
Rooturl=[]
count_done=0
try:
    for r in zip(fullDataText['url'],fullDataText['label']):
            root  = re.split(regexPattern, r[0])
            Rooturl.append({'Root':root[0],
                                 'id':r[1]})
            count_done=count_done+1
except Exception:
    print ("In Excception Block",count_done)
    pass
print("--- %s seconds ---" % (time.time() - start_time)) 
#add it to fullData
fullDataRoot=pd.DataFrame.from_records(Rooturl)
fullDataRoot.shape[0]
fullDataText['Root']=fullDataRoot['Root']
#then convert to numerical codes as in code above
#Run  ---SECOND HIGH SCORE.86
##############################################################################################################
#Run 2 --Added root website
x_train = IndixDM_Train[list(features)].values #removed Train because auc is not working
y_train = IndixDM_Train["label"].values #
x_test=IndixDM_Test[list(features)].values
features
import random as random
from  sklearn.ensemble import RandomForestClassifier
random.seed(100)
rf = RandomForestClassifier(n_estimators=1000)
rf.fit(x_train, y_train)
final_status_class = rf.predict(x_test)
len(final_status_class)
df=pd.DataFrame.from_records((final_status_class.astype('str')))
df[0].value_counts()
df[0]=df[0].apply(lambda x:yesno(x))
df[0].value_counts()
df.to_csv('RF_submit3.csv',columns=[0],index=False)



