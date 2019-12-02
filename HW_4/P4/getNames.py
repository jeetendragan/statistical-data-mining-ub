f = open("FeatureNames.txt", "r")
lines = f.readlines()
featureNames = []
for line in lines:
	splitLine = line.split(' ')
	if splitLine[0] != '':
		featureNames.append(splitLine[0].replace(':', ''))
f.close()
featureNames.append("IsSpam")
strcsv = ','.join(featureNames)
f = open('FeatureNames.csv', 'w')
f.write(strcsv)
f.close()


