# read the Diabetes.txt data
fileHanl = open("Diabetes.txt", "r")
destFileHanl = open("Diabetes.csv", "w")
destFileHanl.write("g.area,i.area,SSPG,weight,fp.glucose,class\n")
while True:
    line = fileHanl.readline()
    if not line:
        break
    vals = line.split(' ')
    finalVals = []
    colsSeen = 0
    for val in vals:        
        if val != "":
            if colsSeen >= 3:
                finalVals.append(val.strip().replace('\n',''))
            colsSeen += 1
    
    for i in range(1, len((finalVals))):
        if i == len(finalVals)-1:
            destFileHanl.write(finalVals[i]+"\n")
        else:
            destFileHanl.write(finalVals[i]+",")

fileHanl.close()
destFileHanl.close()