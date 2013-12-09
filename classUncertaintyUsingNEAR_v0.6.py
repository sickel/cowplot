#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      Frits
#
# Created:     11/12/2012
# Copyright:   (c) Frits 2012
# Licence:     <your licence>
#-------------------------------------------------------------------------------


"""
some assumptions:
NEAR table also needs angle!!

1) if the distance in the near file is 0, this is the native class of the point
2) the NEAR table is sorted on FID

The near table is NEAR_AS_TABLE on the points with the aggregated polys as near fc.
the inVegetationPolyTable is the attr tab of the dissolved veg classes fc (remove .000000000000 from the class in 10.1)

classes to be excluded:
	Valdres: Inmark (4) and Solstun/Hytteblabla (8)

New in 0.5: if a class is in the 'forbidden' class list, like in fenced, the percentage is set to 0%
and a flag is raised. The warning will be added to the file (last field, exclusionFlag)

NEW in 0.6: attempt to avoid class jumps while parallel polys are involved
This uses the NEAR angle. angle added to the near dict

	 to do: CHANGE LOCATION OF BARN CODE IN THE POLYGON DATA TO SPECIAL NUMBER
"""
import datetime, sys, math
from datetime import datetime

inGPSpointFile = "ValdresGPSpointsALL_28feb2013.txt" #"valdresLimitedSet04mar2013.txt" #
inVegetationPolyTable = "vegClassTableValdres_28feb2013.txt"
inNEARfileName = "nearTableValdresAll_28feb2013.txt"

outFileName = "valdres_ALL_12maart2013.txt"

f_NEARtable = open(inNEARfileName, "r")
f_vegetationPolys = open(inVegetationPolyTable, "r")
f_GPSpoints = open(inGPSpointFile, "r")
f_outFile = open(outFileName, "w")
cleanedNEARdict = {}
vegClassDict = {}

nearRadius = 5 # 5meter
circleSurface = math.pi*(nearRadius**2)
excludedClassesList = ["4","8"]

def makeVegetationClassDict():
	firstLine = True
	for line in f_vegetationPolys:
		if firstLine:
			firstLine = False
		else:
			items = line.split(",")

# DIT GAAT FOUT...
			FID = items[0] # CHECK THIS, OBJECT ID IN THIS FILE....
			vegCode = items[1].strip("\n")

			vegClassDict[FID] = vegCode
			#print FID, vegCode
			#print vegClassDict

	return


def cleanNEARfile():
	firstLine = True
	maxClasses = 0

	for line in f_NEARtable:
		tmpList = list()
		if firstLine:
			firstLine = False
		else:
			items = line.split(",")
			inFID = items[1]
			nearFID = items[2]
			vegCode = vegClassDict[nearFID]
			distance = float(items[3].strip("\n"))
			angle = float(items[4].strip("\n"))
			tmpKey = str(inFID)

			if tmpKey in cleanedNEARdict: # there is already polygon within the search range
				tmpList = cleanedNEARdict[tmpKey]
				tmpIDdistanceTuple = (nearFID, distance, vegCode, angle)
				tmpList.append(tmpIDdistanceTuple)
				cleanedNEARdict[tmpKey] = tmpList
			else: # no entry yet exists
				tmpIDdistanceTuple = (nearFID, distance, vegCode, angle)
				tmpList.append(tmpIDdistanceTuple)
				cleanedNEARdict[tmpKey] = tmpList

		if len(tmpList) > maxClasses:
			#print inFID, tmpList
			maxClasses += 1
			#print "max. number of classes:" + str(maxClasses)

	print str(maxClasses)
	return(maxClasses)


def runPointsFile(maxClasses):
	counter = 0
	counter100k = 0
	firstLine = True
	tooBigFlag = False
	exclusionFlag = False

	for line in f_GPSpoints:
		counter += 1
		nativeFlag = False
		exclusionText = ""
		if counter == 100000:
			counter100k += 100000
			counter = 0
			print "line: "+str(counter100k)

		percentageTotal = 0
		nativePercentage = 100
		previousAngle = -9999

		if firstLine:
			# make the header line...
			headerLine = line.strip("\n")
			headerLine = headerLine.replace(",", "\t") + "\tnativeClass\tperc_native"
			for i in range (1, maxClasses+1):
				className = "class_" + str(i)
				percName = "perc_" + str(i)
				headerLine = headerLine + "\t" + className + "\t" + percName
			f_outFile.write(headerLine + "\texcludedClassFlag\n")
			firstLine = False
		else:
			items = line.split(",")
			FID = items[0]
			try:
				del chanceTuple
				del chanceList
			except:
				pass

			if FID in cleanedNEARdict:
				#percentageTotal = 0
				#nativePercentage = 100
				chanceList = []
				#print FID,cleanedNEARdict[FID]
				possibleVegetationClassesList = cleanedNEARdict[FID]
				for nearFID, distance, vegCode, angle in possibleVegetationClassesList:
					thisClassPercentage = 0
					#print FID,nearFID,str(distance)
					#print str(distance)
					if distance == 0: # the original class for this point...
						#print "classTuple 0: " + str(classTuple)
						nativeClass = vegCode
						nativeFlag = True
						if vegCode in excludedClassesList:
							exclusionFlag = True
							exclusionText = "excluded class"
							#print "exclusionFlag"
					else:
						# print "classTuple non 0: " + str(classTuple)
						# http://www.mathopenref.com/segmentarea.html
						vegetationClass = vegCode

						angleAlpha = math.degrees(math.acos(distance/nearRadius)) * 2
						#angleAlpha = 90
						segmentSurface = ((nearRadius**2)/2) * (((math.pi /180)*angleAlpha) - math.sin(math.radians(angleAlpha)))

						thisClassPercentage = (segmentSurface / circleSurface) * 100


						if int(FID) in [488729, 126891]: # list to test specific points
							print str(FID), str(nearFID), str(distance), str(angleAlpha),str(segmentSurface), str(thisClassPercentage), str(percentageTotal)

						if thisClassPercentage < 0 and distance > 0.074:
							tooBigFlag = True
						#print "thisClass: "+str(thisClassPercentage) + "%"
						#percentageTotal += thisClassPercentage

						nativePercentage = nativePercentage - thisClassPercentage
						percentageTotal += thisClassPercentage

						#check for parallel polys problem
						if angle <> -9999 :
							if angle < (previousAngle + 10) and angle > (previousAngle - 10):
								nativePercentage += thisClassPercentage
								thisClassPercentage = 0.0
								exclusionText = "parallel polys"
								#print "parallelFlag"
						previousAngle = angle

						chanceTuple = (vegetationClass, "%.2f" % thisClassPercentage)
						#nativePercentage -= thisClassPercentage
						#print "native: "+str(nativePercentage) +"%"
						if chanceTuple:
							chanceList.append(chanceTuple)

				if nativeFlag:
					percentageTotal += nativePercentage
					if exclusionFlag:
						nativePercentage = 0.0
						exclusionFlag = False
					chanceTuple = (nativeClass, "%.2f" % nativePercentage)
					chanceList.append(chanceTuple)
					#print str(FID), str(nearFID), str(angleAlpha),str(segmentSurface), str(nativePercentage), str(percentageTotal)


				sortedChanceList = sorted(chanceList, key=lambda perc: float(perc[1]), reverse= True)
				#print "sorted list: "+str(sortedChanceList)

				rm = "[]()'"
				cleanList1 = filter(lambda x: not (x in rm), str(sortedChanceList))
				cleanList = cleanList1.replace(", ","\t")
				#print cleanList
				line = line.replace(",", "\t")

				# THERE ARE LIKELY LESS OPTIONS THAN THE MAX NUMBER OF CLASSES FOUND
				# SO THE MISSING FIELD SEPERATORS ARE ADDED AS ONE STRING
				numberOfEmptyFields = maxClasses - len(sortedChanceList)
				#print str(numberOfEmptyFields)
				emptyFieldsString = ""
				for i in range(0, numberOfEmptyFields):
					emptyFieldsString = emptyFieldsString + "\t\t"

				writeLine = line.strip("\n") + "\t" + str(nativeClass) + "\t" + str("%.2f" % nativePercentage) + "\t" + cleanList + emptyFieldsString + "\t" + exclusionText +"\n"
				f_outFile.write(writeLine)

				if tooBigFlag:
					print "segment surface too big"
					print line
					possibleVegetationClassesList
					print cleanList1

					print chanceList
					print writeLine
					print "\n\n"
					sys.exit("___")

#		if counter == 3:
#			sys.exit("___")


def main():
	startTime = datetime.now()
	print "start time: ", startTime

	makeVegetationClassDict()
	maxClasses = cleanNEARfile()
	runPointsFile(maxClasses)

	usedTime = datetime.now() - startTime
	print "used time: ", usedTime

	f_GPSpoints.close()
	f_NEARtable.close()
	f_outFile.close()
	f_vegetationPolys.close()





if __name__ == '__main__':
	main()

