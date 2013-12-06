#!/usr/bin/python
import sys
file=sys.argv[1]
output=file.replace('.xlsx','.html')
sheetname='Ark1'
colbase=1
firstrow=3
#if file=='Halingdal\ beitelok.xlsx' or file=='Valdres beitelok.xlsx':
#  firstrow=3
import pprint
pp = pprint.PrettyPrinter(indent=4)
from xlrd import open_workbook
book = open_workbook(file,on_demand=True)
sheet = book.sheet_by_name(sheetname)
ncols=sheet.ncols-1
nrows=sheet.nrows-1
colIndex=-1
ppranks={}
vegtypes=[]
sortspec=[]
for cell in sheet.row(0): # 
   colIndex=colIndex+1
   if cell.value != ""and cell.value!="%" and colIndex<(ncols):
      print colIndex
      templist={}
      print cell.value
      vegtype=cell.value
      vegtypes.append(vegtype)
      ncol=0
      pcol=colIndex+colbase
      #print sheet.cell_value(3,ncol)
      print "=========="
      oldprec=0
      for r in range(firstrow,nrows):
	 prec=sheet.cell_value(r,pcol)
	 if prec>0:
	    spec=sheet.cell_value(r,ncol)
	    ppranks[vegtype,spec,'prec']=prec
	    templist[spec]=prec
      tempsortspec=sorted(templist, key=templist.get, reverse=True)
      rank=0
      oldprec=0
      for spec in tempsortspec:
	prec=ppranks[vegtype,spec,'prec']
	if oldprec!=prec:
	  rank=rank+1
	ppranks[vegtype,spec,'rank']=rank
	oldprec=prec
      #pp.pprint(ppranks)
      for spec in tempsortspec:
	 if (not(spec in sortspec)):
	   sortspec.append(spec)
#     exit()
# print sortspec
#exit()
book.unload_sheet(sheetname) 
# Finished reading in data.

minrank={}
ranksum={}
ranknum={}
for key,item in ppranks.iteritems():
  if key[2]=='rank':
    if key[1] in minrank:
      minrank[key[1]]=min(minrank[key[1]],item)
    else:
      minrank[key[1]]=item
    
rankavg={}
highranks={}
highrankspec={}
# Wants only to keep those that are rank >= 10 in at least one location
for key,item in ppranks.iteritems():
#  print(key)
#  print key[1]
  if minrank[key[1]]>10:
    #highranks[key]=item
    if key[1] in sortspec:
      sortspec.remove(key[1])
    #avgrank[key[1]]=ranksum[key[1]]/ranknum[key[1]]
    

#pp.pprint(highranks)
#pp.pprint(highrankspec)
#sortspec=sorted(highrankspec, key=highrankspec.get)
#sortspec=highrankspec
#print(sortspec)
#print(highrankspec)
f=open(output,'w')

f.write('<html><head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /></head><body>')
f.write("<table border=\"1\"><thead><tr><th></th>")
for v in vegtypes:
  f.write(u"<th>"+v+u"</th>")
f.write("</tr></thead><tbody>")
for s in sortspec:
  # print type(s.encode('utf8'))
  f.write(u"<tr><td>")
  f.write(s.encode('utf8'))
  f.write(u"</td>")
  for v in vegtypes:
    if (v,s,'prec') in ppranks:
      f.write(u"<td>"+str(round(ppranks[v,s,'prec'],2))+u" ("+str(ppranks[v,s,'rank'])+u")</td>")
    else:
      f.write(u"<td> </td>")
  f.write(u"</tr>")
f.write(u"</tbody></table>")
f.write('</body></html>')


