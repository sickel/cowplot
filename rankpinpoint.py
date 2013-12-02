#!/usr/bin/python
file='Utvalg beitelokaliteter_Hallingdal_lat.xlsx'
sheetname='Hallingdalv1b'
colbase=3
firstrow=3
output='Ranktable_Hallingdal.html'
file='Utvalg_beiteflekker_Buodden_lat.xlsx'
sheetname='Ark1'
colbase=2
firstrow=4
output='Ranktable_Buodden.html'
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
for cell in sheet.row(0): # 
   colIndex=colIndex+1
   if cell.value != "" and colIndex<(ncols-3):
      #print colIndex
      print cell.value
      vegtype=cell.value
      vegtypes.append(vegtype)
      ncol=colIndex+colbase
      pcol=colIndex+colbase+1
      #print sheet.cell_value(3,ncol)
      print "=========="
      rank=0
      oldprec=0
      for r in range(firstrow,nrows):
	 prec=sheet.cell_value(r,pcol)
	 if prec>0:
	    spec=sheet.cell_value(r,ncol)
	    ppranks[vegtype,spec,'prec']=prec
	    if oldprec!=prec:
	      rank=rank+1
	    ppranks[vegtype,spec,'rank']=rank
	    oldprec=prec
book.unload_sheet(sheetname) 
# Finished reading in data.


minrank={}
ranksum={}
ranknum={}
for key,item in ppranks.iteritems():
  if key[2]=='rank':
    if key[1] in minrank:
      minrank[key[1]]=min(minrank[key[1]],item)
      ranksum[key[1]]+=item
      ranknum[key[1]]+=1
    else:
      minrank[key[1]]=item
      ranksum[key[1]]=item
      ranknum[key[1]]=1

rankavg={}
highranks={}
highrankspec={}
# Wants only to keep those that are rank >= 10 in at least one location
for key,item in ppranks.iteritems():
  if minrank[key[1]]<=10:
    highranks[key]=item
    #avgrank[key[1]]=ranksum[key[1]]/ranknum[key[1]]
    
for key,item in ranksum.iteritems():
  if minrank[key]<=10:
    highrankspec[key]=item/ranknum[key] # Calculates the avrage rank for each species

#pp.pprint(highranks)
#pp.pprint(highrankspec)
sortspec=sorted(highrankspec, key=highrankspec.get)
#print(sortspec)
#print(highrankspec)
f=open(output,'w')


f.write("<table><thead><tr><th></th>")
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



