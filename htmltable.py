#!/usr/bin/python
import sys


input=sys.argv[1]
output=input.split('.')[0]+'.html'
print(output)
fi=open(input,'r')
fo=open(output,'w')
countcol=10
goods=[11,13,17,21]
bads=[12,14,15,16,18,19,20]
statics=[3,4,5,6,7,8]
goods=[10,12,16,20]
bads=[11,13,14,15,17,18,19]
statics=[1,2,3,4,5,6]
fo.write("""<html><head>
<style type="text/css">
td.good {
  color: black;
  background-color: lightgreen;
}
td.bad {
  color: black;
  background-color: pink;
}

td.mytdclass:hover {
  color: yellow;
  background-color: #c0c0c0;
}
</style>
</head><body><table><thead><tr><th></th>\n""")
header=fi.readline()
for head in header.split(','):
   fo.write("<th>"+head+"</th>")
fo.write("</tr></thead><tbody>")
totals=[]
statvalues=[]
# statstore=false
for line in fi.readlines(): 
    index=0
    fo.write("<tr>")
    for item in line.split(','):
      # if index in statics:
          
          
      try:
          value=int(item)
      except ValueError:
          value=0
      if len(totals)<index+1 :
          totals.append(value)
      else:
          totals[index]+=value;
      if index in goods:
         fo.write('<td class="good">')
      elif index in bads:
          fo.write('<td class="bad">')
      else:
          fo.write('<td>')
      fo.write(item+"</td>")
      index+=1
    fo.write("</tr>")

#fo.write("</tbody><tfooter><tr>")
fo.write("<tr>")
index=0
for item in totals:
    if index<countcol:
        fo.write("<td></td>")
    else:
      if index in goods:
         fo.write('<td class="good">')
      elif index in bads:
          fo.write('<td class="bad">')
      else:
          fo.write('<td>')
      fo.write(str(item)+"</td>")
    index+=1

fo.write("</tr></tfooter></table></body></html>")
fo.close()
