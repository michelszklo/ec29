#---------------------------------------------------------------------
#--- (0) Formatting function
#---------------------------------------------------------------------
def makeTable(TableName,rows,last):
    """This function formats tables from event study output"""
    with open(TableName, 'w') as fileOut:
        ## Make Table for spending
        year = 1998
        for i,line in enumerate(lines):
            print(line)
            if i==last:
                E = line.split(',')
                estimates = []
                for item in rows:
                    estimate = E[item]
                    estimate = "{:,}".format(int(estimate))
                    estimates.append(estimate)
                    N = '\\\\ Observations &' + '&'.join(estimates) 
                print(N)
                fileOut.writelines(N)
                fileOut.writelines("\n")
            elif i!=0:
                E = line.split(',')
                estimates = []
                for item in rows:
                    estimate = E[item]
                    print(estimate)
                    estimate = estimate.replace('NaNNA','--')
                    estimate = estimate.replace('(0)','--')
                    estimate = estimate.replace('0NA','0')
                    estimates.append(estimate)
                
                if i%2==1:
                    Beta = 'Year = ' + str(year) + '&' + '&'.join(estimates) + r'\\'
                    print(Beta)
                    fileOut.writelines(Beta)
                    fileOut.writelines("\n")
                elif i%2==0:
                    SE = '&' + '&'.join(estimates) + r'\\'
                    if year == 2000:
                        SE = '&(--)'*len(rows)+r'\\'
                    print(SE)
                    fileOut.writelines(SE)
                    fileOut.writelines("\n")
                    year = year+1


#---------------------------------------------------------------------
#--- (0) Single
#---------------------------------------------------------------------
with open('event.csv', 'r') as file:
    lines = file.readlines()
last = len(lines)-1

## Spending                    
rows = [3,8,9,10,11,12,13,14]
makeTable('eventTabs/SpendEvent.tex',rows,last)

## IMR
rows = [59,71,72,73]
makeTable('eventTabs/IMREvent.tex',rows,last)

## Inputs
rows = [75,76,77,78,79,80]
makeTable('eventTabs/InputEvent.tex',rows,last)

## Systems Spillovers
rows = [40,41,42,85]
makeTable('eventTabs/SystemsEvent.tex',rows,last)

## Geographic Spillovers
rows = [89,90,91,86,87,88]
makeTable('eventTabs/SpilloverEvent.tex',rows,last)

## Adults
rows = [93,94,95,97,98,99]
makeTable('eventTabs/AdultEvent.tex',rows,last)


#---------------------------------------------------------------------
#--- (1) Above below
#---------------------------------------------------------------------
with open('eventAB.csv', 'r') as file:
    lines = file.readlines()
last = len(lines)-1


## Spending                    
rows = [5,6,15,16,17,18,19,20]
makeTable('eventTabs/SpendEventAB_1.tex',rows,last)
rows = [21,22,23,24,25,26,27,28]
makeTable('eventTabs/SpendEventAB_2.tex',rows,last)

## IMR
rows = [117,118,141,142,143,144,145,146]
makeTable('eventTabs/IMREventAB.tex',rows,last)

## Inputs
rows = [149,150,151,152,153,154,155,156,157,158,159,160]
makeTable('eventTabs/InputEventAB.tex',rows,last)

## Systems Spillovers
rows = [79,80,81,82,83,84,169,170]
makeTable('eventTabs/SystemsEventAB.tex',rows,last)

## Geographic Spillovers
rows = [177,178,179,180,181,182,171,172,173,174,175,176]
makeTable('eventTabs/SpilloverEventAB.tex',rows,last)

## Adults
rows = [185,186,187,188,189,190,193,194,195,196,197,198]
makeTable('eventTabs/AdultEventAB.tex',rows,last)
