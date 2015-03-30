
f = open('segmentation.data')

for l in f:
    for n in l.split(','):
        print n, float(n)

