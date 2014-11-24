
l1 = [['0', '7'], ['1', '8'], ['2', '9'], ['3'], ['4'], ['5'], ['6']]
l2 = [7, 0, 1, 2, 3, 4, 5, 6]
offsets = [-7, 0, 2, 4, -1, 1, -4, -2]

for (s, offset) in zip(l2, offsets):
    for i, chars in enumerate(l1):
        for char in chars:
            print("(('{0}', {1}), {2}),".format(char, s, (s+offset+i)%7))          
