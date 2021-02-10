pretty = {True: 'True ', False: 'False'}
letters = {'P':0,'Q':1,'R':2}

def T(p, q, r):
    return True


def F(p, q, r):
    return False


def P(p, q, r):
    return p


def Q(p, q, r):
    return q


def R(p, q, r):
    return r


def NOT(func1):
    def func(p, q, r):
        return not func1(p, q, r)
    return func


def OR(func1, func2):
    def func(p, q, r):
        return func1(p, q, r) or func2(p, q, r)
    return func


def AND(func1, func2):
    def func(p, q, r):
        return func1(p, q, r) and func2(p, q, r)
    return func


def IMPLIES(func1, func2):
    def func(p, q, r):
        return not (func1(p, q, r) and not func2(p, q, r))
    return func


def CIMPLIES(func1, func2):
    return IMPLIES(func2, func1)


def XOR(func1, func2):
    def func(p, q, r):
        return (func1(p, q, r) and func2(p, q, r)) or (not func1(p, q, r) and not func2(p, q, r))
    return func


def iterTable(func, arr):
    has, ind = [], [iter0, iter1, iter2, iter3]
    for i in ['P','Q','R']:
        if i in arr:
            has.append(i)
    ind[len(has)](func,has)
    print('\n')


def iter0(func, vars):
    print('RESULT')
    print(func(True,True,True))

def iter1(func,vars):
    print(f'   {vars[0]}   :  RESULT')
    inp = [True, True, True]
    inp[letters[vars[0]]] = False
    print(f' True  :  {func(True, True, True)}')
    print(f' False :  {func(inp[0], inp[1], inp[2])}')

def iter2(func, vars):
    print(f'   {vars[0]}       {vars[1]}    :  RESULT')
    inp = [True,True,True]
    for i in range(2):
        inp[letters[vars[1]]] = True
        for j in range(2):
            print(f' {pretty[inp[letters[vars[0]]]]}   {pretty[inp[letters[vars[1]]]]}  '
                  f':  {func(inp[0], inp[1], inp[2])}')
            inp[letters[vars[1]]] = False
        inp[letters[vars[0]]] = False

def iter3(func, vars):
    print('   P       Q       R    :  RESULT')
    for p in [True, False]:
        for q in [True, False]:
            for r in [True, False]:
                print(f' {pretty[p]}   {pretty[q]}   {pretty[r]}  :  {func(p, q, r)}')


wordKeys = {'and': ['&'],
            'or': ['V'],
            'implies': ['I'],
            'true': ['T'],
            'false': ['F'],
            'not': ['!'],
            'nor': ['!','V'],
            'nand': ['!','&']}

unopDict = {'P': P,
            'Q': Q,
            'R': R,
            'T': T,
            'F': F}

biopDict = {'V': OR,
            '&': AND,
            'I': IMPLIES,
            'C': CIMPLIES,
            'X': XOR}


def parse(inp):
    if inp == 'quit':
        quit()
    arr, inp, j = [], inp.split(' '), 0
    while ' ' in inp:
        inp.remove(' ')
    for j in range(len(inp)):
        input = inp[j]
        if input.lower() in wordKeys.keys():
            arr = arr + wordKeys[input.lower()]
        elif input.lower() == 'if':
            try:
                if inp[j+1].lower() == 'and' and inp[j+2].lower() == 'only' and inp[j+3].lower() == 'if':
                    arr.append('X')
                else:
                    print("Unrecognised word order. Try 'if and only if'.")
                    quit()
            except IndexError:
                print("Unrecognised word order. Try 'if and only if'.")
                quit()
        else:
            i = 0
            while i < len(input):
                if input[i].upper() in ['V', '&', 'T', 'F', 'P', 'Q', 'R', '(', ')', '!','X']:
                    arr.append(input[i].upper())
                elif input[i] == '<':
                    if input[i + 1] == '-':
                        if input[i + 2] == '>':
                            arr.append('X')
                            i += 2
                        else:
                            arr.append('C')
                            i += 1
                    else:
                        print(f'Unrecognised character {input[i + 1]} after <, please try <- or <->')
                        quit()
                elif input[i] == '-':
                    if input[i + 1] == '>':
                        arr.append('I')
                        i += 1
                    else:
                        print(f'Unrecognised character {input[i + 1]} after -, please try ->')
                        quit()
                elif input[i] == ' ':
                    pass
                else:
                    print(f'Unrecognised character {input[i]}')
                    quit()
                i += 1
        j += 1
    print()
    return arr

def parenth(arr):
    b, i = 1, 0
    while b > 0:
        if arr[i] == ')': b -= 1
        elif arr[i] == '(': b += 1
        i += 1
    return i

def process(arr, biop2=False):
    nah = 0
    while True:
        temporary = arr.copy()
        for i in range(len(arr)-1):
            if arr[i] == '!' and arr[i+1] == '!':
                arr.pop(i+1)
                arr.pop(i)
                break
        if temporary == arr: break
    if arr.count('(') != arr.count(')'):
        print('Unequal amounts ( and ).')
        quit()
    if arr[0] == '!':
        nah = 1
    if arr[0+nah] == '(':
        par = parenth(arr[1+nah:]) + 1 + nah
        if par == len(arr) and nah:
            return NOT(process(arr[1:]))
        elif par == len(arr):
            return process(arr[1:len(arr)-1])
        elif biop2 and (arr[par] in biopDict.keys() or (arr[par] == '!' and arr[par+1] in biopDict.keys())):
            print("Binary operations can only take 2 statements, you can't lump them like that")
            quit()
        else:
            if arr[par] in biopDict.keys():
                return biopDict[arr[par]](process(arr[:par]),process(arr[(par+1):],True))
            elif arr[par] == '!' and arr[par+1] in biopDict.keys():
                return NOT(biopDict[arr[par+1]](process(arr[:par]),process(arr[(par+2):],True)))
            else:
                print('Invalid Syntax')
                quit()
    elif arr[0+nah] in unopDict.keys():
        if len(arr) == 1 + nah:
            return NOT(unopDict[arr[1]]) if nah else unopDict[arr[0]]
        elif biop2 and (arr[1+nah] in biopDict.keys() or (arr[1+nah] == '!' and arr[2+nah] in biopDict.keys())):
            print("Binary operations can only take 2 statements, you can't lump them like that")
            quit()
        else:
            if arr[1+nah] in biopDict.keys():
                if nah:
                    return biopDict[arr[2]](NOT(unopDict[arr[1]]), process(arr[3:],True))
                else:
                    return biopDict[arr[1]](unopDict[arr[0]], process(arr[2:],True))
            elif arr[1+nah] == '!' and arr[2+nah] in biopDict.keys():
                if nah:
                    return NOT(biopDict[arr[3]](NOT(unopDict[arr[1]]), process(arr[4:],True)))
                else:
                    return NOT(biopDict[arr[2]](unopDict[arr[0]], process(arr[3:],True)))
            else:
                print('Invalid Syntax')
                quit()


print('\n\nType in a logic statement, with variables p,q,r and the following operations\n'
      'Accepted Inputs - meaning\n'
      'T, true       - truth / tautology\n'
      'F, false      - contradiction\n'
      '&, and        - and\n'
      'V, or         - or\n'
      '!, not        - not (you can place this before an operation or variable to negate it eg. !V for nor)\n'
      '->, implies   - implies\n'
      '<-            - converse implies (implication but the other way round)\n'
      'if and only if \n'
      '<->           - if and only if\n'
      '!V, nor       - nor\n'
      '!&, nand      - not and\n'
      'quit          - quit the program\n')

while True:
    a = parse(input('? '))
    b = process(a)
    iterTable(b, a)
