pretty = {True: ' True   ', False: ' False  '}
letters = {'P': 0, 'Q': 1, 'R': 2}
unopDict = {'T': lambda a: True,
            'F': lambda a: False}

NOT = lambda f1: lambda a: not f1(a)
OR = lambda f1, f2: lambda a: f1(a) or f2(a)
AND = lambda f1, f2: lambda a: f1(a) and f2(a)
IMPLIES = lambda f1, f2: lambda a: not (f1(a) and not f2(a))
CIMPLIES = lambda f1, f2: IMPLIES(f2, f1)
XOR = lambda f1, f2: lambda a: (f1(a) and f2(a)) or (not f1(a) and not f2(a))

wordKeys = {'and': ['&'],
            'or': ['V'],
            'implies': ['I'],
            'true': ['T'],
            'false': ['F'],
            'not': ['!'],
            'nor': ['!', 'V'],
            'nand': ['!', '&'],
            'xor': ['X']}

biopDict = {'V': OR,
            '&': AND,
            'I': IMPLIES,
            'C': CIMPLIES,
            'X': XOR,
            '↔': XOR,
            '→': IMPLIES,
            '∨': OR,
            '∧': AND}

def unopInit(vars):
    global unopDict
    unopDict = {'T': lambda a: True, 'F': lambda a: False}
    for i in range(len(vars)):
        if len(vars[i]) != 1:
            print('Has to be one letter only')
            quit()
        elif vars[i].upper() in ['V', '&', '(', ')', '!', 'X']:
            print('Cannot be an operation')
            quit()
        elif vars[i].upper() in ['T', 'F']:
            print("'T' and 'F' already defined")
            quit()
        unopDict[vars[i].upper()] = lambda a, b=i: a[b]

def head(vars):
    return ''.join([f'   {var}    ' for var in vars]) + ':  RESULT' if vars else 'RESULT'

def body(func, vars, i):
    if not vars: return f' {func(None)}'
    varVal = [(i // (2**j)) % 2 == 0 for j in range(len(vars)-1, -1, -1)]
    inputs = [varVal[vars.index(var)] if var in vars else True for var in list(unopDict.keys())[2:]]
    return ''.join([pretty[inp] for inp in varVal]) + f':   {func(inputs)}'

def iterTable(func, opList):
    vars = [var for var in list(unopDict.keys())[2:] if var in opList]
    print(head(vars))
    for i in range(2**len(vars)):
        print(body(func, vars, i))
    print()


def parse(inp):
    if inp.lower() == 'quit':
        quit()
    arr, inp, j = [], inp.split(' '), 0
    while ' ' in inp:
        inp.remove(' ')
    while j < len(inp):
        word = inp[j]
        if word.lower() in wordKeys.keys():
            arr = arr + wordKeys[word.lower()]
        elif word[0] == '(' and word[1:].lower() in wordKeys.keys():
            arr = arr + ['('] + wordKeys[word[1:].lower()]
        elif word.lower() == 'if':
            try:
                if inp[j + 1].lower() == 'and' and inp[j + 2].lower() == 'only' and inp[j + 3].lower() == 'if':
                    arr.append('X')
                    j += 3
                else:
                    print("Unrecognised word order. Try 'if and only if'.")
                    quit()
            except IndexError:
                print("Unrecognised word order. Try 'if and only if'.")
                quit()
        else:
            i = 0
            while i < len(word):
                if word[i].upper() in ['(', ')', '!'] + list(unopDict.keys()) + list(biopDict.keys()):
                    arr.append(word[i].upper())
                elif word[i] == '~':
                    arr.append('!')
                elif word[i] == '<':
                    if word[i + 1] == '-':
                        try:
                            if word[i + 2] == '>':
                                arr.append('X')
                                i += 2
                            else:
                                arr.append('C')
                                i += 1
                        except IndexError:
                            arr.append('C')
                            i += 1
                    else:
                        print(f'Unrecognised character {word[i + 1]} after <, please try <- or <->')
                        quit()
                elif word[i] == '-':
                    if word[i + 1] == '>':
                        arr.append('I')
                        i += 1
                    else:
                        print(f'Unrecognised character {word[i + 1]} after -, please try ->')
                        quit()
                elif word[i] == ' ':
                    pass
                else:
                    print(f'Unrecognised character {word[i]}')
                    quit()
                i += 1
        j += 1
    return arr


def parenth(arr):
    b, i = 1, 0
    while b > 0:
        if arr[i] == ')':
            b -= 1
        elif arr[i] == '(':
            b += 1
        i += 1
    return i


def process(arr, biop2=False):
    nah = 0
    while True:
        temporary = arr.copy()
        for i in range(len(arr) - 1):
            if arr[i] == '!' and arr[i + 1] == '!':
                arr.pop(i + 1)
                arr.pop(i)
                break
        if temporary == arr: break
    if arr.count('(') != arr.count(')'):
        print('Unequal amounts ( and ).')
        quit()
    if arr[0] == '!':
        nah = 1
    if arr[0 + nah] == '(':
        par = parenth(arr[1 + nah:]) + 1 + nah
        if par == len(arr) and nah:
            return NOT(process(arr[1:]))
        elif par == len(arr):
            return process(arr[1:len(arr) - 1])
        elif biop2 and (arr[par] in biopDict.keys() or (arr[par] == '!' and arr[par + 1] in biopDict.keys())):
            print("Binary operations can only take 2 statements, you can't lump them like that")
            quit()
        else:
            if arr[par] in biopDict.keys():
                return biopDict[arr[par]](process(arr[:par]), process(arr[(par + 1):], True))
            elif arr[par] == '!' and arr[par + 1] in biopDict.keys():
                return NOT(biopDict[arr[par + 1]](process(arr[:par]), process(arr[(par + 2):], True)))
            else:
                print('Invalid Syntax')
                quit()
    elif arr[0 + nah] in unopDict.keys():
        if len(arr) == 1 + nah:
            return NOT(unopDict[arr[1]]) if nah else unopDict[arr[0]]
        elif biop2 and (arr[1 + nah] in biopDict.keys() or (arr[1 + nah] == '!' and arr[2 + nah] in biopDict.keys())):
            print("Binary operations can only take 2 statements, you can't lump them like that")
            quit()
        else:
            if arr[1 + nah] in biopDict.keys():
                if nah:
                    return biopDict[arr[2]](NOT(unopDict[arr[1]]), process(arr[3:], True))
                else:
                    return biopDict[arr[1]](unopDict[arr[0]], process(arr[2:], True))
            elif arr[1 + nah] == '!' and arr[2 + nah] in biopDict.keys():
                if nah:
                    return NOT(biopDict[arr[3]](NOT(unopDict[arr[1]]), process(arr[4:], True)))
                else:
                    return NOT(biopDict[arr[2]](unopDict[arr[0]], process(arr[3:], True)))
            else:
                print('Invalid Syntax')
                quit()


print("\n\nWhen prompted 'vars?' please define one-letter variable names separated by spaces\n"
      "When prompted '?' type in a logic statement, with defined variables and following operations:\n"
      'Accepted Inputs - meaning\n'
      'T, true        - truth / tautology\n'
      'F, false       - contradiction\n'
      '&, ∧, and      - and\n'
      'V, ∨, or       - or\n'
      '!, ~, not      - not (you can place this before an operation or variable to negate it eg. !V for nor)\n'
      '->, →, implies - implies\n'
      '<-             - converse implies (implication but the other way round)\n'
      'if and only if \n'
      '<->, ↔, xor    - if and only if\n'
      '!V, nor        - nor\n'
      '!&, nand       - not and\n'
      'redef          - redefine variable names\n'
      'quit           - quit the program\n')

if __name__ == '__main__':
    unopInit(input('vars? ').split())
    while True:
        a = input('? ')
        if a == 'redef':
            unopInit(input('vars? ').split())
            continue
        opList = parse(a)
        func = process(opList)
        iterTable(func, opList)
