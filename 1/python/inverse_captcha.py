import sys

test_cases = [('1122', 3),
              ('1111', 4),
              ('1234', 0),
              ('91212129', 9)
              ]

def read_input(fname):
    with open(fname, 'r') as f:
        while True:
            char = f.read(1)
            if not char or char in ['\n', ' ', '\t']: break
            try:
                yield int(char)
            except:
                print("Invalid non-digit char {}".format(char))
                sys.exit(1)

def calculate_sum(reader):
    accum = 0
    first = current = reader.send(None)
    while True:
        try:
            next_num = reader.send(None)
            if current == next_num:
                accum += current
            current = next_num
        except StopIteration:
            if first == current:
                accum += first
            break
    return accum

def main():
    reader = read_input('../input')
    print(calculate_sum(reader))

def test():
    for case in test_cases:
        gen = (int(c) for c in case[0])
        assert calculate_sum(gen) == case[1]

if __name__=="__main__":
    test()
    main()
