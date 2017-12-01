import sys

test_cases = [('1212', 6),
              ('1221', 0),
              ('123425', 4),
              ('123123', 12),
              ('12131415', 4)
              ]



def read_input(fname):
    with open(fname, 'r') as f:
        contents = f.read()
    return [int(char) for char in contents.strip()]

def calculate_sum(numbers):
    def check_pair(n1, n2):
        return n1 + n2 if n1 == n2 else 0
    size = len(numbers) / 2
    return sum(check_pair(*t) for t in  zip(numbers[:size], numbers[size:]))

def test():
    for case in test_cases:
        numbers = [int(char) for char in case[0]]
        try:
            assert calculate_sum(numbers) == case[1]
        except:
            print("Expected {} for {}, got {}".format(case[1], case[0], calculate_sum(numbers)))

def main():
    print(calculate_sum(read_input('../input')))

if __name__=='__main__':
    test()
    main()
