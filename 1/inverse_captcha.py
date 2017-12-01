import sys

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

def check(reader):
    accum = 0
    current = reader.send(None)
    while True:
        try:
            next_num = reader.send(None)
            if current == next_num:
                accum += current
        except StopIteration:
            break
    return accum

def main():
    reader = read_input('input')
    print(check(reader))

if __name__=="__main__":
    main()
