from hexdump import hexdump
from json import load


if __name__ == '__main__':
    with open('mario.nes', 'rb') as rom_file:
        hd = hexdump(rom_file, result='return')
    with open('8080-databook.json', 'r') as databook:
        databook = load(databook)
    codes = list()
    for line in hd.split('\n'):
        code_set = line.split(':', 1).pop(1)
        codes += ' '.join(code_set.split('  ')[:-1]).split(' ')[1:17]
    a = list()
    for code in codes:
        a.append(databook['0x{0}'.format(code.lower())]['instruction'])
    with open('out-py.s', 'w+') as outfile:
        outfile.write('\n'.join(a))
