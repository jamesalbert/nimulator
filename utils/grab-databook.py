from bs4 import BeautifulSoup
from requests import get
from re import search
from json import dumps

if __name__ == '__main__':
    instructions = dict()
    res = get('http://www.emulator101.com/8080-by-opcode.html')
    s = BeautifulSoup(res.text, 'html.parser')
    for tr in s.table.find_all('tr'):
        items = list(tr.find_all('td'))
        if not items:
            continue
        opcode = items.pop(0).text.strip()
        instructions[opcode] = dict()
        fields = ('instruction',
                  'size',
                  'flags',
                  'function')
        for item, field in zip(items, fields):
            if field == 'instruction':
                instructions[opcode][field] = item.text.strip().replace(", ", "") or None
            else:
                instructions[opcode][field] = item.text.strip() or None
    print(dumps(instructions))
