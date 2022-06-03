import sys
import os
import io
from text_stats import get_clean_word
import random
import re
from itertools import count


def generate_text(filename, start, max_number):
    if not os.path.isfile(filename):
        sys.exit("The file  {} does not exist!".format(filename))
    with io.open(filename, encoding="utf-8") as f:
        if not isinstance(start, str):
            sys.exit('wrong input type.Start should be a string')
        try:
            max_number = int(max_number)
        except:
            sys.exit('max_number should be an integer')
        content = f.read()
    content = get_clean_word(content)
    content = re.sub(r'\s+', ' ', content)
    word_list = content.split(' ')
    length = len(word_list)
    candidate = {}
    for i in count(0, 1):
        if i == length:
            break
        if word_list[i] not in candidate.keys():
            candidate[word_list[i]] = {}
        else:
            if word_list[i + 1] not in candidate[word_list[i]].keys():
                candidate[word_list[i]][word_list[i + 1]] = 1
            else:
                candidate[word_list[i]][word_list[i + 1]] += 1
    it = 0
    cur = start
    res = ''
    while it <= max_number:
        res += (' ' + cur)
        temp = candidate[cur].copy()
        if len(list(temp.keys())) == 0:
            cur = start
        else:
            cur = random.choices(list(temp.keys()), list(temp.values()))[0]
        it += 1
    return res


if __name__ == '__main__':
    param = sys.argv
    if len(param) < 4:
        sys.exit('3 parameters are needed filename,start words and max number of words')
    print(generate_text(param[1], param[2], param[3]))

