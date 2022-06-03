#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
import os
import io
import re
from collections import Counter
import string




def get_clean_word(input_string):
    '''
    This function will take a string as input and return another string with pure words(only english letters).
    :param
    input_string: a string
    :return
    a string
    '''

    text = input_string.lower()
    pure_text = re.sub(r"[-()\"#/@;:<>{}\[`%+=\]~|.!_'?,&^$*]", "", text)
    pure_words = re.sub(r'[0-9]+', '', pure_text)
    return pure_words


def get_letter_frequency(input_string):
    '''
    This function will count occurence(upper class and lower class will be counted together) of all the 26 letters respectively.
    :param
    input_string: a string
    :return
    a dictionary about all 26 letters and their occurrence
    '''
    input_string = input_string.lower()
    letter_dic = {'a': 0, 'b': 0, 'c': 0, 'd': 0, 'e': 0, 'f': 0, 'g': 0,
                  'h': 0, 'i': 0, 'j': 0, 'k': 0, 'l': 0, 'm': 0, 'n': 0,
                  'o': 0, 'p': 0, 'q': 0, 'r': 0, 's': 0, 't': 0, 'u': 0,
                  'v': 0, 'w': 0, 'x': 0, 'y': 0, 'z': 0}
    for i in input_string:
        if i in letter_dic.keys():
            letter_dic[i] += 1
    return letter_dic


def get_word_count(input_string):
    '''
    This function will count the number of words in a given string.A 'word' means characters between blank space.
    Multiple blank space will be seen as one blank space.
    :param
    input_string: a string that contains only english words and blank space
    :return:
    number of the words
    :example:
    [in] get_word_count('My name is Wuhao')
    [out] 4
    [in] get_word_count('sdsr dfdt      freg##%^&fr')
    [out] 3
    '''
    newstr = re.sub(r'\s+', ' ', input_string)
    list1 = newstr.split(' ')

    return len(list1)


def get_unique_word(input_string):
    '''
    This function will count the number of words in a given string.A 'word' means characters between blank space.
    Multiple blank space will be seen as one blank space. Upper class and lower class English letters will be treated
    as same thing.
    :param
    input_string: a string contains characters and/or blank space.
    :return:
    number of the words
    :example:
    [in] get_unique_word('He he is     from china!')
    [out] 4
    [in] get_word_count('Wuhao wuhao 43 43!')
    [out] 3
    '''
    input_string = input_string.lower()
    newstr = re.sub(r'\s+', ' ', input_string)
    list1 = newstr.split(' ')
    list1 = set(list1)
    return len(list1)


def get_most_follow(input_string, max_follow, max_common=5):
    '''
    This function will find most common words in the given string(without punctuation or any sign) and also find most
    common words after these most common words.A 'word' means characters between blank space. Multiple blank space
    will be seen as one blank space. Upper class and lower class English letters will be treated as same thing.
    :param input_string: a string
    :param max_common: the number of common words need to be find, default value is 5.
    :param max_follow: the number of most common following words, this parameter will be controlled by max() and min()
    to make sure this parameter can only be 1,2 or 3.
    :return: a dictionary contains (max_common) most common words
    and its (max_follow) most common following words.
    '''
    max_follow = min(max_follow, 3)
    max_follow = max(max_follow, 1)
    input_string = re.sub(r'\s+', ' ', input_string)
    list1 = input_string.split(' ')
    common_count = Counter(list1).most_common(max_common)
    most_common_word = [x[0] for x in common_count]
    list2 = []
    for word in most_common_word:
        pattern = str(word) + ' ' + '(\w+)'
        m = re.compile(pattern)
        all_words = m.findall(input_string)

        top_words = Counter(all_words).most_common(max_follow)

        list2.append(top_words)

    res = dict(zip(common_count, list2))
    return res


def write_to_file(dst, frequency, total_count, unique_count, most_common_follow):
    '''
    This function will write some information into a target file.It will over write the file.
    :param dst: destination file path
    :param frequency: result from function get_letter_frequency
    :param total_count: result from function get_word_count
    :param unique_count: result from function get_unique_word
    :param most_common_follow: result from function get_most_follow
    '''
    with open(str(dst), 'w') as f:
        f.writelines("frequency: ")
        f.writelines("\n")
        for k, v in sorted(frequency.items(), key=lambda x: x[1], reverse=True):
            f.writelines("{0} : {1}".format(k, v))
            f.writelines("\n")
        f.writelines("\n")
        f.writelines("The number of all the words: {0} ".format(total_count))
        f.writelines("\n")
        f.writelines("The number of unique words: {0}".format(unique_count))
        f.writelines("\n")
        for k, v in most_common_follow.items():
            f.writelines("\n")
            f.writelines("{0}({1} occurrences)".format(k[0], k[1]))
            for val in v:
                f.writelines("\n")
                f.writelines("--- {0}, {1}".format(val[0], val[1]))


def run(filename, dst=None):
    if not os.path.isfile(filename):
        sys.exit("The file  {} does not exist!".format(filename))
    with io.open(filename, encoding="utf-8") as f:
        print('start to read the file {}'.format(filename))
        content = f.read()
        print('reading finish')
    cleanword = get_clean_word(content)
    frequency = get_letter_frequency(cleanword)
    total_count = get_word_count(cleanword)
    unique_count = get_unique_word(cleanword)

    print("frequency:")
    for k, v in sorted(frequency.items(), key=lambda x: x[1], reverse=True):
        print("{0} : {1}".format(k, v))

    print("\n")
    print("The number of all the words: ", total_count)
    print("\n")
    print("The number of unique words: ", unique_count)
    print("\n")
    mostFollow = get_most_follow(cleanword, max_follow=3, max_common=5)

    for k, v in mostFollow.items():
        print("{0}({1} occurrences)".format(k[0], k[1]))
        for val in v:
            print("--- {0}, {1}".format(val[0], val[1]))
    if dst is not None:
        print('start to write result to {}'.format(dst))
        write_to_file(dst, frequency, total_count, unique_count, mostFollow)
        print("\n")
        print("Write finish,existing")
    else:
        print("\n")
        print("Exiting")


if __name__ == '__main__':
    param = sys.argv
    if len(param) < 2:
        sys.exit('please enter the input file path')
    if len(param) == 2:
        run(param[1])
    else:
        run(param[1], param[2])
