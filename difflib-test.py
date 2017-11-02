#!/usr/bin/env python
from my_difflib import *
s = SequenceMatcher(None, " abcd", "abcd abcd")
s.find_longest_match(0, 5, 0, 9)
