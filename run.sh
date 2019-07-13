#!/bin/bash
calc_bounds "abc" <input.pro | sort >temp.pro
diff -q temp.pro golden.pro

