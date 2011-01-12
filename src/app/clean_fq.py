#!/usr/bin/python

# Script to remove all entries from FASTQ file that contain a '.' in
# read sequence (unresolved read)
# Generates two new files, one with only "cleaned" reads, and another
# file listing all sequence ids of reads that were found invalid

from Bio import SeqIO
import sys

def append_lines(dest_file, lines) :
    out_handle = open(dest_file, "a")
    out_handle.writelines(lines)
    out_handle.close()

if len(sys.argv) < 2 :
    print "usage: python %s FASTQ file" % sys.argv[0]
    sys.exit(1)

fastq_file = sys.argv[1]
print "USING FASTQ FILE: %s" % fastq_file

import site
from datetime import datetime, date, time
from time import gmtime, strftime
    
exec_tag = datetime.now().strftime("%m%d-%H%M")

fq_flds = fastq_file.split(".")
out_file = fq_flds[0] + "-" + exec_tag + ".fq"
ignore_fl = fq_flds[0] + "-bad-" + exec_tag + ".txt"

print "OUT FILE: %s" % out_file
print "PRE-READ TIMESTAMP:", exec_tag, " FASTQ FILE: ", fastq_file

handle = open(fastq_file, "r")
lines = handle.readlines()  
handle.close()

read_done = datetime.now().strftime("%m%d-%H%M")
print "POST-READ TIMESTAMP:", read_done, " (PRE: ", exec_tag, ") FASTQ FILE: ",fastq_file

#
# Generate new set of FASTQ entries with "." omitted from actual sequence reads
#

clean_set = []

read_id = ""
read_data = ""
in_read = False
in_color = False

is_garbage = False
bad_ids = []
nth_pass = 0

for entry in lines :
    first = entry[0]

    if first == "@" :
        read_id = entry
        in_read = True

    elif first == "+" :
        color_id = entry
        in_color = True

    elif in_read :
        in_read = False
        read_data = entry
        dot_idx = read_data.find(".")

        if dot_idx == -1 :
            continue
        is_garbage = True  #            print "DOT IDX=%d, read: " % dot_idx, read_data

    elif in_color :
        in_color = False
        nth_pass = nth_pass + 1

        if is_garbage :
            is_garbage = False
            bad_ids.append(read_id) #            print "GARBAGE DATA: ", read_id
        else :
            clean_set.append(read_id + read_data + "+\n" + entry)

        if nth_pass >= 100000 :
            nth_pass = 0
            append_lines(ignore_fl, bad_ids)
            append_lines(out_file, clean_set)
            clean_set = []
            print "BAD IDS: ", bad_ids
            bad_ids = []
            sys.stdout.flush() 

append_lines(ignore_fl, bad_ids)
append_lines(out_file, clean_set)



